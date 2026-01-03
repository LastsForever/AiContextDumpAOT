open System
open System.IO
open System.Text
open System.Text.Json
open System.Text.RegularExpressions
open System.Diagnostics
open System.Runtime.InteropServices

// ==========================================
// 1. 基础辅助与 Null 安全处理
// ==========================================
module NullUtils =
    let inline strToOpt (s: string | null) : string option = 
        match s with
        | null -> None
        | v -> Some v

    let inline procToOpt (p: Process | null) : Process option = 
        match p with
        | null -> None
        | v -> Some v
    
    let safeDirName (path: string) : string =
        match Path.GetDirectoryName(path) with
        | null -> ""
        | s -> s

    let safeFileName (path: string) : string =
        match Path.GetFileName(path) with
        | null -> "" 
        | s -> s

    let safeExtension (path: string) : string =
        match Path.GetExtension(path) with
        | null -> ""
        | s -> s

module Util =
    let toPosix (s: string) = s.Replace('\\', '/')
    
    let normExt (s: string) =
        let t = s.Trim().ToLowerInvariant()
        if String.IsNullOrEmpty t then ""
        elif t.StartsWith "." then t 
        else "." + t

    let globToRegex (pattern: string) =
        let sb = StringBuilder("^")
        pattern |> Seq.iter (fun c ->
            match c with
            | '*' -> sb.Append(".*") |> ignore
            | '?' -> sb.Append(".") |> ignore
            | '.' | '(' | ')' | '+' | '|' | '^' | '$' | '@' | '%' -> sb.Append("\\").Append(c) |> ignore
            | _ -> sb.Append(c) |> ignore
        )
        sb.Append("$").ToString()

// ==========================================
// 2. 配置模型
// ==========================================
type OutputConfig =
    { Mode: string
      SingleFile: string
      StructureFile: string
      CodeFile: string
      PathStyle: string }

type ClipboardConfig =
    { Enabled: bool; Text: string }

type IgnoreConfig =
    { Extensions: Set<string>
      Patterns: string array
      DirRules: string array }

type Settings =
    { IterRoot: string
      OsStyle: string
      Output: OutputConfig
      Clipboard: ClipboardConfig
      Ignore: IgnoreConfig }

// ==========================================
// 3. 配置加载 (JSON)
// ==========================================
module JsonConfig =
    let private tryProp (name: string) (el: JsonElement) : JsonElement option =
        let mutable v = Unchecked.defaultof<JsonElement>
        if el.TryGetProperty(name, &v) then Some v else None

    let private getStr (def: string) (v: JsonElement option) : string =
        v 
        |> Option.bind (fun x -> 
            if x.ValueKind = JsonValueKind.String then NullUtils.strToOpt (x.GetString()) 
            else None)
        |> Option.defaultValue def

    let private getBool (def: bool) (v: JsonElement option) : bool =
        match v with
        | Some x -> x.GetBoolean()
        | None -> def

    let private getArrayStr (v: JsonElement option) : string array =
        match v with
        | Some x when x.ValueKind = JsonValueKind.Array ->
            x.EnumerateArray()
            |> Seq.choose (fun it -> 
                if it.ValueKind = JsonValueKind.String then NullUtils.strToOpt (it.GetString()) 
                else None)
            |> Seq.toArray
        | _ -> [||]

    let load (path: string) =
        let lines = File.ReadAllLines(path, Encoding.UTF8)
        let json =
            lines
            |> Array.filter (fun l -> 
                let t = l.TrimStart()
                not (t.StartsWith "//" || t.StartsWith "#"))
            |> String.concat "\n"
        
        use doc = JsonDocument.Parse(json)
        let root = doc.RootElement

        let iterRoot = 
            tryProp "iter_root" root 
            |> Option.orElseWith (fun () -> tryProp "root" root)
            |> getStr "." 
            |> Path.GetFullPath

        let osStyle = tryProp "os" root |> getStr "auto" |> fun s -> s.Trim().ToLowerInvariant()

        let outEl = tryProp "output" root
        let output = 
            { Mode = outEl |> Option.bind (tryProp "mode") |> getStr "both" |> fun s -> s.ToLowerInvariant()
              SingleFile = outEl |> Option.bind (tryProp "single_file") |> getStr "structure&code.txt"
              StructureFile = outEl |> Option.bind (tryProp "structure_file") |> getStr "structure.txt"
              CodeFile = outEl |> Option.bind (tryProp "code_file") |> getStr "code.txt"
              PathStyle = outEl |> Option.bind (tryProp "path_style") |> getStr "relative" |> fun s -> s.ToLowerInvariant() }

        let cbEl = tryProp "clipboard" root
        let clipboard = 
            { Enabled = cbEl |> Option.bind (tryProp "enabled") |> getBool false
              Text = cbEl |> Option.bind (tryProp "text") |> getStr "" }

        let igEl = tryProp "ignore" root
        let patterns = igEl |> Option.bind (tryProp "patterns") |> getArrayStr
        let dirRules =
            patterns
            |> Array.choose (fun p ->
                let p2 = Util.toPosix (p.Trim())
                if p2.EndsWith("/*") then Some(p2.Substring(0, p2.Length - 2))
                elif p2.EndsWith("/") then Some(p2.Substring(0, p2.Length - 1))
                else None)
        let extensions =
            igEl |> Option.bind (tryProp "extensions") |> getArrayStr
            |> Array.map Util.normExt
            |> Set.ofArray

        { IterRoot = iterRoot
          OsStyle = osStyle
          Output = output
          Clipboard = clipboard
          Ignore = { Extensions = extensions; Patterns = patterns; DirRules = dirRules } }

// ==========================================
// 4. 核心逻辑: 忽略匹配与遍历
// ==========================================
module Core =
    let matchPattern (relPosix: string) (parts: string array) (basename: string) (pat: string) =
        let rp = relPosix.ToLowerInvariant()
        let bn = basename.ToLowerInvariant()
        let pt = Util.toPosix(pat).ToLowerInvariant()
        let regex = Regex(Util.globToRegex pt, RegexOptions.IgnoreCase)

        if pt.Contains("/") then
            regex.IsMatch(rp)
        else
            if regex.IsMatch(bn) then true
            else parts |> Array.exists (fun seg -> regex.IsMatch(seg.ToLowerInvariant()))

    let isPrunedDir (dirname: string) (s: Settings) =
        let dn = Util.toPosix(dirname).ToLowerInvariant()
        s.Ignore.DirRules |> Array.exists (fun rule -> 
            let ruleRegex = Regex(Util.globToRegex (rule.ToLowerInvariant()), RegexOptions.IgnoreCase)
            ruleRegex.IsMatch(dn)
        )

    let isIgnoredPath (iterRoot: string) (p: string) (s: Settings) (settingsFilename: string) =
        let name = NullUtils.safeFileName p
        if name = settingsFilename then true
        else
            let rel = Path.GetRelativePath(iterRoot, p)
            let relPosix = Util.toPosix(rel)
            let parts = relPosix.Split('/')
            
            let isDir = Directory.Exists(p)
            if not isDir then
                let ext = Util.normExt(NullUtils.safeExtension p)
                if s.Ignore.Extensions.Contains(ext) then true
                else s.Ignore.Patterns |> Array.exists (matchPattern relPosix parts name)
            else
                s.Ignore.Patterns |> Array.exists (matchPattern relPosix parts name)

    let collect (iterRoot: string) (s: Settings) (settingsFilename: string) =
        let allDirs = ResizeArray<string>()
        let allFiles = ResizeArray<string>()
        let iterRootIgnored = isIgnoredPath (NullUtils.safeDirName iterRoot) iterRoot s settingsFilename

        let rec walk (currentDir: string) =
            let subDirs = try Directory.GetDirectories(currentDir) with _ -> [||]
            let validSubDirs = subDirs |> Array.filter (fun d -> not (isPrunedDir (NullUtils.safeFileName d) s))
            
            for fullPath in validSubDirs do
                if not (isIgnoredPath iterRoot fullPath s settingsFilename) then 
                    allDirs.Add(fullPath)
                    walk fullPath 

            let files = try Directory.GetFiles(currentDir) with _ -> [||]
            for f in files do
                if not (isIgnoredPath iterRoot f s settingsFilename) then
                    allFiles.Add(f)

        if not iterRootIgnored then walk iterRoot
        
        let sortedFiles = 
            allFiles.ToArray()
            |> Array.sortWith (fun a b -> 
                let ra = Util.toPosix(Path.GetRelativePath(iterRoot, a)).ToLowerInvariant()
                let rb = Util.toPosix(Path.GetRelativePath(iterRoot, b)).ToLowerInvariant()
                String.Compare(ra, rb, StringComparison.Ordinal)
            )
        Set.ofSeq allDirs, sortedFiles, iterRootIgnored

// ==========================================
// 5. 树形结构生成 (Structure) - 仅显示非空路径
// ==========================================
module Structure = 
    let formatRel (root: string) (p: string) (osStyle: string) =
        let rel = Path.GetRelativePath(root, p)
        match osStyle with
        | "windows" -> rel.Replace('/', '\\')
        | "posix" -> rel.Replace('\\', '/')
        | _ -> rel 

    /// 核心逻辑：基于文件列表逆向构建目录树索引
    let buildIndex (iterRoot: string) (files: string array) =
        // 使用显式类型标注以适配 AOT
        let idx = new System.Collections.Generic.Dictionary<string, ResizeArray<string>>()
        let fullRoot = Path.GetFullPath(iterRoot).TrimEnd(Path.DirectorySeparatorChar, Path.AltDirectorySeparatorChar)
        
        let addPathToMap (filePath: string) =
            let mutable currentChild = Path.GetFullPath(filePath).TrimEnd(Path.DirectorySeparatorChar, Path.AltDirectorySeparatorChar)
            let mutable hasReachedRoot = false
            
            while not hasReachedRoot do
                let parent = Path.GetDirectoryName(currentChild)
                match parent with
                | null -> hasReachedRoot <- true
                | pStr ->
                    let parentFull = pStr.TrimEnd(Path.DirectorySeparatorChar, Path.AltDirectorySeparatorChar)
                    
                    // 确保父节点的容器存在
                    if not (idx.ContainsKey(parentFull)) then 
                        idx.[parentFull] <- ResizeArray<string>()
                    
                    // 如果该子级（文件或文件夹）尚未注册到父节点下，则添加
                    if not (idx.[parentFull].Contains(currentChild)) then 
                        idx.[parentFull].Add(currentChild)
                    
                    // 向上追溯：如果当前父节点已经是根目录或更高，则停止
                    if parentFull.Equals(fullRoot, StringComparison.OrdinalIgnoreCase) || parentFull.Length <= fullRoot.Length then 
                        hasReachedRoot <- true
                    else 
                        currentChild <- parentFull

        // 只通过文件来决定哪些文件夹是“存活”的
        files |> Array.iter addPathToMap

        // 对每一层进行排序：文件夹排在文件前面
        for KeyValue(_, children) in idx do
            children.Sort(fun a b ->
                let aIsDir = Directory.Exists(a)
                let bIsDir = Directory.Exists(b)
                if aIsDir && not bIsDir then -1
                elif not aIsDir && bIsDir then 1
                else String.Compare(NullUtils.safeFileName(a).ToLowerInvariant(), NullUtils.safeFileName(b).ToLowerInvariant(), StringComparison.Ordinal)
            )
        idx

    let render (iterRoot: string) (idx: System.Collections.Generic.Dictionary<string, ResizeArray<string>>) : string array =
        let lines = ResizeArray<string>()
        lines.Add("## Project structure\n\n")
        
        let fullRoot = Path.GetFullPath(iterRoot).TrimEnd(Path.DirectorySeparatorChar, Path.AltDirectorySeparatorChar)
        
        // 只有当存在有效文件时才渲染树
        if idx.Count > 0 then
            lines.Add(NullUtils.safeFileName(fullRoot) + "/\n")

            let rec recRender (node: string) (depth: int) =
                let nodeFull = Path.GetFullPath(node).TrimEnd(Path.DirectorySeparatorChar, Path.AltDirectorySeparatorChar)
                if idx.ContainsKey(nodeFull) then
                    for child in idx.[nodeFull] do
                        let indent = String.replicate depth "  "
                        let isDir = Directory.Exists(child)
                        let suffix = if isDir then "/" else ""
                        lines.Add(sprintf "%s%s%s\n" indent (NullUtils.safeFileName child) suffix)
                        if isDir then recRender child (depth + 1)
            
            recRender fullRoot 1
        else
            lines.Add("[No files matching the criteria]\n")
        
        lines.Add("\n")
        lines.ToArray()

// ==========================================
// 6. 文件内容处理
// ==========================================
module FileDump =
    let dumpFile (iterRoot: string) (s: Settings) (file: string) : string =
        let rel = Structure.formatRel iterRoot file s.OsStyle
        let pathDisplay = if s.Output.PathStyle = "absolute" then Path.GetFullPath(file) else rel
        let header = sprintf "//\n//\t# File Path: %s #\n//\n\n" pathDisplay
        let content = try File.ReadAllText(file, Encoding.UTF8) with _ -> "// [Skipped: unreadable or binary file]\n"
        header + content + "\n\n"

// ==========================================
// 7. 系统交互 (IO Boundary)
// ==========================================
module IO =
    let writeLines (path: string) (lines: string array) =
        let d = NullUtils.safeDirName path
        if not (String.IsNullOrEmpty d) then Directory.CreateDirectory(d) |> ignore
        File.WriteAllLines(path, lines, Encoding.UTF8)

    let copyToClipboard (text: string) : bool =
        try
            let psi =
                if RuntimeInformation.IsOSPlatform OSPlatform.Windows then ProcessStartInfo("cmd.exe", "/c clip", RedirectStandardInput=true, UseShellExecute=false)
                elif RuntimeInformation.IsOSPlatform OSPlatform.OSX then ProcessStartInfo("pbcopy", "", RedirectStandardInput=true, UseShellExecute=false)
                else ProcessStartInfo("xclip", "-selection clipboard", RedirectStandardInput=true, UseShellExecute=false)
            match Process.Start(psi) |> NullUtils.procToOpt with
            | Some p -> 
                p.StandardInput.Write(text)
                p.StandardInput.Close()
                p.WaitForExit()
                p.ExitCode = 0
            | None -> false
        with _ -> false

// ==========================================
// 8. 应用程序逻辑 (Orchestration)
// ==========================================
module App =
    // 简单的 Token 估算函数
    let estimateTokens (text: string) =
        let count = float text.Length / 4.0
        int (System.Math.Ceiling(count))

    let run (args: string array) : int =
        let settingsFile = if args.Length > 0 then args.[0] else "settings.jsonc"
        let settingsPath = Path.GetFullPath(settingsFile)
        
        if not (File.Exists settingsPath) then
            Console.Error.WriteLine(sprintf "Settings file not found: %s" settingsPath)
            2
        else
            let s = JsonConfig.load settingsPath
            let settingsFilename = NullUtils.safeFileName settingsPath
            
            let _, files, iterRootIgnored = Core.collect s.IterRoot s settingsFilename

            if iterRootIgnored then
                Console.Error.WriteLine(sprintf "Error: Iteration root '%s' is ignored." s.IterRoot)
                3
            else
                let idx = Structure.buildIndex s.IterRoot files
                let structLines: string array = Structure.render s.IterRoot idx
                
                let rawContents = files |> Array.Parallel.map (FileDump.dumpFile s.IterRoot s)
                let codeContents = rawContents |> Array.choose NullUtils.strToOpt
                let allCodeLines: string array = Array.append [| "## Files\n\n" |] codeContents

                // 存储生成结果用于统计
                let reportData = ResizeArray<string * string>()

                let safeWrite (fileName: string) (content: string array) =
                    if not (String.IsNullOrEmpty fileName) then
                        let fullOut = Path.Combine(AppContext.BaseDirectory, fileName)
                        let fullText = String.Concat(content)
                        IO.writeLines fullOut content
                        reportData.Add(fileName, fullText)

                // --- 写入逻辑 ---
                match s.Output.Mode with
                | "structure" -> safeWrite s.Output.SingleFile structLines
                | "code" -> safeWrite s.Output.SingleFile allCodeLines
                | "split" ->
                    safeWrite s.Output.StructureFile structLines
                    safeWrite s.Output.CodeFile allCodeLines
                | _ -> 
                    let combined = Array.concat [| structLines; allCodeLines |]
                    safeWrite s.Output.SingleFile combined

                // --- 剪贴板逻辑 ---
                if s.Clipboard.Enabled && not (String.IsNullOrWhiteSpace s.Clipboard.Text) then
                    IO.copyToClipboard s.Clipboard.Text |> ignore

                // --- 控制台简洁输出 ---
                Console.WriteLine "================================  ai-context-dump ================================"
                Console.WriteLine "Generated files report:\n"
                for (name, content) in reportData do 
                    let charCount = content.Length
                    let tokens = estimateTokens content
                    Console.WriteLine $" - Path: {name}"
                    Console.WriteLine $"   Chars: {charCount}"
                    Console.WriteLine $"   Tokens: ~{tokens}"
                
                Console.WriteLine "ai-context-dump finished successfully."
                Console.WriteLine "=================================================================================="
                0

// ==========================================
// 9. 入口点
// ==========================================
[<EntryPoint>]
let main argv =
    App.run argv