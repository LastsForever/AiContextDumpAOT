# AI Context Dump AOT

[English](#english) | [中文](#chinese)

<a name="english"></a>

## English

**AI Context Dump AOT** is a high-performance, native tool written in F# using **.NET 10 Ahead-of-Time (AOT)** compilation. It dumps a project’s directory structure and source code into an AI-friendly text format.

### Why F# AOT Version?

Compared to the original Python version, this version offers significant advantages:

* **Blazing Fast**: Compiled to native machine code via .NET 10 AOT. No interpreter overhead, significantly faster directory walking and file processing.
* **Zero Dependencies**: It is a single standalone binary. You don't need Python or even the .NET Runtime installed on the target machine.
* **Low Memory Footprint**: AOT-compiled binaries are highly optimized for minimal resource usage.
* **Concurrency**: Leverages F#'s efficient parallel processing to handle large-scale codebases.

---

### Requirements

* **To Run**: No dependencies! Just the executable and a `settings.jsonc`.
* **To Compile**: .NET 10 SDK (if you wish to build from source).

---

### Quick Start

1. **Download**: Get the pre-compiled binary for your platform (Windows/macOS/Linux) from our **Release** page.
2. **Setup**: Place the binary in a folder with your `settings.jsonc`.
3. **Execution**:

#### **Windows**

Open PowerShell or CMD:

```powershell
.\AiContextDumpAOT.exe

```


```

#### **macOS / Linux** (Will be added in future.)

Open Terminal (ensure executable permissions):

```bash
chmod +x AiContextDumpAOT
./AiContextDumpAOT

```

---

### Self-Compilation (Build from Source)

If you have the .NET 10 SDK installed:

```bash
# Publish for your specific platform with AOT optimization
dotnet publish -c Release -r win-x64 --self-contained
# Or for macOS/Linux
dotnet publish -c Release -r osx-arm64 --self-contained
dotnet publish -c Release -r linux-x64 --self-contained

```

The output will be a single executable in the `publish` folder.

---

### Settings (`settings.jsonc`)

**IMPORTANT**: The tool MUST be used in conjunction with `settings.jsonc`. The default file name is `settings.jsonc`.

The settings file supports JSON with full-line comments. Lines starting with `//` or `#` are ignored.

#### iter_root (or root)

* `iter_root`: traversal root directory (relative or absolute)
Backward compatibility: if `iter_root` is missing, the tool falls back to `root`.

#### os

Controls how file paths are rendered in the output:

* `auto`: current OS style
* `posix`: forward slashes `/`
* `windows`: backslashes `\`
**Note:** this affects path DISPLAY only (headers and console output), not traversal or matching.

#### output

* `output.mode`: `structure | code | both | split`
* `output.single_file`: used by `structure | code | both`
* `output.structure_file` and `output.code_file`: used by `split`
* `output.path_style`: controls whether displayed paths are relative or absolute:
* `relative`:
* in file content headers (`// # File Path:`), paths are relative to `iter_root`
* in console output (generated file list), paths are relative to the script directory


* `absolute`: use absolute paths



**Note:** Output files are always written relative to the directory containing the executable.

#### clipboard

* `clipboard.enabled`: if true, try to copy `clipboard.text` to the clipboard after completion.
* `clipboard.text`: the prompt text to copy.

#### ignore

* `ignore.extensions`: ignore by file extension (case-insensitive; extensions are normalized to lowercase and forced to start with `.`)
* `ignore.patterns`: ignore by glob patterns (fnmatch-style)

**Pattern matching behavior**

* If a pattern contains `/`, it is matched against the full relative path (POSIX-form, relative to `iter_root`).
* Otherwise it is matched against:
* the basename, and
* any path segment (directory or file name)
Matching is case-insensitive.



**Directory pruning behavior**
Directory pruning rules are derived ONLY from patterns that end with:

* `xxx/`
* `xxx/*`
These derived rules are used to prune directory traversal.

---

<a name="chinese"></a>

## 中文 (Chinese)

**AI Context Dump AOT** 是一款使用 F# 开发，并基于 **.NET 10 Ahead-of-Time (AOT)** 技术编译的高性能原生工具。它可以将项目的目录结构和源代码转储为 AI 友好的文本文件。

### 为什么选择 F# AOT 版本？

相比于原生的 Python 版本，此版本具有以下显著优势：

* **极速性能**：通过 .NET 10 AOT 编译为原生机器码。无解释器开销，在处理大规模代码库时，目录遍历和文件读取速度极快。
* **零依赖运行**：生成的是独立的可执行文件。目标机器无需安装 Python 环境，甚至不需要安装 .NET Runtime。
* **极低内存占用**：AOT 编译对二进制文件进行了高度优化，运行时内存占用极小。
* **高效并行**：利用 F# 的异步并行特性，极大提升了文件处理效率。

---

### 环境要求

* **运行**：零依赖！只需可执行文件和 `settings.jsonc` 配置文件。
* **编译**：如果你想自行编译，需要安装 .NET 10 SDK。

---

### 快速开始

1. **下载**：从 **Release** 页面下载对应你平台的二进制文件（Windows/macOS/Linux）。
2. **配置**：将下载的可执行文件放置在包含 `settings.jsonc` 的文件夹中。
3. **运行**：

#### **Windows 平台**

打开 PowerShell 或 CMD：

```powershell
.\AiContextDumpAOT.exe

```

#### **macOS / Linux 平台**

打开终端（需确保可执行权限）：

```bash
# 赋予执行权限
chmod +x AiContextDumpAOT
# 运行
./AiContextDumpAOT

```

---

### 自行编译 (Build from Source)

如果你安装了 .NET 10 SDK，可以运行以下命令：

```bash
# 为特定平台发布 AOT 优化版本
dotnet publish -c Release -r win-x64 --self-contained
# macOS (Apple Silicon)
dotnet publish -c Release -r osx-arm64 --self-contained
# Linux
dotnet publish -c Release -r linux-x64 --self-contained

```

编译后的独立可执行文件将生成在 `publish` 目录下。

---

### 配置说明 (`settings.jsonc`)

**特别注意**：本工具必须配合 `settings.jsonc` 使用。

配置文件支持带注释的 JSON（整行注释）。以 `//` 或 `#` 开头的行将被忽略。

#### iter_root (或 root)

* `iter_root`: 遍历的根目录（相对路径或绝对路径）
向后兼容：如果缺少 `iter_root`，工具将退而查找 `root`。

#### os

控制输出中文件路径的显示风格：

* `auto`: 当前操作系统风格
* `posix`: 正斜杠 `/`
* `windows`: 反斜杠 `\`
**注意：** 仅影响路径的“显示”（文件头和控制台输出），不影响内部匹配。

#### output

* `output.mode`: `structure | code | both | split`
* `output.single_file`: 用于 `structure | code | both` 模式
* `output.structure_file` 和 `output.code_file`: 用于 `split` 模式
* `output.path_style`: 控制显示的路径是相对路径还是绝对路径：
* `relative`:
* 在文件内容头部 (`// # File Path:`)，路径相对于 `iter_root`
* 在控制台输出中，路径相对于程序所在目录


* `absolute`: 使用绝对路径



**注意：** 输出文件始终生成在可执行文件所在的目录下。

#### clipboard

* `clipboard.enabled`: 如果为 true，生成完成后尝试将 `clipboard.text` 拷贝至剪贴板。
* `clipboard.text`: 预定义的 Prompt 文本。

#### ignore

* `ignore.extensions`: 按扩展名忽略（不区分大小写；扩展名会被统一处理为小写并强制以 `.` 开头）
* `ignore.patterns`: 使用 Glob 模式忽略

**模式匹配行为**

* 如果模式包含 `/`，则匹配相对路径（POSIX 形式，相对于 `iter_root`）。
* 否则匹配：
* 文件/文件夹名（basename）
* 路径中的任何一段
匹配不区分大小写。



**目录剪枝行为**
仅当模式以以下形式结尾时，才会生成目录剪枝规则：

* `xxx/`
* `xxx/*`
这些规则用于在遍历时直接跳过特定目录，从而提高效率。

---

## License

MIT