# Subprocess Motivation

I recently had a need to run external commands within `C++`. There is no
cross-platform STL support for this at the moment.

## Existing Solutions

I evaluated several existing options for this purpose, and realized they were
all lacking in one way or another.

### Native APIs

The first and obvious choice is to just use whatever the operating system gives
you.

All native solutions are platform-specific, have zero cross-platform support
and require significant boilerplate code to get right.

#### `system`

The simplest way to run a subprocess is to use the `system` function from
`<cstdlib>`. This function takes a command string, invokes the system shell to
execute it, and returns the exit status of the command.

This is a very simple way to run a command, but it has several drawbacks:
- **Security Risks** -- Since `system` invokes the shell, it is vulnerable to
  shell injection attacks if the command string is constructed from untrusted
  input.
- **Limited Control** -- `system` does not provide a way to capture the output
  of the command, redirect input/output, or set environment variables for the
  subprocess.
- **Portability Issues** -- The behavior of `system` can vary between
  different operating systems, making it less portable.

#### `fork+exec`

The common way to spawn a subprocess on POSIX systems is to use a combination
of `fork(2)` and `exec()`. Then, you need to handle pipes and file descriptors
yourself if you want to capture output or provide input.

A common way to spawn a process is to combine `fork` and `exec`. `fork` creates
a new system process with the current executable image and `exec` replaces the
process image of the child with a new executable:

```cpp
auto RunSubprocess() -> int {
  const pid_t pid = fork();
  if (pid == -1) { // -1 means an error.
    throw std::system_error(errno, std::system_category(), "fork error");
  }

  if (pid == 0) { // 0 is given to the child, and the child pid to the parent
    execl("/bin/ls", "ls", "-l", (char *)NULL); // we become ls -l
    exit(1); // exec only returns on error
  }

  int stat_loc;
  const pid_t wait_pid = waitpid(pid, &stat_loc, 0); // wait for child

  if (wait_pid == 0) {
    return 0;
  }

  if (wait_pid == -1) { // error while waiting
    throw std::system_error(errno, std::system_category(), "waitpid error");
  }

  if (WIFEXITED(stat_loc)) { // child exited normally
    return WEXITSTATUS(stat_loc); // child's error code is returned
  }

  if (WIFSIGNALED(stat_loc)) { // child was terminated by a signal
    throw std::runtime_error(std::format("child terminated by signal {}",
                             WTERMSIG(stat_loc)));
  }

  if (WIFSTOPPED(stat_loc)) { // child was stopped by a signal
    throw std::runtime_error(std::format("child stopped by signal {}",
                             WSTOPSIG(stat_loc)));
  }
}
```

As you can imagine, this is quite a bit of code to run a single command. Where
did the output of the `ls -l` command go? How do we capture it? How do we
signal the process? All of these things need to be handled manually.

For example, to talk to the subprocess via pipes, you need to set up the pipes
before the `fork`, then `dup2` the pipe file descriptors to `stdin`, `stdout`
and/or `stderr` in the child process after the `fork` but before the `exec`.
Then, in the parent process, you need to read/write to the pipe file
descriptors as needed. This adds even more boilerplate code and complexity.
This naive Claude-generated example demoes that:

```c
#include <unistd.h>
#include <sys/wait.h>

int pipefd[2];
pipe(pipefd);  // [0] = read end, [1] = write end

pid_t pid = fork();
if (pid == 0) {
  // Child process
  close(pipefd[1]);  // Close write end
  dup2(pipefd[0], STDIN_FILENO);  // Redirect stdin to pipe
  close(pipefd[0]);

  execl("/bin/cat", "cat", nullptr);
  _exit(1);
} else {
  // Parent process
  close(pipefd[0]);  // Close read end
  write(pipefd[1], "Hello child\n", 12);
  close(pipefd[1]);
  waitpid(pid, nullptr, 0);
}
```

#### Windows

Windows has its own complex set of syscalls for process creation and
management, such as `CreateProcess`, `WaitForSingleObject`, and various pipe
functions. These APIs are quite different from POSIX and require a different
approach.

### Boost

Klemens Morgenstern has authored a solution in Boost.Process. While it is a
significant improvement over native APIs, it still has some drawbacks:

- Tons of people find Boost heavy and cumbersome to use.
- `Boost.Process` has a relatively archaic API in the context of modern C++. I
  am unable to currently expand on that.

For detailed documentation, please see
[Boost.Process](https://www.boost.org/doc/libs/1_65_1/doc/html/boost_process/tutorial.html#boost_process.tutorial.starting_a_process).

## Other Environments

Most other environments have built-in support for subprocesses. Support varies
across environments, but is present in some form in most popular languages.

### Rust

Rust is the obvious comparison point for modern C++. The Rust standard library
has built-in support for subprocesses via the `std::process` module.

```rust
use std::process::Command;

fn main() {
  let output = Command::new("ls")
    .arg("-l")
    .output()
    .expect("Failed to execute command");

  if output.status.success() {
    let stdout = String::from_utf8_lossy(&output.stdout);
    println!("Output:\n{}", stdout);
  } else {
    let stderr = String::from_utf8_lossy(&output.stderr);
    eprintln!("Error:\n{}", stderr);
  }
}
```

That's not ideal for C++ (missing exception support, for one), but it's a huge
improvement over native APIs.

### Zig

Zig has built-in support for subprocesses via the `std.process` module.

```zig
const std = @import("std");
const process = std.process;

fn main() !void {
  var gpa = std.heap.GeneralPurposeAllocator(.{}){};
  defer std.debug.assert(!gpa.deinit());
  const allocator = &gpa.allocator;

  var cmd = process.Command.init(allocator, "ls");
  defer cmd.deinit();

  try cmd.arg("-l");

  const result = try cmd.spawnAndWait();
  if (result.exit_code == 0) {
      std.debug.print("Command executed successfully\n", .{});
  } else {
      std.debug.print("Command failed with exit code: {}\n", .{result.exit_code});
  }
}
```

### Python

Python has built-in support for subprocesses via the `subprocess` module.

```python
import subprocess

result = subprocess.run(['ls', '-l'], capture_output=True, text=True)
```

Wow! One line to run a command and capture its output. This is very convenient.

### Java

Java has built-in support for subprocesses via the `ProcessBuilder` class.

```java
import java.io.BufferedReader;
import java.io.InputStreamReader;
import java.io.IOException;

public class SubprocessExample {
  public static void main(String[] args) {
    ProcessBuilder processBuilder = new ProcessBuilder("ls", "-l");
    try {
      Process process = processBuilder.start();
      BufferedReader reader = new BufferedReader(new InputStreamReader(process.getInputStream()));
      String line;
      while ((line = reader.readLine()) != null) {
          System.out.println(line);
      }
      int exitCode = process.waitFor();
      System.out.println("Exited with code: " + exitCode);
    } catch (IOException | InterruptedException e) {
      e.printStackTrace();
    }
  }
}
```

As all things Java, it is quite verbose. I would avoid our implementation
become this verbose.

### Haskell

Haskell has built-in support for subprocesses via the `System.Process` module.

```haskell
import System.Process
main :: IO ()
main = do
    (exitCode, stdout, stderr) <- readProcessWithExitCode "ls" [" -l"] ""
    putStrLn stdout
```

Now that is beautifully concise.

### Go

Even Go has built-in support for subprocesses via the `os/exec` package.

```go
package main

import (
    "fmt"
    "os/exec"
)

func main() {
    cmd := exec.Command("ls", "-l")
    output, err := cmd.CombinedOutput()
    if err != nil {
        fmt.Println("Error:", err)
    }
    fmt.Println(string(output))
}
```

## Previous Work

Morgenstern (the author of Boost.Process) has proposed a [subprocess library
for C++](https://www.open-std.org/jtc1/sc22/wg21/docs/papers/2019/p1750r1.pdf)
to the committee. I briefly interacted with him on the [C++
Slack](https://cppalliance.org/slack/) but he seemed to have abandoned the
effort. The reasons he listed are:

- Push-back
- Jadedness with the state of IO in C++.

## Design Goals

Here are the goals and their justifications:

### Hard Requirements

- **Library-only** -- Merging this into ISO C++ will be significantly more
  difficult if we need to change the language. A library-only solution can be
  easier to swallow and will allow us to iterate and ideate faster.
- **Cross-Platform** support -- We need to have excellent cross-platform
  support for all platforms that C++ supports, really. At a minimum, this
  includes Windows, macOS and Linux. This includes BSD OS' too -- `FreeBSD`,
  `OpenBSD`, `NetBSD`.

### Important Requirements

- **Tier-2 OS Support** -- We should support niche OS' where possible --
  Solaris, AIX, HP-UX.
- **Mobile Operating Systems** -- Ideally, we would ensure support for Android
  and iOS. I suppose that a POSIX implementation will result in these being
  easy wins, but we should try to verify this.

### Random Ideas

- **Tier 3 OS Support** -- QNX, Haiku, Fuchsia
- **Embedded Support** -- Many bare-metal platforms have support for
  process-like abstractions. FreeRTOS tasks are better thought of as threads,
  but I suppose there are RTOS' that might have process abstractions.

## What if we fail?

This is ISO C++ after all, and failure to merge is always a likely outcome. The
nice thing about requiring our solution to be _library-only_ is that even if we
fail, we will end up with a high-quality and robust library that people can
use.
