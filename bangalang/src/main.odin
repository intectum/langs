package main

import "core:fmt"
import "core:os"

main :: proc()
{
  src_data, read_ok := os.read_entire_file("./examples/example_01.bang")
  if !read_ok
  {
    fmt.println("Failed to read src file")
    os.exit(1)
  }

  asm_file, asm_file_error := os.open("./bin/out.asm", os.O_CREATE | os.O_WRONLY | os.O_TRUNC, 0o666)
  if asm_file_error != nil
  {
    fmt.println("Failed to open asm file")
    os.exit(1)
  }
  defer os.close(asm_file)

  src := string(src_data)
  tokens := tokenize(src)

  fmt.fprintln(asm_file, "global _start")
  fmt.fprintln(asm_file, "_start:")

  if tokens[0] == "exit" && tokens[1] == "(" && tokens[3] == ")"
  {
    fmt.fprintln(asm_file, "  mov rax, 60 ; syscall: exit")
    fmt.fprintfln(asm_file, "  mov rdi, %s ; arg0: exit_code", tokens[2])
    fmt.fprintln(asm_file, "  syscall ; call kernel")
  }

  fmt.fprintln(asm_file, "  mov rax, 60 ; syscall: exit")
  fmt.fprintln(asm_file, "  mov rdi, 0 ; arg0: exit_code")
  fmt.fprintln(asm_file, "  syscall ; call kernel")
}
