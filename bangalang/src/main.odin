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

  stack_pointer := 0

  stack_vars := make(map[string]int)
  defer delete(stack_vars)

  for index := 0; index < len(tokens);
  {
    #partial switch tokens[index].type
    {
    case .IDENTIFIER:
      if index + 1 >= len(tokens)
      {
        fmt.println("Invalid statement, end of file encountered")
        os.exit(1)
      }

      #partial switch tokens[index + 1].type
      {
      case .EQUALS:
        fmt.fprintln(asm_file, "  ; assign")

        if index + 2 >= len(tokens)
        {
          fmt.println("Invalid statement, end of file encountered")
          os.exit(1)
        }

        if tokens[index + 2].type == .IDENTIFIER
        {
          if !(tokens[index + 2].value in stack_vars)
          {
            fmt.printfln("Undeclared identifier: %s", tokens[index + 2].value)
            os.exit(1)
          }

          var_pointer := stack_vars[tokens[index + 2].value]
          var_offset := stack_pointer - var_pointer
          fmt.fprintfln(asm_file, "  mov rax, [rsp+%i] ; value to register", var_offset)
        }
        else if tokens[index + 2].type == .INTEGER_LITERAL
        {
          fmt.fprintfln(asm_file, "  mov rax, %s ; assign value", tokens[index + 2].value)
        }
        else
        {
          fmt.println("Invalid statement")
          os.exit(1)
        }

        if tokens[index].value in stack_vars
        {
          var_pointer := stack_vars[tokens[index].value]
          var_offset := stack_pointer - var_pointer
          fmt.fprintfln(asm_file, "  mov [rsp+%i], rax ; assign value", var_offset)
        }
        else
        {
          fmt.fprintln(asm_file, "  mov [rsp], rax ; assign value")
          stack_vars[tokens[index].value] = stack_pointer

          fmt.fprintln(asm_file, "  sub rsp, 8 ; allocate space on stack")
          stack_pointer += 8
        }

        index += 3
      case .OPENING_BRACKET:
        fmt.fprintln(asm_file, "  ; exit")

        if index + 3 >= len(tokens)
        {
          fmt.println("Invalid statement, end of file encountered")
          os.exit(1)
        }

        fmt.fprintln(asm_file, "  mov rax, 60 ; syscall: exit")
        if tokens[index + 2].type == .IDENTIFIER && tokens[index + 3].type == .CLOSING_BRACKET
        {
          var_pointer := stack_vars[tokens[index + 2].value]
          var_offset := stack_pointer - var_pointer
          fmt.fprintfln(asm_file, "  mov rdi, [rsp+%i] ; arg0: exit_code", var_offset)
        }
        else if tokens[index + 2].type == .INTEGER_LITERAL && tokens[index + 3].type == .CLOSING_BRACKET
        {
          fmt.fprintfln(asm_file, "  mov rdi, %s ; arg0: exit_code", tokens[index + 2].value)
        }
        else
        {
          fmt.println("Invalid statement")
          os.exit(1)
        }
        fmt.fprintln(asm_file, "  syscall ; call kernel")

        index += 4
      case:
        fmt.println("Invalid statement")
        os.exit(1)
      }
    case:
      fmt.println("Invalid statement")
      os.exit(1)
    }
  }

  fmt.fprintln(asm_file, "  mov rax, 60 ; syscall: exit")
  fmt.fprintln(asm_file, "  mov rdi, 0 ; arg0: exit_code")
  fmt.fprintln(asm_file, "  syscall ; call kernel")
}
