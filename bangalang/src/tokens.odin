package main

import "core:fmt"
import "core:os"
import "core:strings"

tokenize :: proc(src: string) -> [dynamic]string
{
  tokens: [dynamic]string

  for index := 0; index < len(src); index += 1
  {
    if strings.is_space(rune(src[index]))
    {
      // do nothing
    }
    else if src[index] == '('
    {
      append(&tokens, "(")
    }
    else if src[index] == ')'
    {
      append(&tokens, ")")
    }
    else if src[index] == 'e'
    {
      if index > len(src) - 3
      {
        fmt.println("Not enough space for 'exit'")
        os.exit(1)
      }

      if src[index + 1] != 'x' || src[index + 2] != 'i' || src[index + 3] != 't'
      {
        fmt.println("You spelled exit wrong ya dummy")
        os.exit(1)
      }

      append(&tokens, "exit")
      index += 3
    }
    else if (src[index] >= '0' && src[index] <= '9') || src[index] == '-'
    {
      start_index := index
      end_index := index + 1

      for src[end_index] >= '0' && src[end_index] <= '9'
      {
        end_index += 1
      }

      if src[start_index] == '-' && end_index == start_index + 1
      {
        fmt.println("Negative what tho?")
        os.exit(1)
      }

      append(&tokens, src[start_index:end_index])
      index = end_index - 1
    }
    else
    {
      fmt.println("Unrecognised token")
      os.exit(1)
    }
  }

  return tokens
}
