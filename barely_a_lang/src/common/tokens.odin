package common

import "core:fmt"
import "core:strings"

tokenize :: proc(src: ^string) -> (tokens: [dynamic]string, ok: bool)
{
  line_number := 1
  column_number := 1
  for index := 0; index < len(src); index += 1
  {
    if strings.is_space(rune(src[index]))
    {
      if src[index] == '\n'
      {
        line_number += 1
        column_number = 1
      }
      continue
    }
    else if src[index] >= 'A' && src[index] <= 'Z'
    {
      token_start := index
      for token_index := token_start + 1; token_index < len(src); token_index += 1
      {
        if strings.is_space(rune(src[token_index]))
        {
          token, ok := strings.substring(src^, token_start, token_index)
          append(&tokens, token)

          column_number += token_index - token_start
          index = token_index - 1
          break
        }
        else if !(src[token_index] >= 'A' && src[token_index] <= 'Z') && !(src[token_index] >= '0' && src[token_index] <= '9') && src[token_index] != '.' && src[token_index] != ':'
        {
          fmt.printfln("Invalid token, found character %c in keyword at line %i, column %i", src[token_index], line_number, column_number)
          return
        }
      }
    }
    else if (src[index] >= '0' && src[index] <= '9') || src[index] == '-'
    {
      token_start := index
      for token_index := token_start + 1; token_index < len(src); token_index += 1
      {
        if strings.is_space(rune(src[token_index]))
        {
          token, ok := strings.substring(src^, token_start, token_index)
          append(&tokens, token)

          column_number += token_index - token_start
          index = token_index - 1
          break
        }
        else if !(src[token_index] >= '0' && src[token_index] <= '9')
        {
          fmt.printfln("Invalid token, found character %c in integer literal at line %i, column %i", src[token_index], line_number, column_number)
          return
        }
      }
    }
    else if src[index] == '"'
    {
      token_start := index + 1
      for token_index := token_start; token_index < len(src); token_index += 1
      {
        if src[token_index] == '"'
        {
          token, ok := strings.substring(src^, token_start, token_index)
          append(&tokens, token)

          column_number += token_index - token_start
          index = token_index
          break
        }
        else if src[token_index] == '\n'
        {
          fmt.printfln("Invalid token, found character %c in string literal at line %i, column %i", src[token_index], line_number, column_number)
          return
        }
      }
    }

    column_number += 1
  }

  ok = true
  return
}
