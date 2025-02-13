package main

import "core:fmt"
import "core:os"
import "core:strings"

token_type :: enum
{
  OPENING_BRACKET,
  CLOSING_BRACKET,
  OPENING_SQUIGGLY_BRACKET,
  CLOSING_SQUIGGLY_BRACKET,
  COLON,
  EQUALS,
  IDENTIFIER,
  INTEGER_LITERAL
}

token :: struct
{
  type: token_type,
  value: string
}

tokenize :: proc(src: string) -> (tokens: [dynamic]token)
{
  for index := 0; index < len(src); index += 1
  {
    if strings.is_space(rune(src[index]))
    {
      // do nothing
    }
    else if src[index] == '('
    {
      append(&tokens, token { .OPENING_BRACKET, "(" })
    }
    else if src[index] == ')'
    {
      append(&tokens, token { .CLOSING_BRACKET, ")" })
    }
    else if src[index] == '{'
    {
      append(&tokens, token { .OPENING_SQUIGGLY_BRACKET, "{" })
    }
    else if src[index] == '}'
    {
      append(&tokens, token { .CLOSING_SQUIGGLY_BRACKET, "}" })
    }
    else if src[index] == ':'
    {
      append(&tokens, token { .COLON, ":" })
    }
    else if src[index] == '='
    {
      append(&tokens, token { .EQUALS, "=" })
    }
    else if (src[index] >= 'a' && src[index] <= 'z') || (src[index] >= 'A' && src[index] <= 'Z') || src[index] == '_'
    {
      start_index := index
      end_index := index + 1

      for (src[end_index] >= 'a' && src[end_index] <= 'z') || (src[end_index] >= 'A' && src[end_index] <= 'Z') || src[end_index] == '_' || (src[end_index] >= '0' && src[end_index] <= '9')
      {
        end_index += 1
      }

      append(&tokens, token { .IDENTIFIER, src[start_index:end_index] })
      index = end_index - 1
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

      append(&tokens, token { .INTEGER_LITERAL, src[start_index:end_index] })
      index = end_index - 1
    }
    else
    {
      fmt.println("Unrecognised token")
      os.exit(1)
    }
  }

  return
}
