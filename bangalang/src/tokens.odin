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
  PLUS,
  MINUS,
  ASTERISK,
  BACKSLASH,
  IDENTIFIER,
  INTEGER_LITERAL
}

token :: struct
{
  type: token_type,
  value: string,
  line_number: int,
  column_number: int
}

token_stream :: struct
{
  tokens: []token,
  next_index: int
}

peek_token :: proc(stream: ^token_stream, offset: int = 0) -> token
{
  if stream.next_index + offset >= len(stream.tokens)
  {
    last_token := stream.tokens[len(stream.tokens) - 1]

    fmt.printfln("Unexpected end of file after token at line %i, column %i", last_token.line_number, last_token.column_number)
    os.exit(1)
  }

  return stream.tokens[stream.next_index + offset]
}

next_token :: proc
{
  next_token_any,
  next_token_of_type
}

next_token_any :: proc(stream: ^token_stream) -> token
{
  next_token := peek_token(stream)
  stream.next_index += 1

  return next_token
}

next_token_of_type :: proc(stream: ^token_stream, types: []token_type) -> token
{
  next_token := next_token_any(stream)

  type_found := false
  for type in types
  {
    if type == next_token.type
    {
      type_found = true
      break
    }
  }

  if !type_found
  {
    fmt.printfln("Invalid token at line %i, column %i", next_token.line_number, next_token.column_number)
    fmt.printfln("Expected: %s", types)
    fmt.printfln("Found: %s", next_token.type)
    os.exit(1)
  }

  return next_token
}

tokenize :: proc(src: string) -> (tokens: [dynamic]token)
{
  line_number := 1
  column_number := 1

  for index := 0; index < len(src);
  {
    if strings.is_space(rune(src[index]))
    {
      if src[index] == '\n'
      {
        line_number += 1
        column_number = 1
      }
      else
      {
        column_number += 1
      }

      index += 1
    }
    else if src[index] == '('
    {
      append(&tokens, token { .OPENING_BRACKET, "(", line_number, column_number })

      index += 1
      column_number += 1
    }
    else if src[index] == ')'
    {
      append(&tokens, token { .CLOSING_BRACKET, ")", line_number, column_number })

      index += 1
      column_number += 1
    }
    else if src[index] == '{'
    {
      append(&tokens, token { .OPENING_SQUIGGLY_BRACKET, "{", line_number, column_number })

      index += 1
      column_number += 1
    }
    else if src[index] == '}'
    {
      append(&tokens, token { .CLOSING_SQUIGGLY_BRACKET, "}", line_number, column_number })

      index += 1
      column_number += 1
    }
    else if src[index] == ':'
    {
      append(&tokens, token { .COLON, ":", line_number, column_number })

      index += 1
      column_number += 1
    }
    else if src[index] == '='
    {
      append(&tokens, token { .EQUALS, "=", line_number, column_number })

      index += 1
      column_number += 1
    }
    else if src[index] == '+'
    {
      append(&tokens, token { .PLUS, "+", line_number, column_number })

      index += 1
      column_number += 1
    }
    else if src[index] == '-'
    {
      append(&tokens, token { .MINUS, "-", line_number, column_number })

      index += 1
      column_number += 1
    }
    else if src[index] == '*'
    {
      append(&tokens, token { .ASTERISK, "*", line_number, column_number })

      index += 1
      column_number += 1
    }
    else if src[index] == '/'
    {
      append(&tokens, token { .BACKSLASH, "/", line_number, column_number })

      index += 1
      column_number += 1
    }
    else if (src[index] >= 'a' && src[index] <= 'z') || (src[index] >= 'A' && src[index] <= 'Z') || src[index] == '_'
    {
      start_index := index
      end_index := index + 1

      for (src[end_index] >= 'a' && src[end_index] <= 'z') || (src[end_index] >= 'A' && src[end_index] <= 'Z') || src[end_index] == '_' || (src[end_index] >= '0' && src[end_index] <= '9')
      {
        end_index += 1
      }

      append(&tokens, token { .IDENTIFIER, src[start_index:end_index], line_number, column_number })

      index = end_index
      column_number += end_index - start_index
    }
    else if (src[index] >= '0' && src[index] <= '9')
    {
      start_index := index
      end_index := index + 1

      for src[end_index] >= '0' && src[end_index] <= '9'
      {
        end_index += 1
      }

      append(&tokens, token { .INTEGER_LITERAL, src[start_index:end_index], line_number, column_number })

      index = end_index
      column_number += end_index - start_index
    }
    else
    {
      fmt.printfln("Invalid token at line %i, column %i", line_number, column_number)
      os.exit(1)
    }
  }

  return
}
