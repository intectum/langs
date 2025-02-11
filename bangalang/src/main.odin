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

  src := string(src_data)
  tokens := tokenize(src)

  ast_nodes := parse_program(tokens)
  fmt.println("ast_nodes")
  fmt.println(ast_nodes)

  generate_program("./bin/out.asm", ast_nodes)
}
