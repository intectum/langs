package main

import "core:fmt"
import "core:os"
import "core:strings"
import "core:strconv"

main :: proc() {
    if len(os.args) == 1 {
        fmt.println("Usage: barelyai <file>")
        os.exit(1)
    }

    input_file, file_open_err := os.open(os.args[1])
    if file_open_err != os.ERROR_NONE {
        fmt.println("Can't find barely src")
        os.exit(1)
    }
    defer os.close(input_file)

    input_data, file_read_success := os.read_entire_file(input_file)
    if !file_read_success {
        fmt.println("Can't read barely src")
        os.exit(1)
    }

    src := string(input_data)

    // Tokenize

    tokens: [dynamic]string
    line_number := 1
    column_number := 1
    for index := 0; index < len(src); index += 1 {
        if strings.is_space(rune(src[index])) {
            if src[index] == '\n' {
                line_number += 1
                column_number = 1
            }
            continue
        } else if src[index] >= 'A' && src[index] <= 'Z' {
            token_start := index
            for token_index := token_start + 1; token_index < len(src); token_index += 1 {
                if strings.is_space(rune(src[token_index])) {
                    token, ok := strings.substring(src, token_start, token_index)
                    append(&tokens, token)

                    column_number += token_index - token_start
                    index = token_index - 1
                    break
                } else if !(src[token_index] >= 'A' && src[token_index] <= 'Z') && !(src[token_index] >= '0' && src[token_index] <= '9') && src[token_index] != '.' && src[token_index] != ':' {
                    fmt.printfln("Invalid token, found character %c in keyword at line %i, column %i", src[token_index], line_number, column_number)
                    os.exit(1)
                }
            }
        } else if (src[index] >= '0' && src[index] <= '9') || src[index] == '-' {
            token_start := index
            for token_index := token_start + 1; token_index < len(src); token_index += 1 {
                if strings.is_space(rune(src[token_index])) {
                    token, ok := strings.substring(src, token_start, token_index)
                    append(&tokens, token)

                    column_number += token_index - token_start
                    index = token_index - 1
                    break
                } else if !(src[token_index] >= '0' && src[token_index] <= '9') {
                    fmt.printfln("Invalid token, found character %c in integer literal at line %i, column %i", src[token_index], line_number, column_number)
                    os.exit(1)
                }
            }
        } else if src[index] == '"' {
            token_start := index + 1
            for token_index := token_start; token_index < len(src); token_index += 1 {
                if src[token_index] == '"' {
                    token, ok := strings.substring(src, token_start, token_index)
                    append(&tokens, token)

                    column_number += token_index - token_start
                    index = token_index
                    break
                } else if src[token_index] == '\n' {
                    fmt.printfln("Invalid token, found character %c in string literal at line %i, column %i", src[token_index], line_number, column_number)
                    os.exit(1)
                }
            }
        }

        column_number += 1
    }

    // Interpret

    stack: [dynamic]i64

    jump_type :: enum { NONE, EQ_0, GT_0 }
    current_jump_type := jump_type.NONE

    for index := 0; index < len(tokens); index += 1 {
        token := tokens[index]

        switch token {
        case "PUSH":
            index += 1
            if index == len(tokens) {
                fmt.printfln("Nothing to push")
                os.exit(1)
            }

            int, convOk := strconv.parse_i64(tokens[index])
            if !convOk {
                fmt.printfln("Invalid int")
                os.exit(1)
            }

            append(&stack, int)
        case "ADD":
            if len(stack) < 2 {
                fmt.printfln("Stack too short")
                os.exit(1)
            }

            b := pop(&stack)
            a := pop(&stack)
            c := a + b
            append(&stack, c)
        case "SUB":
            if len(stack) < 2 {
                fmt.printfln("Stack too short")
                os.exit(1)
            }

            b := pop(&stack)
            a := pop(&stack)
            c := a - b
            append(&stack, c)
        case "READ":
            data: [256]byte
            read_count, err := os.read(os.stdin, data[:])
            int, convOk := strconv.parse_i64(string(data[:read_count - 1]))
            if !convOk {
                fmt.printfln("Invalid int")
                os.exit(1)
            }
            append(&stack, int)
        case "PRINT":
            index += 1
            if index == len(tokens) {
                fmt.printfln("Nothing to print")
                os.exit(1)
            }

            msg := tokens[index]
            fmt.println(msg)
        case "JUMP.EQ.0":
            if current_jump_type == jump_type.NONE {
                current_jump_type = jump_type.EQ_0
            }
            fallthrough
        case "JUMP.GT.0":
            if current_jump_type == jump_type.NONE {
                current_jump_type = jump_type.GT_0
            }
            fallthrough
        case "JUMP.ANY":
            index += 1
            if index == len(tokens) {
                fmt.printfln("Nothing to jump to")
                os.exit(1)
            }

            if len(stack) < 1 {
                fmt.printfln("Stack too short")
                os.exit(1)
            }

            jump := false
            jump_input := stack[len(stack) - 1]
            jump_label := strings.concatenate({tokens[index], ":"});

            switch current_jump_type {
                case .NONE:
                    break
                case .EQ_0:
                    jump = jump_input == 0
                case .GT_0:
                    jump = jump_input > 0
            }

            current_jump_type = jump_type.NONE
            if !jump {
                continue
            }

            jumped := false
            for jump_index := 0; jump_index < len(tokens); jump_index += 1 {
                if tokens[jump_index] == jump_label {
                    index = jump_index
                    jumped = true
                    break
                }
            }

            if !jumped {
                fmt.printfln("Label not found")
                os.exit(1)
            }
        case "HALT":
            return
        case:
            if strings.has_suffix(token, ":") {
                continue
            }

            fmt.printfln("Unknown token %s", token)
            os.exit(1)
        }
    }
}
