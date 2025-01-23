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
    token_start := 0
    quote_open := false
    for char, index in src {
        if char == '"' {
            quote_open = !quote_open
        } else if !quote_open && strings.is_space(char) {
            if token_start != index {
                token, ok := strings.substring(src, token_start, index)
                append(&tokens, token)
            }

            token_start = index + 1
        }
    }

    // Interpret

    stack: [dynamic]i64

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
            fmt.println(msg[1:len(msg) - 1])
        case "JUMP.EQ.0":
            index += 1
            if index == len(tokens) {
                fmt.printfln("Nothing to jump to")
                os.exit(1)
            }

            if len(stack) < 1 {
                fmt.printfln("Stack too short")
                os.exit(1)
            }

            jump_label := strings.concatenate({tokens[index], ":"});
            if stack[len(stack) - 1] == 0 {
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
            }
        case "JUMP.GT.0":
            index += 1
            if index == len(tokens) {
                fmt.printfln("Nothing to jump to")
                os.exit(1)
            }

            if len(stack) < 1 {
                fmt.printfln("Stack too short")
                os.exit(1)
            }

            jump_label := strings.concatenate({tokens[index], ":"});
            if stack[len(stack) - 1] > 0 {
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
