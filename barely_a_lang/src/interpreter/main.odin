package main

import "core:fmt"
import "core:os"
import "core:strings"
import "core:strconv"

main :: proc() {
    f, err := os.open("examples/test_01.barely")
    if err != os.ERROR_NONE {
        fmt.println("Can't find barely src")
        os.exit(1)
    }
    defer os.close(f)

    barely_src, success := os.read_entire_file(f)
    if err != os.ERROR_NONE {
        fmt.println("Can't read barely src")
        os.exit(1)
    }

    lines := strings.split(string(barely_src), "\n")

    stack: [dynamic]i64
    jump_to_label: string

    for line, line_index in lines {
        space_index := strings.index(line, " ")
        op_code, ok := strings.substring(line, 0, space_index)
        if len(op_code) == 0 {
            op_code = line
        }

        if len(op_code) == 0 {
            continue
        }

        if jump_to_label != "" {
            if strings.has_suffix(op_code, ":") {
                label, ok := strings.substring(op_code, 0, len(op_code) - 1)
                if !ok || label == "" {
                    fmt.printfln("Invalid label at line %i", line_index + 1)
                    os.exit(1)
                }
                if label == jump_to_label {
                    jump_to_label = ""
                }
            }
        } else if op_code == "READ" {
            data: [256]byte
            read_count, err := os.read(os.stdin, data[:])
            int, convOk := strconv.parse_i64(string(data[:read_count - 1]))
            if !convOk {
                fmt.printfln("Invalid int at line %i", line_index + 1)
                os.exit(1)
            }
            append(&stack, int)
        } else if op_code == "SUB" {
            b := pop(&stack)
            a := pop(&stack)
            c := a - b
            append(&stack, c)
        } else if op_code == "JUMP.EQ.0" {
            a := pop(&stack)
            if a == 0 {
                label, ok := strings.substring(line, space_index + 1, len(line))
                if !ok {
                    fmt.printfln("Label not provided at line %i", line_index + 1)
                    os.exit(1)
                }
                jump_to_label = label
            }
        } else if op_code == "PRINT" {
            msg, ok := strings.substring(line, space_index + 2, len(line) - 1)
            if !ok {
                fmt.printfln("Message not provided at line %i", line_index + 1)
                os.exit(1)
            }
            fmt.println(msg)
        } else if op_code == "HALT" {
            return
        }
    }
}
