package main

import "core:fmt"
import "core:os"
import "core:strings"
import "../common"

main :: proc() {
    if len(os.args) == 1 {
        fmt.println("Usage: barelyac <file>")
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
    tokens, tokenizeOk := common.tokenize(&src)
    if !tokenizeOk {
        os.exit(1)
    }

    // Compile

    out := strings.builder_make_none()
    defer strings.builder_destroy(&out)

    strings.write_string(&out, "global _start\n")
    strings.write_string(&out, "_start: ; entry point\n")

    for index := 0; index < len(tokens); index += 1 {
        token := tokens[index]

        switch token {
        case "PUSH":

        case "ADD":

        case "SUB":

        case "READ":

        case "PRINT":

        case "JUMP.EQ.0":

        case "JUMP.GT.0":

        case "HALT":
            strings.write_string(&out, "  ; HALT\n")
            strings.write_string(&out, "  mov rax, 60 ; syscall: exit\n")
            strings.write_string(&out, "  mov rdi, 0 ; exit code\n")
            strings.write_string(&out, "  syscall\n")
        case:
            if strings.has_suffix(token, ":") {
                continue
            }

            fmt.printfln("Unknown token %s", token)
            //os.exit(1)
        }
    }

    os.write_entire_file("out.asm", transmute([]u8) strings.to_string(out))
}
