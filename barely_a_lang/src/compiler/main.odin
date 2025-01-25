package main

import "core:fmt"
import "core:os"
import "core:strings"
import "core:strconv"
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

    strings.write_string(&out, "section .text\n")
    strings.write_string(&out, "  global _start\n")
    strings.write_string(&out, "  _start: ; entry point\n")

    // TODO remove
    strings.write_string(&out, "    push 0\n")

    string_constants: [dynamic]string

    for index := 0; index < len(tokens); index += 1 {
        token := tokens[index]

        switch token {
        case "PUSH":

        case "ADD":

        case "SUB":

        case "READ":

        case "PRINT":
            index += 1
            if index == len(tokens) {
                fmt.printfln("Nothing to print")
                os.exit(1)
            }

            string_constant_index := len(string_constants)

            data: [256]byte
            str := strconv.itoa(data[:], string_constant_index)

            append(&string_constants, tokens[index])

            strings.write_string(&out, "    ; PRINT\n")
            strings.write_string(&out, "    mov rax, 1 ; syscall: write\n")
            strings.write_string(&out, "    mov rdi, 1 ; stdout\n")
            strings.write_string(&out, "    mov rsi, str_")
            strings.write_string(&out, str)
            strings.write_string(&out, "\n")
            strings.write_string(&out, "    mov rdx, str_len_")
            strings.write_string(&out, str)
            strings.write_string(&out, "\n")
            strings.write_string(&out, "    syscall\n")
        case "JUMP.EQ.0":
            index += 1
            if index == len(tokens) {
                fmt.printfln("Nothing to jump to")
                os.exit(1)
            }

            strings.write_string(&out, "    ; JUMP.EQ.0\n")
            strings.write_string(&out, "    mov rax, [rsp]\n")
            strings.write_string(&out, "    cmp rax, 0\n")
            strings.write_string(&out, "    jz ")
            strings.write_string(&out, tokens[index])
            strings.write_string(&out, "\n")
        case "JUMP.GT.0":
            index += 1
            if index == len(tokens) {
                fmt.printfln("Nothing to jump to")
                os.exit(1)
            }

            strings.write_string(&out, "    ; JUMP.GT.0\n")
            strings.write_string(&out, "    mov rax, [rsp]\n")
            strings.write_string(&out, "    cmp rax, 0\n")
            strings.write_string(&out, "    ja ")
            strings.write_string(&out, tokens[index])
            strings.write_string(&out, "\n")
        case "HALT":
            strings.write_string(&out, "    ; HALT\n")
            strings.write_string(&out, "    mov rax, 60 ; syscall: exit\n")
            strings.write_string(&out, "    mov rdi, 0 ; exit code\n")
            strings.write_string(&out, "    syscall\n")
        case:
            if strings.has_suffix(token, ":") {
                strings.write_string(&out, "  ")
                strings.write_string(&out, token)
                strings.write_string(&out, "\n")
                continue
            }

            fmt.printfln("Unknown token %s", token)
            //os.exit(1)
        }
    }

    strings.write_string(&out, "\nsection .data\n")

    for string_constant, index in string_constants {
        index_data: [256]byte
        index_str := strconv.itoa(index_data[:], index)
        length_data: [256]byte
        length_str := strconv.itoa(length_data[:], len(string_constant) + 3)

        strings.write_string(&out, "  str_")
        strings.write_string(&out, index_str)
        strings.write_string(&out, ": db \"")
        strings.write_string(&out, string_constant)
        strings.write_string(&out, "\", ")
        strings.write_string(&out, length_str)
        strings.write_string(&out, "\n")
        strings.write_string(&out, "  str_len_")
        strings.write_string(&out, index_str)
        strings.write_string(&out, ": equ $-str_")
        strings.write_string(&out, index_str)
        strings.write_string(&out, "\n")
    }

    os.write_entire_file("out.asm", transmute([]u8) strings.to_string(out))
}
