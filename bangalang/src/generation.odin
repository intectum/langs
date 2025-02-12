package main

import "core:fmt"
import "core:os"

stack :: struct
{
    top: int,
    vars: map[string]int
}

generate_program :: proc(file_name: string, nodes: [dynamic]ast_node)
{
    file, file_error := os.open(file_name, os.O_CREATE | os.O_WRONLY | os.O_TRUNC, 0o666)
    if file_error != nil
    {
        fmt.println("Failed to open asm file")
        os.exit(1)
    }
    defer os.close(file)

    fmt.fprintln(file, "global _start")
    fmt.fprintln(file, "_start:")

    stack: stack

    for index := 0; index < len(nodes); index += 1
    {
        generate_statement(file, nodes[index], &stack)
    }

    fmt.fprintln(file, "  mov rax, 60 ; syscall: exit")
    fmt.fprintln(file, "  mov rdi, 0 ; arg0: exit_code")
    fmt.fprintln(file, "  syscall ; call kernel")
}

generate_statement :: proc(file: os.Handle, node: ast_node, stack: ^stack)
{
    #partial switch node.type
    {
    case .DECLARATION_STATEMENT:
        generate_declaration_statement(file, node, stack)
    case .ASSIGNMENT_STATEMENT:
        generate_assignment_statement(file, node, stack)
    case .EXIT_STATEMENT:
        generate_exit_statement(file, node, stack)
    case:
        fmt.println("Invalid statement")
        os.exit(1)
    }
}

generate_declaration_statement :: proc(file: os.Handle, node: ast_node, stack: ^stack)
{
    fmt.fprintln(file, "  ; declare")

    lhs_node := node.children[0]
    rhs_node := node.children[1]

    if rhs_node.children[0].type == .IDENTIFIER
    {
        if !(rhs_node.children[0].value in stack.vars)
        {
            fmt.printfln("Undeclared identifier: %s", rhs_node.children[0].value)
            os.exit(1)
        }

        var_pointer := stack.vars[rhs_node.children[0].value]
        var_offset := stack.top - var_pointer
        fmt.fprintfln(file, "  mov rax, [rsp+%i] ; value to register", var_offset)
    }
    else if rhs_node.children[0].type == .INTEGER_LITERAL
    {
        fmt.fprintfln(file, "  mov rax, %s ; assign value", rhs_node.children[0].value)
    }
    else
    {
        fmt.println("Invalid statement")
        os.exit(1)
    }

    if lhs_node.value in stack.vars
    {
        fmt.println("Identifier already exists")
        os.exit(1)
    }

    fmt.fprintln(file, "  mov [rsp], rax ; assign value")
    stack.vars[lhs_node.value] = stack.top

    fmt.fprintln(file, "  sub rsp, 8 ; allocate space on stack")
    stack.top += 8
}

generate_assignment_statement :: proc(file: os.Handle, node: ast_node, stack: ^stack)
{
    fmt.fprintln(file, "  ; assign")

    lhs_node := node.children[0]
    rhs_node := node.children[1]

    if rhs_node.children[0].type == .IDENTIFIER
    {
        if !(rhs_node.children[0].value in stack.vars)
        {
            fmt.printfln("Undeclared identifier: %s", rhs_node.children[0].value)
            os.exit(1)
        }

        var_pointer := stack.vars[rhs_node.children[0].value]
        var_offset := stack.top - var_pointer
        fmt.fprintfln(file, "  mov rax, [rsp+%i] ; value to register", var_offset)
    }
    else if rhs_node.children[0].type == .INTEGER_LITERAL
    {
        fmt.fprintfln(file, "  mov rax, %s ; assign value", rhs_node.children[0].value)
    }
    else
    {
        fmt.println("Invalid statement")
        os.exit(1)
    }

    if !(lhs_node.value in stack.vars)
    {
        fmt.println("Undeclared identifier")
        os.exit(1)
    }

    var_pointer := stack.vars[lhs_node.value]
    var_offset := stack.top - var_pointer
    fmt.fprintfln(file, "  mov [rsp+%i], rax ; assign value", var_offset)
}

generate_exit_statement :: proc(file: os.Handle, node: ast_node, stack: ^stack)
{
    fmt.fprintln(file, "  ; exit")

    param_node := node.children[0]

    fmt.fprintln(file, "  mov rax, 60 ; syscall: exit")
    if param_node.children[0].type == .IDENTIFIER
    {
        var_pointer := stack.vars[param_node.children[0].value]
        var_offset := stack.top - var_pointer
        fmt.fprintfln(file, "  mov rdi, [rsp+%i] ; arg0: exit_code", var_offset)
    }
    else if param_node.children[0].type == .INTEGER_LITERAL
    {
        fmt.fprintfln(file, "  mov rdi, %s ; arg0: exit_code", param_node.children[0].value)
    }
    else
    {
        fmt.println("Invalid statement")
        os.exit(1)
    }
    fmt.fprintln(file, "  syscall ; call kernel")
}
