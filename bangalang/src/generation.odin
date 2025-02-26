package main

import "core:fmt"
import "core:os"
import "core:strings"
import "core:strconv"

gen_context :: struct
{
    stack_top: int,
    stack_vars: map[string]int,

    if_index: int,
    for_index: int
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

    ctx: gen_context

    for node in nodes
    {
        generate_statement(file, node, &ctx)
    }

    fmt.fprintln(file, "  ; default exit")
    fmt.fprintln(file, "  mov rax, 60 ; syscall: exit")
    fmt.fprintln(file, "  mov rdi, 0 ; arg0: exit_code")
    fmt.fprintln(file, "  syscall ; call kernel")
}

generate_statement :: proc(file: os.Handle, node: ast_node, ctx: ^gen_context)
{
    #partial switch node.type
    {
    case .IF:
        generate_if(file, node, ctx)
    case .FOR:
        generate_for(file, node, ctx)
    case .SCOPE:
        generate_scope(file, node, ctx)
    case .DECLARATION:
        generate_declaration(file, node, ctx)
    case .ASSIGNMENT:
        generate_assignment(file, node, ctx)
    case .EXIT:
        generate_exit(file, node, ctx)
    case:
        fmt.println("Failed to generate statement")
        fmt.printfln("Invalid node at line %i, column %i", node.line_number, node.column_number)
        os.exit(1)
    }
}

generate_if :: proc(file: os.Handle, node: ast_node, ctx: ^gen_context)
{
    if_index := ctx.if_index
    ctx.if_index += 1

    fmt.fprintfln(file, "; if_%i", if_index)

    expression_node := node.children[0]
    scope_node := node.children[1]

    child_index := 2
    else_index := 0

    generate_expression(file, expression_node, ctx)
    fmt.fprintln(file, "  test r8, r8 ; test expression")
    fmt.fprintfln(file, "  jz .if_%i_%s ; skip main scope when false/zero", if_index, child_index < len(node.children) ? "else_0" : "end")

    generate_scope(file, scope_node, ctx)

    for child_index + 1 < len(node.children)
    {
        fmt.fprintfln(file, "  jmp .if_%i_end ; skip else scope", if_index)
        fmt.fprintfln(file, ".if_%i_else_%i:", if_index, else_index)
        else_index += 1

        generate_expression(file, node.children[child_index], ctx)
        child_index += 1

        fmt.fprintln(file, "  test r8, r8 ; test expression")

        buf: [256]byte
        else_with_index := strings.concatenate({ "else_", strconv.itoa(buf[:], else_index) })
        fmt.fprintfln(file, "  jz .if_%i_%s ; skip else scope when false/zero", if_index, child_index + 1 < len(node.children) ? else_with_index : "end")

        generate_scope(file, node.children[child_index], ctx)
        child_index += 1
    }

    if child_index < len(node.children)
    {
        fmt.fprintfln(file, "  jmp .if_%i_end ; skip else scope", if_index)
        fmt.fprintfln(file, ".if_%i_else_%i:", if_index, else_index)
        else_index += 1

        generate_scope(file, node.children[child_index], ctx)
        child_index += 1
    }

    fmt.fprintfln(file, ".if_%i_end:", if_index)
}

generate_for :: proc(file: os.Handle, node: ast_node, ctx: ^gen_context)
{
    for_index := ctx.for_index
    ctx.for_index += 1

    if node.children[0].type == .DECLARATION
    {
        declaration_node := node.children[0]
        generate_declaration(file, declaration_node, ctx)

        fmt.fprintfln(file, ".for_%i:", for_index)

        expression_node := node.children[1]
        generate_expression(file, expression_node, ctx)
        fmt.fprintln(file, "  test r8, r8 ; test expression")
        fmt.fprintfln(file, "  jz .for_%i_end ; skip for scope when false/zero", for_index)
    }
    else
    {
        fmt.fprintfln(file, ".for_%i:", for_index)

        expression_node := node.children[0]
        generate_expression(file, expression_node, ctx)
        fmt.fprintln(file, "  test r8, r8 ; test expression")
        fmt.fprintfln(file, "  jz .for_%i_end ; skip for scope when false/zero", for_index)
    }

    scope_node := node.children[len(node.children) - 1]
    generate_scope(file, scope_node, ctx)

    if node.children[0].type == .DECLARATION
    {
        assignment_node := node.children[2]
        generate_assignment(file, assignment_node, ctx)
    }

    fmt.fprintfln(file, "  jmp .for_%i ; back to top", for_index)

    fmt.fprintfln(file, ".for_%i_end:", for_index)
}

generate_scope :: proc(file: os.Handle, node: ast_node, parent_ctx: ^gen_context)
{
    fmt.fprintln(file, "; scope start")

    scope_ctx: gen_context
    scope_ctx.stack_top = parent_ctx.stack_top
    for key in parent_ctx.stack_vars
    {
        scope_ctx.stack_vars[key] = parent_ctx.stack_vars[key]
    }
    scope_ctx.if_index = parent_ctx.if_index

    for child_node in node.children
    {
        generate_statement(file, child_node, &scope_ctx)
    }

    parent_ctx.if_index = scope_ctx.if_index

    scope_stack_size := scope_ctx.stack_top - parent_ctx.stack_top
    if scope_stack_size > 0
    {
        fmt.fprintln(file, "  ; close scope")
        fmt.fprintfln(file, "  add rsp, %i ; clear scope's stack", scope_stack_size)
    }

    fmt.fprintln(file, "; scope end")
}

generate_declaration :: proc(file: os.Handle, node: ast_node, ctx: ^gen_context)
{
    fmt.fprintln(file, "  ; declare")

    lhs_node := node.children[0]
    rhs_node := node.children[1]

    if lhs_node.value in ctx.stack_vars
    {
        fmt.println("Failed to generate declaration")
        fmt.printfln("Duplicate identifier '%s' at line %i, column %i", lhs_node.value, lhs_node.line_number, lhs_node.column_number)
        os.exit(1)
    }

    generate_expression(file, rhs_node, ctx)
    fmt.fprintln(file, "  push r8 ; push to stack")
    ctx.stack_top += 8
    ctx.stack_vars[lhs_node.value] = ctx.stack_top
}

generate_assignment :: proc(file: os.Handle, node: ast_node, ctx: ^gen_context)
{
    fmt.fprintln(file, "  ; assign")

    lhs_node := node.children[0]
    rhs_node := node.children[1]

    if !(lhs_node.value in ctx.stack_vars)
    {
        fmt.println("Failed to generate assignment")
        fmt.printfln("Undeclared identifier '%s' at line %i, column %i", lhs_node.value, lhs_node.line_number, lhs_node.column_number)
        os.exit(1)
    }

    var_pointer := ctx.stack_vars[lhs_node.value]
    var_offset := ctx.stack_top - var_pointer
    generate_expression(file, rhs_node, ctx)
    fmt.fprintfln(file, "  mov [rsp+%i], r8 ; assign value", var_offset)
}

generate_exit :: proc(file: os.Handle, node: ast_node, ctx: ^gen_context)
{
    fmt.fprintln(file, "  ; exit")

    param_node := node.children[0]

    generate_expression(file, param_node, ctx)
    fmt.fprintln(file, "  mov rax, 60 ; syscall: exit")
    fmt.fprintln(file, "  mov rdi, r8 ; arg0: exit_code")
    fmt.fprintln(file, "  syscall ; call kernel")
}

generate_expression :: proc(file: os.Handle, node: ast_node, ctx: ^gen_context, register_num: int = 8)
{
    if len(node.children) < 2
    {
        generate_primary(file, node, ctx, register_num)
        return
    }

    lhs_node := node.children[0]
    rhs_node := node.children[1]

    lhs_register_num := register_num / 2 * 2 + 2
    rhs_register_num := lhs_register_num + 1

    generate_expression(file, lhs_node, ctx, lhs_register_num)
    generate_expression(file, rhs_node, ctx, rhs_register_num)

    #partial switch node.type
    {
    case .ADD:
        fmt.fprintfln(file, "  mov r%i, r%i ; add: assign lhs", register_num, lhs_register_num)
        fmt.fprintfln(file, "  add r%i, r%i ; add: do it!", register_num, rhs_register_num)
    case .SUBTRACT:
        fmt.fprintfln(file, "  mov r%i, r%i ; subtract: assign lhs", register_num, lhs_register_num)
        fmt.fprintfln(file, "  sub r%i, r%i ; subtract: do it!", register_num, rhs_register_num)
    case .MULTIPLY:
        fmt.fprintfln(file, "  mov r%i, r%i ; multiply: assign lhs", register_num, lhs_register_num)
        fmt.fprintfln(file, "  imul r%i, r%i ; multiply: do it!", register_num, rhs_register_num)
    case .DIVIDE:
        // dividend / divisor
        fmt.fprintln(file, "  mov rdx, 0 ; divide: assign zero to dividend high part")
        fmt.fprintfln(file, "  mov rax, r%i ; divide: assign lhs to dividend low part", lhs_register_num)
        fmt.fprintfln(file, "  idiv r%i ; divide: do it!", rhs_register_num)
        fmt.fprintfln(file, "  mov r%i, rax ; divide: assign result", register_num)
    case:
        fmt.println("Failed to generate expression")
        fmt.printfln("Invalid node at line %i, column %i", node.line_number, node.column_number)
        os.exit(1)
    }
}

generate_primary :: proc(file: os.Handle, node: ast_node, ctx: ^gen_context, register_num: int)
{
    if node.type == .IDENTIFIER
    {
        if !(node.value in ctx.stack_vars)
        {
            fmt.println("Failed to generate term")
            fmt.printfln("Undeclared identifier '%s' at line %i, column %i", node.value, node.line_number, node.column_number)
            os.exit(1)
        }

        var_pointer := ctx.stack_vars[node.value]
        var_offset := ctx.stack_top - var_pointer
        fmt.fprintfln(file, "  mov r%i, [rsp+%i] ; assign primary", register_num, var_offset)
    }
    else if node.type == .INTEGER_LITERAL
    {
        fmt.fprintfln(file, "  mov r%i, %s ; assign primary", register_num, node.value)
    }
    else if node.type == .NEGATE
    {
        generate_primary(file, node.children[0], ctx, register_num)
        fmt.fprintfln(file, "  neg r%i ; negate", register_num)
    }
    else
    {
        generate_expression(file, node, ctx, register_num)
    }
}
