package main

import "core:fmt"
import "core:os"
import "core:strings"
import "core:strconv"

procedure :: struct
{
    name: string,
    param_count: int
}

type_check_context :: struct
{
    procedures: [dynamic]procedure,

    variable_types: map[string]string,
}

built_in_procedures := []procedure { { "add", 2 }, { "exit", 1 }, { "plus_one", 1 } }

type_check_program :: proc(nodes: [dynamic]ast_node) -> (ok: bool = true)
{
    ctx: type_check_context

    for node in nodes
    {
        if node.type == .PROCEDURE
        {
            append(&ctx.procedures, procedure { node.children[0].value, len(node.children) - 2 })
        }
    }

    for node in nodes
    {
        if node.type != .PROCEDURE
        {
            statement_ok := type_check_statement(node, &ctx)
            if !statement_ok
            {
                ok = false
            }
        }
    }

    for node in nodes
    {
        if node.type == .PROCEDURE
        {
            procedure_ok := type_check_procedure(node, &ctx)
            if !procedure_ok
            {
                ok = false
            }
        }
    }

    return
}

type_check_procedure :: proc(node: ast_node, parent_ctx: ^type_check_context) -> (ok: bool)
{
    child_index := 0
    name_node := node.children[child_index]
    child_index += 1

    scope_ctx := copy_type_check_context(parent_ctx^)
    for child_index + 1 < len(node.children)
    {
        param_node := node.children[child_index]
        child_index += 1

        scope_ctx.variable_types[param_node.value] = ""
    }

    scope_node := node.children[child_index]
    child_index += 1

    ok = type_check_scope(scope_node, &scope_ctx)

    return
}

type_check_statement :: proc(node: ast_node, ctx: ^type_check_context) -> (ok: bool)
{
    #partial switch node.type
    {
    case .IF:
        ok = type_check_if(node, ctx)
    case .FOR:
        ok = type_check_for(node, ctx)
    case .SCOPE:
        ok = type_check_scope(node, ctx)
    case .DECLARATION:
        ok = type_check_declaration(node, ctx)
    case .ASSIGNMENT:
        ok = type_check_assignment(node, ctx)
    case .RETURN:
        ok = type_check_return(node, ctx)
    case .CALL:
        ok = type_check_call(node, ctx)
    case:
        fmt.println("BUG: Failed to type check statement")
        fmt.printfln("Invalid node at line %i, column %i", node.line_number, node.column_number)
        os.exit(1)
    }

    return
}

type_check_if :: proc(node: ast_node, ctx: ^type_check_context) -> (ok: bool = true)
{
    child_index := 0
    expression_node := node.children[child_index]
    child_index += 1

    expression_ok := type_check_expression(expression_node, ctx)
    if !expression_ok
    {
        ok = false
    }

    if_scope_node := node.children[child_index]
    child_index += 1

    if_scope_ok := type_check_scope(if_scope_node, ctx)
    if !if_scope_ok
    {
        ok = false
    }

    for child_index + 1 < len(node.children)
    {
        else_if_expression_node := node.children[child_index]
        child_index += 1

        else_if_expression_ok := type_check_expression(else_if_expression_node, ctx)
        if !else_if_expression_ok
        {
            ok = false
        }

        else_if_scope_node := node.children[child_index]
        child_index += 1

        else_if_scope_ok := type_check_scope(else_if_scope_node, ctx)
        if !else_if_scope_ok
        {
            ok = false
        }
    }

    if child_index < len(node.children)
    {
        else_scope_node := node.children[child_index]
        child_index += 1

        else_scope_ok := type_check_scope(else_scope_node, ctx)
        if !else_scope_ok
        {
            ok = false
        }
    }

    return
}

type_check_for :: proc(node: ast_node, ctx: ^type_check_context) -> (ok: bool = true)
{
    if node.children[0].type == .DECLARATION
    {
        // TODO should be scoped to for loop
        declaration_node := node.children[0]
        declaration_ok := type_check_declaration(declaration_node, ctx)
        if !declaration_ok
        {
            ok = false
        }

        expression_node := node.children[1]
        expression_ok := type_check_expression(expression_node, ctx)
        if !expression_ok
        {
            ok = false
        }
    }
    else
    {
        expression_node := node.children[0]
        expression_ok := type_check_expression(expression_node, ctx)
        if !expression_ok
        {
            ok = false
        }
    }

    if node.children[0].type == .DECLARATION
    {
        assignment_node := node.children[2]
        assignment_ok := type_check_assignment(assignment_node, ctx)
        if !assignment_ok
        {
            ok = false
        }
    }

    scope_node := node.children[len(node.children) - 1]
    scope_ok := type_check_scope(scope_node, ctx)
    if !scope_ok
    {
        ok = false
    }

    return
}

type_check_scope :: proc(node: ast_node, parent_ctx: ^type_check_context) -> (ok: bool = true)
{
    scope_ctx := copy_type_check_context(parent_ctx^, true)

    for child_node in node.children
    {
        statement_ok := type_check_statement(child_node, &scope_ctx)
        if !statement_ok
        {
            ok = false
        }
    }

    return
}

type_check_declaration :: proc(node: ast_node, ctx: ^type_check_context) -> (ok: bool)
{
    lhs_node := node.children[0]
    rhs_node := node.children[1]

    if lhs_node.value in ctx.variable_types
    {
        fmt.println("Failed to type check declaration")
        fmt.printfln("Duplicate identifier '%s' at line %i, column %i", lhs_node.value, lhs_node.line_number, lhs_node.column_number)
        os.exit(1)
    }

    ok = type_check_expression(rhs_node, ctx)
    ctx.variable_types[lhs_node.value] = ""

    return
}

type_check_assignment :: proc(node: ast_node, ctx: ^type_check_context) -> (ok: bool)
{
    lhs_node := node.children[0]
    rhs_node := node.children[1]

    if !(lhs_node.value in ctx.variable_types)
    {
        fmt.println("Failed to type check assignment")
        fmt.printfln("Undeclared identifier '%s' at line %i, column %i", lhs_node.value, lhs_node.line_number, lhs_node.column_number)
        os.exit(1)
    }

    ok = type_check_expression(rhs_node, ctx)

    return
}

type_check_return :: proc(node: ast_node, ctx: ^type_check_context) -> (ok: bool)
{
    expression_node := node.children[0]

    ok = type_check_expression(expression_node, ctx)

    return
}

type_check_expression :: proc(node: ast_node, ctx: ^type_check_context) -> (ok: bool = true)
{
    if node.type != .ADD && node.type != .SUBTRACT && node.type != .MULTIPLY && node.type != .DIVIDE
    {
        ok = type_check_primary(node, ctx)
        return
    }

    lhs_node := node.children[0]
    rhs_node := node.children[1]

    lhs_ok := type_check_expression(lhs_node, ctx)
    if !lhs_ok
    {
        ok = false
    }

    rhs_ok := type_check_expression(rhs_node, ctx)
    if !rhs_ok
    {
        ok = false
    }

    return
}

type_check_primary :: proc(node: ast_node, ctx: ^type_check_context) -> (ok: bool)
{
    #partial switch node.type
    {
    case .CALL:
        ok = type_check_call(node, ctx)
    case .IDENTIFIER:
        if !(node.value in ctx.variable_types)
        {
            fmt.println("Failed to type check primary")
            fmt.printfln("Undeclared identifier '%s' at line %i, column %i", node.value, node.line_number, node.column_number)
            os.exit(1)
        }

        ok = true
    case .INTEGER_LITERAL:
        ok = true
    case .NEGATE:
        ok = type_check_primary(node.children[0], ctx)
    case:
        ok = type_check_expression(node, ctx)
    }

    return
}

type_check_call :: proc(node: ast_node, ctx: ^type_check_context) -> (ok: bool = true)
{
    child_index := 0
    name_node := node.children[child_index]
    child_index += 1

    found_procedure := false
    for procedure in built_in_procedures
    {
        if procedure.name == name_node.value && procedure.param_count == len(node.children) - 1
        {
            found_procedure = true
            break
        }
    }
    for procedure in ctx.procedures
    {
        if procedure.name == name_node.value && procedure.param_count == len(node.children) - 1
        {
            found_procedure = true
            break
        }
    }

    if !found_procedure
    {
        fmt.println("Failed to type check call")
        fmt.printfln("Undeclared identifier '%s' at line %i, column %i", name_node.value, name_node.line_number, name_node.column_number)
        os.exit(1)
    }

    for child_index < len(node.children)
    {
        param_node := node.children[child_index]
        child_index += 1

        expression_ok := type_check_expression(param_node, ctx)
        if !expression_ok
        {
            ok = false
        }
    }

    return
}

copy_type_check_context := proc(ctx: type_check_context, inline := false) -> type_check_context
{
    ctx_copy: type_check_context
    ctx_copy.procedures = ctx.procedures

    if inline
    {
        for key in ctx.variable_types
        {
            ctx_copy.variable_types[key] = ctx.variable_types[key]
        }
    }

    return ctx_copy
}
