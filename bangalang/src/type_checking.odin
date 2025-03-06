package main

import "core:fmt"
import "core:slice"
import "core:strings"
import "core:strconv"

procedure :: struct
{
    name: string,
    param_data_types: [dynamic]string,
    return_data_type: string
}

type_check_context :: struct
{
    procedures: [dynamic]procedure,
    procedure: procedure,

    variable_types: map[string]string,
}

type_check_program :: proc(nodes: [dynamic]ast_node) -> (ok: bool = true)
{
    ctx: type_check_context

    exit := procedure { name = "exit" }
    append(&exit.param_data_types, "i64")
    append(&ctx.procedures, exit)

    plus_one := procedure { name = "plus_one", return_data_type = "i64" }
    append(&plus_one.param_data_types, "i64")
    append(&ctx.procedures, plus_one)

    add := procedure { name = "add", return_data_type = "i64" }
    append(&add.param_data_types, "i64")
    append(&add.param_data_types, "i64")
    append(&ctx.procedures, add)

    for node in nodes
    {
        if node.type == .PROCEDURE
        {
            procedure: procedure

            for child_node, index in node.children
            {
                if index == 0
                {
                    procedure.name = child_node.value
                }
                else if index + 1 < len(node.children)
                {
                    append(&procedure.param_data_types, child_node.data_type)
                }
            }

            procedure.return_data_type = node.data_type

            append(&ctx.procedures, procedure)
        }
    }

    for &node in nodes
    {
        if node.type != .PROCEDURE
        {
            statement_ok := type_check_statement(&node, &ctx)
            if !statement_ok
            {
                ok = false
            }
        }
    }

    for &node in nodes
    {
        if node.type == .PROCEDURE
        {
            procedure_ok := type_check_procedure(&node, &ctx)
            if !procedure_ok
            {
                ok = false
            }
        }
    }

    return
}

type_check_procedure :: proc(node: ^ast_node, parent_ctx: ^type_check_context) -> (ok: bool)
{
    child_index := 0
    name_node := node.children[child_index]
    child_index += 1

    scope_ctx := copy_type_check_context(parent_ctx^)

    for the_procedure in scope_ctx.procedures
    {
        if the_procedure.name == name_node.value
        {
            scope_ctx.procedure = the_procedure
            break
        }
    }

    for child_index + 1 < len(node.children)
    {
        param_node := node.children[child_index]
        child_index += 1

        scope_ctx.variable_types[param_node.value] = param_node.data_type
    }

    scope_node := node.children[child_index]
    child_index += 1

    ok = type_check_scope(&scope_node, &scope_ctx)

    return
}

type_check_statement :: proc(node: ^ast_node, ctx: ^type_check_context) -> (ok: bool)
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
        ok = type_check_call(node, ctx, "")
    case:
        fmt.println("BUG: Failed to type check statement")
        fmt.printfln("Invalid node at line %i, column %i", node.line_number, node.column_number)
    }

    return
}

type_check_if :: proc(node: ^ast_node, ctx: ^type_check_context) -> (ok: bool = true)
{
    child_index := 0
    expression_node := node.children[child_index]
    child_index += 1

    expression_ok := type_check_expression(&expression_node, ctx, "")
    if !expression_ok
    {
        ok = false
    }

    if_scope_node := node.children[child_index]
    child_index += 1

    if_scope_ok := type_check_scope(&if_scope_node, ctx)
    if !if_scope_ok
    {
        ok = false
    }

    for child_index + 1 < len(node.children)
    {
        else_if_expression_node := node.children[child_index]
        child_index += 1

        else_if_expression_ok := type_check_expression(&else_if_expression_node, ctx, "")
        if !else_if_expression_ok
        {
            ok = false
        }

        else_if_scope_node := node.children[child_index]
        child_index += 1

        else_if_scope_ok := type_check_scope(&else_if_scope_node, ctx)
        if !else_if_scope_ok
        {
            ok = false
        }
    }

    if child_index < len(node.children)
    {
        else_scope_node := node.children[child_index]
        child_index += 1

        else_scope_ok := type_check_scope(&else_scope_node, ctx)
        if !else_scope_ok
        {
            ok = false
        }
    }

    return
}

type_check_for :: proc(node: ^ast_node, ctx: ^type_check_context) -> (ok: bool = true)
{
    if node.children[0].type == .DECLARATION
    {
        // TODO should be scoped to for loop
        declaration_node := node.children[0]
        declaration_ok := type_check_declaration(&declaration_node, ctx)
        if !declaration_ok
        {
            ok = false
        }

        expression_node := node.children[1]
        expression_ok := type_check_expression(&expression_node, ctx, "")
        if !expression_ok
        {
            ok = false
        }
    }
    else
    {
        expression_node := node.children[0]
        expression_ok := type_check_expression(&expression_node, ctx, "")
        if !expression_ok
        {
            ok = false
        }
    }

    if node.children[0].type == .DECLARATION
    {
        assignment_node := node.children[2]
        assignment_ok := type_check_assignment(&assignment_node, ctx)
        if !assignment_ok
        {
            ok = false
        }
    }

    scope_node := node.children[len(node.children) - 1]
    scope_ok := type_check_scope(&scope_node, ctx)
    if !scope_ok
    {
        ok = false
    }

    return
}

type_check_scope :: proc(node: ^ast_node, parent_ctx: ^type_check_context) -> (ok: bool = true)
{
    scope_ctx := copy_type_check_context(parent_ctx^, true)

    for &child_node in node.children
    {
        statement_ok := type_check_statement(&child_node, &scope_ctx)
        if !statement_ok
        {
            ok = false
        }
    }

    return
}

type_check_declaration :: proc(node: ^ast_node, ctx: ^type_check_context) -> (ok: bool)
{
    lhs_node := &node.children[0]
    rhs_node := &node.children[1]

    if lhs_node.value in ctx.variable_types
    {
        fmt.println("Failed to type check declaration")
        fmt.printfln("Duplicate identifier '%s' at line %i, column %i", lhs_node.value, lhs_node.line_number, lhs_node.column_number)
        ok = false
    }


    ok = type_check_expression(rhs_node, ctx, lhs_node.data_type)

    type, coerce_ok := coerce_type(node^, { lhs_node.data_type, rhs_node.data_type })
    if !coerce_ok
    {
        ok = false
    }

    lhs_node.data_type = type
    ctx.variable_types[lhs_node.value] = lhs_node.data_type

    return
}

type_check_assignment :: proc(node: ^ast_node, ctx: ^type_check_context) -> (ok: bool)
{
    lhs_node := &node.children[0]
    rhs_node := &node.children[1]

    if !(lhs_node.value in ctx.variable_types)
    {
        fmt.println("Failed to type check assignment")
        fmt.printfln("Undeclared identifier '%s' at line %i, column %i", lhs_node.value, lhs_node.line_number, lhs_node.column_number)
        ok = false
    }

    lhs_node.data_type = ctx.variable_types[lhs_node.value]

    ok = type_check_expression(rhs_node, ctx, lhs_node.data_type)

    _, coerce_ok := coerce_type(node^, { lhs_node.data_type, rhs_node.data_type })
    if !coerce_ok
    {
        ok = false
    }

    return
}

type_check_return :: proc(node: ^ast_node, ctx: ^type_check_context) -> (ok: bool = true)
{
    expression_node := node.children[0]

    expression_ok := type_check_expression(&expression_node, ctx, ctx.procedure.return_data_type)
    if !expression_ok
    {
        ok = false
    }

    type, coerce_ok := coerce_type(node^, { expression_node.data_type, ctx.procedure.return_data_type })
    if !coerce_ok
    {
        ok = false
    }

    return
}

type_check_expression :: proc(node: ^ast_node, ctx: ^type_check_context, expected_type: string) -> (ok: bool = true)
{
    if node.type != .ADD && node.type != .SUBTRACT && node.type != .MULTIPLY && node.type != .DIVIDE
    {
        ok = type_check_primary(node, ctx, expected_type)
        return
    }

    lhs_node := node.children[0]
    rhs_node := node.children[1]

    lhs_ok := type_check_expression(&lhs_node, ctx, expected_type)
    if !lhs_ok
    {
        ok = false
    }

    rhs_ok := type_check_expression(&rhs_node, ctx, expected_type)
    if !rhs_ok
    {
        ok = false
    }

    type, _ := coerce_type(node^, { lhs_node.data_type, rhs_node.data_type, expected_type }, true)
    node.data_type = type

    return
}

type_check_primary :: proc(node: ^ast_node, ctx: ^type_check_context, expected_type: string) -> (ok: bool)
{
    #partial switch node.type
    {
    case .CALL:
        ok = type_check_call(node, ctx, expected_type)
    case .IDENTIFIER:
        if !(node.value in ctx.variable_types)
        {
            fmt.println("Failed to type check primary")
            fmt.printfln("Undeclared identifier '%s' at line %i, column %i", node.value, node.line_number, node.column_number)
            ok = false
        }

        node.data_type = ctx.variable_types[node.value]
        ok = true
    case .NUMBER:
        node.data_type = "number"
        ok = true
    case .NEGATE:
        ok = type_check_primary(&node.children[0], ctx, expected_type)
        node.data_type = node.children[0].data_type
    case:
        ok = type_check_expression(node, ctx, expected_type)
    }

    return
}

type_check_call :: proc(node: ^ast_node, ctx: ^type_check_context, expected_type: string) -> (ok: bool = true)
{
    child_index := 0
    name_node := node.children[child_index]
    child_index += 1

    procedure: procedure
    for the_procedure in ctx.procedures
    {
        if the_procedure.name == name_node.value
        {
            procedure = the_procedure
            break
        }
    }

    if procedure.name == ""
    {
        fmt.println("Failed to type check call")
        fmt.printfln("Undeclared identifier '%s' at line %i, column %i", name_node.value, name_node.line_number, name_node.column_number)
        ok = false
    }

    if len(procedure.param_data_types) != len(node.children) - 1
    {
        fmt.println("Failed to type check call")
        fmt.printfln("Wrong number of parameters at line %i, column %i", node.line_number, node.column_number)
        ok = false
    }

    for child_index < len(node.children)
    {
        param_data_type := procedure.param_data_types[child_index - 1]

        param_node := node.children[child_index]
        child_index += 1

        expression_ok := type_check_expression(&param_node, ctx, expected_type)
        if !expression_ok
        {
            ok = false
        }

        _, coerce_ok := coerce_type(param_node, { param_node.data_type, param_data_type })
        if !coerce_ok
        {
            ok = false
        }
    }

    node.data_type = procedure.return_data_type

    return
}

copy_type_check_context := proc(ctx: type_check_context, inline := false) -> type_check_context
{
    ctx_copy: type_check_context
    ctx_copy.procedures = ctx.procedures
    ctx_copy.procedure = ctx.procedure

    if inline
    {
        for key in ctx.variable_types
        {
            ctx_copy.variable_types[key] = ctx.variable_types[key]
        }
    }

    return ctx_copy
}

coerce_type :: proc(node: ast_node, types: []string, silent := false) -> (string, bool)
{
    unique_types: [dynamic]string
    for type in types
    {
        if type == ""
        {
            continue
        }

        _, found := slice.linear_search(unique_types[:], type)
        if !found
        {
            append(&unique_types, type)
        }
    }

    coerced_type := ""
    for type in unique_types
    {
        if type != "number"
        {
            if coerced_type != ""
            {
                if !silent
                {
                    fmt.println("Failed to coerce types")
                    fmt.printfln("Incompatible data types %s at line %i, column %i", unique_types, node.line_number, node.column_number)
                }

                return "number", false
            }

            coerced_type = type
        }
    }

    if coerced_type == ""
    {
        fmt.println("Failed to coerce types")
        fmt.printfln("Incompatible data types %s at line %i, column %i", unique_types, node.line_number, node.column_number)
        return "number", false
    }

    return coerced_type, true
}
