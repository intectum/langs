package main

import "core:fmt"
import "core:os"

ast_node_type :: enum
{
    SCOPE,
    DECLARATION_STATEMENT,
    ASSIGNMENT_STATEMENT,
    EXIT_STATEMENT,
    ADD,
    SUBTRACT,
    MULTIPLY,
    DIVIDE,
    IDENTIFIER,
    INTEGER_LITERAL
}

ast_node :: struct
{
    type: ast_node_type,
    value: string,
    children: [dynamic]ast_node,
    line_number: int,
    column_number: int
}

parse_program :: proc(stream: ^token_stream) -> (nodes: [dynamic]ast_node)
{
    for stream.next_index < len(stream.tokens)
    {
        append(&nodes, parse_statement(stream))
    }

    return
}

parse_statement :: proc(stream: ^token_stream) -> (node: ast_node)
{
    #partial switch peek_token(stream).type
    {
    case .OPENING_SQUIGGLY_BRACKET:
        node = parse_scope(stream)
    case .IDENTIFIER:
        #partial switch peek_token(stream, 1).type
        {
        case .COLON:
            node = parse_declaration_statement(stream)
        case .EQUALS:
            node = parse_assignment_statement(stream)
        case .OPENING_BRACKET:
            node = parse_exit_statement(stream)
        case:
            token := peek_token(stream, 1)
            fmt.println("Failed to parse statement")
            fmt.printfln("Invalid token '%s' at line %i, column %i", token.value, token.line_number, token.column_number)
            os.exit(1)
        }
    case:
        token := peek_token(stream)
        fmt.println("Failed to parse statement")
        fmt.printfln("Invalid token '%s' at line %i, column %i", token.value, token.line_number, token.column_number)
        os.exit(1)
    }

    return
}

parse_scope :: proc(stream: ^token_stream) -> (node: ast_node)
{
    node.type = .SCOPE
    node.line_number = peek_token(stream).line_number
    node.column_number = peek_token(stream).column_number

    next_token(stream, []token_type { .OPENING_SQUIGGLY_BRACKET })

    for stream.next_index < len(stream.tokens)
    {
        if peek_token(stream).type == .CLOSING_SQUIGGLY_BRACKET
        {
            next_token(stream, []token_type { .CLOSING_SQUIGGLY_BRACKET })
            return
        }

        append(&node.children, parse_statement(stream))
    }

    fmt.println("Scope never ends")
    os.exit(1)
}

parse_declaration_statement :: proc(stream: ^token_stream) -> (node: ast_node)
{
    node.type = .DECLARATION_STATEMENT
    node.line_number = peek_token(stream).line_number
    node.column_number = peek_token(stream).column_number

    lhs_node := ast_node { type = .IDENTIFIER, value = next_token(stream, []token_type { .IDENTIFIER }).value }
    append(&node.children, lhs_node)

    next_token(stream, []token_type { .COLON })
    next_token(stream, []token_type { .EQUALS })

    rhs_node := parse_expression(stream)
    append(&node.children, rhs_node)

    return
}

parse_assignment_statement :: proc(stream: ^token_stream) -> (node: ast_node)
{
    node.type = .ASSIGNMENT_STATEMENT
    node.line_number = peek_token(stream).line_number
    node.column_number = peek_token(stream).column_number

    lhs_node := ast_node { type = .IDENTIFIER, value = next_token(stream, []token_type { .IDENTIFIER }).value }
    append(&node.children, lhs_node)

    next_token(stream, []token_type { .EQUALS })

    rhs_node := parse_expression(stream)
    append(&node.children, rhs_node)

    return
}

parse_exit_statement :: proc(stream: ^token_stream) -> (node: ast_node)
{
    node.type = .EXIT_STATEMENT
    node.line_number = peek_token(stream).line_number
    node.column_number = peek_token(stream).column_number

    exit_token := next_token(stream, []token_type { .IDENTIFIER })
    if exit_token.value != "exit"
    {
        fmt.println("Failed to parse exit statement")
        fmt.println("That doesn't say exit!")
        fmt.printfln("Invalid token '%s' at line %i, column %i", exit_token.value, exit_token.line_number, exit_token.column_number)
        os.exit(1)
    }

    next_token(stream, []token_type { .OPENING_BRACKET })

    param_node := parse_expression(stream)
    append(&node.children, param_node)

    next_token(stream, []token_type { .CLOSING_BRACKET })

    return
}

parse_expression :: proc(stream: ^token_stream) -> (node: ast_node)
{
    node.line_number = peek_token(stream).line_number
    node.column_number = peek_token(stream).column_number

    lhs_node := parse_term(stream)
    append(&node.children, lhs_node)

    operator_token := peek_token(stream)

    operator_found := false
    operator_token_types := []token_type { .PLUS, .MINUS, .ASTERISK, .BACKSLASH };
    for operator_token_type in operator_token_types
    {
        if operator_token_type == operator_token.type
        {
            operator_found = true
            break
        }
    }

    if !operator_found
    {
        node = lhs_node
        return
    }

    next_token(stream, operator_token_types)

    #partial switch operator_token.type
    {
    case .PLUS:
        node.type = .ADD
    case .MINUS:
        node.type = .SUBTRACT
    case .ASTERISK:
        node.type = .MULTIPLY
    case .BACKSLASH:
        node.type = .DIVIDE
    }

    rhs_node := parse_term(stream)
    append(&node.children, rhs_node)

    return
}

parse_term :: proc(stream: ^token_stream) -> (node: ast_node)
{
    node.line_number = peek_token(stream).line_number
    node.column_number = peek_token(stream).column_number

    token := next_token(stream, []token_type { .IDENTIFIER, .INTEGER_LITERAL })

    node.value = token.value

    #partial switch token.type
    {
    case .IDENTIFIER:
        node.type = .IDENTIFIER
    case .INTEGER_LITERAL:
        node.type = .INTEGER_LITERAL
    case:
        fmt.println("Failed to parse term")
        fmt.printfln("Invalid token '%s' at line %i, column %i", token.value, token.line_number, token.column_number)
        os.exit(1)
    }

    return
}
