#include "parser.h"
#include "arena.h"

AstNode *create_ast_node(AstNodeType type, TokenType token_type, char *value)
{
    AstNode *node = ARENA_ALLOC(arena, AstNode);
    node->type = type;
    node->token_type = token_type;
    node->value = value;
    node->left = NULL;
    node->right = NULL;

    node->statements = NULL;
    node->statement_count = 0;
    node->statement_capacity = 0;
    node->scope_depth = INT64_MAX;
    return node;
}

static void add_statement(AstNode *program, AstNode *statement)
{
    if (program->statement_capacity == 0)
    {
        program->statement_capacity = 1000;
        program->statements = ARENA_ALLOC_ARRAY(arena, AstNode *, program->statement_capacity);
    }
    else if (program->statement_count >= program->statement_capacity)
    {
        assert(0); // todo: arena re-alloc statements
    }
    program->statements[program->statement_count++] = statement;
}

// Parser helper functions
static inline TokenType peek(Parser *parser)
{
    if (parser->current >= parser->tokens->size) return TOKEN_EOF;
    return parser->tokens->IDs[parser->current];
}

static inline TokenType peek_next(Parser *parser)
{
    if (parser->current + 1 >= parser->tokens->size) return TOKEN_EOF;
    return parser->tokens->IDs[parser->current + 1];
}

static inline TokenType previous(Parser *parser)
{
    return parser->tokens->IDs[parser->current - 1];
}

static inline char *previous_data(Parser *parser)
{
    return parser->tokens->data[parser->current - 1];
}

static inline TokenType advance(Parser *parser)
{
    if (parser->current < parser->tokens->size) 
        parser->current++;
    
    return previous(parser);
}

static inline int match(Parser *parser, TokenType type)
{
    if (peek(parser) != type) return 0;
    advance(parser);
    return 1;
}

static inline int is_at_end(Parser *parser)
{
    return peek(parser) == TOKEN_EOF || parser->current >= parser->tokens->size;
}

static inline void error_at_current(Parser *parser, const char *message)
{
    fprintf(stderr, "Error at token %zu: %s\n", parser->current, message);
    parser->had_error = 1;
}

static inline void consume(Parser *parser, TokenType type, const char *message)
{
    if (peek(parser) == type)
    {
        advance(parser);
        return;
    }
    error_at_current(parser, message);
}

static inline AstNode *parse_number(Parser *parser)
{
    return create_ast_node(AST_LITERAL, NUMBER, previous_data(parser));
}

static inline AstNode *parse_string(Parser *parser)
{
    return create_ast_node(AST_LITERAL, STRING, previous_data(parser));
}

static inline AstNode *parse_literal(Parser *parser)
{
    TokenType token = previous(parser);
    if (token == FALSE || token == TRUE || token == NIL)
        return create_ast_node(AST_LITERAL, token, NULL);
    return NULL;
}

static inline AstNode *parse_variable(Parser *parser)
{
    return create_ast_node(AST_VARIABLE, IDENTIFIER, previous_data(parser));
}

static inline AstNode *parse_this(Parser *parser)
{
    return create_ast_node(AST_VARIABLE, THIS, "this");
}
static inline AstNode *parse_super(Parser *parser)
{
    return create_ast_node(AST_VARIABLE, SUPER, "super");
}
static inline AstNode *parse_unary(Parser *parser)
{
    TokenType operator = previous(parser);
    AstNode *right = parse_precedence(parser, PREC_UNARY);
    
    AstNode *node = create_ast_node(AST_UNARY, operator, NULL);
    node->right = right;
    return node;
}

static inline AstNode* parse_grouping(Parser *parser)
{
    AstNode *expr = parse_expression(parser);

    if (match(parser, EQUAL))
    {
        AstNode *value = parse_expression(parser);
        consume(parser, RIGHT_PAREN, "Expected ')' after expression");

        AstNode *assign = create_ast_node(AST_ASSIGNMENT, EQUAL, NULL);
        assign->left = expr;
        assign->right = value;
        return assign;
    }
    consume(parser, RIGHT_PAREN, "Expected ')' after expression");

    AstNode *node = create_ast_node(AST_GROUPING, LEFT_PAREN, NULL);
    node->left = expr;
    return node;
}

static inline AstNode* parse_binary(Parser *parser, AstNode *left)
{
    TokenType operator = previous(parser);
    ParseRule *rule = get_rule(operator);
    AstNode *right = parse_precedence(parser, rule->precedence + 1);
    
    AstNode *node = create_ast_node(AST_BINARY, operator, NULL);
    node->left = left;
    node->right = right;
    return node;
}

static ParseRule rules[] =
{
    [LEFT_PAREN]    = {parse_grouping, NULL,         PREC_NONE},
    [RIGHT_PAREN]   = {NULL,           NULL,         PREC_NONE},
    [LEFT_BRACE]    = {NULL,           NULL,         PREC_NONE}, 
    [RIGHT_BRACE]   = {NULL,           NULL,         PREC_NONE},
    [COMMA]         = {NULL,           NULL,         PREC_NONE},
    [DOT]           = {NULL,           NULL,         PREC_NONE},
    [MINUS]         = {parse_unary,    parse_binary, PREC_TERM},
    [PLUS]          = {NULL,           parse_binary, PREC_TERM},
    [SEMICOLON]     = {NULL,           NULL,         PREC_NONE},
    [SLASH]         = {NULL,           parse_binary, PREC_FACTOR},
    [STAR]          = {NULL,           parse_binary, PREC_FACTOR},
    [BANG]          = {parse_unary,    NULL,         PREC_NONE},
    [BANG_EQUAL]    = {NULL,           parse_binary, PREC_EQUALITY},
    [EQUAL]         = {NULL,           NULL,         PREC_NONE},
    [EQUAL_EQUAL]   = {NULL,           parse_binary, PREC_EQUALITY},
    [GREATER]       = {NULL,           parse_binary, PREC_COMPARISON},
    [GREATER_EQUAL] = {NULL,           parse_binary, PREC_COMPARISON},
    [LESS]          = {NULL,           parse_binary, PREC_COMPARISON},
    [LESS_EQUAL]    = {NULL,           parse_binary, PREC_COMPARISON},
    [IDENTIFIER]    = {parse_variable, NULL,         PREC_NONE},
    [STRING]        = {parse_string,   NULL,         PREC_NONE},
    [NUMBER]        = {parse_number,   NULL,         PREC_NONE},
    [AND]           = {NULL,           parse_binary, PREC_AND},
    [CLASS]         = {NULL,           NULL,         PREC_NONE},
    [ELSE]          = {NULL,           NULL,         PREC_NONE},
    [FALSE]         = {parse_literal,  NULL,         PREC_NONE},
    [FOR]           = {NULL,           NULL,         PREC_NONE},
    [FUN]           = {NULL,           NULL,         PREC_NONE},
    [IF]            = {NULL,           NULL,         PREC_NONE},
    [NIL]           = {parse_literal,  NULL,         PREC_NONE},
    [OR]            = {NULL,           parse_binary, PREC_OR},
    [PRINT]         = {NULL,           NULL,         PREC_NONE},
    [RETURN]        = {NULL,           NULL,         PREC_NONE},
    [SUPER]         = {parse_super,    NULL,         PREC_NONE},
    [THIS]          = {parse_this,     NULL,         PREC_NONE},
    [TRUE]          = {parse_literal,  NULL,         PREC_NONE},
    [VAR]           = {NULL,           NULL,         PREC_NONE},
    [WHILE]         = {NULL,           NULL,         PREC_NONE},
    [TOKEN_EOF]     = {NULL,           NULL,         PREC_NONE},
};

static inline ParseRule *get_rule(TokenType type)
{
    return &rules[type];
}

AstNode *parse_func_call(Parser *parser)
{
    AstNode *func_call = create_ast_node(AST_FUNC_CALL, IDENTIFIER, NULL);
    while (!match(parser, RIGHT_PAREN))
    {
        AstNode *arg = parse_expression(parser);
        if (parser->had_error)
            break;
        add_statement(func_call, arg);
        if (!match(parser, COMMA))
        {
            if (!match(parser, RIGHT_PAREN))
                error_at_current(parser, "missing comma ,");
            break;
        }
    }

    return func_call;
}

AstNode *parse_precedence(Parser *parser, Precedence precedence)
{
    advance(parser);
    PrefixParseFn prefix_rule = get_rule(previous(parser))->prefix;
    if (prefix_rule == NULL)
    {
        error_at_current(parser, "Expected expression");
        return NULL;
    }

    AstNode *left = prefix_rule(parser);
    if (match(parser, LEFT_PAREN))
    {
        AstNode *func_call = parse_func_call(parser);
        func_call->left = left;
        left = func_call;
    }
    while (match(parser, DOT))
    {
        consume(parser, IDENTIFIER, "error");
        AstNode *property = create_ast_node(AST_PROPERTY, DOT, NULL);
        property->left = left; // instance
        prefix_rule = get_rule(previous(parser))->prefix;
        property->right = prefix_rule(parser);
        if (match(parser, LEFT_PAREN))
        {
            AstNode *func_call = parse_func_call(parser);
            func_call->left = property->right;
            property->right = func_call;
        }
        left = property;
    }

    while (precedence <= get_rule(peek(parser))->precedence)
    {
        advance(parser);
        InfixParseFn infix_rule = get_rule(previous(parser))->infix;
        left = infix_rule(parser, left);
    }

    return left;
}

AstNode *parse_expression(Parser *parser)
{
    AstNode *expr = parse_precedence(parser, PREC_ASSIGNMENT);
    if (match(parser, LEFT_PAREN))
    {
        AstNode *func_call = parse_func_call(parser);
        func_call->left = expr;
        return func_call;
    }
    
    return expr;
}

AstNode* parse_assignment(Parser *parser)
{
    AstNode *expr = parse_expression(parser);
    
    if (match(parser, EQUAL))
    {
        AstNode *value = parse_assignment(parser); // Right-associative
        
        if (expr->type == AST_VARIABLE || expr->type == AST_PROPERTY)
        {
            AstNode *assign = create_ast_node(AST_ASSIGNMENT, EQUAL, NULL);
            assign->left = expr;  // Variable being assigned to
            assign->right = value; // Value being assigned
            return assign;
        }
        
        error_at_current(parser, "Invalid assignment target");
        return NULL;
    }
    return expr;
}

static AstNode* parse_print_statement(Parser *parser)
{
    AstNode *expr = parse_expression(parser);
    consume(parser, SEMICOLON, "Expected ';' after print statement");
    
    AstNode *stmt = create_ast_node(AST_PRINT_STMT, PRINT, NULL);
    stmt->left = expr;
    return stmt;
}

static AstNode* parse_var_declaration(Parser *parser)
{
    consume(parser, IDENTIFIER, "Expected variable name");
    char *name = previous_data(parser);
    
    AstNode *initializer = NULL;
    if (match(parser, EQUAL))
    {
        initializer = parse_expression(parser);
    }
    
    consume(parser, SEMICOLON, "Expected ';' after variable declaration");
    
    AstNode *var_node = create_ast_node(AST_VAR_DECL, VAR, name);
    var_node->right = initializer;
    return var_node;
}

static AstNode* parse_expression_statement(Parser *parser)
{
    AstNode *expr = parse_assignment(parser);
    consume(parser, SEMICOLON, "Expected ';' after expression");
    
    AstNode *stmt = create_ast_node(AST_EXPRESSION_STMT, SEMICOLON, NULL);
    stmt->left = expr;
    return stmt;
}

static AstNode* parse_block(Parser *parser)
{
    AstNode *block = create_ast_node(AST_BLOCK, LEFT_BRACE, NULL);
    
    while (!match(parser, RIGHT_BRACE))
    {
        if (is_at_end(parser))
            error_at_current(parser, "missing closing brace }");

        AstNode *stmt = parse_statement(parser);
        if (stmt)
            add_statement(block, stmt);
        
        if (parser->had_error)
            break;
    }
    return block;
}

AstNode* parse_if_statement(Parser *parser)
{
    AstNode *if_expr = parse_expression(parser);
    AstNode *if_block = parse_statement(parser);

    AstNode *stmt = create_ast_node(AST_IF_STMT, IF, NULL);
    stmt->left = if_expr;
    stmt->right = if_block;
    
    AstNode *if_stmt = create_ast_node(AST_IF_STMT, IF, NULL);

    add_statement(if_stmt, stmt);

    while (match(parser, ELSE))
    {        
        AstNode *else_expr = create_ast_node(AST_LITERAL, TRUE, NULL);
        if (match(parser, IF))
            else_expr = parse_expression(parser);

        AstNode *else_block = parse_statement(parser);
        
        AstNode *stmt = create_ast_node(AST_IF_STMT, ELSE, NULL);
        stmt->left = else_expr;
        stmt->right = else_block;
        add_statement(if_stmt, stmt);
    }

    return if_stmt;
}

AstNode *parse_while_loop(Parser *parser)
{
    AstNode *while_expr = parse_expression(parser);
    AstNode *while_block = parse_statement(parser);

    AstNode *loop = create_ast_node(AST_WHILE_LOOP, WHILE, NULL);
    loop->left = while_expr;
    loop->right = while_block;
    return loop;
}

AstNode *parse_for_loop(Parser *parser)
{
    consume(parser, LEFT_PAREN, "Expected '('");
    AstNode *initializer = NULL;
    if (!match(parser, SEMICOLON))
    {
        initializer = parse_statement(parser);
    }
    AstNode *condition = NULL;
    if (!match(parser, SEMICOLON))
    {
        condition = parse_expression(parser);
        consume(parser, SEMICOLON, "Expected ';'");
    }
    AstNode *update = NULL;
    if (!match(parser, RIGHT_PAREN))
    {
        update = create_ast_node(AST_EXPRESSION_STMT, SEMICOLON, NULL);
        update->left = parse_assignment(parser);
        consume(parser, RIGHT_PAREN, "Expected ')'");
    }

    AstNode *block = parse_statement(parser);

    AstNode *loop = create_ast_node(AST_FOR_LOOP, FOR, NULL);
    loop->right = block;
    if (block->type == AST_VAR_DECL && block->right == NULL)
    {
        error_at_current(parser, "Error at 'var': Expect expression.");
    }
    add_statement(loop, initializer);
    add_statement(loop, condition);
    add_statement(loop, update);
    return loop;
}
AstNode* parse_function_declaration(Parser *parser)
{
    AstNode *func_name = parse_expression(parser);
    consume(parser, LEFT_BRACE, "Expected '{'");
    AstNode *block = parse_block(parser);

    AstNode *func = create_ast_node(AST_FUNC_DECL, FUN, NULL);
    func->left = func_name;
    func->right = block;
    return func;
};

static AstNode* parse_class_block(Parser *parser)
{
    AstNode *block = create_ast_node(AST_BLOCK, LEFT_BRACE, NULL);
    
    while (!match(parser, RIGHT_BRACE))
    {
        if (is_at_end(parser))
            error_at_current(parser, "missing closing brace }");

        AstNode *method = parse_function_declaration(parser);
        if (method)
            add_statement(block, method);
        
        if (parser->had_error)
            break;
    }
    return block;
}

AstNode* parse_statement(Parser *parser)
{
    if (match(parser, PRINT))
    {
        if (peek(parser) == IDENTIFIER && peek_next(parser) == EQUAL)
        { 
            AstNode *var_assign = parse_var_declaration(parser);
            AstNode *print_stmt = create_ast_node(AST_PRINT_STMT, PRINT, NULL);
            AstNode *var = create_ast_node(AST_VARIABLE, VAR, var_assign->value);
            print_stmt->left = var;
            
            AstNode *multi = create_ast_node(AST_MULTI_STMT, TOKEN_EOF, NULL);
            add_statement(multi, var_assign);
            add_statement(multi, print_stmt);
            return multi;
            
        }
        return parse_print_statement(parser);
    }
    
    if (match(parser, VAR))
    {
        return parse_var_declaration(parser);
    }

    if (match(parser, LEFT_BRACE))
    {
        return parse_block(parser);
    }
    if (match(parser, IF))
    {
        return parse_if_statement(parser);
    }
    if (match(parser, WHILE))
    {
        return parse_while_loop(parser);
    }
    if (match(parser, FOR))
    {
        return parse_for_loop(parser);
    }
    if (match(parser, FUN))
    {
        return parse_function_declaration(parser);
    }

    if (match(parser, RETURN))
    {
        AstNode *return_stmt = create_ast_node(AST_RETURN_STMT, RETURN, NULL);
        if (match(parser, SEMICOLON))
        {
            return return_stmt;
        }
        AstNode *expr = parse_expression(parser);
        consume(parser, SEMICOLON, "Expected ';' after print statement");
        return_stmt->left = expr;
        return return_stmt;
    }
    if (match(parser, CLASS))
    {
        consume(parser, IDENTIFIER, "Expected Class Name");
        AstNode *class_name = parse_variable(parser);
        if (match(parser, LESS))
        {
            consume(parser, IDENTIFIER, "Expected Superclass");
            AstNode *super_class = parse_variable(parser);
            class_name->left = super_class;
        }
        consume(parser, LEFT_BRACE, "Expected '{'");
        AstNode *block = parse_class_block(parser);
        AstNode *class = create_ast_node(AST_CLASS_DECL, CLASS, NULL);
        class->left = class_name;
        class->right = block;
        return class;
    }


    return parse_expression_statement(parser);
}

AstNode* parse_program(Parser *parser)
{
    AstNode *program = create_ast_node(AST_PROGRAM, 0, NULL);

    while (!is_at_end(parser) && !parser->had_error)
    {
        AstNode *stmt = parse_statement(parser);
        if (stmt)
        {
            add_statement(program, stmt);
        }
    }
    
    return program;
}

int var_exist(char **vars, size_t var_cnt, char *looking_for)
{
    for (size_t i = 0; i < var_cnt; ++i)
    {
        if (is_str_eq(looking_for, vars[i]))
            return 1;
    }
    return 0;
}

void analyze_block(AstNode *node, Parser *parser)
{
    if (parser->had_error)
        return;
    char **var_names = ARENA_CALLOC_ARRAY(arena, char *, 100);
    size_t var_name_cnt = 0;
    int in_func = 0;
    AstNode *block = 0;
    if (node->type == AST_BLOCK)
        block = node;
    else if (node->type == AST_IF_STMT)
    {
        for (int i = 0; i < node->statement_count; ++i)
        {
            AstNode *stmt = node->statements[i];
            if (stmt->right->type == AST_RETURN_STMT)
            {
                error_at_current(parser, "return");
                return;
            }
            if (stmt->right->type != AST_BLOCK)
                continue;
            analyze_block(stmt->right, parser);
        }
        return;
    }
    else
    {
        in_func = 1;
        for (int i = 0; i < node->left->statement_count; ++i)
        {
            if (var_exist(var_names, var_name_cnt, node->left->statements[i]->value))
            {
                error_at_current(parser, "Function parameters must have unique names");
                return;
            }
            var_names[var_name_cnt++] = node->left->statements[i]->value;
        }
        block = node->right;
    }

    for (int i = 0; i < block->statement_count; ++i)
    {
        AstNode *stmt = block->statements[i];
        if (stmt->type == AST_RETURN_STMT && !in_func)
        {
            error_at_current(parser, "Return statements are not allowed outside of functions");
            return;
        }
        if (stmt->type == AST_BLOCK || stmt->type == AST_FUNC_DECL || (stmt->type == AST_IF_STMT && !in_func))
            analyze_block(stmt, parser);

        if (stmt && stmt->type == AST_VAR_DECL)
        {
            if (stmt->right && stmt->right->type == AST_VARIABLE && is_str_eq(stmt->right->value, stmt->value))
                error_at_current(parser, "Attempting to declare local var initialized");

            if (stmt->right && stmt->right->type == AST_FUNC_CALL && stmt->right->statement_count &&
                is_str_eq(stmt->right->statements[0]->value, stmt->value))
                error_at_current(parser, "Attempting to declare local var initialized");

            if (parser->had_error)
                return;

            if (var_exist(var_names, var_name_cnt, stmt->value))
            {
                error_at_current(parser, "Attempting to redeclare in local scope");
                return;
            }
            var_names[var_name_cnt++] = stmt->value;
        }
    }
}

void analyze_expression(AstNode *node, Parser *parser)
{
    if (node->token_type == THIS)
    { 
        error_at_current(parser, "using `this` outside of a class");
    }
}
void analyze_statement(AstNode *node, Parser *parser)
{
    if (!node) return;
    switch (node->type)
    {
        case AST_EXPRESSION_STMT: analyze_expression(node->left, parser); break;
        case AST_PRINT_STMT: analyze_expression(node->left, parser); break;
        case AST_FUNC_DECL:
        {
            for (int i = 0; i < node->right->statement_count; ++i)
                analyze_statement(node->right->statements[i], parser);
            break;
        }
        case AST_CLASS_DECL:
        {
            char *super_class = 0;
            if (node->left->left)
            {
                char *this_class = node->left->value;
                super_class = node->left->left->value;
                if (is_str_eq(this_class, super_class))
                {
                    error_at_current(parser, "A class can't inherit from itself.");
                    return;
                }
            }
            for (int i = 0; i < node->right->statement_count; ++i)
            {
                AstNode *func_node = node->right->statements[i];
                for (int j = 0; j < func_node->right->statement_count; ++j)
                {
                    AstNode *stmt = func_node->right->statements[j];
                    if (super_class)
                    {
                        if (stmt->left->type == AST_PROPERTY && stmt->left->left->token_type == SUPER)
                        {
                            stmt->left->left = create_ast_node(AST_FUNC_CALL, IDENTIFIER, NULL);
                            stmt->left->left->left = create_ast_node(AST_VARIABLE, IDENTIFIER, super_class);    
                        }
                        if (stmt->type == AST_FUNC_DECL)
                        {
                            for (int k = 0; k < stmt->right->statement_count; ++k)
                            {
                                AstNode *k_stmt = stmt->right->statements[k];
                                if (k_stmt->left->type == AST_PROPERTY && k_stmt->left->left->token_type == SUPER)
                                {
                                    k_stmt->left->left = create_ast_node(AST_FUNC_CALL, IDENTIFIER, NULL);
                                    k_stmt->left->left->left = create_ast_node(AST_VARIABLE, IDENTIFIER, super_class);    
                                }
                            }
                        }
                        int d = 3;
                    }
                    if (stmt->type == AST_RETURN_STMT && stmt->left && is_str_eq(func_node->left->left->value, "init"))
                    {
                        error_at_current(parser, "constructor should not return");
                        return;
                    }
                }
            }
            break;
        }
        default:
            break;
    }
}

 void analyze(AstNode *program, Parser *parser)
{
    ArenaScope scope = arena_scope_begin(arena);
    for (size_t i = 0; i < program->statement_count && !parser->had_error; i++)
    {
        if (program->statements[i]->type == AST_BLOCK ||
            program->statements[i]->type == AST_FUNC_DECL ||
            program->statements[i]->type == AST_IF_STMT)
        {
            AstNode *block = program->statements[i];
            analyze_block(block, parser);
        }

        if (program->statements[i]->type == AST_RETURN_STMT)
            error_at_current(parser, "Return statements are not allowed at the top level");

        analyze_statement(program->statements[i], parser);
    }
    // arena_scope_end(arena, scope);
}