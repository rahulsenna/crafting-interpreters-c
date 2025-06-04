#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include <stdint.h>

#include "arena.h"
Arena *arena;

#define MAX(a,b) ((a) > (b) ? (a) : (b))

char *read_file_contents(const char *filename);

static inline int is_str_eq_n(char *a, char *b, size_t len)
{
    return strncmp(a, b, len) == 0;
}
static inline int is_str_eq(char *a, char *b)
{
    return strcmp(a, b) == 0;
}
typedef enum TokenType
{
    AND = 0x0,
    CLASS,
    ELSE,
    FALSE,
    FOR,
    FUN,
    IF,
    NIL,
    OR,
    PRINT,
    RETURN,
    SUPER,
    THIS,
    TRUE,
    VAR,
    WHILE,
    LEFT_PAREN,
    RIGHT_PAREN,
    LEFT_BRACE,
    RIGHT_BRACE,
    STAR,
    DOT,
    COMMA,
    PLUS,
    MINUS,
    SEMICOLON,
    EQUAL_EQUAL,
    EQUAL,
    BANG_EQUAL,
    BANG,
    LESS_EQUAL,
    LESS,
    GREATER_EQUAL,
    GREATER,
    SLASH,
    STRING,
    NUMBER,
    IDENTIFIER,
    ERROR,
    TOKEN_EOF 
} TokenType;
char *reserved[] = {
    "and", "class", "else", "false", "for", "fun", "if", "nil", "or", "print", "return", "super", "this", "true", "var", "while",
    "(", ")", "{", "}", "*", ".", ",", "+", "-", ";",
    "==", "=", "!=", "!", "<=", "<", ">=", ">", "/", "", "", "", "EOF",
};
char *reservedU[] = {
    "AND", "CLASS", "ELSE", "FALSE", "FOR", "FUN", "IF", "NIL", "OR", "PRINT", "RETURN", "SUPER", "THIS", "TRUE", "VAR", "WHILE",
    "LEFT_PAREN", "RIGHT_PAREN", "LEFT_BRACE", "RIGHT_BRACE", "STAR", "DOT", "COMMA", "PLUS", "MINUS", "SEMICOLON", 
    "EQUAL_EQUAL", "EQUAL", "BANG_EQUAL", "BANG", "LESS_EQUAL", "LESS", "GREATER_EQUAL", "GREATER", "SLASH", "STRING", "NUMBER", "IDENTIFIER", "EOF",
};

typedef struct
{
    uint8_t *IDs;
    char **data;
    uint64_t size;
    int error;
} Tokens;

typedef enum AstNodeType
{
    AST_LITERAL,
    AST_UNARY,
    AST_BINARY,
    AST_GROUPING,
    AST_VARIABLE,
    AST_ASSIGNMENT,
    AST_PRINT_STMT,
    AST_EXPRESSION_STMT,
    AST_VAR_DECL,
    AST_PROGRAM,
    AST_BLOCK
} AstNodeType;

typedef struct AstNode
{
    AstNodeType type;
    TokenType token_type;
    char *value;
    struct AstNode *left;
    struct AstNode *right;
    // For program/block nodes that can have multiple children
    struct AstNode **statements;
    size_t statement_count;
    size_t statement_capacity;
} AstNode;

// Parser state
typedef struct
{
    Tokens *tokens;
    size_t current;
    int had_error;
} Parser;

typedef enum Precedence
{
    PREC_NONE,
    PREC_ASSIGNMENT, // =
    PREC_OR,         // or
    PREC_AND,        // and
    PREC_EQUALITY,   // == !=
    PREC_COMPARISON, // > >= < <=
    PREC_TERM,       // + -
    PREC_FACTOR,     // * /
    PREC_UNARY,      // ! -
    PREC_CALL,       // . ()
    PREC_PRIMARY
} Precedence;

typedef AstNode* (*PrefixParseFn)(Parser *parser);
typedef AstNode* (*InfixParseFn)(Parser *parser, AstNode *left);

typedef struct
{
    PrefixParseFn prefix;
    InfixParseFn infix;
    Precedence precedence;
} ParseRule;

AstNode* parse_expression(Parser *parser);
AstNode* parse_precedence(Parser *parser, Precedence precedence);
static inline ParseRule *get_rule(TokenType type);

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
    [SUPER]         = {NULL,           NULL,         PREC_NONE},
    [THIS]          = {NULL,           NULL,         PREC_NONE},
    [TRUE]          = {parse_literal,  NULL,         PREC_NONE},
    [VAR]           = {NULL,           NULL,         PREC_NONE},
    [WHILE]         = {NULL,           NULL,         PREC_NONE},
    [TOKEN_EOF]     = {NULL,           NULL,         PREC_NONE},
};

static inline ParseRule *get_rule(TokenType type)
{
    return &rules[type];
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
    return parse_precedence(parser, PREC_ASSIGNMENT);
}

AstNode* parse_assignment(Parser *parser)
{
    AstNode *expr = parse_expression(parser);
    
    if (match(parser, EQUAL))
    {
        AstNode *value = parse_assignment(parser); // Right-associative
        
        if (expr->type == AST_VARIABLE)
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

AstNode* parse_statement(Parser *parser, AstNode *program)
{
    if (match(parser, PRINT))
    {
        if (peek(parser) == IDENTIFIER && peek_next(parser) == EQUAL)
        { 
            AstNode *var = parse_var_declaration(parser);
            if (var)
            {
                add_statement(program, var);
                AstNode *stmt = create_ast_node(AST_PRINT_STMT, PRINT, NULL);
                AstNode *ID = create_ast_node(AST_VARIABLE, VAR, var->value);
                stmt->left = ID;
                return stmt;
            }
        }
        return parse_print_statement(parser);
    }
    
    if (match(parser, VAR))
    {
        return parse_var_declaration(parser);
    }
    
    return parse_expression_statement(parser);
}

AstNode* parse_program(Parser *parser)
{
    AstNode *program = create_ast_node(AST_PROGRAM, 0, NULL);

    while (!is_at_end(parser) && !parser->had_error)
    {
        AstNode *stmt = parse_statement(parser, program);
        if (stmt)
        {
            add_statement(program, stmt);
        }
    }
    
    return program;
}
typedef union Value
{
    double number;
    char* string;
    int boolean;
} Value;

typedef enum ValueType
{
    VAL_NUMBER,
    VAL_STRING, 
    VAL_BOOLEAN,
    VAL_NIL
} ValueType;

typedef struct RuntimeValue
{
    ValueType type;
    Value as;
} RuntimeValue;

// Runtime error handling
static int runtime_error_occurred = 0;

static void runtime_error(const char *message)
{
    fprintf(stderr, "Runtime error: %s\n", message);
    runtime_error_occurred = 1;
}

// Simple hash table for variables
#define VAR_TABLE_SIZE 1000
typedef struct VarEntry
{
    char *name;
    RuntimeValue value;
    struct VarEntry *next;
} VarEntry;

typedef struct Environment
{
    VarEntry *table[VAR_TABLE_SIZE];
} Environment;

static unsigned int hash_string(const char *str)
{
    unsigned int hash = 5381;
    int c;
    while ((c = *str++))
        hash = ((hash << 5) + hash) + c;
    return hash % VAR_TABLE_SIZE;
}

static void env_set(Environment *env, const char *name, RuntimeValue value)
{
    unsigned int index = hash_string(name);
    VarEntry *entry = env->table[index];
    
    // Check if variable already exists
    while (entry)
    {
        if (is_str_eq(entry->name, (char*)name))
        {
            entry->value = value;
            return;
        }
        entry = entry->next;
    }
    
    // Create new entry
    VarEntry *new_entry = ARENA_ALLOC(arena, VarEntry);
    new_entry->name = (char*)name;
    new_entry->value = value;
    new_entry->next = env->table[index];
    env->table[index] = new_entry;
}

static RuntimeValue* env_get(Environment *env, const char *name)
{
    unsigned int index = hash_string(name);
    VarEntry *entry = env->table[index];
    
    while (entry)
    {
        if (is_str_eq(entry->name, (char*)name))
        {
            return &entry->value;
        }
        entry = entry->next;
    }
    
    return NULL; // Variable not found
}

// Evaluation functions
static RuntimeValue eval_expression(AstNode *node, Environment *env);

static RuntimeValue make_number(double value)
{
    RuntimeValue val;
    val.type = VAL_NUMBER;
    val.as.number = value;
    return val;
}

static RuntimeValue make_boolean(int value)
{
    RuntimeValue val;
    val.type = VAL_BOOLEAN;
    val.as.boolean = value;
    return val;
}

static RuntimeValue make_nil()
{
    RuntimeValue val;
    val.type = VAL_NIL;
    return val;
}

static RuntimeValue make_string(char *value)
{
    RuntimeValue val;
    val.type = VAL_STRING;
    val.as.string = value;
    return val;
}

static int is_truthy(RuntimeValue val)
{
    if (val.type == VAL_BOOLEAN) return val.as.boolean;
    if (val.type == VAL_NIL) return 0;
    return 1;
}

static RuntimeValue eval_binary(AstNode *node, Environment *env)
{
    RuntimeValue left = eval_expression(node->left, env);
    if (runtime_error_occurred) return make_nil();
    
    RuntimeValue right = eval_expression(node->right, env);
    if (runtime_error_occurred) return make_nil();

    if (node->token_type == PLUS)
    {
        if (left.type == VAL_NUMBER && right.type == VAL_NUMBER)
            return make_number(left.as.number + right.as.number);
        else if (left.type == VAL_STRING && right.type == VAL_STRING)
        {
            char *result = arena_strcat(arena, left.as.string, right.as.string);
            return make_string(result);
        }
        else
        {
            runtime_error("Operands must be two numbers or two strings");
            return make_nil();
        }
    }

    if (node->token_type == MINUS && (left.type == VAL_NUMBER && right.type == VAL_NUMBER))
        return make_number(left.as.number - right.as.number);
    
        if (node->token_type == STAR && (left.type == VAL_NUMBER && right.type == VAL_NUMBER))
        return make_number(left.as.number * right.as.number);

    if (node->token_type == SLASH && (left.type == VAL_NUMBER && right.type == VAL_NUMBER))
        return make_number(left.as.number / right.as.number);

    if (node->token_type == GREATER && (left.type == VAL_NUMBER && right.type == VAL_NUMBER))
        return make_boolean(left.as.number > right.as.number);

    if (node->token_type == GREATER_EQUAL && (left.type == VAL_NUMBER && right.type == VAL_NUMBER))
        return make_boolean(left.as.number >= right.as.number);

    if (node->token_type == LESS && (left.type == VAL_NUMBER && right.type == VAL_NUMBER))
        return make_boolean(left.as.number < right.as.number);

    if (node->token_type == LESS_EQUAL && (left.type == VAL_NUMBER && right.type == VAL_NUMBER))
        return make_boolean(left.as.number <= right.as.number);

    if (node->token_type == EQUAL_EQUAL)
    {
        if (left.type != right.type)
            return make_boolean(0);
        switch (left.type)
        {
            case VAL_NUMBER: return make_boolean(left.as.number == right.as.number);
            case VAL_BOOLEAN: return make_boolean(left.as.boolean == right.as.boolean);
            case VAL_STRING: return make_boolean(strcmp(left.as.string, right.as.string) == 0);
            case VAL_NIL: return make_boolean(1); // nil == nil is true
        }
    }
    if (node->token_type == BANG_EQUAL)
    {
        if (left.type != right.type)
            return make_boolean(1);
        switch (left.type)
        {
            case VAL_NUMBER: return make_boolean(left.as.number != right.as.number);
            case VAL_BOOLEAN: return make_boolean(left.as.boolean != right.as.boolean);
            case VAL_STRING: return make_boolean(strcmp(left.as.string, right.as.string) != 0);
            case VAL_NIL: return make_boolean(0); // nil != nil is false
        }
    }

    runtime_error("Unknown binary operator");    
    return make_nil();
}

static RuntimeValue eval_unary(AstNode *node, Environment *env)
{
    RuntimeValue operand = eval_expression(node->right, env);
    if (runtime_error_occurred) return make_nil();
    
    switch (node->token_type)
    {
        case MINUS:
            if (operand.type == VAL_NUMBER)
                return make_number(-operand.as.number);
            runtime_error("Operand must be a number");
            return make_nil();
        case BANG:
            return make_boolean(!is_truthy(operand));
        default:
            runtime_error("Unknown unary operator");
            return make_nil();
    }
}

static RuntimeValue eval_expression(AstNode *node, Environment *env)
{
    if (!node || runtime_error_occurred) return make_nil();
    
    switch (node->type)
    {
        case AST_LITERAL:
            switch (node->token_type)
            {
                case NUMBER: return make_number(strtod(node->value, NULL));
                case STRING: return make_string(node->value);
                case TRUE:   return make_boolean(1);
                case FALSE:  return make_boolean(0);
                case NIL:    return make_nil();
                default:
                    break;
            }
            break;
            
        case AST_VARIABLE:
        {
            RuntimeValue *val = env_get(env, node->value);
            if (val)
                return *val;
            runtime_error("Undefined variable");
            return make_nil();
        }
        
        case AST_BINARY:    return eval_binary(node, env);
        case AST_UNARY:     return eval_unary(node, env);
        case AST_GROUPING:  return eval_expression(node->left, env);
        case AST_ASSIGNMENT:
        {
            RuntimeValue value = eval_expression(node->right, env);
            if (runtime_error_occurred) return make_nil();
            env_set(env, node->left->value, value);
            return value;
        }
        
        default:
            break;
    }
    
    return make_nil();
}

static void eval_statement(AstNode *node, Environment *env)
{
    if (!node || runtime_error_occurred) return;
    
    switch (node->type)
    {
        case AST_EXPRESSION_STMT:
            eval_expression(node->left, env);
            break;
            
        case AST_PRINT_STMT:
        {
            RuntimeValue val = eval_expression(node->left, env);
            if (runtime_error_occurred) return;
            
            switch (val.type)
            {
                case VAL_NUMBER:
                    printf("%.6g\n", val.as.number);
                    break;
                case VAL_STRING:
                    printf("%s\n", val.as.string);
                    break;
                case VAL_BOOLEAN:
                    printf("%s\n", val.as.boolean ? "true" : "false");
                    break;
                case VAL_NIL:
                    printf("nil\n");
                    break;
            }
            break;
        }
        
        case AST_VAR_DECL:
        {
            RuntimeValue value = make_nil();
            if (node->right)
            {
                value = eval_expression(node->right, env);
                if (runtime_error_occurred) return;
            }
            env_set(env, node->value, value);
            break;
        }
        
        default:
            break;
    }
}

int eval_program(AstNode *program)
{
    Environment env = {0};
    runtime_error_occurred = 0;
    
    for (size_t i = 0; i < program->statement_count && !runtime_error_occurred; i++)
    {
        eval_statement(program->statements[i], &env);
    }
    
    return runtime_error_occurred ? 70 : 0;
}

void print_ast(AstNode *node)
{
    if (!node) return;
    
    switch (node->type)
    {
        case AST_LITERAL:
            if (node->token_type == STRING) 
                printf("%s", node->value);
             else if (node->token_type == NUMBER)
             {
                double num = strtod(node->value, NULL);
                if (num == (int)num) 
                    printf("%.1f", num);
                 else 
                    printf("%g", num);   
             } else 
                printf("%s", reserved[node->token_type]);
            
            break;
        case AST_VARIABLE: printf("%s", node->value); break;
        case AST_UNARY:
            printf("(%s ", reserved[node->token_type]);
            print_ast(node->right);
            printf(")");
            break;
        case AST_BINARY:
            printf("(%s ", reserved[node->token_type]);
            print_ast(node->left);
            printf(" ");
            print_ast(node->right);
            printf(")");
            break;
        case AST_GROUPING:
            printf("(group ");
            print_ast(node->left);
            printf(")");
            break;
        default: break;
    }
}

Tokens tokenize(const char *file_contents)
{
    size_t line_number = 1;
    const size_t file_len = strlen(file_contents);

    Tokens tokens = {.size = 0, .error = 0};
    tokens.IDs = ARENA_CALLOC_ARRAY(arena, uint8_t, 1000000);
    tokens.data = ARENA_CALLOC_ARRAY(arena, char*, 1000000);

    for (size_t i = 0; i < file_len; ++i)
    {
        switch (file_contents[i])
        {
            case '(': tokens.IDs[tokens.size++] = LEFT_PAREN;  break;
            case ')': tokens.IDs[tokens.size++] = RIGHT_PAREN; break;
            case '{': tokens.IDs[tokens.size++] = LEFT_BRACE;  break;
            case '}': tokens.IDs[tokens.size++] = RIGHT_BRACE; break;
            case '*': tokens.IDs[tokens.size++] = STAR;        break; 
            case '.': tokens.IDs[tokens.size++] = DOT;         break; 
            case ',': tokens.IDs[tokens.size++] = COMMA;       break; 
            case '+': tokens.IDs[tokens.size++] = PLUS;        break; 
            case '-': tokens.IDs[tokens.size++] = MINUS;       break; 
            case ';': tokens.IDs[tokens.size++] = SEMICOLON;   break; 

            case '=':
                if (file_contents[i + 1] == '=')
                {
                    tokens.IDs[tokens.size++] = EQUAL_EQUAL;
                    i++;
                } else {
                    tokens.IDs[tokens.size++] = EQUAL;
                }
                break;

            case '!':
                if (file_contents[i + 1] == '=')
                {
                    tokens.IDs[tokens.size++] = BANG_EQUAL;
                    i++;
                }
                else
                {
                    tokens.IDs[tokens.size++] = BANG;
                }
                break;

            case '<':
                if (file_contents[i + 1] == '=')
                {
                    tokens.IDs[tokens.size++] = LESS_EQUAL;
                    i++;
                }
                else
                {
                    tokens.IDs[tokens.size++] = LESS;
                }
                break;

            case '>':
                if (file_contents[i + 1] == '=')
                {
                    tokens.IDs[tokens.size++] = GREATER_EQUAL;
                    i++;
                }
                else
                {
                    tokens.IDs[tokens.size++] = GREATER;
                }
                break;

            case '/':
                if (file_contents[i + 1] == '/')
                {
                    while (i < file_len && file_contents[i] != '\n') i++;
                    line_number++;
                }
                else
                {
                    tokens.IDs[tokens.size++] = SLASH;
                }
                break;

            case '"': {
                
                i++; // skip opening "
                int start = i;
                while (i < file_len && file_contents[i] != '"')
                {
                    if (file_contents[i++] == '\n')
                        line_number++;
                }
                if (file_contents[i] != '"')
                {
                    fprintf(stderr, "[line %lu] Error: Unterminated string.\n", line_number);
                    tokens.error = 65;
                    break;
                }

                tokens.IDs[tokens.size] = STRING;
                tokens.data[tokens.size] = arena_strdup_len(arena,&file_contents[start], i-start);
                tokens.size++;
                break;
            }

            case '\n': line_number++; break;
            case ' ':
            case '\t': break; // ignore whitespace

            default:
                if (isdigit(file_contents[i]))
                {
                    int is_decimal = 0;
                    int start = i;
                    for (;i < file_len && (isdigit(file_contents[i]) || file_contents[i] == '.'); ++i)
                        if (file_contents[i] == '.') is_decimal = 1;
                    
                    tokens.IDs[tokens.size] = NUMBER;
                    tokens.data[tokens.size] = arena_strdup_len(arena,&file_contents[start], i-start);
                    tokens.size++;
                    i--; // unconsume
                }
                else if (isalpha(file_contents[i]) || file_contents[i] == '_')
                {
                    int start = i;
                    for (;i < file_len && (isalnum(file_contents[i]) || file_contents[i] == '_'); ++i);
                    
                    char *word = arena_strdup_len(arena, &file_contents[start], i-start);
                    i--; // unconsume

                    int is_keyword = 0;
                    for (int k = 0; k < 16; ++k) // first 16 enums are keywords
                    {
                        if (is_str_eq_n(reserved[k], word, strlen(reserved[k])))
                        {
                            tokens.IDs[tokens.size++] = k;
                            is_keyword = 1;
                            break;
                        }
                    }
                    if (!is_keyword)
                    {
                        tokens.IDs[tokens.size] = IDENTIFIER;
                        tokens.data[tokens.size] = word;
                        tokens.size++;    
                    }   
                }
                else
                {
                    fprintf(stderr, "[line %lu] Error: Unexpected character: %c\n", line_number, file_contents[i]);
                    tokens.error = 65;
                }
        }
    }

    return tokens;
}


int main(int argc, char *argv[])
{
    arena = arena_init(ARENA_DEFAULT_SIZE);
    // Disable output buffering
    setbuf(stdout, NULL);
    setbuf(stderr, NULL);

    if (argc < 3) {
        fprintf(stderr, "Usage: ./your_program tokenize <filename>\n");
        return 1;
    }

    char *command = argv[1];

    if (is_str_eq_n(command, "tokenize", strlen("tokenize")))
    {
        // You can use print statements as follows for debugging, they'll be visible when running tests.
        // fprintf(stderr, "Logs from your program will appear here!\n");
        
        char *file_contents = read_file_contents(argv[2]);

        Tokens tokens = tokenize(file_contents);
        for (size_t i = 0; i < tokens.size; ++i)
        {
            uint8_t id = tokens.IDs[i];
            if (id == STRING)
                    printf("%s \"%s\" %s\n", reservedU[id], tokens.data[i], tokens.data[i]);
            else if (id == IDENTIFIER)
            {
                printf("IDENTIFIER %s null\n", tokens.data[i]);
            }
            else if (id == NUMBER)
            {
                double num = strtod(tokens.data[i], 0);
                if (num == (int)num) 
                    printf("%s %s %.1f\n", reservedU[id], tokens.data[i], num);
                else 
                    printf("%s %s %.10g\n", reservedU[id], tokens.data[i], num);
            }
            else
            { 
                printf("%s %s null\n", reservedU[id], reserved[id]);
            }
        }
        printf("EOF  null\n");
        
        // fflush(stderr);
        // fflush(stdout);
        return tokens.error;
    }
    else if (is_str_eq_n(command, "parse", strlen("parse")))
    {
        char *file_contents = read_file_contents(argv[2]);
        if (!file_contents) return 1;
        
        Tokens tokens = tokenize(file_contents);
        if (tokens.error)
            return tokens.error;        

        Parser parser = {&tokens, 0, 0};
        AstNode *ast = parse_expression(&parser);
        
        if (parser.had_error || !ast)
        {
            fprintf(stderr, "Parse error occurred\n");
            return 65;
        }

        print_ast(ast);
        printf("\n");
    }

    else if (is_str_eq_n(command, "evaluate", strlen("evaluate")))
    {
        char *file_contents = read_file_contents(argv[2]);
        if (!file_contents) return 1;
        
        Tokens tokens = tokenize(file_contents);
        if (tokens.error)
            return tokens.error;        

        Parser parser = {&tokens, 0, 0};
        AstNode *ast = parse_expression(&parser);
        
        Environment env = {0};
        AstNode *print = create_ast_node(AST_PRINT_STMT, PRINT, NULL);
        print->left = ast;
        eval_statement(print, &env);
        return runtime_error_occurred ? 70 : 0;

    }
    else if (is_str_eq_n(command, "run", strlen("run")))
    {
        char *file_contents = read_file_contents(argv[2]);
        if (!file_contents) return 1;
        
        Tokens tokens = tokenize(file_contents);
        if (tokens.error)
            return tokens.error;

        Parser parser = {&tokens, 0, 0};
        AstNode *program = parse_program(&parser);
        if (parser.had_error)
            return 65;

        int result = eval_program(program);
        return result;
    }
    else
    {
        fprintf(stderr, "Unknown command: %s\n", command);
        return 1;
    }

    return 0;
}

char *read_file_contents(const char *filename)
{
    FILE *file = fopen(filename, "r");
    if (file == NULL)
    {
        fprintf(stderr, "Error reading file: %s\n", filename);
        return NULL;
    }

    fseek(file, 0, SEEK_END);
    long file_size = ftell(file);
    rewind(file);

    char *file_contents = ARENA_ALLOC_ARRAY(arena, char, file_size + 1);
    if (file_contents == NULL)
    {
        fprintf(stderr, "Memory allocation failed\n");
        fclose(file);
        return NULL;
    }

    size_t bytes_read = fread(file_contents, 1, file_size, file);
    if (bytes_read < file_size)
    {
        fprintf(stderr, "Error reading file contents\n");
        fclose(file);
        return NULL;
    }

    file_contents[file_size] = '\0';
    fclose(file);

    return file_contents;
}