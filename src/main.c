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
    AST_PRINT_STMT,
    AST_EXPRESSION_STMT,
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
AstNode* parse_number(Parser *parser);
AstNode* parse_string(Parser *parser);
AstNode* parse_literal(Parser *parser);
AstNode* parse_variable(Parser *parser);
AstNode* parse_unary(Parser *parser);
AstNode* parse_grouping(Parser *parser);
AstNode* parse_binary(Parser *parser, AstNode *left);
ParseRule* get_rule(TokenType type);

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

// Parser helper functions
static inline TokenType peek(Parser *parser)
{
    if (parser->current >= parser->tokens->size) return TOKEN_EOF;
    return parser->tokens->IDs[parser->current];
}

static inline TokenType previous(Parser *parser)
{
    return parser->tokens->IDs[parser->current - 1];
}

char *previous_data(Parser *parser)
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

static inline void error_at_current(Parser *parser, const char *message)
{
    fprintf(stderr, "Error at token %zu: %s\n", parser->current, message);
    parser->had_error = 1;
}

AstNode *parse_number(Parser *parser)
{
    return create_ast_node(AST_LITERAL, NUMBER, previous_data(parser));
}

AstNode *parse_string(Parser *parser)
{
    return create_ast_node(AST_LITERAL, STRING, previous_data(parser));
}

AstNode *parse_literal(Parser *parser)
{
    uint8_t token = previous(parser);
    if (token == FALSE || token == TRUE || token == NIL)
        return create_ast_node(AST_LITERAL, token, 0);
    
    return NULL;
}

AstNode *parse_variable(Parser *parser)
{
    return create_ast_node(AST_VARIABLE, IDENTIFIER, previous_data(parser));
}

AstNode *parse_unary(Parser *parser)
{
    uint8_t operator = previous(parser);
    AstNode *right = parse_precedence(parser, PREC_UNARY);
    
    AstNode *node = create_ast_node(AST_UNARY, operator, NULL);
    node->right = right;
    return node;
}

AstNode* parse_grouping(Parser *parser)
{
    AstNode *expr = parse_expression(parser);
    if (!match(parser, RIGHT_PAREN))
    {
        error_at_current(parser, "Expected ')' after expression");
        return NULL;
    }

    AstNode *node = create_ast_node(AST_GROUPING, LEFT_PAREN, NULL);
    node->left = expr;
    return node;
}

AstNode* parse_binary(Parser *parser, AstNode *left)
{
    uint8_t operator = previous(parser);
    ParseRule *rule = get_rule(operator);
    AstNode *right = parse_precedence(parser, rule->precedence + 1);
    
    AstNode *node = create_ast_node(AST_BINARY, operator, NULL);
    node->left = left;
    node->right = right;
    return node;
}

ParseRule rules[] =
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

ParseRule *get_rule(TokenType type)
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

AstNode *program;
void add_statement(AstNode *program, AstNode *statement);

// Parse chained assignments (a = b = c = expr;)
static AstNode* parse_chained_assignment(Parser *parser)
{
    size_t pre_idx = parser->current;
    int var_cnt = 0;
    for (int i = 0;
        parser->tokens->IDs[parser->current + 0+i] == EQUAL &&
        parser->tokens->IDs[parser->current + 1+i] == IDENTIFIER &&
        parser->tokens->IDs[parser->current + 2+i] == EQUAL;
        i+=2)
    {
        var_cnt++;
    }

    parser->current += var_cnt*2;
    if (!match(parser, EQUAL))
    { 
        if (!match(parser, SEMICOLON))
        {
            error_at_current(parser, "Expected ';' after expression");
        }
        return NULL;
    }
    AstNode *expr = parse_expression(parser);
    if (expr == NULL)
        return NULL;

    if (!match(parser, SEMICOLON))
    {
        error_at_current(parser, "Expected ';' after expression");
        return NULL;
    }
    size_t post_idx = parser->current;
    parser->current = pre_idx;
    for (int i = 0; i < var_cnt; ++i)
    {
        if (!match(parser, EQUAL)) 
        	assert(0);
        if (!match(parser, IDENTIFIER)) 
        	assert(0);
        
        PrefixParseFn prefix_rule = get_rule(previous(parser))->prefix;
        AstNode *var_name = prefix_rule(parser);
        var_name->left = expr;
        AstNode *var = create_ast_node(AST_VARIABLE, VAR, NULL);
        var->left = var_name;
        add_statement(program, var);
    }
    parser->current = post_idx;
    return expr;
}

AstNode* parse_assignment(Parser *parser)
{
    PrefixParseFn prefix_rule = get_rule(previous(parser))->prefix;
    if (prefix_rule == NULL)
    {
        error_at_current(parser, "Expected expression");
        return NULL;
    }
    AstNode *var_name = prefix_rule(parser);
    var_name->left = parse_chained_assignment(parser);
    AstNode *var = create_ast_node(AST_VARIABLE, VAR, NULL);
    var->left = var_name;
    return var;
}

AstNode* parse_statement_(Parser *parser, AstNodeType node_type, TokenType token_type)
{
    AstNode *expr = parse_expression(parser);
    AstNode *assignment = NULL;
    if (peek(parser) == EQUAL)
    {
        assignment = parse_chained_assignment(parser);
        if (assignment)
        {
            AstNode *var = create_ast_node(AST_VARIABLE, VAR, NULL);
            AstNode *var_name = create_ast_node(AST_VARIABLE, IDENTIFIER, expr->value);
            var->left = var_name;
            var_name->left = assignment;
            add_statement(program, var);
        }
    }
    if (!match(parser, SEMICOLON) && assignment == NULL)
    {
        error_at_current(parser, "Expected ';' after expression");
        return NULL;
    }

    AstNode *node = create_ast_node(node_type, token_type, NULL);
    node->left = expr;
    return node;
}

AstNode* parse_statement(Parser *parser)
{
    if (match(parser, PRINT))
    {
        return parse_statement_(parser, AST_PRINT_STMT, PRINT);
    }
    if (match(parser, VAR))
    {
        if (!match(parser, IDENTIFIER))
        {
            error_at_current(parser, "Error: var declaration");
            return NULL;
        }
        
        AstNode *var = parse_assignment(parser);
        return var;
    }
    if (match(parser, IDENTIFIER))
    {
        AstNode *var = parse_assignment(parser);
        return var;
    }

    return parse_statement_(parser, AST_EXPRESSION_STMT, SEMICOLON);
}

int is_at_end(Parser *parser)
{
    return peek(parser) == TOKEN_EOF || parser->current >= parser->tokens->size;
}

void add_statement(AstNode *program, AstNode *statement)
{
    if (program->statement_capacity == 0)
    {
        program->statement_capacity = 1000;
        program->statements = ARENA_ALLOC_ARRAY(arena, AstNode *,  program->statement_capacity);
    }
    else if (program->statement_count >= program->statement_capacity)
    {
        assert(0); // todo: arena re-alloc statements
    }
    program->statements[program->statement_count++] = statement;
}


AstNode* parse_program(Parser *parser)
{
    program = create_ast_node(AST_PROGRAM, 0, NULL);

    while (!is_at_end(parser))
    {
        AstNode *stmt = parse_statement(parser);
        if (stmt)
        {
            add_statement(program, stmt);
        }
        if (parser->had_error)
        {
            return program;
        }
    }
    return program;
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

typedef struct
{
    char *str;
    TokenType type;
    double number;
} Token;

#include <math.h>
#define HASHMAP_SIZE 1000
typedef struct Entry {
    char* key;
    Token* value;
    struct Entry* next;
} Entry;

typedef struct HashMap {
    Entry **entries;
} HashMap;

// Hash function using djb2 algorithm
unsigned int hash(const char* str)
{
    unsigned int hash = 5381;
    int c;
    while ((c = *str++))
        hash = ((hash << 5) + hash) + c;
    
    return hash % HASHMAP_SIZE;
}

Token *hashmap_get_token(HashMap *map, char *key)
{
    unsigned int index = hash(key);
    Entry* current = map->entries[index];

    while (current != NULL)
    {
        if (is_str_eq(current->key, key))
        {
            return current->value;
        }
        current = current->next;
    }
    return NULL;
}

// Create new hashmap
HashMap* hashmap_create()
{
    HashMap *map = ARENA_ALLOC(arena, HashMap);
    map->entries = ARENA_CALLOC_ARRAY(arena, Entry*, HASHMAP_SIZE);
    return map;
}
void hashmap_put_token(HashMap *map, char *key, Token *value)
{
    uint32_t index = hash(key);
    Entry *current = map->entries[index];
    // Check if key already exists
    while (current != NULL)
    {
        if (is_str_eq(current->key, key))
        {
            memcpy(current->value, value, sizeof(Entry));
            return;
        }
        current = current->next;
    }
    
    // Create new entry
    Entry* newEntry = ARENA_ALLOC(arena, Entry);
    assert(newEntry);
    
    newEntry->key = arena_strdup(arena, key);
    newEntry->value = (Token*)ARENA_ALLOC(arena, Token);
    memcpy(newEntry->value, value, sizeof(Token));
    newEntry->next = map->entries[index];
    map->entries[index] = newEntry;
}
HashMap *map;

double eval_arith(AstNode *ast)
{
    if (ast->token_type == STRING || ast->token_type == TRUE || ast->token_type == FALSE || ast->token_type == NIL)
    {
        return NAN;
    }
    if (ast->token_type == NUMBER)
    {
        double number = strtod(ast->value, NULL);
        return number;
    }
    if (ast->token_type == IDENTIFIER)
    {
        Token *val = hashmap_get_token(map, ast->value);
        if (!val || val->type != NUMBER)
            return NAN;
        return val->number;
    }

    if (ast->type == AST_GROUPING)
        return eval_arith(ast->left);

    if (ast->type == AST_UNARY)
    {
        if (ast->token_type == BANG)
            return NAN;
        double number = eval_arith(ast->right);
        return -number;
    }

    double a = eval_arith(ast->left);
    double b = eval_arith(ast->right);
    double result;
    switch(ast->token_type)
    {
        case PLUS:  result = a+b; break;
        case MINUS: result = a-b; break;
        case STAR:  result = a*b; break;
        case SLASH: result = a/b; break;
        default: return NAN;
    }
    return result;
}

char *eval_str(AstNode *ast)
{

    if (ast->token_type != PLUS && ast->token_type != STRING && ast->type != AST_GROUPING)
    {
        return 0;
    }

    if (ast->token_type == STRING)
    {
        return ast->value;
    }
    if (ast->token_type == IDENTIFIER)
    {
        Token *val = hashmap_get_token(map, ast->value);
        if (!val || val->type != STRING)
            return 0;
        return val->str;
    }
    if (ast->type == AST_GROUPING)
        return eval_str(ast->left);

    char *a = eval_str(ast->left);
    char *b = eval_str(ast->right);
    if (a==0 || b ==0)
        return 0;
    return arena_strcat(arena, a, b);
}

TokenType eval_boolean_expr(AstNode *ast)
{
    if (ast->type == AST_LITERAL)
        return ast->token_type;
    
    if (ast->type == AST_GROUPING)
        return eval_boolean_expr(ast->left);
    
    if (ast->type == AST_UNARY)
    {
        if (ast->token_type == MINUS)
        {
            fprintf(stderr, "Error: MINUS operator not supported in boolean context\n");
            exit(99);
        }
        return eval_boolean_expr(ast->right) == TRUE ? FALSE : TRUE;
    }

    double a = eval_boolean_expr(ast->left);
    double b = eval_boolean_expr(ast->right);

    if (a == ERROR || b == ERROR || a == STRING || a == NUMBER || b == STRING || b == NUMBER)
        return ERROR;

    TokenType result;
    switch(ast->token_type)
    {
        case EQUAL_EQUAL:  result = a==b?TRUE:FALSE; break;
        case BANG_EQUAL:   result = a!=b?TRUE:FALSE; break;
        default: return FALSE;
    }
    return result;
}

Token evaluate(AstNode *ast)
{
    TokenType token_type = ast->token_type;
    
    // ----------- [for inline debugger]-------------
    AstNodeType type = ast->type;
    AstNode *Left = ast->left;
    AstNode *Right = ast->right;
    // ----------- [for inline debugger]-------------
        
    if (token_type == TRUE || token_type == FALSE || token_type == NIL)
        return (Token){.type = token_type, .str = NULL};

    if (token_type == NUMBER || token_type == STRING || token_type == IDENTIFIER)
    {
        Token token = {.type = token_type, .str = ast->value};
        if (token_type == NUMBER)
            token.number = strtod(ast->value, NULL);
        return token;
    }

    if (ast->type == AST_UNARY)
    {
        if (token_type == MINUS)
        {
            if (ast->right->token_type != NUMBER)
            {
                exit(70);
            }
            return (Token){.type = NUMBER, .number = -strtod(ast->right->value, NULL)};
        }            
        
        if (token_type == BANG)
        {
            TokenType right_type = ast->right->token_type;
            return (Token){.type = (right_type == TRUE) ? FALSE : TRUE, .str = NULL};
        }
    }
    if (ast->type == AST_BINARY)
    {
         if (token_type == PLUS || token_type == MINUS || token_type == STAR || token_type == SLASH)
         { 
         	double number = eval_arith(ast);
            if (!isnan(number)) // it's math operation and both oprands are numeric
                return (Token){.type = NUMBER, .number = number};
            
            if (token_type != PLUS)
            	exit(70);
            
            char *str = eval_str(ast);
            if (str) // concatenation op on both string operands
                return (Token){.type = STRING, .str = str};
            
            exit(70); // if above fails its an error
         }

        // from this point only comparision ops will be considered
        double left_number = eval_arith(ast->left);
        double right_number = eval_arith(ast->right);

        if (!isnan(left_number) && !isnan(right_number)) // both oprands are numeric
        {
            int result;
            switch (token_type)
            {
                case EQUAL_EQUAL:    result = (left_number == right_number); break;
                case BANG_EQUAL:     result = (left_number != right_number); break;
                case GREATER:        result = (left_number >  right_number); break;
                case GREATER_EQUAL:  result = (left_number >= right_number); break;
                case LESS:           result = (left_number <  right_number); break;
                case LESS_EQUAL:     result = (left_number <= right_number); break;
                default: fprintf(stderr, "[%s:%d] [Error]: Not supported operator ", __FILE__, __LINE__);
                exit(99);
            }
            if (result)
                return (Token){.type = TRUE};
            else
                return (Token){.type = FALSE};
        }
        // relation operations are not possible from here
        if (token_type == GREATER || token_type == GREATER_EQUAL || token_type == LESS || token_type == LESS_EQUAL)
            exit(70);

        if (!isnan(left_number) || !isnan(right_number)) // either oprand is a number
            return (Token){.type = FALSE};
        
        char* left_str = eval_str(ast->left);
        char* right_str = eval_str(ast->right);

        if (left_str && right_str)
        {
            int is_same = is_str_eq(left_str, right_str);
            if ((is_same && token_type == EQUAL_EQUAL) || (!is_same && token_type == BANG_EQUAL))
                return (Token){.type = TRUE};
            else
                return (Token){.type = FALSE};
        }
        if (left_str || right_str)
            return (Token){.type = FALSE};

        TokenType res = eval_boolean_expr(ast);
        return (Token){.type = res};
    }
    
    if (ast->type == AST_GROUPING)
        return evaluate(ast->left);
    
    return (Token){.type = 0, .str = NULL};
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

        Token res = evaluate(ast);

        if (res.type == STRING)
            printf("%s\n", res.str);
        else if (res.type == NUMBER)
        {
            printf("%g\n", res.number);
        }
        else
        { 
            printf("%s\n", reserved[res.type]);
        }

    }
    else if (is_str_eq_n(command, "run", strlen("run")))
    {
        char *file_contents = read_file_contents(argv[2]);
        if (!file_contents) return 1;
        
        Tokens tokens = tokenize(file_contents);
        if (tokens.error)
            return tokens.error;

        Parser parser = {&tokens, 0, 0};
        AstNode *ast = parse_program(&parser);
        if (parser.had_error)
            return 65;

        map = hashmap_create();
        for (int i = 0; i < ast->statement_count; ++i)
        {            
            AstNode *this = ast->statements[i];
            if (this->token_type == VAR)
            {
                Token varname = evaluate(this->left);
                Token value = {.type= NIL};
                if (this->left->left)
                {
                    value = evaluate(this->left->left);   
                }
                 
                if (value.type == IDENTIFIER)
                {
                    Token *actual_value = hashmap_get_token(map, value.str);
                    if (!actual_value)
                    {
                        fprintf(stderr, "var err\n");
                        return 70;
                    }
                    value = *actual_value;
                }
                
                hashmap_put_token(map, varname.str, &value);
                continue;
            }
            Token res = evaluate(this->left);

            if (this->token_type != PRINT)
                continue;

            if (res.type == IDENTIFIER)
            {
                Token *val = hashmap_get_token(map, res.str);
                if (!val)
                {
                    fprintf(stderr, "var err\n");
                    return 70;
                }
                res = *val;
            }
            if (res.type == STRING)
                printf("%s\n", res.str);
            else if (res.type == NUMBER)
            {
                printf("%g\n", res.number);
            }
            else if (res.type == NIL)
            {
                printf("nil\n");
            }
            else
            {
                printf("%s\n", reserved[res.type]);
            }
        }
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