#pragma once

#include <stddef.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>
#include <stdio.h>
#include <ctype.h>
#include <time.h>

#include "arena.h"

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
    AST_BLOCK,
    AST_MULTI_STMT,
    AST_IF_STMT,
    AST_WHILE_LOOP,
    AST_FOR_LOOP,
    AST_FUNC_CALL,
    AST_FUNC_DECL,
    AST_RETURN_STMT,
    AST_CLASS_DECL,
    AST_PROPERTY,
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
    size_t scope_depth;
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

AstNode* parse_statement(Parser *parser);
AstNode* parse_expression(Parser *parser);
AstNode* parse_precedence(Parser *parser, Precedence precedence);
AstNode *create_ast_node(AstNodeType type, TokenType token_type, char *value);
static inline ParseRule *get_rule(TokenType type);
AstNode* parse_program(Parser *parser);
void analyze(AstNode *program, Parser *parser);


extern Arena *arena;

