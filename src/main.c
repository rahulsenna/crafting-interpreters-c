#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include <stdint.h>

char *read_file_contents(const char *filename);

static inline int is_str_eq(char *a, char *b, size_t len)
{
    return strncmp(a, b, len) == 0;
}
enum TokenEnum
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
    TOKEN_EOF 
};
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

typedef enum
{
    AST_LITERAL,
    AST_UNARY,
    AST_BINARY,
    AST_GROUPING,
    AST_VARIABLE
} AstNodeType;

typedef struct AstNode
{
    AstNodeType type;
    uint8_t token_type;
    char *value;
    struct AstNode *left;
    struct AstNode *right;
} AstNode;

// Parser state
typedef struct
{
    Tokens *tokens;
    size_t current;
    int had_error;
} Parser;

typedef enum
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
AstNode* parse_grouping(Parser *parser);
ParseRule* get_rule(uint8_t type);

AstNode *create_ast_node(AstNodeType type, uint8_t token_type, char *value)
{
    AstNode *node = malloc(sizeof(AstNode));
    node->type = type;
    node->token_type = token_type;
    node->value = value;
    node->left = NULL;
    node->right = NULL;
    return node;
}

// Parser helper functions
uint8_t peek(Parser *parser)
{
    if (parser->current >= parser->tokens->size) return TOKEN_EOF;
    return parser->tokens->IDs[parser->current];
}

uint8_t previous(Parser *parser)
{
    return parser->tokens->IDs[parser->current - 1];
}

char *previous_data(Parser *parser)
{
    return parser->tokens->data[parser->current - 1];
}

uint8_t advance(Parser *parser)
{
    if (parser->current < parser->tokens->size) 
        parser->current++;
    
    return previous(parser);
}

int match(Parser *parser, uint8_t type)
{
    if (peek(parser) != type) return 0;
    advance(parser);
    return 1;
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

AstNode* parse_grouping(Parser *parser)
{
    AstNode *expr = parse_expression(parser);
    AstNode *node = create_ast_node(AST_GROUPING, LEFT_PAREN, NULL);
    node->left = expr;
    return node;
}
ParseRule rules[] =
{
    [LEFT_PAREN]    = {parse_grouping, NULL,         PREC_NONE},
    [RIGHT_PAREN]   = {NULL,           NULL,         PREC_NONE},
    [STRING]        = {parse_string,   NULL,         PREC_NONE},
    [NUMBER]        = {parse_number,   NULL,         PREC_NONE},
    [FALSE]         = {parse_literal,  NULL,         PREC_NONE},
    [NIL]           = {parse_literal,  NULL,         PREC_NONE},
    [TRUE]          = {parse_literal,  NULL,         PREC_NONE},
};

ParseRule *get_rule(uint8_t type)
{
    return &rules[type];
}

AstNode *parse_precedence(Parser *parser, Precedence precedence)
{
    advance(parser);
    PrefixParseFn prefix_rule = get_rule(previous(parser))->prefix;
    if (prefix_rule == NULL)
    {
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
        case AST_UNARY:
            printf("(%s ", reserved[node->token_type]);
            print_ast(node->right);
            printf(")");
            break;
        case AST_GROUPING:
            printf("(group ");
            print_ast(node->left);
            printf(")");
            break;
    }
}

Tokens tokenize(const char *file_contents)
{
    size_t line_number = 1;
    const size_t file_len = strlen(file_contents);

    Tokens tokens = {.size = 0, .error = 0};
    tokens.IDs = calloc(1000'1000, sizeof(uint8_t));
    tokens.data = calloc(1000'1000, sizeof(char*));

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
                char str_val[256];
                int j = 0;
                i++; // skip opening "
                while (i < file_len && file_contents[i] != '"' && file_contents[i] != '\n')
                    str_val[j++] = file_contents[i++];
                
                str_val[j] = '\0';

                if (file_contents[i] != '"')
                {
                    fprintf(stderr, "[line %lu] Error: Unterminated string.\n", line_number);
                    tokens.error = 65;
                    break;
                }

                tokens.IDs[tokens.size] = STRING;
                tokens.data[tokens.size] = strdup(str_val);
                tokens.size++;
                break;
            }

            case '\n': line_number++; break;
            case ' ':
            case '\t': break; // ignore whitespace

            default:
                if (isdigit(file_contents[i]))
                {
                    char num[256];
                    int j = 0;
                    int is_decimal = 0;

                    while (i < file_len && (isdigit(file_contents[i]) || file_contents[i] == '.'))
                    {
                        if (file_contents[i] == '.') is_decimal = 1;
                        num[j++] = file_contents[i++];
                    }
                    i--; // unconsume
                    num[j] = '\0';

                    tokens.IDs[tokens.size] = NUMBER;
                    tokens.data[tokens.size] = strdup(num);
                    tokens.size++;
                }
                else if (isalpha(file_contents[i]) || file_contents[i] == '_')
                {
                    char id[256];
                    int j = 0;

                    while (i < file_len && (isalnum(file_contents[i]) || file_contents[i] == '_'))
                    {
                        id[j++] = file_contents[i++];
                    }
                    i--; // unconsume
                    id[j] = '\0';

                    int is_keyword = 0;
                    for (int k = 0; k < 16; ++k) // first 16 enums are keywords
                    {
                        if (is_str_eq(reserved[k], id, strlen(reserved[k])))
                        {
                            tokens.IDs[tokens.size++] = k;
                            is_keyword = 1;
                            break;
                        }
                    }
                    if (!is_keyword)
                    {
                        tokens.IDs[tokens.size] = IDENTIFIER;
                        tokens.data[tokens.size] = strdup(id);
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
    // Disable output buffering
    setbuf(stdout, NULL);
    setbuf(stderr, NULL);

    if (argc < 3) {
        fprintf(stderr, "Usage: ./your_program tokenize <filename>\n");
        return 1;
    }

    char *command = argv[1];

    if (is_str_eq(command, "tokenize", strlen("tokenize")))
    {
        // You can use print statements as follows for debugging, they'll be visible when running tests.
        // fprintf(stderr, "Logs from your program will appear here!\n");
        
        char *file_contents = read_file_contents(argv[2]);

        Tokens tokens = tokenize(file_contents);
        for (size_t i = 0; i < tokens.size; ++i)
        {
            uint8_t id = tokens.IDs[i];
            char *str = tokens.data[i];
            if (str == NULL)
            { 
                printf("%s %s null\n", reservedU[id], reserved[id]);
            } else
            {
                if (id == STRING)
                    printf("%s \"%s\" %s\n", reservedU[id], str, str);
                else if (id == IDENTIFIER)
                {
                    printf("IDENTIFIER %s null\n", str);
                }
                else if (id == NUMBER)
                {
                    double num = strtod(str, 0);
                    if (num == (int)num) 
                        printf("%s %s %.1f\n", reservedU[id], str, num);
                    else 
                        printf("%s %s %.10g\n", reservedU[id], str, num);
                }
            }
        }
        printf("EOF  null\n");
        
        // fflush(stderr);
        // fflush(stdout);
        free(file_contents);
        return tokens.error;
    }
    else if (is_str_eq(command, "parse", strlen("parse")))
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

    char *file_contents = malloc(file_size + 1);
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
        free(file_contents);
        fclose(file);
        return NULL;
    }

    file_contents[file_size] = '\0';
    fclose(file);

    return file_contents;
}
