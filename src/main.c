#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>

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

};

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

char * parse(char *line, char *out)
{
    char *token = strtok(line, " ");

    if (is_str_eq(token, "TRUE", strlen("TRUE")) || is_str_eq(token, "FALSE", strlen("FALSE")) || is_str_eq(token, "NIL", strlen("NIL")))
    {
        token = strtok(0, " ");
        sprintf(out, "%s", token) ;
    }
    else if (is_str_eq(token, "NUMBER", strlen("NUMBER")))
    {
        token = strtok(0, " ");
        token = strtok(0, " ");
        sprintf(out, "%s", token) ;
    }
    else if (is_str_eq(token, "STRING", strlen("STRING")))
    {
        token = strtok(0, "\"");
        sprintf(out, "%s", token);
    }
    return out;
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
        }
        
        free(file_contents);
    }
    else
    {
        fprintf(stderr, "Unknown command: %s\n", command);
        return 1;
    }

    return compile_error;
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
