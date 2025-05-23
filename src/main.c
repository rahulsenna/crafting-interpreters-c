#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>

char *read_file_contents(const char *filename);

static inline int is_str_eq(char *a, char *b, size_t len)
{
    return strncmp(a, b, len) == 0;
}
char *reserved[16] = {
    "and", "class", "else", "false", "for", "fun", "if", "nil", "or", "print", "return", "super", "this", "true", "var", "while"
};
char *reservedU[16] = {
    "AND", "CLASS", "ELSE", "FALSE", "FOR", "FUN", "IF", "NIL", "OR", "PRINT", "RETURN", "SUPER", "THIS", "TRUE", "VAR", "WHILE"
};

int tokenize(const char *file_contents, char *tokens)
{
    int compile_error = 0;
    size_t line_number = 1;
    const size_t file_len = strlen(file_contents);
    char *write_ptr = tokens;

    if (file_len == 0)
    {
        write_ptr += sprintf(write_ptr, "EOF  null\n");
        return 0;
    }
    #define APPEND(...) write_ptr += sprintf(write_ptr, __VA_ARGS__)
    for (size_t i = 0; i < file_len; ++i)
    {
        switch (file_contents[i])
        {
            case '(': APPEND("LEFT_PAREN ( null\n"); break;
            case ')': APPEND("RIGHT_PAREN ) null\n"); break;
            case '{': APPEND("LEFT_BRACE { null\n"); break;
            case '}': APPEND("RIGHT_BRACE } null\n"); break;
            case '*': APPEND("STAR * null\n"); break;
            case '.': APPEND("DOT . null\n"); break;
            case ',': APPEND("COMMA , null\n"); break;
            case '+': APPEND("PLUS + null\n"); break;
            case '-': APPEND("MINUS - null\n"); break;
            case ';': APPEND("SEMICOLON ; null\n"); break;

            case '=':
                if (file_contents[i + 1] == '=')
                {
                    APPEND("EQUAL_EQUAL == null\n");
                    i++;
                } else {
                    APPEND("EQUAL = null\n");
                }
                break;

            case '!':
                if (file_contents[i + 1] == '=')
                {
                    APPEND("BANG_EQUAL != null\n");
                    i++;
                }
                else
                {
                    APPEND("BANG ! null\n");
                }
                break;

            case '<':
                if (file_contents[i + 1] == '=')
                {
                    APPEND("LESS_EQUAL <= null\n");
                    i++;
                }
                else
                {
                    APPEND("LESS < null\n");
                }
                break;

            case '>':
                if (file_contents[i + 1] == '=')
                {
                    APPEND("GREATER_EQUAL >= null\n");
                    i++;
                }
                else
                {
                    APPEND("GREATER > null\n");
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
                    APPEND("SLASH / null\n");
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
                    compile_error = 65;
                    break;
                }

                APPEND("STRING \"%s\" %s\n", str_val, str_val);
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

                    APPEND("NUMBER %s ", num);
                    if (is_decimal)
                    {
                        while (num[j-1] == '0' && num[j-2] != '.') num[--j] = '\0';
                        APPEND("%s\n", num);
                    }
                    else
                    {
                        APPEND("%s.0\n", num);
                    }
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
                    for (int k = 0; k < 16; ++k)
                    {
                        if (is_str_eq(reserved[k], id, strlen(reserved[k])))
                        {
                            APPEND("%s %s null\n", reservedU[k], reserved[k]);
                            is_keyword = 1;
                            break;
                        }
                    }
                    if (!is_keyword)
                        APPEND("IDENTIFIER %s null\n", id);
                    
                }
                else
                {
                    fprintf(stderr, "[line %lu] Error: Unexpected character: %c\n", line_number, file_contents[i]);
                    compile_error = 65;
                }
        }
    }

    APPEND("EOF  null\n");
    #undef APPEND
    return compile_error;
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

    int compile_error = 0; // (NO_ERROR)

    if (is_str_eq(command, "tokenize", strlen("tokenize")))
    {
        // You can use print statements as follows for debugging, they'll be visible when running tests.
        // fprintf(stderr, "Logs from your program will appear here!\n");
        
        char *file_contents = read_file_contents(argv[2]);

        char tokens[4096];
        compile_error = tokenize(file_contents, tokens);
        printf("%s", tokens);
        
        // fflush(stderr);
        // fflush(stdout);
        free(file_contents);
    }
    else if (is_str_eq(command, "parse", strlen("parse")))
    {
        char *file_contents = read_file_contents(argv[2]);
        char tokens[4096];
        compile_error = tokenize(file_contents, tokens);

        char *lines[100];
        int total_lines = 0;
        char *line_start = tokens;
        while (*line_start)
        {
            lines[total_lines++] = line_start;
            char *line_end = strchr(line_start, '\n'); // Find the end of the line
            if (line_end) *line_end = 0, line_start = line_end + 1;
            else break;
        }

        total_lines--;
        for (int i = 0; i < total_lines; ++i)
        {
            char *line = lines[i];
            char *token = strtok(line, " ");

            if (is_str_eq(token, "TRUE", strlen("TRUE")) || is_str_eq(token, "FALSE", strlen("FALSE")) || is_str_eq(token, "NIL", strlen("NIL")))
            {
                token = strtok(0, " ");
                printf("%s\n", token);
            }
            else if (is_str_eq(token, "NUMBER", strlen("NUMBER")))
            {
                token = strtok(0, " ");
                token = strtok(0, " ");
                printf("%s\n", token);
            }
            else if (is_str_eq(token, "STRING", strlen("STRING")))
            {
                token = strtok(0, "\"");
                token = strtok(0, "\"");
                printf("%s\n", token+1/* +1 for whitespace */);
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
