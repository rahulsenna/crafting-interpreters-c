#include <stdio.h>
#include <stdlib.h>
#include <string.h>

char *read_file_contents(const char *filename);

static inline int is_str_eq(char *a, char *b, size_t len)
{
    return strncmp(a, b, len) == 0;
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


        if (strlen(file_contents) <= 0)
        {
            printf("EOF  null\n");
            return 0;
        } 
        size_t line_number = 1;
        size_t file_len = strlen(file_contents);
        for (size_t i = 0; i < file_len; ++i)
        {
             // https://godbolt.org/z/zjGhcjo7j # switch vs if-else | -O3 assembly output,pretty much the same, NO jmp table
             if (file_contents[i] == '(')
             { 
                printf("LEFT_PAREN ( null\n");
        	 }
             else if (file_contents[i] == ')')
             {
                printf("RIGHT_PAREN ) null\n");
             }
             else if (file_contents[i] == '{')
             {
                printf("LEFT_BRACE { null\n");
             }
             else if (file_contents[i] == '}')
             {
                printf("RIGHT_BRACE } null\n");
             }
             else if (file_contents[i] == '*')
             {
                printf("STAR * null\n");
             }
             else if (file_contents[i] == '.')
             {
                printf("DOT . null\n");
             }
             else if (file_contents[i] == ',')
             {
                printf("COMMA , null\n");
             }
             else if (file_contents[i] == '+')
             {
                printf("PLUS + null\n");
             }
             else if (file_contents[i] == '-')
             {
                printf("MINUS - null\n");
             }
             else if (file_contents[i] == ';')
             {
                printf("SEMICOLON ; null\n");
             }
             else if (file_contents[i] == '=')
             {
                if (file_contents[i+1] == '=')
                {
                    printf("EQUAL_EQUAL == null\n");
                    i++;
                }else
                {
                    printf("EQUAL = null\n");
                }
             }
             else if (file_contents[i] == '!')
             {
                if (file_contents[i+1] == '=')
                {
                    printf("BANG_EQUAL != null\n");
                    i++;
                }else
                {
                    printf("BANG ! null\n");
                }
             }
             else if (file_contents[i] == '<')
             {
                if (file_contents[i+1] == '=')
                {
                    printf("LESS_EQUAL <= null\n");
                    i++;
                }else
                {
                    printf("LESS < null\n");
                }
             }
             else if (file_contents[i] == '>')
             {
                if (file_contents[i+1] == '=')
                {
                    printf("GREATER_EQUAL >= null\n");
                    i++;
                }else
                {
                    printf("GREATER > null\n");
                }
             }
             else if (file_contents[i] == '/')
             {
                if (file_contents[i+1] == '/')
                {
                    while(i < file_len && file_contents[++i] != '\n');
                    line_number++;
                } else
                {
                    printf("SLASH / null\n");
                }
             }
             else if (file_contents[i] == '"')
             {
                char buf[256];
                int j = 0;

                while(i < file_len && file_contents[i] != '\n' && file_contents[++i] != '"')
                {
                    buf[j++] = file_contents[i];
                }
                if (file_contents[i] != '"')
                {
                    fprintf(stderr, "[line %lu] Error: Unterminated string.\n", line_number);
                    compile_error = 65;
                    continue;
                }
                buf[j] = 0;
                printf("STRING \"%s\" %s\n", buf, buf);
             }
             
             

             else if (file_contents[i] == ' ' || file_contents[i] == '\t')
             {
                
             }
             else if (file_contents[i] == '\n')
             {
                line_number++;
             }


             else
             {
                fprintf(stderr, "[line %lu] Error: Unexpected character: %c\n", line_number, file_contents[i]);
                compile_error = 65; // (Error: Unexpected character)
             }
             
             
        }
        printf("EOF  null\n"); // Placeholder, replace this line when implementing the scanner
        
        // fflush(stderr);
        // fflush(stdout);
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
