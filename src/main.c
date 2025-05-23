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

int tokenize(const char* file_contents, char *tokens)
{
    int compile_error = 0; // (No Error)
    tokens[0] = 0; // init with empty string

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
            sprintf(tokens, "%sLEFT_PAREN ( null\n", tokens);
        }
        else if (file_contents[i] == ')')
        {
            sprintf(tokens, "%sRIGHT_PAREN ) null\n", tokens);
        }
        else if (file_contents[i] == '{')
        {
            sprintf(tokens, "%sLEFT_BRACE { null\n", tokens);
        }
        else if (file_contents[i] == '}')
        {
            sprintf(tokens, "%sRIGHT_BRACE } null\n", tokens);
        }
        else if (file_contents[i] == '*')
        {
            sprintf(tokens, "%sSTAR * null\n", tokens);
        }
        else if (file_contents[i] == '.')
        {
            sprintf(tokens, "%sDOT . null\n", tokens);
        }
        else if (file_contents[i] == ',')
        {
            sprintf(tokens, "%sCOMMA , null\n", tokens);
        }
        else if (file_contents[i] == '+')
        {
            sprintf(tokens, "%sPLUS + null\n", tokens);
        }
        else if (file_contents[i] == '-')
        {
            sprintf(tokens, "%sMINUS - null\n", tokens);
        }
        else if (file_contents[i] == ';')
        {
            sprintf(tokens, "%sSEMICOLON ; null\n", tokens);
        }
        else if (file_contents[i] == '=')
        {
            if (file_contents[i + 1] == '=')
            {
                sprintf(tokens, "%sEQUAL_EQUAL == null\n", tokens);
                i++;
            }
            else
            {
                sprintf(tokens, "%sEQUAL = null\n", tokens);
            }
        }
        else if (file_contents[i] == '!')
        {
            if (file_contents[i + 1] == '=')
            {
                sprintf(tokens, "%sBANG_EQUAL != null\n", tokens);
                i++;
            }
            else
            {
                sprintf(tokens, "%sBANG ! null\n", tokens);
            }
        }
        else if (file_contents[i] == '<')
        {
            if (file_contents[i + 1] == '=')
            {
                sprintf(tokens, "%sLESS_EQUAL <= null\n", tokens);
                i++;
            }
            else
            {
                sprintf(tokens, "%sLESS < null\n", tokens);
            }
        }
        else if (file_contents[i] == '>')
        {
            if (file_contents[i + 1] == '=')
            {
                sprintf(tokens, "%sGREATER_EQUAL >= null\n", tokens);
                i++;
            }
            else
            {
                sprintf(tokens, "%sGREATER > null\n", tokens);
            }
        }
        else if (file_contents[i] == '/')
        {
            if (file_contents[i + 1] == '/')
            {
                while (i < file_len && file_contents[++i] != '\n')
                    ;
                line_number++;
            }
            else
            {
                sprintf(tokens, "%sSLASH / null\n", tokens);
            }
        }
        else if (file_contents[i] == '"')
        {
            char buf[256];
            int j = 0;

            while (i < file_len && file_contents[i] != '\n' && file_contents[++i] != '"')
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
            sprintf(tokens, "%sSTRING \"%s\" %s\n", tokens, buf, buf);
        }
        else if (isdigit(file_contents[i]))
        {
            char buf[256] = {file_contents[i], 0};
            int j = 1;
            int is_decimal = 0;
            while (isdigit(file_contents[++i]) || file_contents[i] == '.')
            {
                buf[j++] = file_contents[i];
                if (file_contents[i] == '.')
                    is_decimal = 1;
            }
            buf[j] = 0;

            sprintf(tokens, "%sNUMBER %s ", tokens, buf);
            if (is_decimal)
            {
                while (buf[--j] == '0' && buf[j - 1] != '.')
                    buf[j] = 0;

                sprintf(tokens, "%s%s\n", tokens, buf);
            }
            else
            {
                sprintf(tokens, "%s%s.0\n", tokens, buf);
            }

            if (!isdigit(file_contents[i])) // un-consuming char if not part of this nummber
            {
                i--;
            }
        }
        else if (isalpha(file_contents[i]) || file_contents[i] == '_')
        {
            char buf[256];
            int j = 0;
            while (isdigit(file_contents[i]) || isalpha(file_contents[i]) || file_contents[i] == '_')
            {
                buf[j++] = file_contents[i++];
            }
            buf[j] = 0;

            for (int i = 0; i < 16; ++i)
            {
                if (is_str_eq(reserved[i], buf, strlen(reserved[i])))
                {
                    sprintf(tokens, "%s%s %s null\n", tokens, reservedU[i], reserved[i]);
                    goto IDENTIFIER_END;
                }
            }
            sprintf(tokens, "%sIDENTIFIER %s null\n", tokens, buf);
        IDENTIFIER_END:
            i--; // un-consuming
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
    sprintf(tokens, "%sEOF  null\n", tokens); // Placeholder, replace this line when implementing the scanner

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
