#pragma once
#include "parser.h"

typedef union Value
{
    double number;
    char* string;
    int boolean;
    AstNode *node;
} Value;

typedef enum ValueType
{
    VAL_NUMBER,
    VAL_STRING, 
    VAL_BOOLEAN,
    VAL_FUNCTION,
    VAL_CLASS,
    VAL_CLASS_INST,
    VAL_NIL
} ValueType;

struct RuntimeValue;

struct Environment;
typedef struct RuntimeValue
{
    ValueType type;
    Value as;
    struct Environment *env;
} RuntimeValue;


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
    VarEntry **table;
    struct Environment *enclosing; // Parent scope
} Environment;

// Runtime error handling
extern int runtime_error_occurred;

void eval_statement(AstNode *node, Environment *env);
int eval_program(AstNode *program);