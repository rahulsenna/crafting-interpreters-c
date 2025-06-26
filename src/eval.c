#include "main.h"
#include "arena.h"
#include "eval.h"

int runtime_error_occurred = 0;

static void runtime_error(const char *message)
{
    fprintf(stderr, "Runtime error: %s\n", message);
    runtime_error_occurred = 1;
}


static Environment* create_environment(Environment *enclosing)
{
    Environment *env = ARENA_ALLOC(arena, Environment);
    env->table = ARENA_CALLOC_ARRAY(arena, VarEntry*, VAR_TABLE_SIZE);
    env->enclosing = enclosing;
    return env;
}

static unsigned int hash_string(const char *str)
{
    unsigned int hash = 5381;
    int c;
    while ((c = *str++))
        hash = ((hash << 5) + hash) + c;
    return hash % VAR_TABLE_SIZE;
}

static void env_set_with_arena(Environment *env, const char *name, RuntimeValue value, Arena *target_arena)
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
    VarEntry *new_entry = ARENA_ALLOC(target_arena, VarEntry);
    new_entry->name = (char*)name;
    new_entry->value = value;
    new_entry->next = env->table[index];
    env->table[index] = new_entry;
}

static void env_set(Environment *env, const char *name, RuntimeValue value)
{
    env_set_with_arena(env, name, value, arena);
}
Arena *obj_arena;
static void env_set_obj(Environment *env, const char *name, RuntimeValue value)
{
    env_set_with_arena(env, name, value, obj_arena);
}

static void env_assign(Environment *env, const char *name, RuntimeValue value)
{
    unsigned int index = hash_string(name);
    VarEntry *entry = env->table[index];
    
    while (entry)
    {
        if (is_str_eq(entry->name, (char*)name))
        {
            entry->value = value;
            return;
        }
        entry = entry->next;
    }

    if (env->enclosing)
        env_assign(env->enclosing, name, value);
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

    if (env->enclosing)
        return env_get(env->enclosing, name);
    
    return NULL; // Variable not found
}

static size_t env_get_depth(Environment *env, const char *name, size_t depth)
{
    unsigned int index = hash_string(name);
    VarEntry *entry = env->table[index];
    while (entry)
    {
        if (is_str_eq(entry->name, (char *)name))
            return depth;

        entry = entry->next;
    }

    if (env->enclosing)
        return env_get_depth(env->enclosing, name, depth + 1);

    return INT64_MAX; // Variable not found
}
static inline Environment *set_env_depth(Environment *env, size_t depth)
{
    for (size_t i = 0; i < depth; ++i)
        env = env->enclosing;
    return env;
}
typedef struct
{
	RuntimeValue* val;
	Environment* env;
} ValAndScope;

static ValAndScope *env_get_with_scope(Environment *env, const char *name, size_t depth)
{
    env = set_env_depth(env, depth);
    unsigned int index = hash_string(name);
    VarEntry *entry = env->table[index];
    
    while (entry)
    {
        if (is_str_eq(entry->name, (char*)name))
        {
            ValAndScope *res = ARENA_ALLOC(arena, ValAndScope);
            res->val = &entry->value;
            res->env = env;
            return res;
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
static RuntimeValue make_function(AstNode *value)
{
    RuntimeValue val;
    val.type = VAL_FUNCTION;
    val.as.node = ARENA_ALLOC(arena, AstNode);
    memcpy(val.as.node, value, sizeof(AstNode));
    val.env = 0;
    return val;
}

static RuntimeValue make_class(AstNode *value)
{
    RuntimeValue val;
    val.type = VAL_CLASS;
    val.as.node = value;
    return val;
}

void get_methods(AstNode *value, Environment *env)
{
    if (value->left->left) // has superclass
    {
        RuntimeValue *super = env_get(env, value->left->left->value);
        get_methods(super->as.node, env);
    }
    for (size_t i = 0; i < value->right->statement_count && !runtime_error_occurred; i++)
        eval_statement(value->right->statements[i], env);
}

static RuntimeValue make_class_inst(AstNode *value, Environment *env)
{
    RuntimeValue val;
    val.type = VAL_CLASS_INST;
    val.as.node = value;
    val.env = create_environment(env);

    get_methods(value, val.env);
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
    if (node->token_type == OR)
    {
        RuntimeValue left = eval_expression(node->left, env);
        if (is_truthy(left)) // short-circuit
            return left;
        RuntimeValue right = eval_expression(node->right, env);
        if (is_truthy(right))
            return right;

        return make_boolean(0);
    }

    if (node->token_type == AND)
    {
        RuntimeValue left = eval_expression(node->left, env);
        if (!is_truthy(left)) // short-circuit
            return make_boolean(0);
        RuntimeValue right = eval_expression(node->right, env);
        if (is_truthy(left) && is_truthy(right))
            return right;
        return make_boolean(0);
    }

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
            default: return make_nil();
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
            default: return make_nil();
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


int runtime_return = 0;

void set_func_params(AstNode *node, Environment *env, RuntimeValue *func, Environment *stack_params_env, Arena *target_arena)
{
    for (int i = 0; i < node->statement_count; i++) // setting parameters on stack
    {
        RuntimeValue val = eval_expression(node->statements[i], env);
        char *param_name = func->as.node->left->statements[i]->value;
        env_set_with_arena(stack_params_env, param_name, val, target_arena);
    }
}

RuntimeValue run_function(RuntimeValue *func, Environment *stack_params_env)
{
    for (size_t i = 0; i < func->as.node->right->statement_count && !runtime_error_occurred; i++)
    {
        eval_statement(func->as.node->right->statements[i], stack_params_env);
        if (runtime_return)
            break;
    }
    RuntimeValue return_val = make_nil();
    if (runtime_return)
    {
        return_val = *env_get(stack_params_env, "return");
        runtime_return = 0;
    }
    return return_val;
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

            if (node->scope_depth == INT64_MAX)
                node->scope_depth = env_get_depth(env, node->value, 0);
            RuntimeValue *val = node->scope_depth == INT64_MAX ? NULL :
                env_get(set_env_depth(env, node->scope_depth), node->value);
            if (val)
                return *val;
            char msg[1024];
            sprintf(msg, "Undefined variable: %s", node->value);
            runtime_error(msg);
            return make_nil();
        }
        
        case AST_BINARY:    return eval_binary(node, env);
        case AST_UNARY:     return eval_unary(node, env);
        case AST_GROUPING:  return eval_expression(node->left, env);
        case AST_ASSIGNMENT:
        {
            RuntimeValue value = eval_expression(node->right, env);
            if (node->left->type == AST_PROPERTY)
            {
                RuntimeValue *inst = env_get(env, node->left->left->value);
                env_set_obj(inst->env, node->left->right->value, value);
                return value;
            }
            if (runtime_error_occurred) return make_nil();
            if (node->scope_depth == INT64_MAX)
                node->scope_depth = env_get_depth(env, node->left->value, 0);
            env_assign(set_env_depth(env, node->scope_depth), node->left->value, value);
            return value;
        }
        case AST_PROPERTY:
        {
            while (node->left->type == AST_PROPERTY)
            {
                eval_expression(node->left, env);
                node->left = node->left->left;
            }

            RuntimeValue inst, val;
            if (node->left->type == AST_FUNC_CALL)
                inst = eval_expression(node->left, env);
            else
                inst = *env_get(env, node->left->value);

            if (node->right->type == AST_FUNC_CALL)
            {
                RuntimeValue *func = env_get(inst.env, node->right->left->value);
                set_func_params(node->right, env, func, inst.env, obj_arena);
                for (int i = 0; i < node->right->statement_count; i++) // setting parameters on stack
                    node->right->statements[i]->scope_depth = INT64_MAX; // reseting scope depth
                
                if (func->as.node->value)
                {
                    RuntimeValue func_inst = *env_get(env, func->as.node->value);
                    env_set_obj(inst.env, "this", func_inst);
                } else
                    env_set_obj(inst.env, "this", inst);
                val = eval_expression(node->right, inst.env);
                // val = run_function(func, inst.env);
                // val.env = inst.env;
            }
            else
            {
                RuntimeValue *v = env_get(inst.env, node->right->value);
                if (v == NULL)
                {
                    runtime_error("can't be accessed");
                    return make_nil();
                }
                val = *v;
            }

            return val;
        }
        case AST_FUNC_CALL:
        {
            char *func_name;
            if (node->left->type == AST_FUNC_CALL)
            {
                RuntimeValue val = eval_expression(node->left, env);
                func_name = val.as.node->left->left->value;
            }
            else if (node->left->type == AST_PROPERTY)
            {
                RuntimeValue val = eval_expression(node->left, env);
                func_name = val.as.node->left->left->value;
                env = val.env;
            }
            else
                func_name= node->left->value;
            if (func_name == NULL || is_str_eq(func_name, "this"))
            {
                runtime_error("Can only call functions and classes.");
                return make_nil();
            }

            if (is_str_eq(func_name, "clock"))
            {
                time_t t = time(NULL);
                return make_number(t);
            }
            else
            {
                if (node->scope_depth == INT64_MAX)
                    node->scope_depth = env_get_depth(env, func_name, 0);
                ValAndScope *func_and_scope = node->scope_depth == INT64_MAX ? NULL :
                    env_get_with_scope(env, func_name, node->scope_depth);
                if (func_and_scope == NULL || node->scope_depth == INT64_MAX)
                {
                    runtime_error("Can only call functions and classes.");
                    return make_nil();
                }
                RuntimeValue *func = func_and_scope->val;
                if (func->type == VAL_CLASS)
                {
                    RuntimeValue inst = make_class_inst(func->as.node, env);
                    VarEntry *entry = inst.env->table[hash_string("init")];
                    if (entry)
                    {
                        RuntimeValue init = entry->value;
                        env_set(inst.env, "this", inst);
                        set_func_params(node, env, &init, inst.env, arena);
                        run_function(&init, inst.env);
                    }
                    return inst;
                }

                if (node->statement_count != func->as.node->left->statement_count)
                {
                    char msg[1024];
                    sprintf(msg, "Expected %zu arguments but got %zu.", func->as.node->left->statement_count, node->statement_count);
                    runtime_error(msg);
                    return make_nil();
                }
                                
                Environment *func_env = env;
                if (func_and_scope->env)
                    func_env = func_and_scope->env;

                Environment *stack_params_env;
                ArenaScope scope = {.saved_offset = 0};
                if (func->env)
                    stack_params_env = func->env;
                else
                {
                    scope = arena_scope_begin(arena);
                    stack_params_env = create_environment(func_env);
                }

                set_func_params(node, env, func, stack_params_env, arena);
                if (scope.saved_offset == 0)
                    scope = arena_scope_begin(arena);

                RuntimeValue return_val = run_function(func, stack_params_env);
                if (return_val.type == VAL_FUNCTION)
                {
                    return_val.env = stack_params_env;
                    return return_val; // keeping the scope for closures
                }
                
                arena_scope_end(arena, scope);
                return return_val;
            }
            
        }

        default:
            break;
    }
    
    return make_nil();
}



static void eval_block(AstNode *block, Environment *env)
{
    ArenaScope scope = arena_scope_begin(arena);
    Environment *block_env = create_environment(env);
    
    for (size_t i = 0; i < block->statement_count && !runtime_error_occurred; i++)
    {
        eval_statement(block->statements[i], block_env);
        if (runtime_return)
        {
            break;
        }
    }
    if (runtime_return)
    {
        RuntimeValue *return_val = env_get(block_env, "return");
        env_set(env, "return", *return_val); 
    }
    arena_scope_end(arena, scope);
}


void eval_statement(AstNode *node, Environment *env)
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
                case VAL_NUMBER:      printf("%.10g\n", val.as.number);                      break;
                case VAL_STRING:      printf("%s\n", val.as.string);                         break;
                case VAL_BOOLEAN:     printf("%s\n", val.as.boolean ? "true" : "false");     break;
                case VAL_NIL:         printf("nil\n");                                       break;
                case VAL_FUNCTION:    printf("<fn %s>\n", val.as.node->left->left->value);   break;
                case VAL_CLASS:       printf("%s\n", val.as.node->left->value);              break;
                case VAL_CLASS_INST:  printf("%s instance\n", val.as.node->left->value);     break;
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
            if (value.type == VAL_CLASS_INST)
            {
            	for (int i = 0; i < value.as.node->right->statement_count; ++i)
            	{   // setting instance name to each method
                    char *method_name = value.as.node->right->statements[i]->left->left->value;
                    VarEntry *method = value.env->table[hash_string(method_name)];
                    method->value.as.node->value = node->value;
            	}
            }
            env_set(env, node->value, value);
            break;
        }
        case AST_BLOCK: eval_block(node, env); break;
        
        case AST_MULTI_STMT:
            for (int i = 0; i < node->statement_count; ++i)
                eval_statement(node->statements[i], env);
            break;

        case AST_IF_STMT:
        {
            for (int i = 0; i < node->statement_count; ++i)
            {
                RuntimeValue value = eval_expression(node->statements[i]->left, env);
                if (is_truthy(value))
                {
                    eval_statement(node->statements[i]->right, env);
                    break;
                }
            }
            break;
        }
        case AST_WHILE_LOOP:
        {
            while (is_truthy(eval_expression(node->left, env)) && runtime_return == 0)
            {
                eval_statement(node->right, env);
            }
            break;
        }
        case AST_FOR_LOOP:
        {
            for (
                eval_statement(node->statements[0], env);
                is_truthy(eval_expression(node->statements[1], env));
                eval_statement(node->statements[2], env)
            )
            {
                if (runtime_return)
                	break;
                eval_statement(node->right, env);
            }
            break;
        }
        case AST_FUNC_DECL:
        {
            char *function_name = node->left->left->value;
            RuntimeValue function_val = make_function(node);
            env_set(env, function_name, function_val);
            break;
        }
        case AST_CLASS_DECL:
        {
            char *class_name = node->left->value;
            RuntimeValue class_val = make_class(node);
            env_set(env, class_name, class_val);
            break;
        }
        case AST_RETURN_STMT:
        {
            RuntimeValue return_val = eval_expression(node->left, env);
            env_set(env, "return", return_val);
            runtime_return = 1;
            break;
        }


        default:
            break;
    }
}

int eval_program(AstNode *program)
{
    Environment *global_env = create_environment(NULL); // Global scope
    obj_arena = arena_init(ARENA_DEFAULT_SIZE);
    runtime_error_occurred = 0;
    
    for (size_t i = 0; i < program->statement_count && !runtime_error_occurred; i++)
    {
        eval_statement(program->statements[i], global_env);
    }
    
    return runtime_error_occurred ? 70 : 0;
}