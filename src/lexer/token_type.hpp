#pragma once

#include <string>
#include <ostream>

enum class TokenType {
    UNKNOWN,
    IDENTIFIER,
    INTEGER_LITERAL,
    EQUAL_OPERATOR,
    EOF_TOKEN,
    NONE,
    DOT,
    LROUND,
    RROUND,
    LSCOPE,
    RSCOPE,
    FOR,
    IF,
    THEN,
    ELIF,
    ELSE,
    PLUS,
    MINUS,
    CONNECT,
    DIVIDE,
    MULTI,
    EQ,
    NEQ,
    GREATER,
    LESSER,
    NOT,
    AND,
    OR,
    FUNC_DEF,
    FUNC_RET,
    FUNC_START,
    RETURN_ARROW,
    RETURN,
    SEPARATOR,
    FUNC_OP_EXAMPLE,
    FUNC_OP_BIND,
    ASSIGN,
    EMPTY_VALUE,
    CONST_T,
    BOOLEAN,
    INTEGER,
    DOUBLE,
    STRING,
    BOOL_VALUE,
    INT_VALUE,
    DOUBLE_VALUE,
    STR_VALUE,
    COMMENT,
    CAST,
    FOR_FOLLOW
};

inline std::wostream& operator<<(std::wostream& os, TokenType type)
{
    switch (type)
    {
        case TokenType::UNKNOWN:          return os << L"UNKNOWN";
        case TokenType::IDENTIFIER:       return os << L"IDENTIFIER";
        case TokenType::INTEGER_LITERAL:  return os << L"INTEGER_LITERAL";
        case TokenType::EQUAL_OPERATOR:   return os << L"EQUAL_OPERATOR";
        case TokenType::EOF_TOKEN:        return os << L"EOF_TOKEN";
        case TokenType::DOT:              return os << L"DOT";
        case TokenType::LROUND:           return os << L"LROUND";
        case TokenType::RROUND:           return os << L"RROUND";
        case TokenType::LSCOPE:           return os << L"LSCOPE";
        case TokenType::RSCOPE:           return os << L"RSCOPE";
        case TokenType::FOR:              return os << L"FOR";
        case TokenType::IF:               return os << L"IF";
        case TokenType::THEN:             return os << L"THEN";
        case TokenType::ELIF:             return os << "ELIF";
        case TokenType::ELSE:             return os << L"ELSE";
        case TokenType::PLUS:             return os << L"PLUS";
        case TokenType::MINUS:            return os << L"MINUS";
        case TokenType::CONNECT:          return os << L"CONNECT";
        case TokenType::DIVIDE:           return os << L"DIVIDE";
        case TokenType::MULTI:            return os << L"MULTI";
        case TokenType::EQ:               return os << L"EQ";
        case TokenType::NEQ:              return os << L"NEQ";
        case TokenType::GREATER:          return os << L"GREATER";
        case TokenType::LESSER:           return os << L"LESSER";
        case TokenType::NOT:              return os << L"NOT";
        case TokenType::AND:              return os << L"AND";
        case TokenType::OR:               return os << L"OR";
        case TokenType::FUNC_DEF:         return os << L"FUNC_DEF";
        case TokenType::FUNC_RET:         return os << L"FUNC_RET";
        case TokenType::FUNC_START:       return os << L"FUNC_START";
        case TokenType::RETURN_ARROW:     return os << L"RETURN_ARROW";
        case TokenType::RETURN:           return os << L"RETURN";
        case TokenType::SEPARATOR:        return os << L"SEPARATOR";
        case TokenType::FUNC_OP_EXAMPLE:  return os << L"FUNC_OP_EXAMPLE";
        case TokenType::FUNC_OP_BIND:     return os << L"FUNC_OP_BIND";
        case TokenType::ASSIGN:           return os << L"ASSIGN";
        case TokenType::EMPTY_VALUE:      return os << L"EMPTY_VALUE";
        case TokenType::CONST_T:          return os << L"CONST_T";
        case TokenType::BOOLEAN:          return os << L"BOOLEAN";
        case TokenType::INTEGER:          return os << L"INTEGER";
        case TokenType::DOUBLE:           return os << L"DOUBLE";
        case TokenType::STRING:           return os << L"STRING";
        case TokenType::BOOL_VALUE:       return os << L"BOOL_VALUE";
        case TokenType::INT_VALUE:        return os << L"INT_VALUE";
        case TokenType::DOUBLE_VALUE:     return os << L"DOUBLE_VALUE";
        case TokenType::STR_VALUE:        return os << L"STR_VALUE";
        case TokenType::COMMENT:          return os << L"COMMENT";
        case TokenType::CAST:             return os << L"CAST";
        case TokenType::FOR_FOLLOW:       return os << L"FOR_FOLLOW";
        default:                          return os << L"ERR_NO_TOKEN";
    }
}

inline std::ostream& operator<<(std::ostream& os, TokenType type)
{
    switch (type)
    {
        case TokenType::UNKNOWN:          return os << "UNKNOWN";
        case TokenType::IDENTIFIER:       return os << "IDENTIFIER";
        case TokenType::INTEGER_LITERAL:  return os << "INTEGER_LITERAL";
        case TokenType::EQUAL_OPERATOR:   return os << "EQUAL_OPERATOR";
        case TokenType::EOF_TOKEN:        return os << "EOF_TOKEN";
        case TokenType::DOT:              return os << "DOT";
        case TokenType::LROUND:           return os << "LROUND";
        case TokenType::RROUND:           return os << "RROUND";
        case TokenType::LSCOPE:           return os << "LSCOPE";
        case TokenType::RSCOPE:           return os << "RSCOPE";
        case TokenType::FOR:              return os << "FOR";
        case TokenType::IF:               return os << "IF";
        case TokenType::THEN:             return os << "THEN";
        case TokenType::ELIF:           return os << "ELIF";
        case TokenType::ELSE:             return os << "ELSE";
        case TokenType::PLUS:             return os << "PLUS";
        case TokenType::MINUS:            return os << "MINUS";
        case TokenType::CONNECT:          return os << "CONNECT";
        case TokenType::DIVIDE:           return os << "DIVIDE";
        case TokenType::MULTI:            return os << "MULTI";
        case TokenType::EQ:               return os << "EQ";
        case TokenType::NEQ:              return os << "NEQ";
        case TokenType::GREATER:          return os << "GREATER";
        case TokenType::LESSER:           return os << "LESSER";
        case TokenType::NOT:              return os << "NOT";
        case TokenType::AND:              return os << "AND";
        case TokenType::OR:               return os << "OR";
        case TokenType::FUNC_DEF:         return os << "FUNC_DEF";
        case TokenType::FUNC_RET:         return os << "FUNC_RET";
        case TokenType::FUNC_START:       return os << "FUNC_START";
        case TokenType::RETURN_ARROW:     return os << "RETURN_ARROW";
        case TokenType::RETURN:           return os << "RETURN";
        case TokenType::SEPARATOR:        return os << "SEPARATOR";
        case TokenType::FUNC_OP_EXAMPLE:  return os << "FUNC_OP_EXAMPLE";
        case TokenType::FUNC_OP_BIND:     return os << "FUNC_OP_BIND";
        case TokenType::ASSIGN:           return os << "ASSIGN";
        case TokenType::EMPTY_VALUE:      return os << "EMPTY_VALUE";
        case TokenType::CONST_T:          return os << "CONST_T";
        case TokenType::BOOLEAN:          return os << "BOOLEAN";
        case TokenType::INTEGER:          return os << "INTEGER";
        case TokenType::DOUBLE:           return os << "DOUBLE";
        case TokenType::STRING:           return os << "STRING";
        case TokenType::BOOL_VALUE:       return os << "BOOL_VALUE";
        case TokenType::INT_VALUE:        return os << "INT_VALUE";
        case TokenType::DOUBLE_VALUE:     return os << "DOUBLE_VALUE";
        case TokenType::STR_VALUE:        return os << "STR_VALUE";
        case TokenType::COMMENT:          return os << "COMMENT";
        case TokenType::CAST:             return os << "CAST";
        case TokenType::FOR_FOLLOW:       return os << "FOR_FOLLOW";
        default:                          return os << "ERR_NO_TOKEN";
    }
}