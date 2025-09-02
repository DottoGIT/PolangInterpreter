#include "token.h"
#include <stdexcept>
#include <cmath>
#include <sstream>

Token::Token() : type(TokenType::UNKNOWN), value(std::monostate()), position(Position()) {}

Token::Token(TokenType type, Position position)
    : type(type), value(std::monostate()), position(position)
{
    validate();
}

Token::Token(TokenType type, const TokenValue& value, Position position)
    : type(type), value(value), position(position)
{
    validate();
}

TokenType Token::getType() const {
    return type;
}

Token::TokenValue Token::getValue() const {
    return value;
}

Position Token::getPosition() const {
    return position;
}

void Token::throwError(const std::string& message)
{
    std::stringstream ss;
    ss << "Blad Leksera [" << position.line << ":" << position.column << "]: " << message;
    throw std::runtime_error(ss.str());
}


void Token::validate() {

    switch (type) {
        case TokenType::IDENTIFIER:
        case TokenType::STR_VALUE:
        case TokenType::COMMENT:
            if (!std::holds_alternative<std::wstring>(value))
                throwError("Spodziewano sie wartosci tekstowej");
            break;

        case TokenType::INT_VALUE:
            if (!std::holds_alternative<int>(value))
                throwError("Spodziewano sie wartosci numerycznej");
            break;

        case TokenType::DOUBLE_VALUE:
        if (!std::holds_alternative<double>(value))
            throwError("Spodziewano sie wartosci zmiennoprzecinkowej");
        break;

        case TokenType::BOOL_VALUE:
            if (!std::holds_alternative<bool>(value))
                throwError("Spodziewano sie wartosci prawda/falsz");
            break;

        case TokenType::IF: 
        case TokenType::FOR: 
        case TokenType::ELSE: 
        case TokenType::RETURN: 
        case TokenType::FUNC_DEF:
        case TokenType::PLUS: 
        case TokenType::MINUS: 
        case TokenType::EQ: 
        case TokenType::NEQ: 
        case TokenType::AND: 
        case TokenType::OR:
        case TokenType::FUNC_START: 
        case TokenType::FUNC_RET: 
        case TokenType::RETURN_ARROW:
        case TokenType::LSCOPE: 
        case TokenType::RSCOPE: 
        case TokenType::THEN: 
        case TokenType::ELIF: 
        case TokenType::CONST_T: 
        case TokenType::FUNC_OP_BIND: 
        case TokenType::FUNC_OP_EXAMPLE:
        case TokenType::BOOLEAN: 
        case TokenType::INTEGER: 
        case TokenType::DOUBLE: 
        case TokenType::STRING:
        case TokenType::EMPTY_VALUE: 
        case TokenType::ASSIGN: 
        case TokenType::SEPARATOR:
        case TokenType::FOR_FOLLOW:
            if (!std::holds_alternative<std::monostate>(value))
                throwError("Slowo kluczowe przechowuje wartosc");
            break;

        default:
            break;
    }
}
