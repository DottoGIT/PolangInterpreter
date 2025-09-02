#pragma once

#include <variant>
#include <string>
#include <iostream>

#include "position.h"
#include "token_type.hpp"


class Token {
public:
    using TokenValue = std::variant<std::monostate, std::wstring, int, double, bool>;
    Token();
    Token(TokenType type, Position position);
    Token(TokenType type, const TokenValue& value, Position position);
    TokenType getType() const;
    TokenValue getValue() const;
    Position getPosition() const;
private:
    TokenType type;
    TokenValue value;
    Position position;
    void validate();
    void throwError(const std::string& message);
};
