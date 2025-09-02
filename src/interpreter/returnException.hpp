#pragma once

#include <stdexcept>
#include <variant>
#include <optional>

class Function;

using ExprValue = std::variant<std::wstring, int, double, bool, Function*>;

class ReturnException : public std::exception
{
public:
    explicit ReturnException(std::optional<ExprValue> value)
        : returnValue(std::move(value)) {}

    const char* what() const noexcept override {
        return "Function returned a value";
    }

    const std::optional<ExprValue>& getValue() const {
        return returnValue;
    }

private:
    std::optional<ExprValue> returnValue;
};
