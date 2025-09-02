#pragma once

class Interpreter;
class Function;

class InterpretableStatement
{
public:
    virtual ~InterpretableStatement() = default;
    virtual void accept(Interpreter& interpreter) = 0;
};

class InterpretableExpression
{
public:
    using ExprValue = std::variant<std::wstring, int, double, bool, Function*>;
    virtual ~InterpretableExpression() = default;
    virtual ExprValue accept(Interpreter& interpreter) = 0;
};