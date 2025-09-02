#pragma once

#include <memory>
#include <vector>
#include "token.h"
#include "token_type.hpp"
#include "interpretable.h"
#include "interpreter.h"


struct IExpression : public InterpretableExpression {
    virtual ~IExpression() = default;
    virtual ExprValue accept(Interpreter& interpreter) = 0;
};

struct BinaryExpression : IExpression {
    BinaryExpression(std::unique_ptr<IExpression> lex_, TokenType op_, std::unique_ptr<IExpression> rex_)
    : lex(std::move(lex_)), op(std::move(op_)), rex(std::move(rex_)) {}
    std::unique_ptr<IExpression> lex, rex;
    TokenType op;

    virtual ExprValue accept(Interpreter& interpreter){return interpreter.visit(*this);}
};

struct CastExpression : IExpression {
    CastExpression(std::unique_ptr<IExpression> expr_, TokenType type_)
        : expr(std::move(expr_)), targetType(type_) {}
    std::unique_ptr<IExpression> expr;
    TokenType targetType;

    virtual ExprValue accept(Interpreter& interpreter){return interpreter.visit(*this);}
};

struct NegationExpression : IExpression {
    NegationExpression(std::unique_ptr<IExpression> expr_)
        : expr(std::move(expr_)) {}
    std::unique_ptr<IExpression> expr;

    virtual ExprValue accept(Interpreter& interpreter){return interpreter.visit(*this);}
};

struct LiteralExpression : IExpression {
    LiteralExpression(ExprValue literal_) : literal(literal_) {}
    ExprValue literal;

    virtual ExprValue accept(Interpreter& interpreter){return interpreter.visit(*this);}
};

struct IdentifierExpression : IExpression {
    IdentifierExpression(const std::wstring& identifier_) : identifier(identifier_) {}
    std::wstring identifier;

    virtual ExprValue accept(Interpreter& interpreter){return interpreter.visit(*this);}
};

struct ValueExpression : IExpression {
    ValueExpression(std::unique_ptr<IExpression> callee_, std::vector<std::unique_ptr<IExpression>> arguments_)
        : callee(std::move(callee_)), arguments(std::move(arguments_)) {}
    std::unique_ptr<IExpression> callee;
    std::vector<std::unique_ptr<IExpression>> arguments;

    virtual ExprValue accept(Interpreter& interpreter){return interpreter.visit(*this);}
};

struct PrzykladExpression : IExpression {
    PrzykladExpression(std::unique_ptr<IExpression> expr_)
        : expr(std::move(expr_)) {}
        std::unique_ptr<IExpression> expr;

    virtual ExprValue accept(Interpreter& interpreter){return interpreter.visit(*this);}
};

struct SkroconaExpression : IExpression {
    SkroconaExpression(std::vector<std::unique_ptr<IExpression>> arguments_, std::unique_ptr<IExpression> expr_)
        : arguments(std::move(arguments_)), expr(std::move(expr_)) {}
    std::vector<std::unique_ptr<IExpression>> arguments;
    std::unique_ptr<IExpression> expr;

    virtual ExprValue accept(Interpreter& interpreter){return interpreter.visit(*this);}
};