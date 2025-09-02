#pragma once

#include "expressions.hpp"
#include "interpretable.h"
#include "interpreter.h"

#include <memory>

struct FunctionType;

using ParserVariableType = std::variant<TokenType, std::unique_ptr<FunctionType>>;
using ParserArgumentDeclaration = std::pair<ParserVariableType, std::wstring>;

struct IStatement : public InterpretableStatement
{
    IStatement(Position pos) : position(pos) {}
    virtual ~IStatement() = default;
    virtual void accept(Interpreter& interpreter) = 0;
    Position position;
};

struct FunctionType
{
    FunctionType(std::vector<ParserVariableType> args_, ParserVariableType ret_type_)
        : args(std::move(args_)), ret_type(std::move(ret_type_)) {}
    std::vector<ParserVariableType> args;
    ParserVariableType ret_type;
};

struct ExpressionStatement : public IStatement
{
    ExpressionStatement(std::unique_ptr<IExpression> expr_, Position pos)
        : IStatement(pos), expr(std::move(expr_)) {}

    std::unique_ptr<IExpression> expr;

    virtual void accept(Interpreter& interpreter) { interpreter.visit(*this); }
};

struct ReturnStatement : public IStatement
{
    ReturnStatement(std::unique_ptr<IExpression> expr_, Position pos)
        : IStatement(pos), expr(std::move(expr_)) {}

    std::unique_ptr<IExpression> expr;

    virtual void accept(Interpreter& interpreter) { interpreter.visit(*this); }
};

struct ArgumentDeclaration
{
    ArgumentDeclaration(ParserVariableType type_, const std::wstring& name_) : type(std::move(type_)), name(name_) {};
    ParserVariableType type;
    std::wstring name;
};

struct FunctionDeclaration : public IStatement
{
    FunctionDeclaration(const std::wstring& name_, std::vector<ParserArgumentDeclaration> parameters_,
                        ParserVariableType ret_type_, std::unique_ptr<IStatement> body_, Position pos)
        : IStatement(pos), name(std::move(name_)), parameters(std::move(parameters_)),
          ret_type(std::move(ret_type_)), body(std::move(body_)) {}

    std::wstring name;
    std::vector<ParserArgumentDeclaration> parameters;
    std::unique_ptr<IStatement> body;
    ParserVariableType ret_type;

    virtual void accept(Interpreter& interpreter) { interpreter.visit(*this); }
};

struct ConditionalBranch : public InterpretableStatement
{
    ConditionalBranch(std::unique_ptr<IExpression> condition_, std::unique_ptr<IStatement> body_)
        : condition(std::move(condition_)), body(std::move(body_)) {}
    std::unique_ptr<IExpression> condition;
    std::unique_ptr<IStatement> body;

    virtual void accept(Interpreter& interpreter) {interpreter.visit(*this);}
};

struct IfStatement : public IStatement
{
    IfStatement(ConditionalBranch mainBranch_, std::vector<ConditionalBranch> elseIfBranches_,
                std::unique_ptr<IStatement> elseBranch_, Position pos)
        : IStatement(pos), mainBranch(std::move(mainBranch_)),
          elseIfBranches(std::move(elseIfBranches_)), elseBranch(std::move(elseBranch_)) {}

    ConditionalBranch mainBranch;
    std::vector<ConditionalBranch> elseIfBranches;
    std::unique_ptr<IStatement> elseBranch;

    virtual void accept(Interpreter& interpreter) { interpreter.visit(*this); }
};

struct LoopStatement : public IStatement
{
    LoopStatement(std::unique_ptr<IExpression> times_, std::unique_ptr<IStatement> statement_, Position pos)
        : IStatement(pos), statement(std::move(statement_)), times(std::move(times_)) {}

    std::unique_ptr<IStatement> statement;
    std::unique_ptr<IExpression> times;

    virtual void accept(Interpreter& interpreter) { interpreter.visit(*this); }
};

struct ScopeStatement : public IStatement
{
    ScopeStatement(std::vector<std::unique_ptr<IStatement>> statements_, Position pos)
        : IStatement(pos), statements(std::move(statements_)) {}

    std::vector<std::unique_ptr<IStatement>> statements;

    virtual void accept(Interpreter& interpreter) { interpreter.visit(*this); }
};

struct VariableAssignment : public IStatement
{
    VariableAssignment(const std::wstring& varName_, std::unique_ptr<IExpression> value_, Position pos)
        : IStatement(pos), varName(varName_), value(std::move(value_)) {}

    std::wstring varName;
    std::unique_ptr<IExpression> value;

    virtual void accept(Interpreter& interpreter) { interpreter.visit(*this); }
};

struct VariableDeclaration : public IStatement
{
    VariableDeclaration(bool isConst_, const std::wstring& varName_, ParserVariableType varType_,
                        std::unique_ptr<IExpression> value_, Position pos)
        : IStatement(pos), isConst(isConst_), varName(varName_),
          varType(std::move(varType_)), value(std::move(value_)) {}

    VariableDeclaration(bool isConst_, const std::wstring& varName_, ParserVariableType varType_, Position pos)
        : IStatement(pos), isConst(isConst_), varName(varName_),
          varType(std::move(varType_)), value(nullptr) {}

    bool isConst;
    ParserVariableType varType;
    std::wstring varName;
    std::unique_ptr<IExpression> value;

    virtual void accept(Interpreter& interpreter) { interpreter.visit(*this); }
};