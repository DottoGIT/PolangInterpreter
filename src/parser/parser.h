#pragma once

#include "abstract_lexer.h"
#include "program.h"
#include "token.h"
#include "expressions.hpp"
#include "statements.hpp"

#include <optional>

using OptionalStatement = std::optional<std::unique_ptr<IStatement>>;
using OptionalExpression = std::optional<std::unique_ptr<IExpression>>;

class Parser
{
public:
    Parser(ILexer& lexer_);
    std::optional<Program> tryParseProgram();
    OptionalStatement tryParseStatement();
    bool endOfProgram();

private:
    ILexer& lexer;
    Token currentToken;
    Position statementPosition;

    [[noreturn]] void throwError(const std::wstring &message);
    
    // Statements
    
    OptionalStatement tryParseVariableDeclaration();
    OptionalStatement tryParseVariableAssignmentOrExpression();
    OptionalStatement tryParseFunctionDeclarationOrFunctionTypeVariable();
    OptionalStatement tryParseFunctionTypeVariable(bool checkForFUNCDEFToken, bool knownConst = false);
    OptionalStatement tryParseFunctionDeclaration();
    OptionalStatement tryParseScope();
    OptionalStatement tryParseLoop();
    OptionalStatement tryParseIf();
    OptionalStatement tryParseReturn();
    
    // Expressions

    OptionalExpression tryParseReturnExpression();
    OptionalExpression tryParseExpression();
    OptionalExpression tryParseLogicalExpression();
    OptionalExpression tryParseRelationalExpression();
    OptionalExpression tryParseAdditiveExpression();
    OptionalExpression tryParseMultiplicativeExpression();
    OptionalExpression tryParseCastExpression();
    OptionalExpression tryParseUnaryExpression();
    OptionalExpression tryParseValueExpression();
    OptionalExpression tryParsePrzykladExpression();
    OptionalExpression tryParseSkroconaExpression();
    OptionalExpression tryParseValue();

    OptionalExpression tryParseLiteralExpression();
    OptionalExpression tryParseIdentifierExpression();
    OptionalExpression tryParsePrioBracketsExpression();
    
    // Arguments

    std::optional<std::vector<ParserVariableType>> tryParseArgumentsDeclaration();
    std::optional<std::vector<std::unique_ptr<IExpression>>> tryParseArguments();
    std::optional<std::vector<ParserArgumentDeclaration>> tryParseArgumentList();
    
    // Variable Types

    std::optional<ParserVariableType> tryParseReturnType();
    std::optional<std::unique_ptr<FunctionType>> tryParseFunctionType(bool checkForFUNCDEFToken = true);
};