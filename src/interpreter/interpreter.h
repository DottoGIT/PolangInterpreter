#pragma once

#include "context.hpp"
#include "token_type.hpp"
#include "position.h"
#include <string>
#include <variant>
#include <vector>

struct IStatement;
struct ExpressionStatement;
struct ReturnStatement;
struct ArgumentDeclaration;
struct FunctionDeclaration;
struct ConditionalBranch;
struct IfStatement;
struct LoopStatement;
struct ScopeStatement;
struct VariableAssignment;
struct VariableDeclaration;
struct FunctionType;
struct BinaryExpression;
struct CastExpression;
struct NegationExpression;
struct ValueExpression;
struct PrzykladExpression;
struct SkroconaExpression;
struct LiteralExpression;
struct IdentifierExpression;

class Interpreter
{
public:
    using ParserVariableType = std::variant<TokenType, std::unique_ptr<FunctionType>>;
    using NativeFunction = std::function<ExprValue(const std::vector<ExprValue>&)>;


    Interpreter();
    ExprValue getVariableValue(const std::wstring& name);
    Function* getFunction(const std::wstring& name);
    void printAllVariables();
    void printAllFunctions();
    size_t countAllVariables();
    size_t countAllFunctions();

    void interpret(IStatement& stmt);


    // Statements
    void visit(ExpressionStatement&   obj);
    void visit(ReturnStatement&       obj);
    void visit(FunctionDeclaration&   obj);
    void visit(ConditionalBranch&     obj);
    void visit(IfStatement&           obj);
    void visit(LoopStatement&         obj);
    void visit(ScopeStatement&        obj);
    void visit(VariableAssignment&    obj);
    void visit(VariableDeclaration&   obj);

    // Expressions
    ExprValue visit(BinaryExpression&      obj);
    ExprValue visit(CastExpression&        obj);
    ExprValue visit(NegationExpression&    obj);
    ExprValue visit(ValueExpression&       obj);
    ExprValue visit(PrzykladExpression&    obj);
    ExprValue visit(SkroconaExpression&    obj);
    ExprValue visit(LiteralExpression&     obj);
    ExprValue visit(IdentifierExpression&  obj);

private:

    std::vector<std::unique_ptr<Context>> context_stack;
    std::unordered_map<std::wstring, NativeFunction> baseFunctions;

    Context* curr_context = nullptr;
    Position statementPos;
    int maxStackSize = 100;

    [[noreturn]] void throwError(const std::wstring &message);

    VariableType parseVarType(const ParserVariableType& parserType);
    bool matchesType(LiteralType declared, const ExprValue& value);
    bool functionSignatureMatches(const FunctionVariableType& expected, const Function& actual);
    bool isFunctionRetrunValid(const Function& func);
    bool variableTypesEqual(const VariableType& a, const VariableType& b);
    bool variableTypesEqual(const std::optional<VariableType>& a, const std::optional<VariableType>& b);

    Context* findVariableContext(const std::wstring& name);
    Variable* findVariable(const std::wstring& name);
    Function* findFunction(const std::wstring& name);
    Context* findFunctionContext(const std::wstring& name);
};