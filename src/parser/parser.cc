#include "parser.h"

#include <vector>
#include <memory>
#include <sstream>
#include <stdexcept>

using OptionalStatement = std::optional<std::unique_ptr<IStatement>>;
using OptionalExpression = std::optional<std::unique_ptr<IExpression>>;

Parser::Parser(ILexer &lexer_) : lexer(lexer_)
{
    currentToken = lexer.nextToken();
}

bool Parser::endOfProgram()
{
    return currentToken.getType() == TokenType::EOF_TOKEN;
}

void Parser::throwError(const std::wstring &message)
{
    std::wcout << L"Błąd Parsera: [" << currentToken.getPosition().line << L":" << currentToken.getPosition().column << L"]: " << message;
    throw std::runtime_error("");
}

std::optional<Program> Parser::tryParseProgram()
{
    std::vector<std::unique_ptr<IStatement>> statements;
    while (auto statement = tryParseStatement())
    {
        statements.push_back(std::move(statement.value()));
    }
    
    if (!endOfProgram())
        throwError(L"Nieznany początek instrukcji.");

    return Program(std::move(statements));
}

////////////////////////////////////////////////////////////////////////////////////////////////////////
////                                 S T A T E M E N T S                                            ////
////////////////////////////////////////////////////////////////////////////////////////////////////////

OptionalStatement Parser::tryParseStatement()
{
    statementPosition = currentToken.getPosition();
    if (auto statementOpt = tryParseVariableDeclaration())                          { return statementOpt; }
    if (auto statementOpt = tryParseVariableAssignmentOrExpression())               { return statementOpt; }
    if (auto statementOpt = tryParseScope())                                        { return statementOpt; }
    if (auto statementOpt = tryParseLoop())                                         { return statementOpt; }
    if (auto statementOpt = tryParseIf())                                           { return statementOpt; }
    if (auto statementOpt = tryParseFunctionDeclarationOrFunctionTypeVariable())    { return statementOpt; }
    if (auto statementOpt = tryParseReturn())                                       { return statementOpt; }
    return std::nullopt;
}

OptionalStatement Parser::tryParseVariableDeclaration()
{
    bool isConst = false;
    std::wstring varName;
    TokenType varType;
    std::unique_ptr<VariableDeclaration> retVar;

    if(currentToken.getType() == TokenType::CONST_T)
    {
        isConst = true;
        currentToken = lexer.nextToken();
        if (auto functionDeclVar = tryParseFunctionTypeVariable(true, true)) return functionDeclVar;
    }

    if (currentToken.getType() == TokenType::STRING  ||
        currentToken.getType() == TokenType::INTEGER ||
        currentToken.getType() == TokenType::BOOLEAN ||
        currentToken.getType() == TokenType::DOUBLE)
    {
        varType = currentToken.getType();
    }
    else if(isConst)
    {
        throwError(L"Nieprawidłowa dekleracja stałej zmiennej.");
    }
    else
    {
        return std::nullopt;
    }
        
    currentToken = lexer.nextToken();
    if(currentToken.getType() != TokenType::IDENTIFIER)
        throwError(L"Brak identyfikatora przy deklaracji zmiennej.");

    varName = std::get<std::wstring>(currentToken.getValue());

    currentToken = lexer.nextToken();
    if(currentToken.getType() != TokenType::ASSIGN)
        throwError(L"Brak operatora przypisania przy deklaracji zmiennej.");

    currentToken = lexer.nextToken();
    
    if(currentToken.getType() == TokenType::EMPTY_VALUE)
    {
        retVar = std::make_unique<VariableDeclaration>(isConst, varName, varType, statementPosition);
        currentToken = lexer.nextToken();
    }
    else if(auto value = tryParseExpression())
    {
        retVar = std::make_unique<VariableDeclaration>(isConst, varName, varType, std::move(value.value()), statementPosition);
    }
    else{
        throwError(L"Brak wartości przy deklarowaniu zmiennej.");
    }

    if(currentToken.getType() != TokenType::DOT)
        throwError(L"Brak operatora zakończenia instrukcji przy deklarowaniu zmiennej.");
    currentToken = lexer.nextToken();

    return retVar;
}

OptionalStatement Parser::tryParseVariableAssignmentOrExpression()
{
    auto expr = tryParseExpression();
    if(!expr) return std::nullopt;

    auto identCast = dynamic_cast<IdentifierExpression*>(expr.value().get());
    if(identCast != nullptr && currentToken.getType() == TokenType::ASSIGN)
    {
        std::wstring identifier = identCast->identifier;
        currentToken = lexer.nextToken();
        expr = tryParseExpression();
        if(!expr) throwError(L"Brak wyrażenia po operatorze przypisania.");
        
        if(currentToken.getType() != TokenType::DOT)
            throwError(L"Brak operatora zakończenia instrukcji przy przypisaniu zmiennej.");

        currentToken = lexer.nextToken();
        return std::make_unique<VariableAssignment>(identifier, std::move(expr.value()), statementPosition);
    }
    
    if(currentToken.getType() != TokenType::DOT)
        throwError(L"Brak operatora zakończenia instrukcji po wyrażeniu.");
    currentToken = lexer.nextToken();

    return std::make_unique<ExpressionStatement>(std::move(expr.value()), statementPosition);
}

OptionalStatement Parser::tryParseFunctionDeclarationOrFunctionTypeVariable()
{
    if (currentToken.getType() != TokenType::FUNC_DEF)
        return std::nullopt;
    currentToken = lexer.nextToken();

    if (auto statementOpt = tryParseFunctionDeclaration())  { return statementOpt; }
    if (auto statementOpt = tryParseFunctionTypeVariable(false)) { return statementOpt; }

    throwError(L"Brak nazwy nowej funkcji lub brak argumentów w typie funkcyjnym.");
}

OptionalStatement Parser::tryParseFunctionDeclaration()
{
    if(currentToken.getType() != TokenType::IDENTIFIER)
    {
        return std::nullopt;
    }

    std::wstring identifier = std::get<std::wstring>(currentToken.getValue()); 
    currentToken = lexer.nextToken();
    
    auto arg_list = tryParseArgumentList();
    if(!arg_list)
        throwError(L"Brak podanych argumentów dla nowej pracy.");
    
    if (currentToken.getType() != TokenType::FUNC_RET)
        throwError(L"Brak słowa kluczowego 'daje' po liście argumentów nowej pracy.");
    
    currentToken = lexer.nextToken();
    auto ret_type = tryParseReturnType();
    if(!ret_type)
        throwError(L"Nie podano typu zwracanego przez nową pracę.");
    
    if (currentToken.getType() != TokenType::FUNC_START)
        throwError(L"Brak słowa kluczowego 'robi' po typie zwracanym przez nową pracę.");
    
    currentToken = lexer.nextToken();
    auto funcInstructions = tryParseStatement();
    if (!funcInstructions)
        throwError(L"Brak ciała pracy po jej deklaracji.");
        
    return std::make_unique<FunctionDeclaration>(identifier, std::move(arg_list.value()), std::move(ret_type.value()), std::move(funcInstructions.value()), statementPosition);
}

OptionalStatement Parser::tryParseFunctionTypeVariable(bool checkForFUNCDEFToken, bool knownConst)
{
        auto varType = tryParseFunctionType(checkForFUNCDEFToken);
        if(!varType) return std::nullopt;
        
        if(currentToken.getType() != TokenType::IDENTIFIER)
            throwError(L"Brak identyfikatora przy deklaracji zmiennej pracy.");
    
        auto varName = std::get<std::wstring>(currentToken.getValue());
        std::unique_ptr<VariableDeclaration> retVar;
    
        currentToken = lexer.nextToken();
        if(currentToken.getType() != TokenType::ASSIGN)
            throwError(L"Brak operatora przypisania przy deklaracji zmiennej pracy.");
    
        currentToken = lexer.nextToken();
        
        if(currentToken.getType() == TokenType::EMPTY_VALUE)
        {
            retVar = std::make_unique<VariableDeclaration>(knownConst, varName, std::move(varType.value()), statementPosition);
            currentToken = lexer.nextToken();
        }
        else if(auto value = tryParseExpression())
        {
            retVar = std::make_unique<VariableDeclaration>(knownConst, varName, std::move(varType.value()), std::move(value.value()), statementPosition);
        }
        else{
            throwError(L"Brak wartości przy deklarowaniu zmiennej pracy.");
        }
        
        if(currentToken.getType() != TokenType::DOT)
            throwError(L"Brak operatora zakończenia instrukcji przy deklarowaniu zmiennej.");
        currentToken = lexer.nextToken();

        return retVar;
}

OptionalStatement Parser::tryParseScope()
{
    if(currentToken.getType() != TokenType::LSCOPE)
        return std::nullopt;

    std::vector<std::unique_ptr<IStatement>> statements;
    currentToken = lexer.nextToken();

    while (currentToken.getType() != TokenType::RSCOPE)
    {
        if(auto statement = tryParseStatement())
            statements.push_back(std::move(statement.value()));
        else
            throwError(L"Brak domknięcia zakresu.");
    }
    currentToken = lexer.nextToken();
    
    return std::make_unique<ScopeStatement>(std::move(statements), statementPosition);
}

OptionalStatement Parser::tryParseLoop()
{
    if(currentToken.getType() != TokenType::FOR)
        return std::nullopt;
    currentToken = lexer.nextToken();

    auto expr = tryParseExpression();
    if(!expr) throwError(L"Nieprawidłowe wyrażenie po zadeklarowaniu pętli 'powtórz'.");
    
    if(currentToken.getType() != TokenType::FOR_FOLLOW)
        throwError(L"Brak słowa kluczowego 'krotnie' po wyrażeniu w pętli 'powtórz'.");

    currentToken = lexer.nextToken();
    auto statement = tryParseStatement();
    if(!statement) throwError(L"Brak instrukcji po pętli 'powtórz'.");

    return std::make_unique<LoopStatement>(std::move(expr.value()), std::move(statement.value()), statementPosition);
}

OptionalStatement Parser::tryParseIf()
{
    if (currentToken.getType() != TokenType::IF)
        return std::nullopt;
        
    // Get Main Branch
    currentToken = lexer.nextToken();
    auto condition = tryParseExpression();
    if (!condition)
        throwError(L"Brak wyrażenia po instrukcji 'jeżeli'.");
    
    if (currentToken.getType() != TokenType::THEN)
        throwError(L"Brak 'wtedy' po wyrażeniu warunkowym.");
    
    currentToken = lexer.nextToken();
    auto thenStatement = tryParseStatement();
    if (!thenStatement)
        throwError(L"Brak instrukcji po 'wtedy'.");
    
    ConditionalBranch mainBranch(std::move(condition.value()), std::move(thenStatement.value()));

    // Search For ElseIf Branches
    std::vector<ConditionalBranch> elseIfBranches;
    while (currentToken.getType() == TokenType::ELIF)
    {
        currentToken = lexer.nextToken();
        auto elseIfCond = tryParseExpression();
        if (!elseIfCond)
            throwError(L"Brak wyrażenia po 'inaczej gdy'.");

        if (currentToken.getType() != TokenType::THEN)
            throwError(L"Brak 'wtedy' po wyrażeniu warunkowym.");

        currentToken = lexer.nextToken();
        auto elseIfStatement = tryParseStatement();
        if (!elseIfStatement)
            throwError(L"Brak instrukcji po 'wtedy' w 'inaczej gdy'.");

        ConditionalBranch elseIfBranch(std::move(elseIfCond.value()), std::move(elseIfStatement.value()));
        elseIfBranches.push_back(std::move(elseIfBranch));
    }

    // Search For Else Statement
    std::unique_ptr<IStatement> elseBranch;
    if (currentToken.getType() == TokenType::ELSE)
    {
        currentToken = lexer.nextToken();
        auto elseStatement = tryParseStatement();
        if (!elseStatement)
            throwError(L"Brak zakresu po 'ostatecznie'.");
        elseBranch = std::move(elseStatement.value());
    }

    return std::make_unique<IfStatement>(std::move(mainBranch), std::move(elseIfBranches), std::move(elseBranch), statementPosition);
}

OptionalStatement Parser::tryParseReturn()
{
    if(currentToken.getType() != TokenType::RETURN)
        return std::nullopt;
    currentToken = lexer.nextToken();
    auto retExpr = tryParseReturnExpression();
    if (!retExpr) throwError(L"Nie podano zwaracanej wartości.");
    if(currentToken.getType() != TokenType::DOT)
        throwError(L"Brak operatora zakończenia instrukcji przy przypisaniu zmiennej.");
    currentToken = lexer.nextToken();
    return std::make_unique<ReturnStatement>(std::move(retExpr.value()), statementPosition);
}

////////////////////////////////////////////////////////////////////////////////////////////////////////
////                                E X P R E S S I O N S                                           ////
////////////////////////////////////////////////////////////////////////////////////////////////////////

OptionalExpression Parser::tryParseReturnExpression()
{
    if (currentToken.getType() == TokenType::EMPTY_VALUE)
    {
        currentToken = lexer.nextToken();
        return nullptr;
    }
    auto expr = tryParseExpression();
    if (!expr) return std::nullopt;

    return std::move(expr.value());
}

OptionalExpression Parser::tryParseExpression() 
{
    return tryParseLogicalExpression();
}

OptionalExpression Parser::tryParseLogicalExpression() 
{
    auto left = tryParseRelationalExpression();
    if (!left) return std::nullopt;

    while (currentToken.getType() == TokenType::OR || currentToken.getType() == TokenType::AND) {
        TokenType op = currentToken.getType();
        currentToken = lexer.nextToken();
        auto right = tryParseRelationalExpression();
        if (!right) return std::nullopt;
        left = std::make_unique<BinaryExpression>(std::move(left.value()), op, std::move(right.value()));
    }
    
    return left;
}

OptionalExpression Parser::tryParseRelationalExpression() 
{
    auto left = tryParseAdditiveExpression();
    if (!left) return std::nullopt;

    while (currentToken.getType() == TokenType::EQ || currentToken.getType() == TokenType::NEQ ||
           currentToken.getType() == TokenType::GREATER || currentToken.getType() == TokenType::LESSER) {
        TokenType op = currentToken.getType();
        currentToken = lexer.nextToken();
        auto right = tryParseAdditiveExpression();
        if (!right) return std::nullopt;
        left = std::make_unique<BinaryExpression>(std::move(left.value()), op, std::move(right.value()));
    }

    return left;
}

OptionalExpression Parser::tryParseAdditiveExpression() 
{
    auto left = tryParseMultiplicativeExpression();
    if (!left) return std::nullopt;

    while (currentToken.getType() == TokenType::PLUS || currentToken.getType() == TokenType::MINUS ||
           currentToken.getType() == TokenType::CONNECT) {
        TokenType op = currentToken.getType();
        currentToken = lexer.nextToken();
        auto right = tryParseMultiplicativeExpression();
        if (!right) return std::nullopt;
        left = std::make_unique<BinaryExpression>(std::move(left.value()), op, std::move(right.value()));
    }

    return left;
}

OptionalExpression Parser::tryParseMultiplicativeExpression() 
{
    auto left = tryParseCastExpression();
    if (!left) return std::nullopt;

    while (currentToken.getType() == TokenType::MULTI || currentToken.getType() == TokenType::DIVIDE) {
        TokenType op = currentToken.getType();
        currentToken = lexer.nextToken();
        auto right = tryParseCastExpression();
        if (!right) return std::nullopt;
        left = std::make_unique<BinaryExpression>(std::move(left.value()), op, std::move(right.value()));
    }

    return left;
}

OptionalExpression Parser::tryParseCastExpression() 
{
    auto expr = tryParseUnaryExpression();
    if (!expr) return std::nullopt;

    while (currentToken.getType() == TokenType::CAST) {
        Token op = currentToken;
        currentToken = lexer.nextToken();
        TokenType tokenType = currentToken.getType();
        expr = std::make_unique<CastExpression>(std::move(expr.value()), tokenType);
        currentToken = lexer.nextToken();
    }

    return expr;
}

OptionalExpression Parser::tryParseUnaryExpression() 
{
    if (currentToken.getType() == TokenType::NOT) {
        Token op = currentToken;
        currentToken = lexer.nextToken();
        auto operand = tryParseUnaryExpression();
        if (!operand) return std::nullopt;
        return std::make_unique<NegationExpression>(std::move(operand.value()));
    }

    return tryParsePrzykladExpression();
}

OptionalExpression Parser::tryParseValueExpression() 
{
    auto expr = tryParseValue();
    if (!expr) return std::nullopt;
    while (auto args = tryParseArguments())
    {
        expr = std::make_unique<ValueExpression>(std::move(expr.value()), std::move(args.value()));
    }

    return expr;
}

OptionalExpression Parser::tryParsePrzykladExpression() 
{
    if (currentToken.getType() == TokenType::FUNC_OP_EXAMPLE) {
        currentToken = lexer.nextToken();
        auto funcExpr = tryParsePrzykladExpression();
        if (!funcExpr) throwError(L"Nie znaleziono wyrażenia dla operatora przykładu.");
        return std::make_unique<PrzykladExpression>(std::move(funcExpr.value()));
    }

    return tryParseSkroconaExpression();
}

OptionalExpression Parser::tryParseSkroconaExpression() 
{
    if (currentToken.getType() == TokenType::FUNC_OP_BIND) {
        currentToken = lexer.nextToken();
        auto args = tryParseArguments();
        if (!args) throwError(L"Brak argumentów dla operatora skrócenia.");
        auto funcExpr = tryParseSkroconaExpression();
        if (!funcExpr) throwError(L"Nie znaleziono wyrażenia dla operatora skrócenia.");
        return std::make_unique<SkroconaExpression>(std::move(args.value()), std::move(funcExpr.value()));
    }

    return tryParseValueExpression();
}

OptionalExpression Parser::tryParseValue() 
{
    if (auto value = tryParseLiteralExpression())       { return value; }
    if (auto value = tryParseIdentifierExpression())    { return value; }
    if (auto value = tryParsePrioBracketsExpression())  { return value; }

    return std::nullopt;
}

OptionalExpression Parser::tryParseLiteralExpression()
{
    if (currentToken.getType() != TokenType::STR_VALUE &&
        currentToken.getType() != TokenType::INT_VALUE &&
        currentToken.getType() != TokenType::BOOL_VALUE &&
        currentToken.getType() != TokenType::DOUBLE_VALUE)
    {
        return std::nullopt;
    }

    auto retValue = currentToken.getValue();
    currentToken = lexer.nextToken();

    // Remove monostate from TokenType variant
    return std::visit([this](auto&& val) -> OptionalExpression {
        using T = std::decay_t<decltype(val)>;
        if constexpr (!std::is_same_v<T, std::monostate>) {
            return std::make_unique<LiteralExpression>(val);
        } else {
            throwError(L"Token z literalem nie zawiera wartości.");
        }
    }, retValue);
}

OptionalExpression Parser::tryParseIdentifierExpression()
{
    if (currentToken.getType() != TokenType::IDENTIFIER)
        return std::nullopt;
 
    std::wstring identifier = std::get<std::wstring>(currentToken.getValue());
    currentToken = lexer.nextToken();

    return std::make_unique<IdentifierExpression>(identifier);
}

OptionalExpression Parser::tryParsePrioBracketsExpression()
{
    if (currentToken.getType() != TokenType::LROUND)
        return std::nullopt;

    currentToken = lexer.nextToken();
    auto expr = tryParseExpression();
    if (!expr) throwError(L"Puste wyrażenie między nawiasami priorytetowymi.");

    if (currentToken.getType() != TokenType::RROUND) 
        throwError(L"Brak zamknięcia nawiasu.");
    currentToken = lexer.nextToken();

    return std::move(expr.value());
}


////////////////////////////////////////////////////////////////////////////////////////////////////////
////                                   A R G U M E N T S                                            ////
////////////////////////////////////////////////////////////////////////////////////////////////////////

std::optional<std::vector<std::unique_ptr<IExpression>>> Parser::tryParseArguments() 
{
    // Args: (x i y i z)
    if (currentToken.getType() != TokenType::LROUND)
        return std::nullopt;

    std::vector<std::unique_ptr<IExpression>> arguments;
    do {
        currentToken = lexer.nextToken();
        auto expr = tryParseExpression();
        if (!expr) break;
        arguments.push_back(std::move(expr.value()));
    } while (currentToken.getType() == TokenType::SEPARATOR);
    
    if(currentToken.getType() != TokenType::RROUND)
        throwError(L"Brak zamknięcia listy argumentów znakiem ')'.");
    
    currentToken = lexer.nextToken();
    
    return arguments;
}

std::optional<std::vector<ParserArgumentDeclaration>> Parser::tryParseArgumentList()
{
    // Arg list: (numer x i numer y i numer z)
    if(currentToken.getType() != TokenType::LROUND)
        return std::nullopt;
    std::vector<ParserArgumentDeclaration> arguments;
    
    do {
        currentToken = lexer.nextToken();
        ParserVariableType argType;
        if (currentToken.getType() == TokenType::STRING  ||
        currentToken.getType() == TokenType::INTEGER ||
        currentToken.getType() == TokenType::BOOLEAN ||
        currentToken.getType() == TokenType::DOUBLE)
        {
            argType = currentToken.getType();
            currentToken = lexer.nextToken();
        }
        else
        {
            if (auto funcType = tryParseFunctionType()) 
            {
                argType = std::move(funcType.value());
            }
            else break;
        }

        if (currentToken.getType() != TokenType::IDENTIFIER)
            throwError(L"Brak nazwy argumentu po podaniu jedgo typu w liście.");
        std::wstring identifier = std::get<std::wstring>(currentToken.getValue()); 
        arguments.push_back(ParserArgumentDeclaration(std::move(argType), identifier));
        currentToken = lexer.nextToken();
    } while (currentToken.getType() == TokenType::SEPARATOR);
    
    if(currentToken.getType() != TokenType::RROUND)
        throwError(L"Brak zamknięcia listy argumentow znakiem ')'.");
    currentToken = lexer.nextToken();

    return arguments;
}

std::optional<std::vector<ParserVariableType>> Parser::tryParseArgumentsDeclaration()
{
    // Arg list: (numer x i numer y i numer z)
    if(currentToken.getType() != TokenType::LROUND)
        return std::nullopt;
    std::vector<ParserVariableType> arguments;
    
    do {
        currentToken = lexer.nextToken();
        ParserVariableType argType;
        if (currentToken.getType() == TokenType::STRING  ||
        currentToken.getType() == TokenType::INTEGER ||
        currentToken.getType() == TokenType::BOOLEAN ||
        currentToken.getType() == TokenType::DOUBLE)
        {
            argType = currentToken.getType();
            currentToken = lexer.nextToken();
        }
        else
        {
            if (auto funcType = tryParseFunctionType()) 
            {
                argType = std::move(funcType.value());
            }
            else break;
        }

        arguments.push_back(std::move(argType));
    } while (currentToken.getType() == TokenType::SEPARATOR);
    
    if(currentToken.getType() != TokenType::RROUND)
        throwError(L"Brak zamknięcia listy argumentów znakiem ')'.");
    
    currentToken = lexer.nextToken();
    return arguments;
}

////////////////////////////////////////////////////////////////////////////////////////////////////////
////                              V A R I A B L E __ T Y P E S                                      ////
////////////////////////////////////////////////////////////////////////////////////////////////////////

std::optional<ParserVariableType> Parser::tryParseReturnType()
{
    if (currentToken.getType() != TokenType::STRING  &&
    currentToken.getType() != TokenType::INTEGER &&
    currentToken.getType() != TokenType::BOOLEAN &&
    currentToken.getType() != TokenType::EMPTY_VALUE &&
    currentToken.getType() != TokenType::DOUBLE)
    {
        if(auto functType = tryParseFunctionType())
            return std::move(functType);
        return std::nullopt;
    }
    TokenType retype = currentToken.getType(); 
    currentToken = lexer.nextToken();

    return retype;
}

std::optional<std::unique_ptr<FunctionType>> Parser::tryParseFunctionType(bool checkForFUNCDEFToken)
{
    if(checkForFUNCDEFToken)
    {
        if(currentToken.getType() != TokenType::FUNC_DEF)
            return std::nullopt;
        currentToken = lexer.nextToken();
    }

    if(currentToken.getType() != TokenType::LROUND)
    {
        if (checkForFUNCDEFToken) 
            throwError(L"Nie podano argumentów po typie pracy.");
        return std::nullopt;
    }

    auto args = tryParseArgumentsDeclaration();
    if(!args) throwError(L"Niepoprawnie zadeklarowane argumenty typu pracy.");

    if(currentToken.getType() != TokenType::RETURN_ARROW)
        throwError(L"Brak słowa 'dająca' w typie pracy.");
    currentToken = lexer.nextToken();

    auto retType = tryParseReturnType();
    if(!retType) throwError(L"Brak sprecyzowanego typu w deklaracji pracy.");

    return std::make_unique<FunctionType>(std::move(args.value()), std::move(retType.value()));
}
