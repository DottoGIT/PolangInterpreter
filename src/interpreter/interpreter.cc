#include "interpreter.h"
#include "statements.hpp"
#include "returnException.hpp"
#include "baseFunctions.hpp"
#include <algorithm>
#include <random>

#include <iostream>

Interpreter::Interpreter()
{
    context_stack.emplace_back(std::make_unique<Context>());
    curr_context = context_stack.back().get();
}


void Interpreter::throwError(const std::wstring &message)
{
    std::wcout << L"Błąd Interpretera: [" << statementPos.line << L":" << statementPos.column << L"]: " << message;
    throw std::runtime_error("");
}

ExprValue Interpreter::getVariableValue(const std::wstring &name)   
{
    Context* ctx = findVariableContext(name);
    if (!ctx)
        throwError(L"Nie znaleziono zmiennej: " + name + L".");
    return ctx->getVariableValue(name);
}

Function* Interpreter::getFunction(const std::wstring &name)   
{
    Context* ctx = findFunctionContext(name);
    if (!ctx)
        throwError(L"Nie znaleziono pracy: " + name + L".");
    return ctx->getFunction(name);
}


void Interpreter::printAllVariables()                    
{
    int depth = 0;
    for (auto it = context_stack.rbegin(); it != context_stack.rend(); ++it, ++depth)
    {
        it->get()->printVariables();
    }
}

size_t Interpreter::countAllVariables()                 
{
    size_t sum = 0;
    for (auto it = context_stack.rbegin(); it != context_stack.rend(); ++it)
    {
        sum += it->get()->countVariables();
    }
    return sum;
}

void Interpreter::printAllFunctions()                    
{
    int depth = 0;
    for (auto it = context_stack.rbegin(); it != context_stack.rend(); ++it, ++depth)
    {
        it->get()->printFunctions();
    }
}

size_t Interpreter::countAllFunctions()                 
{
    size_t sum = 0;
    for (auto it = context_stack.rbegin(); it != context_stack.rend(); ++it)
    {
        sum += it->get()->countFunctions();
    }
    return sum;
}

void Interpreter::interpret(IStatement &stmt)
{
    statementPos = stmt.position;
    stmt.accept(*this);
}

////////////////////////////// Statements  //////////////////////////////

void Interpreter::visit(ExpressionStatement &obj)       
{
    obj.expr->accept(*this);
}

void Interpreter::visit(ConditionalBranch& obj)         
{
    ExprValue condValue = obj.condition->accept(*this);
    if (!std::holds_alternative<bool>(condValue))
        throwError(L"Warunek musi być faktem.");

    if (std::get<bool>(condValue))
    {
        obj.body->accept(*this);
    }
}

void Interpreter::visit(IfStatement& obj)               
{
    ExprValue condValue = obj.mainBranch.condition->accept(*this);
    if (!std::holds_alternative<bool>(condValue))
        throwError(L"Warunek musi być faktem.");

    if (std::get<bool>(condValue))
    {
        obj.mainBranch.body->accept(*this);
        return;
    }

    for (auto& branch : obj.elseIfBranches)
    {
        condValue = branch.condition->accept(*this);
        if (!std::holds_alternative<bool>(condValue))
            throwError(L"Warunek musi być faktem.");

        if (std::get<bool>(condValue))
        {
            branch.body->accept(*this);
            return;
        }
    }

    if (obj.elseBranch)
        obj.elseBranch->accept(*this);
}

void Interpreter::visit(LoopStatement &obj)             
{
    auto times = obj.times->accept(*this);
    if (!std::holds_alternative<int>(times))
    {
        throwError(L"Liczba iteracji musi być liczbą.");
    }
    int times_int = std::get<int>(times);
    for(int i = 0; i < times_int; i++)
    {
        obj.statement->accept(*this);
    }
}

void Interpreter::visit(ScopeStatement& obj)            
{
    if(context_stack.size() > maxStackSize) 
        throwError(L"Osiągnięto maksymalny rozmiar zagnieżdżenia: " + std::to_wstring(maxStackSize) + L".");
    context_stack.emplace_back(std::make_unique<Context>());
    curr_context = context_stack.back().get();

    for (auto& stmt : obj.statements)
        stmt->accept(*this);

    context_stack.pop_back();
    curr_context = context_stack.empty() ? nullptr : (Context*)&(*context_stack.back());
}

void Interpreter::visit(VariableDeclaration &obj)       
{
    VariableType varType = parseVarType(std::move(obj.varType));

    if (curr_context->hasVariable(obj.varName)) 
    {
        throwError(L"Zmienna już istnieje w tym zakresie: '" + obj.varName + L"'.");
    }

    if (std::holds_alternative<std::shared_ptr<FunctionVariableType>>(varType))
    {
        if (!obj.value)
            throwError(L"Typ pracy wymaga natychmiastowego przypisania.");

        ExprValue value = obj.value->accept(*this);

        if (!std::holds_alternative<Function*>(value))
            throwError(L"Oczekiwano pracy jako wartości zmiennej: '" + obj.varName + L"'.");

        const auto& expectedFnType = std::get<std::shared_ptr<FunctionVariableType>>(varType);
        Function* originalFn = std::get<Function*>(value);

        if (!functionSignatureMatches(*expectedFnType, *originalFn))
            throwError(L"Typ deklarowanej pracy nie pasuje do typu pracy przypisanej.");

        auto newFn = std::make_unique<Function>(
            obj.varName,
            originalFn->getArguments(),
            originalFn->getReturnType(),
            originalFn->getInstructionShared()
        );
        newFn->setRedefinedContext(std::move(originalFn->getRedefinedContext()));

        
        if(curr_context->hasFunction(newFn->getName()))
        {
            throwError(L"Praca '" + newFn->getName() + L"' jest już zdefiniowana w danym zakresie.");
        }
        curr_context->defineFunction(std::move(newFn));
        return;
    }

    LiteralType literalType = std::get<LiteralType>(varType);
    ExprValue variableValue;

    if (!obj.value)
    {
        switch (literalType)
        {
        case LiteralType::STRING:
            variableValue = std::wstring{};
            break;
        case LiteralType::INTEGER:
            variableValue = 0;
            break;
        case LiteralType::DOUBLE:
            variableValue = 0.0;
            break;
        case LiteralType::BOOLEAN:
            variableValue = false;
            break;
        default:
            throwError(L"Niespodziewany typ zmiennej: '" + obj.varName + L"'.");
        }
    }
    else
    {
        variableValue = obj.value->accept(*this);
        if (!matchesType(literalType, variableValue))
        {
            throwError(L"Typ danej '" + obj.varName + L"' nie zgadza się z jej wartością.");
        }
    }

    curr_context->defineVariable(obj.varName, std::move(varType), variableValue, obj.isConst);
}

void Interpreter::visit(VariableAssignment &obj)        
{
    Context* ctx = findVariableContext(obj.varName);
    if (!ctx)
        throwError(L"Nie mozna przypisac wartości, zmienna: '" + obj.varName + L"' nie istnieje.");

    ExprValue newValue = obj.value->accept(*this);
    LiteralType varType = std::get<LiteralType>(ctx->getVariable(obj.varName)->getType());
    if (!matchesType(varType, newValue))
    {
        throwError(L"Typ danej '" + obj.varName + L"' nie zgadza się z jej wartością.");
    }
    if(ctx->getVariable(obj.varName)->isConstant())
    {
        throwError(L"Typ danej '" + obj.varName + L"' jest stały i nie można zmienić jej wartości.");
    }
    ctx->setVariable(obj.varName, std::move(newValue));
}

////////////////////////////// Expressions //////////////////////////////

ExprValue Interpreter::visit(BinaryExpression &obj)     
{
    ExprValue lex = obj.lex->accept(*this);

    switch (obj.op)
    {
    case TokenType::PLUS:
    case TokenType::MINUS:
    {
        ExprValue rex = obj.rex->accept(*this);
        if (std::holds_alternative<int>(lex) && std::holds_alternative<int>(rex))
        {
            if (obj.op == TokenType::PLUS) return std::get<int>(lex) + std::get<int>(rex);
            if (obj.op == TokenType::MINUS) return std::get<int>(lex) - std::get<int>(rex);
        }
        else if ((std::holds_alternative<int>(lex) || std::holds_alternative<double>(lex)) &&
                 (std::holds_alternative<int>(rex) || std::holds_alternative<double>(rex)))
        {
            double lhs = std::holds_alternative<int>(lex) ? static_cast<double>(std::get<int>(lex)) : std::get<double>(lex);
            double rhs = std::holds_alternative<int>(rex) ? static_cast<double>(std::get<int>(rex)) : std::get<double>(rex);

            if (obj.op == TokenType::PLUS) return lhs + rhs;
            if (obj.op == TokenType::MINUS) return lhs - rhs;
        }

        throwError(L"Dodawanie lub odejmowanie musi odbyć się między liczbami.");
    }
    case TokenType::MULTI:
    {
        ExprValue rex = obj.rex->accept(*this);
        if (std::holds_alternative<int>(lex) && std::holds_alternative<int>(rex))
        {
            return std::get<int>(lex) * std::get<int>(rex);
        }
        else if ((std::holds_alternative<int>(lex) || std::holds_alternative<double>(lex)) &&
                 (std::holds_alternative<int>(rex) || std::holds_alternative<double>(rex)))
        {
            double lhs = std::holds_alternative<int>(lex) ? static_cast<double>(std::get<int>(lex)) : std::get<double>(lex);
            double rhs = std::holds_alternative<int>(rex) ? static_cast<double>(std::get<int>(rex)) : std::get<double>(rex);
            return lhs * rhs;
        }

        throwError(L"Mnożenie musi odbyć się między liczbami.");
    }
    case TokenType::DIVIDE:
    {
        ExprValue rex = obj.rex->accept(*this);

        if ((std::holds_alternative<int>(lex) || std::holds_alternative<double>(lex)) &&
                (std::holds_alternative<int>(rex) || std::holds_alternative<double>(rex)))
        {
            double lhs = std::holds_alternative<int>(lex) ? static_cast<double>(std::get<int>(lex)) : std::get<double>(lex);
            double rhs = std::holds_alternative<int>(rex) ? static_cast<double>(std::get<int>(rex)) : std::get<double>(rex);

            if (rhs == 0.0)
            {
                throwError(L"Wykryto próbę dzielenia przez 0.");
            }

            return lhs / rhs;
        }

        throwError(L"Dzielenie może być wykonane tylko na liczbach.");
    }
    case TokenType::LESSER:
    case TokenType::GREATER:
    {
        ExprValue rex = obj.rex->accept(*this);

        if ((std::holds_alternative<int>(lex) || std::holds_alternative<double>(lex)) &&
            (std::holds_alternative<int>(rex) || std::holds_alternative<double>(rex)))
        {
            double lhs = std::holds_alternative<int>(lex) ? static_cast<double>(std::get<int>(lex)) : std::get<double>(lex);
            double rhs = std::holds_alternative<int>(rex) ? static_cast<double>(std::get<int>(rex)) : std::get<double>(rex);

            if (obj.op == TokenType::LESSER) return lhs < rhs;
            if (obj.op == TokenType::GREATER) return lhs > rhs;
        }

        throwError(L"Porównanie większościowe może być wykonane tylko na liczbach.");
    }
    case TokenType::CONNECT:
    {
        ExprValue rex = obj.rex->accept(*this);
        if (std::holds_alternative<std::wstring>(lex) && std::holds_alternative<std::wstring>(rex)) {
            return std::get<std::wstring>(lex) + std::get<std::wstring>(rex);
        }

        throwError(L"Operator konkatenacji działa tylko na wyrazach.");
    }
    case TokenType::EQ:
    case TokenType::NEQ:
    {
        ExprValue rex = obj.rex->accept(*this);

        // numbers
        if ((std::holds_alternative<int>(lex) || std::holds_alternative<double>(lex)) &&
            (std::holds_alternative<double>(rex) || std::holds_alternative<int>(rex))) {
            double lhs = std::holds_alternative<int>(lex) ? static_cast<double>(std::get<int>(lex)) : std::get<double>(lex);
            double rhs = std::holds_alternative<int>(rex) ? static_cast<double>(std::get<int>(rex)) : std::get<double>(rex);
            bool result = lhs == rhs;
            return obj.op == TokenType::EQ ? result : !result;
        }

        // bools
        if (std::holds_alternative<bool>(lex) && std::holds_alternative<bool>(rex)) {
            bool result = std::get<bool>(lex) == std::get<bool>(rex);
            return obj.op == TokenType::EQ ? result : !result;
        }

        // strings
        if (std::holds_alternative<std::wstring>(lex) && std::holds_alternative<std::wstring>(rex)) {
            bool result = std::get<std::wstring>(lex) == std::get<std::wstring>(rex);
            return obj.op == TokenType::EQ ? result : !result;
        }

        throwError(L"Porównanie równe/nierówne może być wykonane tylko na zgodnych typach.");
    }
    case TokenType::AND:
    {
        if (!std::holds_alternative<bool>(lex))
            throwError(L"Operator AND wymaga wartości logicznych.");

        bool lhs = std::get<bool>(lex);
        if (!lhs) return false;

        ExprValue rex = obj.rex->accept(*this);
        if (!std::holds_alternative<bool>(rex))
            throwError(L"Operator AND wymaga wartości logicznych.");

        return lhs && std::get<bool>(rex);
    }
    case TokenType::OR:
    {
        if (!std::holds_alternative<bool>(lex))
            throwError(L"Operator OR wymaga wartości logicznych.");

        bool lhs = std::get<bool>(lex);
        if (lhs) return true;

        ExprValue rex = obj.rex->accept(*this);
        if (!std::holds_alternative<bool>(rex))
            throwError(L"Operator OR wymaga wartości logicznych.");

        return lhs || std::get<bool>(rex);
    }
    default:
        throwError(L"Nieznany operator w wyrażeniu.");
    }
}

ExprValue Interpreter::visit(CastExpression &obj)       
{
    ExprValue value = obj.expr->accept(*this);

    LiteralType target;
    switch (obj.targetType)
    {
        case TokenType::INTEGER:
            target = LiteralType::INTEGER;
            break;
        case TokenType::DOUBLE:
            target = LiteralType::DOUBLE;
            break;
        case TokenType::BOOLEAN:
            target = LiteralType::BOOLEAN;
            break;
        case TokenType::STRING:
            target = LiteralType::STRING;
            break;
        default:
            throwError(L"Nieznany typ rzutowania.");
    }

    try {
        switch (target)
        {
            case LiteralType::INTEGER:
                if (std::holds_alternative<int>(value))
                    return std::get<int>(value);
                else if (std::holds_alternative<double>(value))
                    return static_cast<int>(std::get<double>(value));
                else if (std::holds_alternative<bool>(value))
                    return static_cast<int>(std::get<bool>(value));
                else if (std::holds_alternative<std::wstring>(value)) {
                    std::wstring text = std::get<std::wstring>(value);    
                    size_t pos = 0;
                    int result = std::stoi(text, &pos);
                    if (pos != text.length())
                        throwError(L"W konwersji wyrazu na numer nie znaleziono numeru.");
                    return result;
                }
            case LiteralType::DOUBLE:
                if (std::holds_alternative<double>(value))
                    return std::get<double>(value);
                else if (std::holds_alternative<int>(value))
                    return static_cast<double>(std::get<int>(value));
                else if (std::holds_alternative<bool>(value))
                    return static_cast<double>(std::get<bool>(value));
                else if (std::holds_alternative<std::wstring>(value)) {
                    std::wstring text = std::get<std::wstring>(value);    
                    std::replace(text.begin(), text.end(), L',', L'.');
                    size_t pos = 0;
                    double result = std::stod(text, &pos);
                    if (pos != text.length())
                        throwError(L"W konwersji wyrazu na ułamek nie znaleziono liczby zmiennoprzecinkowej.");
                    return result;
                }

            case LiteralType::BOOLEAN:
                if (std::holds_alternative<bool>(value))
                    return std::get<bool>(value);
                else if (std::holds_alternative<int>(value))
                    return std::get<int>(value) != 0;
                else if (std::holds_alternative<double>(value))
                    return std::get<double>(value) != 0.0;
                else if (std::holds_alternative<std::wstring>(value)) {
                    const std::wstring& str = std::get<std::wstring>(value);
                    if (str == L"prawda")
                        return true;
                    else if (str == L"fałsz")
                        return false;
                    else
                        throwError(L"Niepoprawna wartość logiczna w napisie.");
                }
                break;

            case LiteralType::STRING:
                if (std::holds_alternative<std::wstring>(value))
                    return std::get<std::wstring>(value);
                else if (std::holds_alternative<int>(value))
                    return std::to_wstring(std::get<int>(value));
                else if (std::holds_alternative<double>(value))
                {
                    std::wstring text = std::to_wstring(std::get<double>(value));   
                    std::replace(text.begin(), text.end(), L'.', L',');
                    return text;
                }
                else if (std::holds_alternative<bool>(value))
                    return std::get<bool>(value) ? std::wstring(L"prawda") : std::wstring(L"fałsz");
                break;
        }
    } catch (const std::exception&) {
        throwError(L"Rzutowanie nie powiodło się z nieznanych przyczyn.");
    }

    throwError(L"Nieznany typ rzutowania.");
}

ExprValue Interpreter::visit(NegationExpression &obj)    
{
    ExprValue value = obj.expr->accept(*this);

    if (!std::holds_alternative<bool>(value)) {
        throwError(L"Zaprzeczenie musi dotyczyć faktu.");
    }

    return !std::get<bool>(value);
}

ExprValue Interpreter::visit(LiteralExpression &obj)    
{
    return std::visit([](auto&& value) -> ExprValue {
        return value;
    }, obj.literal);
}

ExprValue Interpreter::visit(IdentifierExpression &obj) 
{
    Context* ctx = findVariableContext(obj.identifier);
    if (ctx)
    {
        return ctx->getVariableValue(obj.identifier);
    }

    ctx = findFunctionContext(obj.identifier);
    if (ctx)
    {
        return ctx->getFunction(obj.identifier);
    }

    throwError(L"Nie znaleziono zmiennej ani pracy o identyifkatorze: '" + obj.identifier + L"'.");
}


//////////////////////////////// Functions ////////////////////////////////

void Interpreter::visit(FunctionDeclaration& obj)       
{
    std::vector<DefinedArgument> convertedArgs;
    for (const auto& param : obj.parameters) {
        VariableType varType = parseVarType(param.first);
        convertedArgs.emplace_back(std::move(varType), param.second);
    }

    std::optional<VariableType> returnType;

    if (std::holds_alternative<TokenType>(obj.ret_type)) {
        if (std::get<TokenType>(obj.ret_type) != TokenType::EMPTY_VALUE) {
            returnType = parseVarType(obj.ret_type);
        }
    } else {
        returnType = parseVarType(obj.ret_type);
    }

    auto func = std::make_unique<Function>(obj.name,std::move(convertedArgs), std::move(returnType), std::shared_ptr<IStatement>(std::move(obj.body)));
    
    if(curr_context->hasFunction(func->getName()))
    {
        throwError(L"Praca '" + func->getName() + L"' jest już zdefiniowana w danym zakresie.");
    }
    curr_context->defineFunction(std::move(func));
}

ExprValue Interpreter::visit(ValueExpression &obj)      
{
    // 0. Base function call check (callee is IdentifierExpression)
    if (auto* idExpr = dynamic_cast<IdentifierExpression*>(obj.callee.get())) {
        if (BaseFunctions::hasFunction(idExpr->identifier)) {
            std::vector<ExprValue> evaluatedArgs;
            for (auto& arg : obj.arguments) {
                evaluatedArgs.push_back(arg->accept(*this));
            }
            return BaseFunctions::call(idExpr->identifier, evaluatedArgs);
        }
    }
    

    // 1. Evaluate the callee
    ExprValue calleeValue = obj.callee->accept(*this);
    
    // 2. Ensure it's a Function*
    if (!std::holds_alternative<Function*>(calleeValue)) {
        throwError(L"Próba wykonania obiektu nie będącym pracą");
    }
    
    Function* function = std::get<Function*>(calleeValue);
    if (!function) {
        throwError(L"Zwrócono pusty wskaźnik do pracy.");
    }
    
    // 3. Arity check
    const auto& params = function->getArguments();
    if (params.size() != obj.arguments.size()) {
        throwError(L"Wywołanie pracy ze złą liczbą argumentów.");
    }
    
    // 4. Evaluate and type-check arguments
    std::vector<std::variant<ExprValue, Variable*>> evaluatedArgs;
    
    for (size_t i = 0; i < obj.arguments.size(); ++i) {
        const auto& argExpr = obj.arguments[i];
        const auto& paramType = params[i].first;
        
        // If it's an identifier expression, we may want to pass by reference
        if (auto identExpr = dynamic_cast<IdentifierExpression*>(argExpr.get())) 
        {
            if(Variable* var = findVariable(identExpr->identifier))
            {
                if (!std::holds_alternative<LiteralType>(paramType)) {
                    throwError(L"W pracy '" + function->getName() + L"' spodziewano się typu pracy dla argumentu " + std::to_wstring(i) + L".");
                }
                const LiteralType expected = std::get<LiteralType>(paramType);
                if (!matchesType(expected, var->getValue())) {
                    throwError(L"W pracy '" + function->getName() + L"' niezgodny typ argumentu " + std::to_wstring(i) + L".");
                }
                evaluatedArgs.push_back(var);
            } else if(Function* func = findFunction(identExpr->identifier))
            {
                const auto& expectedFnType = std::get<std::shared_ptr<FunctionVariableType>>(paramType);
                if (!std::holds_alternative<std::shared_ptr<FunctionVariableType>>(paramType)) {
                    throwError(L"W pracy '" + function->getName() + L"' spodziewano się wyrazenia dla argumentu " + std::to_wstring(i) + L".");
                }
                if (!functionSignatureMatches(*expectedFnType, *func)) {
                    throwError(L"W pracy '" + function->getName() + L"' niezgodna konstrukcja argumentu pracy " + std::to_wstring(i) + L".");
                }
                evaluatedArgs.push_back(func);
            }
            else{
                throwError(L"W pracy '" + function->getName() + L"' nieznana zmienna w argumencie " + std::to_wstring(i) + L".");
            }
        }
        else {
            ExprValue argValue = argExpr->accept(*this);
            
            // Check type
            if (std::holds_alternative<LiteralType>(paramType)) {
                const LiteralType expected = std::get<LiteralType>(paramType);
                if (!matchesType(expected, argValue)) {
                    throwError(L"W pracy '" + function->getName() + L"' niezgodny typ argumentu " + std::to_wstring(i) + L".");
                }
            } else {
                // Expect function type
                if (!std::holds_alternative<Function*>(argValue)) {
                    throwError(L"W pracy '" + function->getName() + L"' spodziewano się typu pracy dla argumentu " + std::to_wstring(i) + L".");
                }
                auto fnPtr = std::get<Function*>(argValue);
                const auto& expectedFnType = std::get<std::shared_ptr<FunctionVariableType>>(paramType);
                if (!functionSignatureMatches(*expectedFnType, *fnPtr)) {
                    throwError(L"W pracy '" + function->getName() + L"' niezgodna konstrukcja argumentu " + std::to_wstring(i) + L".");
                }
            }
            
            evaluatedArgs.push_back(std::move(argValue));
        }
    }

    
    // 5. Push new context
    bool usedPredefinedContext = false;
    if (function->getRedefinedContext() != nullptr) {
        usedPredefinedContext = true;
        auto newContext = std::move(function->getRedefinedContext());
        context_stack.emplace_back(std::move(newContext));
        curr_context = context_stack.back().get();
    }

    if(context_stack.size() > maxStackSize) 
        throwError(L"Osiągnięto maksymalny rozmiar zagnieżdżenia: " + std::to_wstring(maxStackSize) + L".");
    
    context_stack.emplace_back(std::make_unique<Context>());
    curr_context = context_stack.back().get();
    
    // 6. Bind arguments
    for (size_t i = 0; i < params.size(); ++i) {
        const auto& paramName = params[i].second;
        const auto& paramType = params[i].first;
        const auto& arg = evaluatedArgs[i];
        
        if (std::holds_alternative<LiteralType>(paramType)) {
            
            // Literal-type argument
            if (std::holds_alternative<Variable*>(arg)) {
                // Passed by reference (IdentifierExpression)
                Variable* referencedVar = std::get<Variable*>(arg);
                if(curr_context->hasVariable(paramName))
                {
                    throwError(L"Zmienna '" + paramName + L"' już istnieje w danym zakresie.");
                }
                if(!referencedVar)
                {
                    throwError(L"Próba przypisania pustej referencji do zmiennej '" + paramName + L"'.");
                }
                curr_context->defineVariableReference(paramName, referencedVar, paramType, false);
            } else {
                // Passed by value (Literal expression)
                const ExprValue& value = std::get<ExprValue>(arg);
                curr_context->defineVariable(paramName, paramType, value, false);
            }
        } else {
            // Function-type parameter
            if (std::holds_alternative<ExprValue>(arg)) {
                const ExprValue& val = std::get<ExprValue>(arg);
                if (std::holds_alternative<Function*>(val)) {
                    auto argFn = std::get<Function*>(val);
                    if (!argFn) {
                        throwError(L"Zwrócono pusty wskaźnik do pracy.");
                    }
                    if(curr_context->hasFunction(paramName))
                    {
                        throwError(L"Praca '"+ paramName +L"' już istnieje w danym zakresie.");
                    }
                    curr_context->defineFunctionReference(paramName, argFn);
                } else {
                    throwError(L"W pracy oczekiwano typu pracy dla argumentu " + std::to_wstring(i) + L".");
                }
            } else {
                throwError(L"Nieprawidłowy typ wartości pracy.");
            }
        }
    }
    
    // 7. Execute body
    size_t contexts = context_stack.size();
    ExprValue retVal;
    bool skipReturnValidation = false;

    try {
        function->getInstructionShared()->accept(*this);
    } 
    catch (const ReturnException& ret) {
        if (!function->getReturnType().has_value() && !ret.getValue().has_value()) {
            skipReturnValidation = true;
        } 
        else if (function->getReturnType().has_value() && !ret.getValue().has_value()) {
            throwError(L"Praca '"+ function->getName() + L"' oczekuje wartości.");
        }
        else if (!function->getReturnType().has_value() && ret.getValue().has_value()) {
            throwError(L"Praca '"+ function->getName() + L"' nic nie oczekuje.");
        }

        if (!skipReturnValidation) {
            retVal = ret.getValue().value();
            VariableType expectedType = function->getReturnType().value();

            if (std::holds_alternative<Function*>(retVal)) {
                auto returnedFn = std::get<Function*>(retVal);
                if (!returnedFn) {
                    throwError(L"Zwrócono pusty wskaźnik do pracy");
                }
                if (!std::holds_alternative<std::shared_ptr<FunctionVariableType>>(expectedType)) {
                    throwError(L"Praca '"+ function->getName() + L"' oczekiwala zwrocenia wartości, a zwraca prace.");
                }
                auto expectedFnType = std::get<std::shared_ptr<FunctionVariableType>>(expectedType);
                if (!functionSignatureMatches(*expectedFnType, *returnedFn)) {
                    throwError(L"Praca '"+ function->getName() + L"' zwraca prace nie pasującą do zwracanego typu.");
                }
            } else {
                if (!std::holds_alternative<LiteralType>(expectedType)) {
                    throwError(L"Praca '"+ function->getName() + L"' oczekiwała pracy a zwrócono wartość.");
                }
                auto expectedLiteralType = std::get<LiteralType>(expectedType);
                if (!matchesType(expectedLiteralType, retVal)) {
                    throwError(L"Praca '"+ function->getName() + L"' oczekuje innego zwracanego typu.");
                }
            }
        }

        // Cleanup
        std::unique_ptr<Function> funcSaver = nullptr;
        if (!skipReturnValidation && std::holds_alternative<Function*>(retVal)) {
            funcSaver = std::move(curr_context->getFunctionUnique(std::get<Function*>(retVal)->getName()));
        }

        while (context_stack.size() > contexts) {
            context_stack.pop_back();
            curr_context = context_stack.empty() ? nullptr : context_stack.back().get();
        }

        if (usedPredefinedContext) {
            context_stack.pop_back();
            function->setRedefinedContext(std::move(context_stack.back()));
            curr_context = context_stack.empty() ? nullptr : context_stack.back().get();
        }

        context_stack.pop_back();
        curr_context = context_stack.empty() ? nullptr : context_stack.back().get();

        if (funcSaver != nullptr) {
            curr_context->createJITFunction(std::move(funcSaver));
        }

        return retVal;
    }
    
    throwError(L"Praca '"+ function->getName() + L"' nie zwrócila żadnej wartości.");
}

void Interpreter::visit(ReturnStatement& obj)           
{
    if (obj.expr) {
        ExprValue returnVal = obj.expr->accept(*this);
        throw ReturnException(returnVal);
    } else {
        throw ReturnException(std::nullopt);
    }
}

ExprValue Interpreter::visit(PrzykladExpression &obj)
{
    // 1. Evaluate the function expression (must be a Function*)
    ExprValue exprVal = obj.expr->accept(*this);
    if (!std::holds_alternative<Function*>(exprVal)) {
        throwError(L"Oczekiwano funkcji w operatorze 'przykład', a otrzymano wartość.");
    }
    Function* originalFn = std::get<Function*>(exprVal);
    const auto& params = originalFn->getArguments();

    // 2. Fill context with pre-bound variables
    std::unique_ptr<Context> ctx = std::make_unique<Context>();

    if(originalFn->getRedefinedContext() != nullptr)
    {
        for (const auto& variable : originalFn->getRedefinedContext()->variables) {
            ctx->defineVariable(variable.getName(), variable.getType(), variable.getValue(), false);
        }
    }
    
    // 3. Fill remaining arguments with pre-bound random variables
    // Random generators
    std::random_device rd;
    std::mt19937 gen(rd());
    std::uniform_int_distribution<> intDist(0, 1000);
    std::uniform_real_distribution<> doubleDist(0.0, 1.0);
    std::bernoulli_distribution boolDist(0.5);
    std::uniform_int_distribution<int> charDist(static_cast<int>(L'a'), static_cast<int>(L'z'));

    auto generateRandomWString = [&]() -> std::wstring {
        std::wstring str;
        for (int j = 0; j < 5; ++j) {
            str += charDist(gen);
        }
        return str;
    };

    // 3. Fill remaining arguments with pre-bound random variables
    for (size_t i = 0; i < params.size(); ++i)
    {
        ExprValue varValue;

        const VariableType& paramType = params[i].first;
        if (!std::holds_alternative<LiteralType>(paramType)) {
            throwError(L"Operator przykład działa tylko na pracach z argumentami oczekującymi wartości.");
        }

        switch (std::get<LiteralType>(paramType)) {
            case LiteralType::INTEGER:
                varValue = intDist(gen);
                break;
            case LiteralType::DOUBLE:
                varValue = doubleDist(gen);
                break;
            case LiteralType::BOOLEAN:
                varValue = boolDist(gen);
                break;
            case LiteralType::STRING:
                varValue = generateRandomWString();
                break;
            default:
                throwError(L"Nieznany typ wartości w operatorze 'przykład'.");
        }

        ctx->defineVariable(params[i].second, paramType, varValue);
    }

    // 4. Create new function with remaining params and empty params list
    
    auto newFn = std::make_unique<Function>(
        L"",
        std::vector<DefinedArgument>{},
        originalFn->getReturnType(),
        originalFn->getInstructionShared()
    );
    
    newFn->setRedefinedContext(std::move(ctx));
    Function* resultFn = newFn.get();
    curr_context->createJITFunction(std::move(newFn));
    return resultFn;
}

ExprValue Interpreter::visit(SkroconaExpression& obj) {
    // 1. Evaluate the function expression (must be a Function*)
    ExprValue exprVal = obj.expr->accept(*this);
    if (!std::holds_alternative<Function*>(exprVal)) {
        throwError(L"Oczekiwano funkcji w operatorze 'skrócona_o', a optrzymano wartość.");
    }
    
    Function* originalFn = std::get<Function*>(exprVal);
    
    // 2. Evaluate bound arguments
    std::vector<ExprValue> boundValues;
    for (auto& arg : obj.arguments) {
        boundValues.push_back(arg->accept(*this));
    }

    const auto& params = originalFn->getArguments();
    if (boundValues.size() > params.size()) {
        throwError(L"Zbyt wiele wartości zwiaząnych w operatorze 'skrocona_o'.");
    }

    // 3. Create context with pre-bound variables
    std::unique_ptr<Context> ctx = std::make_unique<Context>();

    if(originalFn->getRedefinedContext() != nullptr)
    {
        for (const auto& variable : originalFn->getRedefinedContext()->variables) {
            ctx->defineVariable(variable.getName(), variable.getType(), variable.getValue(), false);
        }
    }
    
    for (size_t i = 0; i < boundValues.size(); ++i) {
        const auto& [type, name] = params[i];
        ctx->defineVariable(name, type, boundValues[i], false);
    }

    // 4. Create new function with remaining params and attach pre-bound context
    std::vector<DefinedArgument> remainingParams(params.begin() + boundValues.size(), params.end());

    auto newFn = std::make_unique<Function>(
        L"",
        std::move(remainingParams),
        originalFn->getReturnType(),
        originalFn->getInstructionShared()
    );
    newFn->setRedefinedContext(std::move(ctx));
    Function* resultFn = newFn.get();
    curr_context->createJITFunction(std::move(newFn));
    return resultFn;
}

//////////////////////////////// OTHERS ////////////////////////////////

VariableType Interpreter::parseVarType(const ParserVariableType& parserType)
{
    if (std::holds_alternative<TokenType>(parserType)) {
        TokenType token = std::get<TokenType>(parserType);

        switch (token) {
            case TokenType::INTEGER:
                return LiteralType::INTEGER;
            case TokenType::DOUBLE:
                return LiteralType::DOUBLE;
            case TokenType::BOOLEAN:
                return LiteralType::BOOLEAN;
            case TokenType::STRING:
                return LiteralType::STRING;
            default:
                throwError(L"Nieprawidlowy typ tokena dla zmiennej.");
        }
    } else if (std::holds_alternative<std::unique_ptr<FunctionType>>(parserType)) {
        const auto& functionType = std::get<std::unique_ptr<FunctionType>>(parserType);
        auto functionVarType = std::make_unique<FunctionVariableType>();

        for (const auto& paramType : functionType->args) {
            functionVarType->paramTypes.push_back(parseVarType(paramType));
        }

        std::optional<VariableType> returnType;
        if (std::holds_alternative<TokenType>(functionType->ret_type)) {
            if (std::get<TokenType>(functionType->ret_type) != TokenType::EMPTY_VALUE) {
                returnType = parseVarType(functionType->ret_type);
            }
        } else {
            returnType = parseVarType(functionType->ret_type);
        }

        functionVarType->returnType = std::move(returnType);
        return std::move(functionVarType);
    }

    throwError(L"Nieznany typ zmiennej.");
}

bool Interpreter::matchesType(LiteralType declared, const ExprValue& value) 
{
    switch (declared)
    {
    case LiteralType::STRING:
        return std::holds_alternative<std::wstring>(value);
    case LiteralType::INTEGER:
        return std::holds_alternative<int>(value);
    case LiteralType::DOUBLE:
        return std::holds_alternative<double>(value);
    case LiteralType::BOOLEAN:
        return std::holds_alternative<bool>(value);
    default:
        return false;
    }
}

bool Interpreter::functionSignatureMatches(const FunctionVariableType& expected, const Function& actual)
{
    const auto& actualArgs = actual.getArguments();
    if (expected.paramTypes.size() != actualArgs.size())
        return false;

    for (size_t i = 0; i < expected.paramTypes.size(); ++i)
    {
        if (!variableTypesEqual(expected.paramTypes[i], actualArgs[i].first))
            return false;
    }

    return variableTypesEqual(expected.returnType, actual.getReturnType());
}

bool Interpreter::variableTypesEqual(const VariableType& a, const VariableType& b)
{
    if (a.index() != b.index())
        return false;

    if (std::holds_alternative<LiteralType>(a))
        return std::get<LiteralType>(a) == std::get<LiteralType>(b);

    const auto& fnA = std::get<std::shared_ptr<FunctionVariableType>>(a);
    const auto& fnB = std::get<std::shared_ptr<FunctionVariableType>>(b);

    if (fnA->paramTypes.size() != fnB->paramTypes.size())
        return false;

    for (size_t i = 0; i < fnA->paramTypes.size(); ++i)
    {
        if (!variableTypesEqual(fnA->paramTypes[i], fnB->paramTypes[i]))
            return false;
    }

    return variableTypesEqual(fnA->returnType, fnB->returnType);
}

bool Interpreter::variableTypesEqual(const std::optional<VariableType>& a, const std::optional<VariableType>& b)
{
    if (!a && !b) return true;
    if (!a || !b) return false;
    return variableTypesEqual(*a, *b);
}

Context* Interpreter::findVariableContext(const std::wstring& name)         
{
    for (auto it = context_stack.rbegin(); it != context_stack.rend(); ++it)
    {
        if (it->get()->hasVariable(name))
            return it->get();
    }
    return nullptr;
}

Variable* Interpreter::findVariable(const std::wstring& name)
{
    for (auto it = context_stack.rbegin(); it != context_stack.rend(); ++it)
    {
        if (it->get()->hasVariable(name))
            return it->get()->getVariable(name);
    }
    return nullptr;
}

Function* Interpreter::findFunction(const std::wstring& name)
{
    for (auto it = context_stack.rbegin(); it != context_stack.rend(); ++it)
    {
        if (it->get()->hasFunction(name))
            return it->get()->getFunction(name);
    }
    return nullptr;
}

Context* Interpreter::findFunctionContext(const std::wstring& name)         
{
    for (auto it = context_stack.rbegin(); it != context_stack.rend(); ++it)
    {
        if (it->get()->hasFunction(name))
            return it->get();
    }
    return nullptr;
}



