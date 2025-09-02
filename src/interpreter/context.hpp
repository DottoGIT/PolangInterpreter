#pragma once

#include <string>
#include <vector>
#include <memory>
#include <variant>
#include <functional>
#include <stdexcept>
#include <iostream>
#include <sstream>
#include <optional>
#include <algorithm>


struct FunctionVariableType;
struct IStatement;
class Function;
class Context;

enum class LiteralType
{
    INTEGER,
    DOUBLE,
    BOOLEAN,
    STRING
};

using ExprValue = std::variant<std::wstring, int, double, bool, Function*>;
using VariableType = std::variant<LiteralType, std::shared_ptr<FunctionVariableType>>;
using DefinedArgument = std::pair<VariableType, std::wstring>;

struct FunctionVariableType {
    std::vector<VariableType> paramTypes;
    std::optional<VariableType> returnType;
};

class Function {
public:
    Function(std::wstring name_,
             std::vector<DefinedArgument> arguments_,
             std::optional<VariableType> returnType_,
             std::shared_ptr<IStatement> instruction_)
        : name(std::move(name_))
        , arguments(std::move(arguments_))
        , returnType(std::move(returnType_))
        , instruction(std::move(instruction_))
        , isReference(false)
        , refFunction(nullptr)
    {}

    Function(std::wstring name_,  Function* refFn_)
        : isReference(true)
        , refFunction(refFn_)
        , name(std::move(name_))
    {}

    const std::wstring& getName() const {
        return name;
    }
    const std::vector<DefinedArgument>& getArguments() const {
        return isReference ? refFunction->getArguments() : arguments;
    }
    const std::shared_ptr<IStatement>& getInstructionShared() const {
        return isReference ? refFunction->getInstructionShared() : instruction;
    }
    const std::optional<VariableType>& getReturnType() const {
        return isReference ? refFunction->getReturnType() : returnType;
    }
    void setRedefinedContext(std::unique_ptr<Context> ctx) {
        if (isReference) {
            refFunction->setRedefinedContext(std::move(ctx));
        } else {
            predefined_context = std::move(ctx);
        }
    }
    std::unique_ptr<Context>& getRedefinedContext() {
        if (isReference) {
            return refFunction->getRedefinedContext();
        }
        return predefined_context;
    }
    bool isReferenceType() const {
        return isReference;
    }

    Function* getReferencedFunction() const {
        return isReference ? refFunction : nullptr;
    }

private:
    std::wstring name;
    std::vector<DefinedArgument> arguments;
    std::optional<VariableType> returnType;
    std::shared_ptr<IStatement> instruction;
    bool isReference;
    Function* refFunction;
    std::unique_ptr<Context> predefined_context;
};


class Variable {
public:
    Variable(const std::wstring& name, VariableType type, ExprValue value, bool isConst = false)
        : name(name), type(std::move(type)), value(std::move(value)), isConst(isConst), isReference(false), refVariable(nullptr) {}

    Variable(const std::wstring& name, Variable* refValue, VariableType type, bool isConst = false)
        : name(name), type(std::move(type)), isConst(isConst), isReference(true), refVariable(refValue) {}

    const std::wstring& getName() const { return name; }
    const VariableType& getType() const { return type; }
    bool isReferenceType() const { return isReference; }
    bool isConstant() const { return isConst; }
    ExprValue getValue() const {
        if (isReference) {
            return refVariable->getValue();
        }
        return value;
    }
    void setValue(ExprValue newValue) {
        if (isReference) {
            refVariable->setValue(std::move(newValue));
            return;
        }
        value = std::move(newValue);
    }

private:
    std::wstring name;
    VariableType type;
    bool isConst;
    bool isReference;
    ExprValue value;
    Variable* refVariable;
};


class Context {
public:
    Context() = default;

    ////////////////////////// VARIABLES //////////////////////////

    void defineVariable(const std::wstring& name, VariableType type, ExprValue value, bool isConst = false) {
        variables.emplace_back(name, std::move(type), std::move(value), isConst);
    }

    void defineVariableReference(const std::wstring& name, Variable* refValue, VariableType type, bool isConst = false) {
        variables.emplace_back(name, refValue, std::move(type), isConst);
    }

    void setVariable(const std::wstring& name, ExprValue value) {
        Variable* var = getVariable(name);
        var->setValue(std::move(value));
    }

    ExprValue getVariableValue(const std::wstring& name) const {
        const Variable* var = getVariable(name);
        return var->getValue();
    }

    Variable* getVariable(const std::wstring& name) {
        auto it = std::find_if(variables.begin(), variables.end(),
            [&](const Variable& v) { return v.getName() == name; });
        return it != variables.end() ? &(*it) : nullptr;
    }

    const Variable* getVariable(const std::wstring& name) const {
        auto it = std::find_if(variables.begin(), variables.end(),
            [&](const Variable& v) { return v.getName() == name; });
        return it != variables.end() ? &(*it) : nullptr;
    }

    bool hasVariable(const std::wstring& name) const {
        return getVariable(name) != nullptr;
    }

    void printVariables() {
        for (const auto& variable : variables) {
            std::wcout << L"Name: " << variable.getName()
                << L", Type: " << variableTypeToString(variable.getType())
                << L", Value: " << exprValueToString(variable.getValue())
                << (variable.isConstant() ? L" (const)" : L"")
                << std::endl;
        }
    }

    size_t countVariables() const {
        return variables.size();
    }

    ////////////////////////// FUNCTIONS //////////////////////////

    void defineFunction(std::unique_ptr<Function> func) {
        const std::wstring& name = func->getName();
        functions.emplace_back(std::move(func));
    }

    void defineFunctionReference(const std::wstring& name,  Function* func) {
        functions.emplace_back(std::make_unique<Function>(name, func));
    }

    Function* getFunction(const std::wstring& name) {
        auto it = std::find_if(functions.begin(), functions.end(),
            [&](const std::unique_ptr<Function>& f) { return f->getName() == name; });
        return it != functions.end() ? it->get() : nullptr;
    }

    std::unique_ptr<Function> getFunctionUnique(const std::wstring& name) {
        auto it = std::find_if(functions.begin(), functions.end(),
            [&](const std::unique_ptr<Function>& f) { return f->getName() == name; });

        if (it != functions.end()) {
            std::unique_ptr<Function> result = std::move(*it);
            functions.erase(it);
            return result;
        }
        return nullptr;
    }

    bool hasFunction(const std::wstring& name) const {
        return std::any_of(functions.begin(), functions.end(),
            [&](const std::unique_ptr<Function>& f) { return f->getName() == name; });
    }

    void printFunctions() {
        for (const auto& function : functions) {
            std::wcout << L"Name: " << function->getName()
                << L", Returns: "
                << (function->getReturnType().has_value()
                        ? variableTypeToString(function->getReturnType().value())
                        : L"NONE")
                << L", Parameters: " << functionParametersToString(*function)
                << std::endl;
        }
    }

    std::wstring functionParametersToString(const Function& func) {
        std::wstringstream ss;
        const auto& params = func.getArguments();
        ss << L"(";
        for (size_t i = 0; i < params.size(); ++i) {
            ss << variableTypeToString(params[i].first) << L" " << params[i].second;
            if (i + 1 < params.size()) ss << L", ";
        }
        ss << L")";
        return ss.str();
    }

    size_t countFunctions() const {
        return functions.size();
    }

    void createJITFunction(std::unique_ptr<Function> func) {
        JIT_functions.emplace_back(std::move(func));
    }

    void clearJITFunctions() {
        JIT_functions.clear();
    }

    ////////////////////////// OTHERS //////////////////////////

    std::wstring exprValueToString(const ExprValue& value) {
        if (std::holds_alternative<int>(value)) {
            return std::to_wstring(std::get<int>(value));
        } else if (std::holds_alternative<double>(value)) {
            return std::to_wstring(std::get<double>(value));
        } else if (std::holds_alternative<bool>(value)) {
            return std::get<bool>(value) ? L"true" : L"false";
        } else if (std::holds_alternative<std::wstring>(value)) {
            return std::get<std::wstring>(value);
        } else {
            return L"null";
        }
    }

    std::wstring variableTypeToString(const VariableType& type) {
        if (std::holds_alternative<LiteralType>(type)) {
            switch (std::get<LiteralType>(type)) {
                case LiteralType::INTEGER: return L"INTEGER";
                case LiteralType::DOUBLE:  return L"DOUBLE";
                case LiteralType::BOOLEAN: return L"BOOLEAN";
                case LiteralType::STRING:  return L"STRING";
            }
        } else if (std::holds_alternative<std::shared_ptr<FunctionVariableType>>(type)) {
            return L"FUNCTION";
        }
        return L"UNKNOWN";
    }

    std::vector<Variable> variables;
    std::vector<std::unique_ptr<Function>> functions;
    std::vector<std::unique_ptr<Function>> JIT_functions;
};
