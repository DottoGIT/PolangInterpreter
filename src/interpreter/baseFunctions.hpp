#pragma once

#include <string>
#include <vector>
#include <unordered_map>
#include <variant>
#include <functional>
#include <iostream>

class Function;

namespace BaseFunctions {
    using NativeFunction = std::function<ExprValue(const std::vector<ExprValue>&)>;
    using ExprValue = std::variant<std::wstring, int, double, bool, Function*>;

    inline std::unordered_map<std::wstring, NativeFunction> registry = {
        {
            L"wypisz",
            [](const std::vector<ExprValue>& args) -> ExprValue {
                for (const auto& arg : args) {
                    std::visit([](const auto& val) {
                        std::wcout << val;
                    }, arg);
                }
                std::wcout << std::endl;
                return ExprValue();
            }
        }
    };

    inline bool hasFunction(const std::wstring& name) {
        return registry.count(name) > 0;
    }

    inline ExprValue call(const std::wstring& name, const std::vector<ExprValue>& args) {
        if (!hasFunction(name)) {
            throw std::runtime_error("Nieznana funkcja bazowa");
        }
        return registry.at(name)(args);
    }
}
