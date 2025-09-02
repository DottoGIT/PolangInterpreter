#pragma once

#include "statements.hpp"
#include <vector>

class Program {
public:
        Program(std::vector<std::unique_ptr<IStatement>> statements_) : statements(std::move(statements_)) {}
        std::vector<std::unique_ptr<IStatement>> statements;
};