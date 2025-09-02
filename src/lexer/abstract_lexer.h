#pragma once

#include "token.h"
#include "position.h"

class ILexer
{
public:
    virtual ~ILexer() = default;
    virtual Token nextToken() = 0;
};