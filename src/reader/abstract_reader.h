#pragma once
#include <iostream>

class AbstractReader
{
public:
    virtual ~AbstractReader() = default;
    
    virtual wchar_t readChar() = 0;
    virtual bool isEOF() const = 0;
};