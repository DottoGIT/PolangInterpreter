#pragma once
#include "abstract_reader.h"
#include <sstream>

class StringReader : public AbstractReader
{
public:
    StringReader();
    void openString(const std::wstring& str);
    
    wchar_t readChar() override;
    bool isEOF() const override;

private:
    std::wistringstream stringStream;
};
