#pragma once
#include <string>
#include <fstream>
#include <sstream>
#include <iostream>
#include "abstract_reader.h"

class ConsoleReader : public AbstractReader
{
public:
    ConsoleReader() : inputStream(std::wcin) {}
    wchar_t readChar() override;
    bool isEOF() const override;
private:
    std::wistream& inputStream;
};