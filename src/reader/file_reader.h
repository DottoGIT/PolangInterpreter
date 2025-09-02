#pragma once
#include "abstract_reader.h"
#include <fstream>

class FileReader : public AbstractReader
{
public:
    FileReader();
    ~FileReader();

    bool openFile(const std::string& filePath);
    wchar_t readChar() override;
    bool isEOF() const override;

private:
    std::wfstream fileStream;
};
