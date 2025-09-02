#include "string_reader.h"

StringReader::StringReader() {}

void StringReader::openString(const std::wstring& str)
{
    stringStream.str(str);
    stringStream.clear();
}

wchar_t StringReader::readChar()
{
    wchar_t ch;
    if (stringStream.get(ch))
    {
        return ch;
    }
    return '\0';
}

bool StringReader::isEOF() const
{
    return stringStream.eof();
}
