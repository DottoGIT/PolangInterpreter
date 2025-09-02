#include "console_reader.h"

wchar_t ConsoleReader::readChar()
{
    wchar_t ch;
    if (inputStream.get(ch))
    {
        return ch;
    }
    return '\0';
}

bool ConsoleReader::isEOF() const
{
    return inputStream.eof();
}
