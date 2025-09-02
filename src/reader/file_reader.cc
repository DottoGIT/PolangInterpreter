#include "file_reader.h"
#include <locale>
#include <codecvt>

FileReader::FileReader() : fileStream() {}

FileReader::~FileReader()
{
    if (fileStream.is_open())
    {
        fileStream.close();
    }
}

bool FileReader::openFile(const std::string& filePath)
{
    // WORKS ONLY FOR FILES WITH 'UTF-16 LE' ENCODING
    fileStream.imbue(std::locale(fileStream.getloc(), new std::codecvt_utf16<wchar_t, 0x10ffff, std::little_endian>));
    fileStream.open(filePath);

    if (fileStream.is_open())
    {
        // Skip the BOM char for UTF-16 LE files
        wchar_t ignore_BOM;
        fileStream.get(ignore_BOM);
        return true;
    }
    return false;
}

wchar_t FileReader::readChar()
{
    wchar_t ch;
    if (fileStream.get(ch))
    {
        return ch;
    }
    return '\0';
}

bool FileReader::isEOF() const
{
    return fileStream.eof();
}
