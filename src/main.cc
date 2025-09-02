#include <iostream>
#include <locale>
#include <memory>
#include "lexer.h"
#include "parser.h"
#include "interpreter.h"
#include "file_reader.h"
#include "console_reader.h"

int main(int argc, char* argv[])
{
    std::unique_ptr<AbstractReader> reader;
    std::locale::global(std::locale("pl_PL.UTF-8"));
    std::wcin.imbue(std::locale());
    std::wcout.imbue(std::locale());
    
    if (argc > 1)
    {
        std::unique_ptr<FileReader> file_reader = std::make_unique<FileReader>();
        if (!file_reader->openFile(argv[1]))
        {
            std::cerr << L"Failed to open file: " << argv[1] << std::endl;
            return 1;
        }
        reader = std::move(file_reader);
    }
    else
    {
        std::unique_ptr<ConsoleReader> console_reader = std::make_unique<ConsoleReader>();
        std::wcout << L"Witam w PoLang'u pierwszym polskim interpreterze!:" << std::endl;
        std::wcout << L"Autor: Maciej Scheffer:" << std::endl;
        std::wcout << L"Wpisz wejscie (Ctrl+C by wyjsc z konsoli):" << std::endl;
        reader = std::move(console_reader);
    }

    Lexer lexer(*reader);
    lexer.setIgnoreComments(true);
    Parser parser(lexer);
    Interpreter interpreter;

    try {
        while (auto statement = parser.tryParseStatement())
        {
            interpreter.interpret(*statement.value());
        }
        
        if (!parser.endOfProgram())
            std::wcout << L"Błąd programu: Interpreter nie rozpoznał instrukcji.";

    } catch (const std::runtime_error& e) {
        std::cerr << e.what() << std::endl;
        return 1;
    }
    return 0;
}