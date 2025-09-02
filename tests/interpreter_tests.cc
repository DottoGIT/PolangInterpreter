#include <vector>
#include <memory>
#include "parser.h"
#include "lexer.h"
#include "interpreter.h"
#include "string_reader.h"
#include "expressions.hpp"

#define BOOST_TEST_MODULE InterpreterTests
#include <boost/test/included/unit_test.hpp>

BOOST_AUTO_TEST_CASE(base_variable_declarations)
{
    std::wstring code = L"wyraz a to nic.\n"
                        L"numer b to 1.\n"
                        L"fakt c to prawda.\n"
                        L"ułamek d to 1,23.\n"
                        L"stały wyraz sa to nic.\n"
                        L"stały numer sb to 1.\n"
                        L"stały fakt sc to prawda.\n"
                        L"stały ułamek sd to 1,23.";

    StringReader reader;
    reader.openString(code);
    Lexer lexer(reader);
    Parser parser(lexer);
    Interpreter interpreter;

    std::optional<Program> program = parser.tryParseProgram();

    for (auto& stmt : program.value().statements)
    {
        interpreter.interpret(*stmt);
    }

    BOOST_REQUIRE(interpreter.countAllVariables() == 8); 
}

BOOST_AUTO_TEST_CASE(repeated_base_variable_declarations)
{
    std::wstring code = L"wyraz a to nic.\n"
                        L"stały numer a to 1.\n"
                        L"numer a to 1.\n"
                        L"numer b to 1.\n";

    StringReader reader;
    reader.openString(code);
    Lexer lexer(reader);
    Parser parser(lexer);
    Interpreter interpreter;

    std::optional<Program> program = parser.tryParseProgram();
    
    program.value().statements[0]->accept(interpreter);
    BOOST_REQUIRE(interpreter.countAllVariables() == 1); 
    BOOST_CHECK_THROW( program.value().statements[1]->accept(interpreter), std::runtime_error);
    BOOST_CHECK_THROW( program.value().statements[2]->accept(interpreter), std::runtime_error);
    program.value().statements[3]->accept(interpreter);
    BOOST_REQUIRE(interpreter.countAllVariables() == 2); 
}

BOOST_AUTO_TEST_CASE(empty_variable_declaration_value)
{
    std::wstring code = 
        L"numer a to nic.\n" 
        L"ułamek b to nic.\n" 
        L"fakt c to nic.\n" 
        L"wyraz d to nic.";

    StringReader reader;
    reader.openString(code);
    Lexer lexer(reader);
    Parser parser(lexer);
    Interpreter interpreter;

    std::optional<Program> program = parser.tryParseProgram();
    BOOST_REQUIRE(program.has_value());

    for (auto& stmt : program.value().statements)
    {
        interpreter.interpret(*stmt);
    }

    BOOST_REQUIRE(interpreter.countAllVariables() == 4);

    auto a = std::get<int>(interpreter.getVariableValue(L"a"));
    BOOST_CHECK_EQUAL(a, 0);

    auto b = std::get<double>(interpreter.getVariableValue(L"b"));
    BOOST_CHECK_CLOSE(b, 0.0, 0.001);

    auto c = std::get<bool>(interpreter.getVariableValue(L"c"));
    BOOST_CHECK_EQUAL(c, false);

    auto d = std::get<std::wstring>(interpreter.getVariableValue(L"d"));
    bool eq = d == L"";
    BOOST_CHECK(eq);
}

BOOST_AUTO_TEST_CASE(literal_variable_declaration)
{
    std::wstring code = 
        L"numer a to 3.\n" 
        L"stały ułamek b to 1,42.\n" 
        L"fakt c to fałsz.\n" 
        L"wyraz d to \"hejka\".";

    StringReader reader;
    reader.openString(code);
    Lexer lexer(reader);
    Parser parser(lexer);
    Interpreter interpreter;

    std::optional<Program> program = parser.tryParseProgram();
    BOOST_REQUIRE(program.has_value());

    for (auto& stmt : program.value().statements)
    {
        interpreter.interpret(*stmt);
    }

    BOOST_REQUIRE(interpreter.countAllVariables() == 4);

    auto a = std::get<int>(interpreter.getVariableValue(L"a"));
    BOOST_CHECK_EQUAL(a, 3);

    auto b = std::get<double>(interpreter.getVariableValue(L"b"));
    BOOST_CHECK_CLOSE(b, 1.42, 0.001);

    auto c = std::get<bool>(interpreter.getVariableValue(L"c"));
    BOOST_CHECK_EQUAL(c, false);

    auto d = std::get<std::wstring>(interpreter.getVariableValue(L"d"));
    bool eq = d == L"hejka";
    BOOST_CHECK(eq);
}

BOOST_AUTO_TEST_CASE(invalid_literal_variable_declaration_type)
{
    // Each string is a separate code snippet that should cause a type error
    std::vector<std::wstring> codes = {
        L"numer a to prawda.\n",    // bool assigned to int
        L"numer a to 1,2.\n",       // double assigned to int
        L"numer a to \"3\".\n",     // string assigned to int

        L"ułamek b to 2.\n",        // int assigned to double (maybe allowed? If not, test should fail)
        L"ułamek b to \"1,2\".\n",  // string assigned to double
        L"ułamek b to fałsz.\n",    // bool assigned to double

        L"fakt c to 3.\n",          // int assigned to bool
        L"fakt c to 1,2.\n",        // double assigned to bool
        L"fakt c to \"fałsz\".\n",  // string assigned to bool

        L"wyraz d to 3.\n",         // int assigned to string
        L"wyraz d to 1,2.\n",       // double assigned to string
        L"wyraz d to fałsz.\n"      // bool assigned to string
    };

    for (auto& code : codes)
    {
        StringReader reader;
        reader.openString(code);
        Lexer lexer(reader);
        Parser parser(lexer);
        Interpreter interpreter;

        auto programOpt = parser.tryParseProgram();
        BOOST_REQUIRE(programOpt.has_value());

        BOOST_CHECK_THROW(
            for (auto& stmt : programOpt.value().statements)
            {
                interpreter.interpret(*stmt);
            },
            std::runtime_error
        );
    }
}

BOOST_AUTO_TEST_CASE(negation_expression)
{
    std::wstring code = 
        L"fakt a to prawda.\n" 
        L"fakt b to nie prawda.\n" 
        L"fakt c to nie nie prawda.\n" 
        L"fakt x to fałsz.\n"
        L"fakt y to nie fałsz.\n"
        L"fakt z to nie nie fałsz.";

    StringReader reader;
    reader.openString(code);
    Lexer lexer(reader);
    Parser parser(lexer);
    Interpreter interpreter;

    std::optional<Program> program = parser.tryParseProgram();
    BOOST_REQUIRE(program.has_value());

    for (auto& stmt : program.value().statements)
    {
        interpreter.interpret(*stmt);
    }

    BOOST_REQUIRE(interpreter.countAllVariables() == 6);

    auto a = std::get<bool>(interpreter.getVariableValue(L"a"));
    BOOST_CHECK_EQUAL(a, true);

    auto b = std::get<bool>(interpreter.getVariableValue(L"b"));
    BOOST_CHECK_EQUAL(b, false);

    auto c = std::get<bool>(interpreter.getVariableValue(L"c"));
    BOOST_CHECK_EQUAL(c, true);

    auto x = std::get<bool>(interpreter.getVariableValue(L"x"));
    BOOST_CHECK_EQUAL(x, false);

    auto y = std::get<bool>(interpreter.getVariableValue(L"y"));
    BOOST_CHECK_EQUAL(y, true);

    auto z = std::get<bool>(interpreter.getVariableValue(L"z"));
    BOOST_CHECK_EQUAL(z, false);
}


BOOST_AUTO_TEST_CASE(invalid_negation_expression)
{
    std::vector<std::wstring> codes = {
        L"numer a to nie prawda.\n", // invalid variable type
        L"ułamek b to nie prawda.\n", // invalid variable type
        L"wyraz c to nie prawda.\n", // invalid variable type
        L"fakt d to nie 3.\n",       // invalid negation type
        L"fakt e to nie 3,2.\n",       // invalid negation type
        L"fakt f to nie \"prawda\".\n",       // invalid negation type
    };

    for (auto& code : codes)
    {
        StringReader reader;
        reader.openString(code);
        Lexer lexer(reader);
        Parser parser(lexer);
        Interpreter interpreter;

        auto programOpt = parser.tryParseProgram();
        BOOST_REQUIRE(programOpt.has_value());

        BOOST_CHECK_THROW(
            for (auto& stmt : programOpt.value().statements)
            {
                interpreter.interpret(*stmt);
            },
            std::runtime_error
        );
    }
}

BOOST_AUTO_TEST_CASE(identifier_expression)
{
    std::wstring code =
        L"numer a to 3.\n"
        L"numer b to a.\n"
        L"wyraz c to \"hejka\".\n"
        L"wyraz d to c.\n"
        L"ułamek e to 3,14.\n"
        L"ułamek f to e.\n"
        L"fakt g to prawda.\n"
        L"fakt h to g.\n"
        L"fakt i_ to nie g.\n";

    StringReader reader;
    reader.openString(code);
    Lexer lexer(reader);
    Parser parser(lexer);
    Interpreter interpreter;

    auto program = parser.tryParseProgram();
    BOOST_REQUIRE(program.has_value());

    for (auto& stmt : program->statements)
    {
        interpreter.interpret(*stmt);
    }

    BOOST_REQUIRE_EQUAL(interpreter.countAllVariables(), 9);

    // a, b: numer (assuming numer maps to int or similar numeric type)
    BOOST_CHECK_EQUAL(std::get<int>(interpreter.getVariableValue(L"a")), 3);
    BOOST_CHECK_EQUAL(std::get<int>(interpreter.getVariableValue(L"b")), 3); // b = a

    // e, f: ułamek (assuming ułamek maps to double or float)
    BOOST_CHECK_CLOSE(std::get<double>(interpreter.getVariableValue(L"e")), 3.14, 0.001);
    BOOST_CHECK_CLOSE(std::get<double>(interpreter.getVariableValue(L"f")), 3.14, 0.001); // f = e

    // g, h, i: fakt (bool)
    BOOST_CHECK_EQUAL(std::get<bool>(interpreter.getVariableValue(L"g")), true);
    BOOST_CHECK_EQUAL(std::get<bool>(interpreter.getVariableValue(L"h")), true);  // h = g
    BOOST_CHECK_EQUAL(std::get<bool>(interpreter.getVariableValue(L"i_")), false); // i = nie g
}


BOOST_AUTO_TEST_CASE(invalid_identifier_expression)
{
    std::vector<std::wstring> codes = {
        L"numer a to b.\n",        // Use of undefined variable 'b'
        L"ułamek b to b.\n" // Invalid negation on a float
    };

    for (const auto& code : codes)
    {
        StringReader reader;
        reader.openString(code);
        Lexer lexer(reader);
        Parser parser(lexer);
        Interpreter interpreter;

        auto programOpt = parser.tryParseProgram();
        BOOST_REQUIRE(programOpt.has_value());

        BOOST_CHECK_THROW(
            {
                for (auto& stmt : programOpt->statements)
                {
                    interpreter.interpret(*stmt);
                }
            },
            std::runtime_error
        );
    }
}

BOOST_AUTO_TEST_CASE(valid_cast_expression)
{
    std::wstring code =
        L"wyraz a to 3 na wyraz.\n"
        L"wyraz b to 3,14 na wyraz.\n"
        L"wyraz c to prawda na wyraz.\n"
        L"wyraz d to fałsz na wyraz.\n"
        L"wyraz e to \"hej\" na wyraz.\n"

        L"numer f to 3 na numer.\n"
        L"numer g to 3,14 na numer.\n"
        L"numer h to prawda na numer.\n"
        L"numer i_ to fałsz na numer.\n"
        L"numer j to \"3\" na numer.\n"

        L"ułamek k to 3 na ułamek.\n"
        L"ułamek l to 3,14 na ułamek.\n"
        L"ułamek m to prawda na ułamek.\n"
        L"ułamek n to fałsz na ułamek.\n"
        L"ułamek o to \"3,14\" na ułamek.\n"

        L"fakt u to 0 na fakt.\n"
        L"fakt p to 3,14 na fakt.\n"
        L"fakt r to prawda na fakt.\n"
        L"fakt s to \"fałsz\" na fakt.\n"
        L"fakt t to \"prawda\" na fakt.\n";

    StringReader reader;
    reader.openString(code);
    Lexer lexer(reader);
    Parser parser(lexer);
    Interpreter interpreter;

    auto program = parser.tryParseProgram();
    BOOST_REQUIRE(program.has_value());

    for (auto& stmt : program->statements)
        interpreter.interpret(*stmt);

    BOOST_REQUIRE_EQUAL(interpreter.countAllVariables(), 20);

    // Sprawdzenie wartości i typów po rzutowaniu:
    bool eq = std::get<std::wstring>(interpreter.getVariableValue(L"a")) == L"3"; BOOST_CHECK(eq);
    eq = std::get<std::wstring>(interpreter.getVariableValue(L"b")) == L"3,140000"; BOOST_CHECK(eq);
    eq = std::get<std::wstring>(interpreter.getVariableValue(L"c")) == L"prawda"; BOOST_CHECK(eq);
    eq = std::get<std::wstring>(interpreter.getVariableValue(L"d")) == L"fałsz"; BOOST_CHECK(eq);
    eq = std::get<std::wstring>(interpreter.getVariableValue(L"e")) == L"hej"; BOOST_CHECK(eq);
    
    BOOST_CHECK_EQUAL(std::get<int>(interpreter.getVariableValue(L"f")), 3);
    BOOST_CHECK_EQUAL(std::get<int>(interpreter.getVariableValue(L"g")), 3);
    BOOST_CHECK_EQUAL(std::get<int>(interpreter.getVariableValue(L"h")), 1);
    BOOST_CHECK_EQUAL(std::get<int>(interpreter.getVariableValue(L"i_")), 0);
    BOOST_CHECK_EQUAL(std::get<int>(interpreter.getVariableValue(L"j")), 3);

    BOOST_CHECK_CLOSE(std::get<double>(interpreter.getVariableValue(L"k")), 3.0, 0.001);
    BOOST_CHECK_CLOSE(std::get<double>(interpreter.getVariableValue(L"l")), 3.14, 0.001);
    BOOST_CHECK_CLOSE(std::get<double>(interpreter.getVariableValue(L"m")), 1.0, 0.001);
    BOOST_CHECK_CLOSE(std::get<double>(interpreter.getVariableValue(L"n")), 0.0, 0.001);
    BOOST_CHECK_CLOSE(std::get<double>(interpreter.getVariableValue(L"o")), 3.14, 0.001);

    BOOST_CHECK_EQUAL(std::get<bool>(interpreter.getVariableValue(L"u")), false);
    BOOST_CHECK_EQUAL(std::get<bool>(interpreter.getVariableValue(L"p")), true);
    BOOST_CHECK_EQUAL(std::get<bool>(interpreter.getVariableValue(L"r")), true);
    BOOST_CHECK_EQUAL(std::get<bool>(interpreter.getVariableValue(L"s")), false);
    BOOST_CHECK_EQUAL(std::get<bool>(interpreter.getVariableValue(L"t")), true);
}

BOOST_AUTO_TEST_CASE(invalid_cast_expression)
{
    std::vector<std::wstring> codes = {
        L"numer x to \"abc\" na numer.\n",
        L"ułamek x to \"3abc,14\" na ułamek.\n",
        L"fakt x to \"może\" na fakt.\n",
        L"numer x to \"3,14,15\" na numer.\n",
        L"numer x to \"3a\" na numer.\n",
        L"ułamek x to \"3abc,14\" na ułamek.\n",
        L"fakt x to \"tak\" na fakt.\n",
        L"numer x to y na numer.\n",
        L"numer x to 0 na ułamek.\n",
        L"numer x to 0 na wyraz.\n",
        L"numer x to 0 na fakt.\n",
        L"ułamek x to 0 na numer.\n",
        L"ułamek x to 0 na wyraz.\n",
        L"ułamek x to 0 na fakt.\n",
        L"fakt x to 0 na numer.\n",
        L"fakt x to 0 na wyraz.\n",
        L"fakt x to 0 na ułamek.\n",
        L"wyraz x to 0 na numer.\n",
        L"wyraz x to 0 na fakt.\n",
        L"wyraz x to 0 na ułamek.\n",
    };

    for (const auto& code : codes)
    {
        StringReader reader;
        reader.openString(code);
        Lexer lexer(reader);
        Parser parser(lexer);
        Interpreter interpreter;

        auto programOpt = parser.tryParseProgram();
        BOOST_REQUIRE(programOpt.has_value());

        BOOST_CHECK_THROW(
            {
                for (auto& stmt : programOpt->statements)
                {
                    interpreter.interpret(*stmt);
                }
            },
            std::runtime_error
        );
    }
}

BOOST_AUTO_TEST_CASE(valid_assignment_test)
{
    std::wstring code =
        L"numer a to 3.\n"
        L"ułamek b to 2,71.\n"
        L"fakt c to prawda.\n"
        L"wyraz d to \"test\".\n"
        L"a to 10.\n"
        L"b to 3,14.\n"
        L"c to fałsz.\n"
        L"d to \"zmieniono\".\n";

    StringReader reader;
    reader.openString(code);
    Lexer lexer(reader);
    Parser parser(lexer);
    Interpreter interpreter;

    auto program = parser.tryParseProgram();
    BOOST_REQUIRE(program.has_value());

    for (auto& stmt : program->statements)
        interpreter.interpret(*stmt);

    BOOST_CHECK_EQUAL(std::get<int>(interpreter.getVariableValue(L"a")), 10);
    BOOST_CHECK_CLOSE(std::get<double>(interpreter.getVariableValue(L"b")), 3.14, 0.001);
    BOOST_CHECK_EQUAL(std::get<bool>(interpreter.getVariableValue(L"c")), false);
    bool eq = std::get<std::wstring>(interpreter.getVariableValue(L"d")) == L"zmieniono"; BOOST_CHECK(eq);
}

BOOST_AUTO_TEST_CASE(invalid_assignment_type_test)
{
    std::vector<std::wstring> invalidAssignments = {
        L"numer a to 3.\n a to \"tekst\".\n",        // string do inta
        L"ułamek b to 2,5.\n b to prawda.\n",        // bool do double
        L"fakt c to prawda.\n c to 123.\n",          // int do bool
        L"wyraz d to \"tekst\".\n d to 3,14.\n",     // double do string
        L"stały numer e to 0.\n e to 1.\n",          // zmiana stałej
    };

    for (const auto& code : invalidAssignments)
    {
        StringReader reader;
        reader.openString(code);
        Lexer lexer(reader);
        Parser parser(lexer);
        Interpreter interpreter;

        auto programOpt = parser.tryParseProgram();
        BOOST_REQUIRE(programOpt.has_value());

        BOOST_CHECK_THROW(
            {
                for (auto& stmt : programOpt->statements)
                    interpreter.interpret(*stmt);
            },
            std::runtime_error
        );
    }
}

BOOST_AUTO_TEST_CASE(expression_evaluation_test)
{
    std::wstring code =
        // ------------------------ Dodawanie ------------------------
        L"numer dod1 to 1 plus 2.\n"
        L"numer dod2 to 1,1 na numer plus 2.\n"
        L"ułamek dod3 to 0,1 plus 0,1.\n"
        L"ułamek dod4 to 1 plus 0,1.\n"
        // ------------------------ Odejmowanie ------------------------
        L"numer od1 to 5 minus 2.\n"
        L"numer od2 to 3,2 na numer minus 2.\n"
        L"ułamek od3 to 0,5 minus 0,2.\n"
        L"ułamek od4 to 2 minus 0,4.\n"
        // ------------------------ Mnożenie ------------------------
        L"numer mn1 to 2 razy 1.\n"
        L"ułamek mn2 to 2 razy 0,1.\n"
        L"numer mn3 to 2 razy 0,1 na numer.\n"
        L"ułamek mn4 to 2,1 razy 2,1.\n"
        // ------------------------ Dzielenie ------------------------
        L"numer dz1 to (10 przez 2) na numer.\n"
        L"ułamek dz2 to 1 przez 2.\n"
        L"ułamek dz3 to 3,0 przez 1,5.\n"
        L"numer dz4 to (6 przez 2) na numer.\n"
        // ------------------------ Porównanie ------------------------
        L"fakt por1 to 2 mniejsze_niż 3.\n"
        L"fakt por2 to 2 większe_niż 3.\n"
        L"fakt por3 to 3 większe_niż 3.\n"
        L"fakt por4 to 3 mniejsze_niż 3.\n"
        L"fakt por5 to 3 większe_niż 2.\n"
        L"fakt por6 to 2,5 większe_niż 1,5.\n"
        L"fakt por7 to 2 mniejsze_niż 2,1.\n"
        L"fakt por8 to 2 większe_niż 2,1.\n"
        // ------------------------ Konkatenacja ------------------------
        L"wyraz s1 to \"Ala\".\n"
        L"wyraz s2 to \" ma kota\".\n"
        L"wyraz s3 to s1 złącz s2.\n"
        L"wyraz s4 to \"1\" złącz \"2\".\n"
        L"wyraz s5 to \"\" złącz \"\".\n"
        L"wyraz s6 to \"test\" złącz \"\".\n"
        L"wyraz s7 to \"\" złącz \"dane\".\n"
        // ------------------------ EQ / NEQ ------------------------
        L"fakt ene1 to 3 równe 3.\n"
        L"fakt ene2 to 3 nierówne 4.\n"
        L"fakt ene3 to 3 równe 3,0.\n"
        L"fakt ene4 to 4,5 nierówne 4,5.\n"
        L"fakt ene5 to prawda równe prawda.\n"
        L"fakt ene6 to \"hej\" równe \"hej\".\n"
        L"fakt ene7 to \"hej\" nierówne \"hoj\".\n"
        // ------------------------ AND / OR ------------------------
        L"fakt ao1 to prawda oraz prawda.\n"
        L"fakt ao2 to prawda oraz fałsz.\n"
        L"fakt ao3 to fałsz lub prawda.\n"
        L"fakt ao4 to fałsz lub fałsz.\n"
        L"fakt ao5 to (1 mniejsze_niż 2) oraz (3 większe_niż 2).\n";

    StringReader reader;
    reader.openString(code);
    Lexer lexer(reader);
    Parser parser(lexer);
    Interpreter interpreter;

    auto program = parser.tryParseProgram();
    BOOST_REQUIRE(program.has_value());

    for (auto& stmt : program->statements)
        interpreter.interpret(*stmt);

    // ------------------------ Dodawanie ------------------------
    BOOST_CHECK_EQUAL(std::get<int>(interpreter.getVariableValue(L"dod1")), 3);
    BOOST_CHECK_EQUAL(std::get<int>(interpreter.getVariableValue(L"dod2")), 3);
    BOOST_CHECK_CLOSE(std::get<double>(interpreter.getVariableValue(L"dod3")), 0.2, 0.001);
    BOOST_CHECK_CLOSE(std::get<double>(interpreter.getVariableValue(L"dod4")), 1.1, 0.001);

    // ------------------------ Odejmowanie ------------------------
    BOOST_CHECK_EQUAL(std::get<int>(interpreter.getVariableValue(L"od1")), 3);
    BOOST_CHECK_EQUAL(std::get<int>(interpreter.getVariableValue(L"od2")), 1);
    BOOST_CHECK_CLOSE(std::get<double>(interpreter.getVariableValue(L"od3")), 0.3, 0.001);
    BOOST_CHECK_CLOSE(std::get<double>(interpreter.getVariableValue(L"od4")), 1.6, 0.001);

    // ------------------------ Mnożenie ------------------------
    BOOST_CHECK_EQUAL(std::get<int>(interpreter.getVariableValue(L"mn1")), 2);
    BOOST_CHECK_CLOSE(std::get<double>(interpreter.getVariableValue(L"mn2")), 0.2, 0.001);
    BOOST_CHECK_EQUAL(std::get<int>(interpreter.getVariableValue(L"mn3")), 0);
    BOOST_CHECK_CLOSE(std::get<double>(interpreter.getVariableValue(L"mn4")), 4.41, 0.001);

    // ------------------------ Dzielenie ------------------------
    BOOST_CHECK_EQUAL(std::get<int>(interpreter.getVariableValue(L"dz1")), 5);
    BOOST_CHECK_CLOSE(std::get<double>(interpreter.getVariableValue(L"dz2")), 0.5, 0.001);
    BOOST_CHECK_CLOSE(std::get<double>(interpreter.getVariableValue(L"dz3")), 2.0, 0.001);
    BOOST_CHECK_EQUAL(std::get<int>(interpreter.getVariableValue(L"dz4")), 3);

    // ------------------------ Porównanie ------------------------
    BOOST_CHECK_EQUAL(std::get<bool>(interpreter.getVariableValue(L"por1")), true);
    BOOST_CHECK_EQUAL(std::get<bool>(interpreter.getVariableValue(L"por2")), false);
    BOOST_CHECK_EQUAL(std::get<bool>(interpreter.getVariableValue(L"por3")), false);
    BOOST_CHECK_EQUAL(std::get<bool>(interpreter.getVariableValue(L"por4")), false);
    BOOST_CHECK_EQUAL(std::get<bool>(interpreter.getVariableValue(L"por5")), true);
    BOOST_CHECK_EQUAL(std::get<bool>(interpreter.getVariableValue(L"por6")), true);
    BOOST_CHECK_EQUAL(std::get<bool>(interpreter.getVariableValue(L"por7")), true);
    BOOST_CHECK_EQUAL(std::get<bool>(interpreter.getVariableValue(L"por8")), false);

    // ------------------------ Konkatenacja ------------------------
    bool eq = std::get<std::wstring>(interpreter.getVariableValue(L"s3")) == L"Ala ma kota"; BOOST_CHECK(eq);
    eq = std::get<std::wstring>(interpreter.getVariableValue(L"s4")) == L"12"; BOOST_CHECK(eq);
    eq = std::get<std::wstring>(interpreter.getVariableValue(L"s5")) == L""; BOOST_CHECK(eq);
    eq = std::get<std::wstring>(interpreter.getVariableValue(L"s6")) == L"test"; BOOST_CHECK(eq);
    eq = std::get<std::wstring>(interpreter.getVariableValue(L"s7")) == L"dane"; BOOST_CHECK(eq);

    // ------------------------ EQ / NEQ ------------------------
    BOOST_CHECK_EQUAL(std::get<bool>(interpreter.getVariableValue(L"ene1")), true);
    BOOST_CHECK_EQUAL(std::get<bool>(interpreter.getVariableValue(L"ene2")), true);
    BOOST_CHECK_EQUAL(std::get<bool>(interpreter.getVariableValue(L"ene3")), true);
    BOOST_CHECK_EQUAL(std::get<bool>(interpreter.getVariableValue(L"ene4")), false);
    BOOST_CHECK_EQUAL(std::get<bool>(interpreter.getVariableValue(L"ene5")), true);
    BOOST_CHECK_EQUAL(std::get<bool>(interpreter.getVariableValue(L"ene6")), true);
    BOOST_CHECK_EQUAL(std::get<bool>(interpreter.getVariableValue(L"ene7")), true);

    // ------------------------ AND / OR ------------------------
    BOOST_CHECK_EQUAL(std::get<bool>(interpreter.getVariableValue(L"ao1")), true);
    BOOST_CHECK_EQUAL(std::get<bool>(interpreter.getVariableValue(L"ao2")), false);
    BOOST_CHECK_EQUAL(std::get<bool>(interpreter.getVariableValue(L"ao3")), true);
    BOOST_CHECK_EQUAL(std::get<bool>(interpreter.getVariableValue(L"ao4")), false);
    BOOST_CHECK_EQUAL(std::get<bool>(interpreter.getVariableValue(L"ao5")), true);
}

BOOST_AUTO_TEST_CASE(invalid_expression_test)
{
    std::vector<std::wstring> invalidExpressions = {
        // Dzielenie przez zero
        L"numer a to 10 przez 0.\n",
        L"ułamek b to 5,0 przez 0.\n",
        L"ułamek c to 1,0 przez 0,0.\n",

        // Konkatenacja nie-stringów
        L"numer a to 1 złącz 2.\n",
        L"fakt b to prawda złącz fałsz.\n",
        L"ułamek c to 3,14 złącz \"abc\".\n",

        // Operatory logiczne na nie-boolach
        L"numer a to 1 oraz 0.\n",
        L"ułamek b to 1,1 lub 0,0.\n",
        L"wyraz c to \"a\" oraz \"b\".\n",

        // Porównania różnych typów
        L"fakt a to 1 większe_niż \"tekst\".\n",
        L"fakt b to prawda mniejsze_niż 3.\n",
        L"fakt c to 3 większe_niż prawda.\n",

        // EQ/NEQ różnych typów
        L"fakt a to 3 równe \"3\".\n",
        L"fakt b to prawda nierówne \"fałsz\".\n",
        L"fakt c to \"tekst\" równe 2,1.\n",
    };

    for (const auto& code : invalidExpressions)
    {
        StringReader reader;
        reader.openString(code);
        Lexer lexer(reader);
        Parser parser(lexer);
        Interpreter interpreter;

        auto programOpt = parser.tryParseProgram();
        BOOST_REQUIRE(programOpt.has_value());

        BOOST_CHECK_THROW(
            {
                for (auto& stmt : programOpt->statements)
                    interpreter.interpret(*stmt);
            },
            std::runtime_error
        );
    }
}

BOOST_AUTO_TEST_CASE(scope_variable_is_local)
{
    std::wstring code =
        L"od\n"
        L"    numer a to 5.\n"
        L"do\n";

    StringReader reader;
    reader.openString(code);
    Lexer lexer(reader);
    Parser parser(lexer);
    Interpreter interpreter;

    auto program = parser.tryParseProgram();
    BOOST_REQUIRE(program.has_value());

    for (auto& stmt : program->statements)
        interpreter.interpret(*stmt);

    BOOST_CHECK_THROW(interpreter.getVariableValue(L"a"), std::runtime_error);
}


BOOST_AUTO_TEST_CASE(scope_variable_shadowing_allowed)
{
    std::wstring code =
        L"numer a to 1.\n"
        L"od\n"
        L"    numer a to 2.\n"
        L"do\n";

    StringReader reader;
    reader.openString(code);
    Lexer lexer(reader);
    Parser parser(lexer);
    Interpreter interpreter;

    auto program = parser.tryParseProgram();
    BOOST_REQUIRE(program.has_value());

    for (auto& stmt : program->statements)
        interpreter.interpret(*stmt);

    int a = std::get<int>(interpreter.getVariableValue(L"a"));
    BOOST_CHECK_EQUAL(a, 1);  // zewnętrzna zmienna
}

BOOST_AUTO_TEST_CASE(scope_inner_variable_not_visible_outside)
{
    std::wstring code =
        L"od\n"
        L"    numer b to 123.\n"
        L"do\n"
        L"numer a to b.";

    StringReader reader;
    reader.openString(code);
    Lexer lexer(reader);
    Parser parser(lexer);
    Interpreter interpreter;

    auto program = parser.tryParseProgram();
    BOOST_REQUIRE(program.has_value());

    BOOST_CHECK_THROW(
        for (auto& stmt : program->statements)
            stmt->accept(interpreter),
        std::runtime_error
    );
}

BOOST_AUTO_TEST_CASE(scope_can_assign_outer_variable)
{
    std::wstring code =
        L"numer a to 10.\n"
        L"od\n"
        L"    a to 99.\n"
        L"do\n";

    StringReader reader;
    reader.openString(code);
    Lexer lexer(reader);
    Parser parser(lexer);
    Interpreter interpreter;

    auto program = parser.tryParseProgram();
    BOOST_REQUIRE(program.has_value());

    for (auto& stmt : program->statements)
        interpreter.interpret(*stmt);

    int a = std::get<int>(interpreter.getVariableValue(L"a"));
    BOOST_CHECK_EQUAL(a, 99);
}

BOOST_AUTO_TEST_CASE(scope_shadowing_value_check)
{
    std::wstring code =
        L"numer x to 1.\n"
        L"numer y to 0.\n"
        L"od\n"
        L"    numer x to 2.\n"
        L"    x to 3.\n"
        L"    y to x.\n"
        L"do\n"
        L"x to 4.\n";

    StringReader reader;
    reader.openString(code);
    Lexer lexer(reader);
    Parser parser(lexer);
    Interpreter interpreter;

    auto program = parser.tryParseProgram();
    BOOST_REQUIRE(program.has_value());

    for (auto& stmt : program->statements)
        interpreter.interpret(*stmt);

    BOOST_CHECK_EQUAL(std::get<int>(interpreter.getVariableValue(L"x")), 4);
    BOOST_CHECK_EQUAL(std::get<int>(interpreter.getVariableValue(L"y")), 3);
}


BOOST_AUTO_TEST_CASE(nested_scopes_variable_isolation)
{
    std::wstring code =
        L"od\n"
        L"    numer x to 10.\n"
        L"    od\n"
        L"        numer y to 20.\n"
        L"    do\n"
        L"do\n"
        L"numer z to 42.\n";

    StringReader reader;
    reader.openString(code);
    Lexer lexer(reader);
    Parser parser(lexer);
    Interpreter interpreter;

    auto program = parser.tryParseProgram();
    BOOST_REQUIRE(program.has_value());

    for (auto& stmt : program->statements)
        interpreter.interpret(*stmt);

    int z = std::get<int>(interpreter.getVariableValue(L"z"));
    BOOST_CHECK_EQUAL(z, 42);
    BOOST_CHECK_THROW(interpreter.getVariableValue(L"x"), std::runtime_error);
    BOOST_CHECK_THROW(interpreter.getVariableValue(L"y"), std::runtime_error);
}

BOOST_AUTO_TEST_CASE(nested_loop_can_assign_outer_variable)
{
    std::wstring code =
        L"numer x to nic.\n"
        L"powtórz 10 krotnie\n"
        L"od\n"
        L"   x to x plus 1.\n"
        L"   powtórz 10 krotnie\n"
        L"       x to x plus 1.\n"
        L"do\n";

    StringReader reader;
    reader.openString(code);
    Lexer lexer(reader);
    Parser parser(lexer);
    Interpreter interpreter;

    auto program = parser.tryParseProgram();
    BOOST_REQUIRE(program.has_value());

    for (auto& stmt : program->statements)
        interpreter.interpret(*stmt);

    int x = std::get<int>(interpreter.getVariableValue(L"x"));
    BOOST_CHECK_EQUAL(x, 110);
}

BOOST_AUTO_TEST_CASE(if_statement_complex_conditions)
{
    std::wstring code =
        L"numer x to 10.\n"
        L"jeżeli x równe 10 wtedy\n"
        L"    x to 20.\n"
        L"inaczej_gdy x większe_niż 10 wtedy\n"
        L"    x to 30.\n"
        L"inaczej_gdy x mniejsze_niż 10 wtedy\n"
        L"    x to 40.\n"
        L"ostatecznie\n"
        L"    x to 50.\n"

        L"numer y to 9.\n"
        L"jeżeli y równe 10 wtedy\n"
        L"    y to 20.\n"
        L"inaczej_gdy y większe_niż 10 wtedy\n"
        L"    y to 30.\n"
        L"inaczej_gdy y mniejsze_niż 10 wtedy\n"
        L"    y to 40.\n"
        L"ostatecznie\n"
        L"    y to 50.\n"

        L"numer z to 11.\n"
        L"jeżeli z równe 10 wtedy\n"
        L"    z to 20.\n"
        L"inaczej_gdy z większe_niż 10 wtedy\n"
        L"    z to 30.\n"
        L"inaczej_gdy z mniejsze_niż 10 wtedy\n"
        L"    z to 40.\n"
        L"ostatecznie\n"
        L"    z to 50.\n"

        L"numer a to 10.\n"
        L"jeżeli a nierówne 10 wtedy\n"
        L"    a to 20.\n"
        L"inaczej_gdy a większe_niż 10 wtedy\n"
        L"    a to 30.\n"
        L"inaczej_gdy a mniejsze_niż 10 wtedy\n"
        L"    a to 40.\n"
        L"ostatecznie\n"
        L"    a to 50.\n";

    StringReader reader;
    reader.openString(code);
    Lexer lexer(reader);
    Parser parser(lexer);
    Interpreter interpreter;

    auto program = parser.tryParseProgram();
    BOOST_REQUIRE(program.has_value());

    for (auto& stmt : program->statements)
        interpreter.interpret(*stmt);

    int x = std::get<int>(interpreter.getVariableValue(L"x"));
    int y = std::get<int>(interpreter.getVariableValue(L"y"));
    int z = std::get<int>(interpreter.getVariableValue(L"z"));
    int a = std::get<int>(interpreter.getVariableValue(L"a"));

    BOOST_CHECK_EQUAL(x, 20);
    BOOST_CHECK_EQUAL(y, 40);
    BOOST_CHECK_EQUAL(z, 30);
    BOOST_CHECK_EQUAL(a, 50);
}

BOOST_AUTO_TEST_CASE(simple_function_declaration)
{
    std::wstring sourceCode = LR"(
        praca testowa1(numer x) daje wyraz robi od
            zwróć x plus 2.
        do

        praca testowa2(fakt x i wyraz y) daje fakt robi od
            zwróć x.
        do

        praca testowa3(numer x i wyraz y i fakt z) daje numer robi od
            zwróć x plus 2.
        do

        praca testowa4() daje ułamek robi od
            zwróć 1,23.
        do

        praca testowa5() daje nic robi od
        do
    )";

    StringReader reader;
    reader.openString(sourceCode);
    Lexer lexer(reader);
    Parser parser(lexer);
    Interpreter interpreter;

    auto program = parser.tryParseProgram();
    BOOST_REQUIRE(program.has_value());

    for (auto& stmt : program->statements)
        interpreter.interpret(*stmt);

    BOOST_CHECK_EQUAL(interpreter.countAllFunctions(), 5);

    auto func1 = interpreter.getFunction(L"testowa1");
    BOOST_REQUIRE(func1 != nullptr);
    BOOST_CHECK(func1->getReturnType().has_value());
    BOOST_CHECK(std::get<LiteralType>(func1->getReturnType().value()) == LiteralType::STRING);
    BOOST_CHECK_EQUAL(func1->getArguments().size(), 1);
    BOOST_CHECK(std::get<LiteralType>(func1->getArguments()[0].first) == LiteralType::INTEGER);

    auto func2 = interpreter.getFunction(L"testowa2");
    BOOST_REQUIRE(func2 != nullptr);
    BOOST_CHECK(func2->getReturnType().has_value());
    BOOST_CHECK(std::get<LiteralType>(func2->getReturnType().value()) == LiteralType::BOOLEAN);
    BOOST_CHECK_EQUAL(func2->getArguments().size(), 2);
    BOOST_CHECK(std::get<LiteralType>(func2->getArguments()[0].first) == LiteralType::BOOLEAN);
    BOOST_CHECK(std::get<LiteralType>(func2->getArguments()[1].first) == LiteralType::STRING);

    auto func3 = interpreter.getFunction(L"testowa3");
    BOOST_REQUIRE(func3 != nullptr);
    BOOST_CHECK(func3->getReturnType().has_value());
    BOOST_CHECK(std::get<LiteralType>(func3->getReturnType().value()) == LiteralType::INTEGER);
    BOOST_CHECK_EQUAL(func3->getArguments().size(), 3);
    BOOST_CHECK(std::get<LiteralType>(func3->getArguments()[0].first) == LiteralType::INTEGER);
    BOOST_CHECK(std::get<LiteralType>(func3->getArguments()[1].first) == LiteralType::STRING);
    BOOST_CHECK(std::get<LiteralType>(func3->getArguments()[2].first) == LiteralType::BOOLEAN);

    auto func4 = interpreter.getFunction(L"testowa4");
    BOOST_REQUIRE(func4 != nullptr);
    BOOST_CHECK(func4->getReturnType().has_value());
    BOOST_CHECK(std::get<LiteralType>(func4->getReturnType().value()) == LiteralType::DOUBLE);
    BOOST_CHECK(func4->getArguments().empty());

    auto func5 = interpreter.getFunction(L"testowa5");
    BOOST_REQUIRE(func5 != nullptr);
    BOOST_CHECK(!func5->getReturnType().has_value());  // None
    BOOST_CHECK(func5->getArguments().empty());
}

BOOST_AUTO_TEST_CASE(nested_function_variable_test)
{
    std::wstring sourceCode = LR"(
        praca testowa1(praca (numer) dająca wyraz x i wyraz y) daje wyraz robi od
            zwróć y plus 2.
        do

        praca testowa2(praca (praca (numer) dająca wyraz) dająca wyraz x i wyraz y) daje wyraz robi od
            zwróć y plus 2.
        do

        praca(praca (numer) dająca wyraz i wyraz) dająca wyraz kopia1 to testowa1.
        praca(praca (praca (numer) dająca wyraz) dająca wyraz i wyraz) dająca wyraz kopia2 to testowa2.
    )";

    StringReader reader;
    reader.openString(sourceCode);
    Lexer lexer(reader);
    Parser parser(lexer);
    Interpreter interpreter;

    auto program = parser.tryParseProgram();
    BOOST_REQUIRE(program.has_value());

    for (auto& stmt : program->statements)
        interpreter.interpret(*stmt);

    BOOST_CHECK_EQUAL(interpreter.countAllFunctions(), 4);

    // testowa1
    auto testowa1 = interpreter.getFunction(L"testowa1");
    BOOST_REQUIRE(testowa1 != nullptr);
    BOOST_CHECK(testowa1->getReturnType().has_value());
    BOOST_CHECK(std::get<LiteralType>(testowa1->getReturnType().value()) == LiteralType::STRING);
    BOOST_CHECK_EQUAL(testowa1->getArguments().size(), 2);
    const auto& arg1_varType = testowa1->getArguments()[0].first;
    auto& fnPtr1 = std::get<std::shared_ptr<FunctionVariableType>>(arg1_varType);
    BOOST_REQUIRE(fnPtr1 != nullptr);
    const FunctionVariableType& fn1 = *fnPtr1;
    BOOST_CHECK_EQUAL(fn1.paramTypes.size(), 1);
    bool eq = std::get<LiteralType>(fn1.paramTypes[0]) == LiteralType::INTEGER; BOOST_CHECK(eq); // numer x
    eq = std::get<LiteralType>(fn1.returnType.value()) == LiteralType::STRING; BOOST_CHECK(eq); // numer x

    // testowa2
    auto testowa2 = interpreter.getFunction(L"testowa2");
    BOOST_REQUIRE(testowa2 != nullptr);
    BOOST_CHECK(testowa2->getReturnType().has_value());
    BOOST_CHECK(std::get<LiteralType>(testowa2->getReturnType().value()) == LiteralType::STRING);
    BOOST_CHECK_EQUAL(testowa2->getArguments().size(), 2);

    const auto& arg2_varType = testowa2->getArguments()[0].first;
    auto& fnPtr2 = std::get<std::shared_ptr<FunctionVariableType>>(arg2_varType);
    BOOST_REQUIRE(fnPtr2 != nullptr);
    const FunctionVariableType& fn2 = *fnPtr2;
    BOOST_CHECK_EQUAL(fn2.paramTypes.size(), 1);

    // // kopia1 should be equal to testowa1
    auto kopia1 = interpreter.getFunction(L"kopia1");
    BOOST_REQUIRE(kopia1 != nullptr);
    BOOST_CHECK(kopia1->getReturnType().has_value());
    BOOST_CHECK(std::get<LiteralType>(kopia1->getReturnType().value()) == LiteralType::STRING);
    BOOST_CHECK_EQUAL(kopia1->getArguments().size(), 2);
    const auto& arg1_varType2 = kopia1->getArguments()[0].first;
    auto& fnPtr3 = std::get<std::shared_ptr<FunctionVariableType>>(arg1_varType2);
    BOOST_REQUIRE(fnPtr3 != nullptr);
    const FunctionVariableType& fn3 = *fnPtr3;
    BOOST_CHECK_EQUAL(fn3.paramTypes.size(), 1);
    eq = std::get<LiteralType>(fn3.paramTypes[0]) == LiteralType::INTEGER; BOOST_CHECK(eq); // numer x
    eq = std::get<LiteralType>(fn3.returnType.value()) == LiteralType::STRING; BOOST_CHECK(eq); // numer x
}

BOOST_AUTO_TEST_CASE(referenced_argument_updates_variable)
{
    std::wstring sourceCode = LR"(
        praca testowa1(numer y) daje numer robi od
            y to 20.
            zwróć 1.
        do

        numer a to 0.
        numer b to testowa1(a).
    )";

    StringReader reader;
    reader.openString(sourceCode);
    Lexer lexer(reader);
    Parser parser(lexer);
    Interpreter interpreter;

    auto program = parser.tryParseProgram();
    BOOST_REQUIRE(program.has_value());

    for (auto& stmt : program->statements)
        interpreter.interpret(*stmt);
    
    int a = std::get<int>(interpreter.getVariableValue(L"a"));
    BOOST_CHECK_EQUAL(a, 20);

    int b = std::get<int>(interpreter.getVariableValue(L"b"));
    BOOST_CHECK_EQUAL(b, 1);
}

BOOST_AUTO_TEST_CASE(skrocona_operator_variables)
{
    std::wstring sourceCode = LR"(
        praca fun (numer a i numer b i numer c) daje numer robi od
            zwróć a plus b plus c.
        do

        praca (numer i numer) dająca numer skrot to skrócona_o (1) fun.
        praca (numer) dająca numer skrot_bardziej to skrócona_o (2) skrot.
        praca () dająca numer skrot_bardziej_bardziej to skrócona_o (3) skrot_bardziej.

        numer a to fun(1 i 2 i 3).
        numer b to skrot(2 i 3).
        numer c to skrot_bardziej(3).
        numer d to skrot_bardziej_bardziej().

    )";

    StringReader reader;
    reader.openString(sourceCode);
    Lexer lexer(reader);
    Parser parser(lexer);
    Interpreter interpreter;

    auto program = parser.tryParseProgram();
    BOOST_REQUIRE(program.has_value());

    for (auto& stmt : program->statements)
        interpreter.interpret(*stmt);
    

    BOOST_CHECK_EQUAL(std::get<int>(interpreter.getVariableValue(L"a")), 6);
    BOOST_CHECK_EQUAL(std::get<int>(interpreter.getVariableValue(L"b")), 6);
    BOOST_CHECK_EQUAL(std::get<int>(interpreter.getVariableValue(L"c")), 6);
    BOOST_CHECK_EQUAL(std::get<int>(interpreter.getVariableValue(L"d")), 6);
}

BOOST_AUTO_TEST_CASE(przyklad_operator)
{
    std::wstring sourceCode = LR"(
        praca rand_int (numer a) daje numer robi od
            zwróć a.
        do

        praca rand_double (ułamek a) daje ułamek robi od
            zwróć a.
        do

        praca rand_string (wyraz a) daje wyraz robi od
            zwróć a.
        do

        praca rand_bool (fakt a) daje fakt robi od
            zwróć a.
        do

        numer a to (przykład rand_int)().
        ułamek b to (przykład rand_double)().
        wyraz c to (przykład rand_string)().
        fakt d to (przykład rand_bool)().

    )";

    StringReader reader;
    reader.openString(sourceCode);
    Lexer lexer(reader);
    Parser parser(lexer);
    Interpreter interpreter;

    auto program = parser.tryParseProgram();
    BOOST_REQUIRE(program.has_value());

    for (auto& stmt : program->statements)
        interpreter.interpret(*stmt);
    

    BOOST_CHECK_GE(std::get<int>(interpreter.getVariableValue(L"a")), 1);
    BOOST_CHECK_LE(std::get<int>(interpreter.getVariableValue(L"a")), 1000);

    BOOST_CHECK_GE(std::get<double>(interpreter.getVariableValue(L"b")), 0);
    BOOST_CHECK_LE(std::get<double>(interpreter.getVariableValue(L"b")), 1);
    
    BOOST_CHECK_EQUAL(std::get<std::wstring>(interpreter.getVariableValue(L"c")).length(), 5);
}

BOOST_AUTO_TEST_CASE(function_in_arguments)
{
    std::wstring sourceCode = LR"(
        praca fun(praca (numer) dająca numer x) daje numer robi od
            zwróć x(10).
        do

        praca pomocna(numer x) daje numer robi od
            zwróć x plus 2.
        do
        praca pomocna2(numer x) daje numer robi od
            zwróć x plus 4.
        do

        numer a to fun(pomocna).
        numer b to fun(pomocna2).

    )";

    StringReader reader;
    reader.openString(sourceCode);
    Lexer lexer(reader);
    Parser parser(lexer);
    Interpreter interpreter;

    auto program = parser.tryParseProgram();
    BOOST_REQUIRE(program.has_value());

    for (auto& stmt : program->statements)
        interpreter.interpret(*stmt);
    

    BOOST_CHECK_EQUAL(std::get<int>(interpreter.getVariableValue(L"a")), 12);
    BOOST_CHECK_EQUAL(std::get<int>(interpreter.getVariableValue(L"b")), 14);
}


BOOST_AUTO_TEST_CASE(function_in_returns)
{
    std::wstring sourceCode = LR"(
        praca fun(numer x i numer y) daje praca (numer) dająca praca (numer) dająca numer robi od
            praca wew(numer x) daje praca (numer) dająca numer robi od
                praca wew_wew(numer x) daje numer robi od
                    zwróć 99 plus x.
                do
                zwróć wew_wew.
            do
            zwróć wew.
        do

        numer a to fun(1 i 2)(3)(5).

    )";

    StringReader reader;
    reader.openString(sourceCode);
    Lexer lexer(reader);
    Parser parser(lexer);
    Interpreter interpreter;

    auto program = parser.tryParseProgram();
    BOOST_REQUIRE(program.has_value());

    for (auto& stmt : program->statements)
        interpreter.interpret(*stmt);
    

    BOOST_CHECK_EQUAL(std::get<int>(interpreter.getVariableValue(L"a")), 104);
}

BOOST_AUTO_TEST_CASE(skrocona_with_functions)
{
    std::wstring sourceCode = LR"(
        praca fun(praca (numer) dająca numer x i numer y) daje numer robi od
            zwróć x(5) plus y.
        do

        praca wew(numer x) daje numer robi od
            zwróć x plus 2.
        do

        praca wew2(numer x) daje numer robi od
            zwróć x plus 4.
        do

        numer a to (skrócona_o (wew) fun)(5).
        numer b to (skrócona_o (wew2) fun)(5).

    )";

    StringReader reader;
    reader.openString(sourceCode);
    Lexer lexer(reader);
    Parser parser(lexer);
    Interpreter interpreter;

    auto program = parser.tryParseProgram();
    BOOST_REQUIRE(program.has_value());

    for (auto& stmt : program->statements)
        interpreter.interpret(*stmt);
    

    BOOST_CHECK_EQUAL(std::get<int>(interpreter.getVariableValue(L"a")), 12);
    BOOST_CHECK_EQUAL(std::get<int>(interpreter.getVariableValue(L"b")), 14);
}

BOOST_AUTO_TEST_CASE(recursion)
{
    std::wstring sourceCode = LR"(
        praca fun(numer x) daje nic robi od
            jeżeli x równe 0 wtedy
                zwróć nic.
            ostatecznie od
                fun(x minus 1).
                zwróć nic.
            do
        do

        praca fun2(numer x) daje nic robi od
            jeżeli x równe 0 wtedy
                zwróć nic.
            ostatecznie od
                x to x minus 1.
                fun2(x).
                zwróć nic.
            do
        do

        numer a to 10.
        fun(a).
        numer b to 10.
        fun2(b).
    )";

    StringReader reader;
    reader.openString(sourceCode);
    Lexer lexer(reader);
    Parser parser(lexer);
    Interpreter interpreter;

    auto program = parser.tryParseProgram();
    BOOST_REQUIRE(program.has_value());

    for (auto& stmt : program->statements)
        interpreter.interpret(*stmt);
    
    BOOST_CHECK_EQUAL(std::get<int>(interpreter.getVariableValue(L"a")), 10);
    BOOST_CHECK_EQUAL(std::get<int>(interpreter.getVariableValue(L"b")), 0);
}


BOOST_AUTO_TEST_CASE(recursion_limit_err)
{
    std::wstring sourceCode = LR"(
        praca x () daje nic robi x().x().
    )";

    StringReader reader;
    reader.openString(sourceCode);
    Lexer lexer(reader);
    Parser parser(lexer);
    Interpreter interpreter;

    auto program = parser.tryParseProgram();
    BOOST_REQUIRE(program.has_value());

    program.value().statements[0]->accept(interpreter);
    BOOST_CHECK_THROW(program.value().statements[1]->accept(interpreter), std::runtime_error);
}

