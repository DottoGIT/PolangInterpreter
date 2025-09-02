#include <vector>
#include <memory>
#include "parser.h"
#include "lexer.h"
#include "interpreter.h"

#include "string_reader.h"
#include "expressions.hpp"

#define BOOST_TEST_MODULE ParserTests
#include <boost/test/included/unit_test.hpp>


////////////////////////////////////////////////////////////////////////////////////////////////////////
////                                 S T A T E M E N T S                                            ////
////////////////////////////////////////////////////////////////////////////////////////////////////////

using TType = decltype(TokenType::INTEGER);

BOOST_AUTO_TEST_CASE(empty_variable_declaration)
{
    using CodeData = std::tuple<std::wstring, VariableDeclaration>;
    CodeData codes[] = {
        {L"numer x to nic.",    VariableDeclaration(false, L"x", TokenType::INTEGER, Position())},
        {L"wyraz y to nic.",    VariableDeclaration(false, L"y", TokenType::STRING, Position())},
        {L"ułamek z to nic.",   VariableDeclaration(false, L"z", TokenType::DOUBLE, Position())},
        {L"fakt q to nic.",     VariableDeclaration(false, L"q", TokenType::BOOLEAN, Position())},
        
        {L"stały numer x to nic.",  VariableDeclaration(true, L"x", TokenType::INTEGER, Position())},
        {L"stały wyraz y to nic.",  VariableDeclaration(true, L"y", TokenType::STRING, Position())},
        {L"stały ułamek z to nic.", VariableDeclaration(true, L"z", TokenType::DOUBLE, Position())},
        {L"stały fakt q to nic.",   VariableDeclaration(true, L"q", TokenType::BOOLEAN, Position())}
    };

    for (const auto& [code, expectedVarDecl] : codes)
    {
        StringReader reader;
        reader.openString(code);
        Lexer lexer(reader);
        Parser parser(lexer);
        std::optional<Program> program = parser.tryParseProgram();

        BOOST_REQUIRE(program.has_value());
        BOOST_REQUIRE_EQUAL(program->statements.size(), 1);

        const auto& stmtPtr = program->statements[0];
        auto* varDecl = dynamic_cast<VariableDeclaration*>(stmtPtr.get());

        BOOST_REQUIRE(varDecl != nullptr);

        // weird syntax due to BOOST inability to print wstrings
        bool eq = varDecl->varName == expectedVarDecl.varName;                                  BOOST_TEST(eq);
        eq = std::get<TType>(varDecl->varType) == std::get<TType>(expectedVarDecl.varType);     BOOST_TEST(eq);
        eq = varDecl->isConst == expectedVarDecl.isConst;                                       BOOST_TEST(eq);
    }
}

BOOST_AUTO_TEST_CASE(function_data_type)
{
    {
        std::wstring code = L"praca (numer i wyraz) dająca fakt y to inna_funkcja.";

        StringReader reader;
        reader.openString(code);
        Lexer lexer(reader);
        Parser parser(lexer);
        std::optional<Program> program = parser.tryParseProgram();

        BOOST_REQUIRE(program.has_value());
        BOOST_REQUIRE_EQUAL(program->statements.size(), 1);

        const auto& stmtPtr = program->statements[0];
        auto* varDecl = dynamic_cast<VariableDeclaration*>(stmtPtr.get());

        BOOST_REQUIRE(varDecl != nullptr);
        bool eq = varDecl->varName == L"y"; BOOST_TEST(eq);
        BOOST_TEST(varDecl->isConst == false);
        BOOST_REQUIRE(std::holds_alternative<std::unique_ptr<FunctionType>>(varDecl->varType));

        const auto& actualType = (std::get<std::unique_ptr<FunctionType>>(varDecl->varType));

        BOOST_REQUIRE_EQUAL(actualType->args.size(), 2);
        BOOST_TEST(std::get<TType>(actualType->args[0]) == TokenType::INTEGER);
        BOOST_TEST(std::get<TType>(actualType->args[1]) == TokenType::STRING);
        BOOST_TEST(std::get<TType>(actualType->ret_type) == TokenType::BOOLEAN);
    }

    {
        std::wstring code = L"praca () dająca ułamek f to jakas_funkcja.";

        StringReader reader;
        reader.openString(code);
        Lexer lexer(reader);
        Parser parser(lexer);
        std::optional<Program> program = parser.tryParseProgram();

        BOOST_REQUIRE(program.has_value());
        BOOST_REQUIRE_EQUAL(program->statements.size(), 1);

        const auto& stmtPtr = program->statements[0];
        auto* varDecl = dynamic_cast<VariableDeclaration*>(stmtPtr.get());

        BOOST_REQUIRE(varDecl != nullptr);
        bool eq = varDecl->varName == L"f"; BOOST_TEST(eq);
        BOOST_TEST(varDecl->isConst == false);
        BOOST_REQUIRE(std::holds_alternative<std::unique_ptr<FunctionType>>(varDecl->varType));

        const auto& actualType = std::get<std::unique_ptr<FunctionType>>(varDecl->varType);

        BOOST_REQUIRE_EQUAL(actualType->args.size(), 0);
        BOOST_TEST(std::get<TType>(actualType->ret_type) == TokenType::DOUBLE);
    }

    {
        std::wstring code = L"stały praca (fakt) dająca wyraz w to funkcja_q.";

        StringReader reader;
        reader.openString(code);
        Lexer lexer(reader);
        Parser parser(lexer);
        std::optional<Program> program = parser.tryParseProgram();

        BOOST_REQUIRE(program.has_value());
        BOOST_REQUIRE_EQUAL(program->statements.size(), 1);

        const auto& stmtPtr = program->statements[0];
        auto* varDecl = dynamic_cast<VariableDeclaration*>(stmtPtr.get());

        BOOST_REQUIRE(varDecl != nullptr);
        bool eq = varDecl->varName == L"w"; BOOST_TEST(eq);
        BOOST_TEST(varDecl->isConst == true);
        BOOST_REQUIRE(std::holds_alternative<std::unique_ptr<FunctionType>>(varDecl->varType));

        const auto& actualType = std::get<std::unique_ptr<FunctionType>>(varDecl->varType);

        BOOST_REQUIRE_EQUAL(actualType->args.size(), 1);
        BOOST_TEST(std::get<TType>(actualType->args[0]) == TokenType::BOOLEAN);
        BOOST_TEST(std::get<TType>(actualType->ret_type) == TokenType::STRING);
    }
}

BOOST_AUTO_TEST_CASE(function_data_type_as_return_value_and_argument)
{
    {
        std::wstring code = L"stały praca (praca (numer i wyraz) dająca nic i praca (numer i wyraz) dająca nic) dająca nic x to nic.";

        StringReader reader;
        reader.openString(code);
        Lexer lexer(reader);
        Parser parser(lexer);
        std::optional<Program> program = parser.tryParseProgram();

        BOOST_REQUIRE(program.has_value());
        BOOST_REQUIRE_EQUAL(program->statements.size(), 1);

        const auto& stmtPtr = program->statements[0];
        auto* varDecl = dynamic_cast<VariableDeclaration*>(stmtPtr.get());

        BOOST_REQUIRE(varDecl != nullptr);
        bool eq = varDecl->varName == L"x"; BOOST_TEST(eq);
        BOOST_TEST(varDecl->isConst == true);
        BOOST_REQUIRE(std::holds_alternative<std::unique_ptr<FunctionType>>(varDecl->varType));

        const auto& outerFunc = std::get<std::unique_ptr<FunctionType>>(varDecl->varType);
        BOOST_REQUIRE_EQUAL(outerFunc->args.size(), 2);
        BOOST_TEST(std::get<TType>(outerFunc->ret_type) == TokenType::EMPTY_VALUE);

        for (const auto& arg : outerFunc->args)
        {
            const auto& innerFunc = std::get<std::unique_ptr<FunctionType>>(arg);
            BOOST_REQUIRE_EQUAL(innerFunc->args.size(), 2);
            BOOST_TEST(std::get<TType>(innerFunc->args[0]) == TokenType::INTEGER);
            BOOST_TEST(std::get<TType>(innerFunc->args[1]) == TokenType::STRING);
            BOOST_TEST(std::get<TType>(innerFunc->ret_type) == TokenType::EMPTY_VALUE);
        }
    }

    {
        std::wstring code = L"stały praca (numer i wyraz) dająca praca (numer i wyraz) dająca nic x to nic.";

        StringReader reader;
        reader.openString(code);
        Lexer lexer(reader);
        Parser parser(lexer);
        std::optional<Program> program = parser.tryParseProgram();

        BOOST_REQUIRE(program.has_value());
        BOOST_REQUIRE_EQUAL(program->statements.size(), 1);

        const auto& stmtPtr = program->statements[0];
        auto* varDecl = dynamic_cast<VariableDeclaration*>(stmtPtr.get());

        BOOST_REQUIRE(varDecl != nullptr);
        bool eq = varDecl->varName == L"x"; BOOST_TEST(eq);
        BOOST_TEST(varDecl->isConst == true);
        BOOST_REQUIRE(std::holds_alternative<std::unique_ptr<FunctionType>>(varDecl->varType));

        const auto& outerFunc = std::get<std::unique_ptr<FunctionType>>(varDecl->varType);
        BOOST_REQUIRE_EQUAL(outerFunc->args.size(), 2);
        BOOST_TEST(std::get<TType>(outerFunc->args[0]) == TokenType::INTEGER);
        BOOST_TEST(std::get<TType>(outerFunc->args[1]) == TokenType::STRING);

        const auto& innerFunc = std::get<std::unique_ptr<FunctionType>>(outerFunc->ret_type);
        BOOST_REQUIRE_EQUAL(innerFunc->args.size(), 2);
        BOOST_TEST(std::get<TType>(innerFunc->args[0]) == TokenType::INTEGER);
        BOOST_TEST(std::get<TType>(innerFunc->args[1]) == TokenType::STRING);
        BOOST_TEST(std::get<TType>(innerFunc->ret_type) == TokenType::EMPTY_VALUE);
    }
}

BOOST_AUTO_TEST_CASE(invalid_function_variable_type)
{
    std::wstring invalidCodes[] = {
        L"stały praca (praca dająca nic i praca (numer i wyraz) dająca nic) dająca nic x to nic.",
        L"stały stały praca (praca (numer i wyraz) dająca nic i praca (numer i wyraz) dająca nic) dająca nic x to nic.",
        L"stały praca (praca (numer i wyraz) dająca nic i praca (numer i wyraz) dająca nic) dająca x to nic.",
        L"stały praca (praca (numer i wyraz) dająca nic i praca (numer wyraz) dająca nic) dająca nic x to nic.",
        L"stały praca ((numer i wyraz) dająca nic i praca (numer i wyraz) dająca nic) dająca nic x to nic."
    };

    for (const auto& code : invalidCodes)
    {
        StringReader reader;
        reader.openString(code);
        Lexer lexer(reader);
        Parser parser(lexer);
        BOOST_CHECK_THROW(parser.tryParseProgram(), std::runtime_error);
    }
}

BOOST_AUTO_TEST_CASE(invalid_empty_variable_declaration)
{
    std::wstring invalidCodes[] = {
        L"x to nic.",
        L"numer to nic.",
        L"numer x nic.",
        L"numer x to .",
        L"numer x to nic"
    };

    for (const auto& code : invalidCodes)
    {
        StringReader reader;
        reader.openString(code);
        Lexer lexer(reader);
        Parser parser(lexer);
        BOOST_CHECK_THROW(parser.tryParseProgram(), std::runtime_error);
    }
}

BOOST_AUTO_TEST_CASE(variable_declaration_statement)
{
    std::wstring code = L"numer a to skrócona_o (1 i 2) skrócona_o (1) funkcja()()."
                        L"wyraz b to \"54\"."
                        L"ułamek c to 12,23."
                        L"fakt d to prawda. fakt e to fałsz.";

    StringReader reader;
    reader.openString(code);
    Lexer lexer(reader);
    Parser parser(lexer);
    std::optional<Program> program = parser.tryParseProgram();

    BOOST_REQUIRE(program.has_value());
    BOOST_REQUIRE_EQUAL(program->statements.size(), 5);
}

BOOST_AUTO_TEST_CASE(expression_statement)
{
    std::wstring code = L"abc."
                        L"func_call(1 i 2)."
                        L"ident razy ident."
                        L"zmiennato(3).a.b.c.";

    StringReader reader;
    reader.openString(code);
    Lexer lexer(reader);
    Parser parser(lexer);
    std::optional<Program> program = parser.tryParseProgram();

    BOOST_REQUIRE(program.has_value());
    BOOST_REQUIRE_EQUAL(program->statements.size(), 7);
}

BOOST_AUTO_TEST_CASE(var_assign_statement)
{
    std::wstring code = L"abc to 32."
                        L"func_call to 2."
                        L"ident to 1 na wyraz."
                        L"zmiennato to 3.a to 2.b to 1.c to 0.";
 
    StringReader reader;
    reader.openString(code);
    Lexer lexer(reader);
    Parser parser(lexer);
    std::optional<Program> program = parser.tryParseProgram();

    BOOST_REQUIRE(program.has_value());
    BOOST_REQUIRE_EQUAL(program->statements.size(), 7);
}

BOOST_AUTO_TEST_CASE(invalid_var_assign_statement)
{
    std::wstring code = L"abc to 32."
                        L"func_call to 2."
                        L"ident to 1 na wyraz."
                        L"zmiennato to 3.a to 2.b to 1.c to 0.";
 
    StringReader reader;
    reader.openString(code);
    Lexer lexer(reader);
    Parser parser(lexer);
    std::optional<Program> program = parser.tryParseProgram();

    BOOST_REQUIRE(program.has_value());
    BOOST_REQUIRE_EQUAL(program->statements.size(), 7);
}

BOOST_AUTO_TEST_CASE(invalid_variable_assignment)
{
    std::wstring invalidCodes[] = {
        L"x to nic.",
        L"x razy 2 to 3.",
        L"x to to 3.",
        L"x to.",
        L"funckcja() to 3"
    };

    for (const auto& code : invalidCodes)
    {
        StringReader reader;
        reader.openString(code);
        Lexer lexer(reader);
        Parser parser(lexer);
        BOOST_CHECK_THROW(parser.tryParseProgram(), std::runtime_error);
    }
}

BOOST_AUTO_TEST_CASE(nested_scope_statement_structure)
{
    std::wstring code = L"od\n"
                        L"    numer x to 1.\n"
                        L"    od \n"
                        L"        numer x to 1.\n"
                        L"        numer y to 1.\n"
                        L"        od \n"
                        L"            numer x to 1.\n"
                        L"            numer y to 1.\n"
                        L"            numer z to 1.\n"
                        L"        do \n"
                        L"    do\n"
                        L"od\n"
                        L"do\n"
                        L"numer y to 1.\n"
                        L"do";

    StringReader reader;
    reader.openString(code);
    Lexer lexer(reader);
    Parser parser(lexer);
    std::optional<Program> program = parser.tryParseProgram();

    BOOST_REQUIRE(program.has_value());
    BOOST_REQUIRE_EQUAL(program->statements.size(), 1);

    auto* outerScope = dynamic_cast<ScopeStatement*>(program->statements[0].get());
    BOOST_REQUIRE(outerScope != nullptr);
    BOOST_REQUIRE_EQUAL(outerScope->statements.size(), 4);

    auto* midScope = dynamic_cast<ScopeStatement*>(outerScope->statements[1].get());
    BOOST_REQUIRE(midScope != nullptr);
    BOOST_REQUIRE_EQUAL(midScope->statements.size(), 3);

    auto* innerScope = dynamic_cast<ScopeStatement*>(midScope->statements[2].get());
    BOOST_REQUIRE(innerScope != nullptr);
    BOOST_REQUIRE_EQUAL(innerScope->statements.size(), 3);
}

BOOST_AUTO_TEST_CASE(invalid_nested_scope_statement_structure)
{
    std::wstring code = L"od\n"
                        L"    numer x to 1.\n"
                        L"    od \n"
                        L"        numer x to 1.\n"
                        L"        numer y to 1.\n"
                        L"        od \n"
                        L"            numer x to 1.\n"
                        L"            numer y to 1.\n"
                        L"            numer z to 1.\n"
                        L"        do \n"
                        L"    do\n"
                        L"od\n"
                        L"numer y to 1.\n"
                        L"do";

    StringReader reader;
    reader.openString(code);
    Lexer lexer(reader);
    Parser parser(lexer);
    BOOST_CHECK_THROW(parser.tryParseProgram(), std::runtime_error);
}



BOOST_AUTO_TEST_CASE(nested_loop_statement_structure)
{
    std::wstring code = L"powtórz 5 krotnie od \n"
                        L"    wypisz(\"hej!\")."
                        L"    powtórz 5 razy 2 plus 3 przez skrócona_o (2 i 3) daj_liczbe(2 i 3) krotnie \n"
                        L"        wypisz(\"wow to działa \" złącz \"!\"). \n"
                        L"do";

    StringReader reader;
    reader.openString(code);
    Lexer lexer(reader);
    Parser parser(lexer);
    std::optional<Program> program = parser.tryParseProgram();

    BOOST_REQUIRE(program.has_value());
    BOOST_REQUIRE_EQUAL(program->statements.size(), 1);

    auto* outerLoop = dynamic_cast<LoopStatement*>(program->statements[0].get());
    BOOST_REQUIRE(outerLoop != nullptr);

    auto* outerScope = dynamic_cast<ScopeStatement*>(outerLoop->statement.get());
    BOOST_REQUIRE(outerScope != nullptr);
    BOOST_REQUIRE_EQUAL(outerScope->statements.size(), 2);

    auto* innerLoop = dynamic_cast<LoopStatement*>(outerScope->statements[1].get());
    BOOST_REQUIRE(innerLoop != nullptr);
    
    auto* innerLoopStatement = dynamic_cast<ExpressionStatement*>(innerLoop->statement.get());
    BOOST_REQUIRE(innerLoopStatement != nullptr);
}

BOOST_AUTO_TEST_CASE(invalid_loop_statements)
{
    std::wstring invalidCodes[] = {
        // no 'krotnie'
        L"powtórz 5 od \n"
                        L"    wypisz(\"hej!\")."
                        L"    powtórz 5 razy 2 plus 3 przez skrócona_o (2 i 3) daj_liczbe(2 i 3) krotnie \n"
                        L"        wypisz(\"wow to działa \" złącz \"!\"). \n"
                        L"do",
        // no expression
        L"powtórz krotnie od \n"
                        L"    wypisz(\"hej!\")."
                        L"    powtórz 5 razy 2 plus 3 przez skrócona_o (2 i 3) daj_liczbe(2 i 3) krotnie \n"
                        L"        wypisz(\"wow to działa \" złącz \"!\"). \n"
                        L"do",
        // od od
        L"powtórz 5 krotnie do \n"
                        L"    wypisz(\"hej!\")."
                        L"    powtórz 2 razy 2 plus 3 przez skrócona_o (2 i 3) daj_liczbe(2 i 3) krotnie \n"
                        L"        wypisz(\"wow to działa \" złącz \"!\"). \n"
                        L"do"
    };

    for (const auto& code : invalidCodes)
    {
        StringReader reader;
        reader.openString(code);
        Lexer lexer(reader);
        Parser parser(lexer);
        BOOST_CHECK_THROW(parser.tryParseProgram(), std::runtime_error);
    }
}

BOOST_AUTO_TEST_CASE(if_statement_structure)
{
    std::wstring code = 
        L"jeżeli prawda wtedy od do\n"

        L"jeżeli prawda wtedy od do\n"
        L"inaczej_gdy prawda wtedy od do\n"

        L"jeżeli prawda wtedy od do\n"
        L"inaczej_gdy prawda wtedy od do\n"
        L"inaczej_gdy prawda wtedy od do\n"
        L"inaczej_gdy prawda wtedy od do\n"

        L"jeżeli prawda wtedy od do\n"
        L"inaczej_gdy prawda wtedy od do\n"
        L"ostatecznie od do\n"

        L"jeżeli prawda wtedy od do\n"
        L"inaczej_gdy prawda wtedy od do\n"
        L"inaczej_gdy prawda wtedy od do\n"
        L"inaczej_gdy prawda wtedy od do\n"
        L"ostatecznie od do\n"
        
        L"jeżeli prawda wtedy od do\n"
        L"ostatecznie od do";


    StringReader reader;
    reader.openString(code);
    Lexer lexer(reader);
    Parser parser(lexer);

    std::optional<Program> program = parser.tryParseProgram();
    BOOST_REQUIRE(program.has_value());
    BOOST_REQUIRE_EQUAL(program->statements.size(), 6);

    {
        auto* stmt = dynamic_cast<IfStatement*>(program->statements[0].get());
        BOOST_REQUIRE(stmt);
        BOOST_CHECK_EQUAL(stmt->elseIfBranches.size(), 0);
        BOOST_CHECK(!stmt->elseBranch);
    }

    {
        auto* stmt = dynamic_cast<IfStatement*>(program->statements[1].get());
        BOOST_REQUIRE(stmt);
        BOOST_CHECK_EQUAL(stmt->elseIfBranches.size(), 1);
        BOOST_CHECK(!stmt->elseBranch);
    }

    {
        auto* stmt = dynamic_cast<IfStatement*>(program->statements[2].get());
        BOOST_REQUIRE(stmt);
        BOOST_CHECK_EQUAL(stmt->elseIfBranches.size(), 3);
        BOOST_CHECK(!stmt->elseBranch);
    }

    {
        auto* stmt = dynamic_cast<IfStatement*>(program->statements[3].get());
        BOOST_REQUIRE(stmt);
        BOOST_CHECK_EQUAL(stmt->elseIfBranches.size(), 1);
        BOOST_REQUIRE(stmt->elseBranch);
    }

    {
        auto* stmt = dynamic_cast<IfStatement*>(program->statements[4].get());
        BOOST_REQUIRE(stmt);
        BOOST_CHECK_EQUAL(stmt->elseIfBranches.size(), 3);
        BOOST_REQUIRE(stmt->elseBranch);
    }

    {
        auto* stmt = dynamic_cast<IfStatement*>(program->statements[5].get());
        BOOST_REQUIRE(stmt);
        BOOST_CHECK_EQUAL(stmt->elseIfBranches.size(), 0);
        BOOST_REQUIRE(stmt->elseBranch);
    }
}


BOOST_AUTO_TEST_CASE(invalid_if_statement_structure)
{
    std::wstring invalidCodes[] = {
        // no expression
        L"jeżeli wtedy od do inaczej_gdy prawda od do ostatecznie od do",
        // no 'wtedy
        L"jeżeli prawda od do inaczej_gdy prawda od do ostatecznie od do",
        // no statement
        L"jeżeli prawda wtedy inaczej_gdy prawda od do ostatecznie od do",
        // no inaczej_gdy
        L"jeżeli prawda wtedy od do prawda od do ostatecznie od do",
        // no second expression
        L"jeżeli prawda wtedy od do inaczej_gdy od do ostatecznie od do",
        // no second statement 
        L"jeżeli prawda wtedy od do inaczej_gdy prawda ostatecznie od do",
        // no third statement
        L"jeżeli prawda wtedy od do inaczej_gdy prawda od do ostatecznie",
    };

    for (const auto& code : invalidCodes)
    {
        StringReader reader;
        reader.openString(code);
        Lexer lexer(reader);
        Parser parser(lexer);
        BOOST_CHECK_THROW(parser.tryParseProgram(), std::runtime_error);
    }
}

BOOST_AUTO_TEST_CASE(function_declaration_variants)
{
    std::wstring code =
        L"praca nowa_praca(numer x i wyraz y) daje numer robi od do\n"
        L"praca nowa_praca() daje ułamek robi od do\n"
        L"praca nowa_praca(numer x) daje wyraz robi od do\n"
        L"praca nowa_praca() daje nic robi od do";

    StringReader reader;
    reader.openString(code);
    Lexer lexer(reader);
    Parser parser(lexer);
    std::optional<Program> program = parser.tryParseProgram();

    BOOST_REQUIRE(program.has_value());
    BOOST_REQUIRE_EQUAL(program->statements.size(), 4);

    auto* func1 = dynamic_cast<FunctionDeclaration*>(program->statements[0].get());
    BOOST_REQUIRE(func1);
    bool eq = func1->name == L"nowa_praca";  BOOST_TEST(eq);

    BOOST_CHECK_EQUAL(func1->parameters.size(), 2);
    BOOST_CHECK(std::get<TType>(func1->ret_type) == TokenType::INTEGER);

    auto* func2 = dynamic_cast<FunctionDeclaration*>(program->statements[1].get());
    BOOST_REQUIRE(func2);
    BOOST_CHECK_EQUAL(func2->parameters.size(), 0);
    BOOST_CHECK(std::get<TType>(func2->ret_type) == TokenType::DOUBLE);

    auto* func3 = dynamic_cast<FunctionDeclaration*>(program->statements[2].get());
    BOOST_REQUIRE(func3);
    BOOST_CHECK_EQUAL(func3->parameters.size(), 1);
    BOOST_CHECK(std::get<TType>(func3->ret_type) == TokenType::STRING);

    auto* func4 = dynamic_cast<FunctionDeclaration*>(program->statements[3].get());
    BOOST_REQUIRE(func4);
    BOOST_CHECK_EQUAL(func4->parameters.size(), 0);
    BOOST_CHECK(std::get<TType>(func4->ret_type) == TokenType::EMPTY_VALUE);
}

BOOST_AUTO_TEST_CASE(invalid_function_declaration_variants)
{
    std::wstring invalidCodes[] = {
        // no name
        L"praca (numer x i wyraz y) daje numer robi od do\n",
        // no args
        L"praca nowa_praca daje numer robi od do\n",
        // no 'daje'
        L"praca nowa_praca(numer x i wyraz y) numer robi od do\n",
        // no rettype
        L"praca nowa_praca(numer x i wyraz y) daje robi od do\n",
        // no 'robi'
        L"praca nowa_praca(numer x i wyraz y) daje numer od do\n",
        //no statement
        L"praca nowa_praca(numer x i wyraz y) daje numer robi\n",
        //no closing args
        L"praca nowa_praca(numer x i wyraz y daje numer robi od do\n"
        //no arg type
        L"praca nowa_praca(x) daje numer robi od do\n"
        //no arg name
        L"praca nowa_praca(numer) daje numer robi od do\n"
    };

    for (const auto& code : invalidCodes)
    {
        StringReader reader;
        reader.openString(code);
        Lexer lexer(reader);
        Parser parser(lexer);
        BOOST_CHECK_THROW(parser.tryParseProgram(), std::runtime_error);
    }
}

BOOST_AUTO_TEST_CASE(return_statement_parsing)
{
    std::wstring code =
        L"zwróć 3.\n"
        L"zwróć nic.";

    StringReader reader;
    reader.openString(code);
    Lexer lexer(reader);
    Parser parser(lexer);
    std::optional<Program> program = parser.tryParseProgram();

    BOOST_REQUIRE(program.has_value());
    BOOST_REQUIRE_EQUAL(program->statements.size(), 2);

    auto* ret1 = dynamic_cast<ReturnStatement*>(program->statements[0].get());
    BOOST_REQUIRE(ret1);
    BOOST_REQUIRE(ret1->expr); // Should not be null for "zwróć 3."

    auto* ret2 = dynamic_cast<ReturnStatement*>(program->statements[1].get());
    BOOST_REQUIRE(ret2);
    BOOST_CHECK(ret2->expr == nullptr); // Should be null for "zwróć nic."
}

BOOST_AUTO_TEST_CASE(invalid_return_statement_parsing)
{
    std::wstring invalidCodes[] = {
        L"zwróć.\n",
        L"zwróć zwróć.\n",
        L"zwróć numer.\n"
    };

    for (const auto& code : invalidCodes)
    {
        StringReader reader;
        reader.openString(code);
        Lexer lexer(reader);
        Parser parser(lexer);
        BOOST_CHECK_THROW(parser.tryParseProgram(), std::runtime_error);
    }
}

////////////////////////////////////////////////////////////////////////////////////////////////////////
////                                E X P R E S S I O N S                                           ////
////////////////////////////////////////////////////////////////////////////////////////////////////////

BOOST_AUTO_TEST_CASE(literal_expression)
{
    using CodeData = std::tuple<std::wstring, LiteralExpression>;
    CodeData codes[] = {
        {L"numer x to 1234.",       LiteralExpression(1234)},
        {L"numer x to 12,34.",      LiteralExpression(12.34)},
        {L"numer x to prawda.",     LiteralExpression(true)},
        {L"numer x to fałsz.",      LiteralExpression(false)},
        {L"wyraz x to \"hej\".",    LiteralExpression(std::wstring(L"hej"))},
    };

    for (const auto& [code, expectedExpression] : codes)
    {
        StringReader reader;
        reader.openString(code);
        Lexer lexer(reader);
        Parser parser(lexer);
        std::optional<Program> program = parser.tryParseProgram();

        BOOST_REQUIRE(program.has_value());
        BOOST_REQUIRE_EQUAL(program->statements.size(), 1);

        const auto& stmtPtr = program->statements[0];
        auto* varDecl = dynamic_cast<VariableDeclaration*>(stmtPtr.get());
        const auto& expr = varDecl->value;
        auto* litrExpr = dynamic_cast<LiteralExpression*>(expr.get());

        BOOST_REQUIRE(litrExpr != nullptr);

        bool eq = litrExpr->literal == expectedExpression.literal;  BOOST_TEST(eq);
    }
}

BOOST_AUTO_TEST_CASE(identifier_expression)
{
    std::wstring code = L"numer x to x.";
    StringReader reader;
    reader.openString(code);
    Lexer lexer(reader);
    Parser parser(lexer);
    std::optional<Program> program = parser.tryParseProgram();

    BOOST_REQUIRE(program.has_value());
    BOOST_REQUIRE_EQUAL(program->statements.size(), 1);

    const auto& stmtPtr = program->statements[0];
    auto* varDecl = dynamic_cast<VariableDeclaration*>(stmtPtr.get());
    const auto& expr = varDecl->value;
    auto* identExpr = dynamic_cast<IdentifierExpression*>(expr.get());

    BOOST_REQUIRE(identExpr != nullptr);

    bool eq = identExpr->identifier == std::wstring(L"x");  BOOST_TEST(eq);
}

BOOST_AUTO_TEST_CASE(negation_expression)
{
    using CodeData = std::tuple<std::wstring, NegationExpression>;
    CodeData codes[] = {
        {L"numer x to nie 1234.",       NegationExpression(std::make_unique<LiteralExpression>(1234))},
        {L"numer x to nie 12,34.",      NegationExpression(std::make_unique<LiteralExpression>(12.34))},
        {L"numer x to nie prawda.",     NegationExpression(std::make_unique<LiteralExpression>(true))},
        {L"numer x to nie fałsz.",      NegationExpression(std::make_unique<LiteralExpression>(false))},
        {L"wyraz x to nie \"hej\".",    NegationExpression(std::make_unique<LiteralExpression>(std::wstring(L"hej")))},
    };    

    for (const auto& [code, expectedExpression] : codes)
    {
        StringReader reader;
        reader.openString(code);
        Lexer lexer(reader);
        Parser parser(lexer);
        std::optional<Program> program = parser.tryParseProgram();
    
        BOOST_REQUIRE(program.has_value());
        BOOST_REQUIRE_EQUAL(program->statements.size(), 1);
    
        const auto& stmtPtr = program->statements[0];
        auto* varDecl = dynamic_cast<VariableDeclaration*>(stmtPtr.get());
        const auto& expr = varDecl->value;
        auto* negatExpr = dynamic_cast<NegationExpression*>(expr.get());
    
        BOOST_REQUIRE(negatExpr != nullptr);
    
        auto* innerLiteral = dynamic_cast<LiteralExpression*>(negatExpr->expr.get());
        BOOST_REQUIRE(innerLiteral != nullptr);
    
        const auto& expectedInner = dynamic_cast<LiteralExpression*>(expectedExpression.expr.get());
        BOOST_REQUIRE(expectedInner != nullptr);
    
        bool eq = innerLiteral->literal == expectedInner->literal;
        BOOST_TEST(eq);
    }
}

BOOST_AUTO_TEST_CASE(recursive_negative_expression)
{
    std::wstring code = L"numer x to nie nie nie 1234.";

    StringReader reader;
    reader.openString(code);
    Lexer lexer(reader);
    Parser parser(lexer);
    std::optional<Program> program = parser.tryParseProgram();

    BOOST_REQUIRE(program.has_value());
    BOOST_REQUIRE_EQUAL(program->statements.size(), 1);

    const auto& stmtPtr = program->statements[0];
    auto* varDecl = dynamic_cast<VariableDeclaration*>(stmtPtr.get());
    BOOST_REQUIRE(varDecl != nullptr);

    const auto& expr = varDecl->value;
    const NegationExpression* neg = dynamic_cast<NegationExpression*>(expr.get());
    BOOST_REQUIRE(neg != nullptr);

    for (int i = 0; i < 2; ++i) {
        neg = dynamic_cast<NegationExpression*>(neg->expr.get());
        BOOST_REQUIRE(neg != nullptr);
    }

    auto* literal = dynamic_cast<LiteralExpression*>(neg->expr.get());
    BOOST_REQUIRE(literal != nullptr);
}

BOOST_AUTO_TEST_CASE(cast_expression)
{
    using CodeData = std::tuple<std::wstring, CastExpression>;
    CodeData negative_codes[] = {
        {L"numer x to nie 1234 na wyraz.",          CastExpression(std::make_unique<NegationExpression>(std::make_unique<LiteralExpression>(1234)), TokenType::STRING)},
        {L"stały numer x to nie prawda na fakt.",   CastExpression(std::make_unique<NegationExpression>(std::make_unique<LiteralExpression>(true)), TokenType::BOOLEAN)},
        {L"numer x to nie fałsz na ułamek.",        CastExpression(std::make_unique<NegationExpression>(std::make_unique<LiteralExpression>(false)), TokenType::DOUBLE)},
        {L"stały numer x to nie 12,34 na numer.",   CastExpression(std::make_unique<NegationExpression>(std::make_unique<LiteralExpression>(12.34)), TokenType::INTEGER)},
    };

    CodeData literal_codes[] = {
        {L"numer x to 1234 na wyraz.",          CastExpression(std::make_unique<LiteralExpression>(1234), TokenType::STRING)},
        {L"stały numer x to prawda na fakt.",   CastExpression(std::make_unique<LiteralExpression>(true), TokenType::BOOLEAN)},
        {L"numer x to \"hej\" na ułamek.",      CastExpression(std::make_unique<LiteralExpression>(std::wstring(L"hej")), TokenType::DOUBLE)},
        {L"stały numer x to 12,34 na numer.",   CastExpression(std::make_unique<LiteralExpression>(12.34), TokenType::INTEGER)},
    };

    for (const auto& [code, expectedExpression] : negative_codes)
    {
        StringReader reader;
        reader.openString(code);
        Lexer lexer(reader);
        Parser parser(lexer);
        std::optional<Program> program = parser.tryParseProgram();

        BOOST_REQUIRE(program.has_value());
        BOOST_REQUIRE_EQUAL(program->statements.size(), 1);

        const auto& stmtPtr = program->statements[0];
        auto* varDecl = dynamic_cast<VariableDeclaration*>(stmtPtr.get());
        const auto& expr = varDecl->value;
        auto* castExpr = dynamic_cast<CastExpression*>(expr.get());

        BOOST_REQUIRE(castExpr != nullptr);
        BOOST_TEST(castExpr->targetType == expectedExpression.targetType);

        auto* negExpr = dynamic_cast<NegationExpression*>(castExpr->expr.get());
        BOOST_REQUIRE(negExpr != nullptr);

        auto* innerLiteral = dynamic_cast<LiteralExpression*>(negExpr->expr.get());
        const auto* expectedInner = dynamic_cast<LiteralExpression*>(
            dynamic_cast<NegationExpression*>(expectedExpression.expr.get())->expr.get());

        BOOST_REQUIRE(innerLiteral != nullptr);
        BOOST_REQUIRE(expectedInner != nullptr);

        bool eq = innerLiteral->literal == expectedInner->literal;
        BOOST_TEST(eq);
    }

    for (const auto& [code, expectedExpression] : literal_codes)
    {
        StringReader reader;
        reader.openString(code);
        Lexer lexer(reader);
        Parser parser(lexer);
        std::optional<Program> program = parser.tryParseProgram();

        BOOST_REQUIRE(program.has_value());
        BOOST_REQUIRE_EQUAL(program->statements.size(), 1);

        const auto& stmtPtr = program->statements[0];
        auto* varDecl = dynamic_cast<VariableDeclaration*>(stmtPtr.get());
        const auto& expr = varDecl->value;
        auto* castExpr = dynamic_cast<CastExpression*>(expr.get());

        BOOST_REQUIRE(castExpr != nullptr);
        BOOST_TEST(castExpr->targetType == expectedExpression.targetType);

        auto* literalExpr = dynamic_cast<LiteralExpression*>(castExpr->expr.get());
        const auto* expectedInner = dynamic_cast<LiteralExpression*>(expectedExpression.expr.get());

        BOOST_REQUIRE(literalExpr != nullptr);
        BOOST_REQUIRE(expectedInner != nullptr);

        bool eq = literalExpr->literal == expectedInner->literal;
        BOOST_TEST(eq);
    }
}

BOOST_AUTO_TEST_CASE(recursive_cast_expression)
{
    std::wstring code = L"numer x to nie nie nie 1234 na wyraz na numer na wyraz na ułamek.";

    StringReader reader;
    reader.openString(code);
    Lexer lexer(reader);
    Parser parser(lexer);

    std::optional<Program> program = parser.tryParseProgram();
    
    BOOST_REQUIRE(program.has_value());
    BOOST_REQUIRE_EQUAL(program->statements.size(), 1);

    const auto& stmtPtr = program->statements[0];
    auto* varDecl = dynamic_cast<VariableDeclaration*>(stmtPtr.get());
    const auto& expr = varDecl->value;

    auto* castExpr1 = dynamic_cast<CastExpression*>(expr.get());
    BOOST_REQUIRE(castExpr1 != nullptr);

    auto* castExpr2 = dynamic_cast<CastExpression*>(castExpr1->expr.get());
    BOOST_REQUIRE(castExpr2 != nullptr);

    auto* castExpr3 = dynamic_cast<CastExpression*>(castExpr2->expr.get());
    BOOST_REQUIRE(castExpr3 != nullptr);

    auto* castExpr4 = dynamic_cast<CastExpression*>(castExpr3->expr.get());
    BOOST_REQUIRE(castExpr4 != nullptr);

    auto* negExpr1 = dynamic_cast<NegationExpression*>(castExpr4->expr.get());
    BOOST_REQUIRE(negExpr1 != nullptr);

    auto* negExpr2 = dynamic_cast<NegationExpression*>(negExpr1->expr.get());
    BOOST_REQUIRE(negExpr2 != nullptr);

    auto* negExpr3 = dynamic_cast<NegationExpression*>(negExpr2->expr.get());
    BOOST_REQUIRE(negExpr3 != nullptr);

    auto* litExpr = dynamic_cast<LiteralExpression*>(negExpr3->expr.get());
    BOOST_REQUIRE(litExpr != nullptr);
    
    auto expoectedValue = std::make_unique<LiteralExpression>(1234);
    bool eq = litExpr->literal == expoectedValue->literal;
    BOOST_TEST(eq);
}

BOOST_AUTO_TEST_CASE(multi_expression)
{
    std::wstring code = L"numer x to 5 razy 3.";
    StringReader reader;
    reader.openString(code);
    Lexer lexer(reader);
    Parser parser(lexer);

    std::optional<Program> program = parser.tryParseProgram();
    BOOST_REQUIRE(program.has_value());
    BOOST_REQUIRE_EQUAL(program->statements.size(), 1);

    const auto& stmtPtr = program->statements[0];
    auto* varDecl = dynamic_cast<VariableDeclaration*>(stmtPtr.get());
    const auto& expr = varDecl->value;

    auto* binExpr = dynamic_cast<BinaryExpression*>(expr.get());
    BOOST_REQUIRE(binExpr != nullptr);

    auto* leftLit = dynamic_cast<LiteralExpression*>(binExpr->lex.get());
    BOOST_REQUIRE(leftLit != nullptr);
    BOOST_REQUIRE_EQUAL(std::get<int>(leftLit->literal), 5);

    BOOST_REQUIRE(binExpr->op == TokenType::MULTI);

    auto* rightLit = dynamic_cast<LiteralExpression*>(binExpr->rex.get());
    BOOST_REQUIRE(rightLit != nullptr);
    BOOST_REQUIRE_EQUAL(std::get<int>(rightLit->literal), 3);

    std::wstring code2 = L"numer x to nie prawda przez \"3\" na wyraz.";
    StringReader reader2;
    reader2.openString(code2);
    Lexer lexer2(reader2);
    Parser parser2(lexer2);
    std::optional<Program> program2 = parser2.tryParseProgram();

    BOOST_REQUIRE(program2.has_value());
    BOOST_REQUIRE_EQUAL(program2->statements.size(), 1);

    const auto& stmtPtr2 = program2->statements[0];
    auto* varDecl2 = dynamic_cast<VariableDeclaration*>(stmtPtr2.get());
    const auto& expr2 = varDecl2->value;

    auto* binExpr2 = dynamic_cast<BinaryExpression*>(expr2.get());
    BOOST_REQUIRE(binExpr2 != nullptr);

    auto* negExpr = dynamic_cast<NegationExpression*>(binExpr2->lex.get());
    BOOST_REQUIRE(negExpr != nullptr);
    auto* litExpr = dynamic_cast<LiteralExpression*>(negExpr->expr.get());
    BOOST_REQUIRE(litExpr != nullptr);
    BOOST_REQUIRE_EQUAL(std::get<bool>(litExpr->literal), true);

    BOOST_REQUIRE(binExpr2->op == TokenType::DIVIDE);

    auto* castExpr = dynamic_cast<CastExpression*>(binExpr2->rex.get());
    BOOST_REQUIRE(castExpr != nullptr);
    BOOST_REQUIRE(castExpr->targetType == TokenType::STRING);
    auto* rightLitExpr = dynamic_cast<LiteralExpression*>(castExpr->expr.get());
    BOOST_REQUIRE(rightLitExpr != nullptr);

    auto expectedValue = std::make_unique<LiteralExpression>(std::wstring(L"3"));
    bool eq = rightLitExpr->literal == expectedValue->literal;
    BOOST_TEST(eq);
}

BOOST_AUTO_TEST_CASE(recursive_multi_expression)
{
    std::wstring code = L"numer x to 5 razy 3 razy 2 przez 3.";
    StringReader reader;
    reader.openString(code);
    Lexer lexer(reader);
    Parser parser(lexer);

    std::optional<Program> program = parser.tryParseProgram();
    BOOST_REQUIRE(program.has_value());
    BOOST_REQUIRE_EQUAL(program->statements.size(), 1);

    const auto& stmtPtr = program->statements[0];
    auto* varDecl = dynamic_cast<VariableDeclaration*>(stmtPtr.get());
    const auto& expr = varDecl->value;

    auto* binExpr1 = dynamic_cast<BinaryExpression*>(expr.get());
    BOOST_REQUIRE(binExpr1 != nullptr);

    auto* right1 = dynamic_cast<LiteralExpression*>(binExpr1->rex.get());
    BOOST_REQUIRE_EQUAL(std::get<int>(right1->literal), 3);
    BOOST_REQUIRE(binExpr1->op == TokenType::DIVIDE);

    auto* left1 = dynamic_cast<BinaryExpression*>(binExpr1->lex.get());
    BOOST_REQUIRE(left1 != nullptr);
    
    auto* right2 = dynamic_cast<LiteralExpression*>(left1->rex.get());
    BOOST_REQUIRE_EQUAL(std::get<int>(right2->literal), 2);
    BOOST_REQUIRE(left1->op == TokenType::MULTI);

    auto* left2 = dynamic_cast<BinaryExpression*>(binExpr1->lex.get());
    BOOST_REQUIRE(left2 != nullptr);
    
    auto* right3 = dynamic_cast<LiteralExpression*>(left2->rex.get());
    BOOST_REQUIRE_EQUAL(std::get<int>(right3->literal), 2);
    BOOST_REQUIRE(left2->op == TokenType::MULTI);

    auto* left3 = dynamic_cast<BinaryExpression*>(left2->lex.get());
    BOOST_REQUIRE(left3 != nullptr);
    
    auto* right4 = dynamic_cast<LiteralExpression*>(left3->rex.get());
    BOOST_REQUIRE_EQUAL(std::get<int>(right4->literal), 3);
    BOOST_REQUIRE(left3->op == TokenType::MULTI);

    auto* left4 = dynamic_cast<LiteralExpression*>(left3->lex.get());
    BOOST_REQUIRE_EQUAL(std::get<int>(left4->literal), 5);
}

BOOST_AUTO_TEST_CASE(addidtion_expression_outside)
{
    std::wstring code = L"numer x to 1 plus 2 razy 3 minus 4.";
    
    StringReader reader;
    reader.openString(code);
    Lexer lexer(reader);
    Parser parser(lexer);

    std::optional<Program> program = parser.tryParseProgram();
    BOOST_REQUIRE(program.has_value());
    BOOST_REQUIRE_EQUAL(program->statements.size(), 1);

    const auto& stmtPtr = program->statements[0];
    auto* varDecl = dynamic_cast<VariableDeclaration*>(stmtPtr.get());
    const auto& expr = varDecl->value;

    auto* binExpr1 = dynamic_cast<BinaryExpression*>(expr.get());
    BOOST_REQUIRE(binExpr1 != nullptr);
    BOOST_REQUIRE(binExpr1->op == TokenType::MINUS);

    auto* right1 = dynamic_cast<LiteralExpression*>(binExpr1->rex.get());
    BOOST_REQUIRE_EQUAL(std::get<int>(right1->literal), 4);

    auto* left1 = dynamic_cast<BinaryExpression*>(binExpr1->lex.get());
    BOOST_REQUIRE(left1 != nullptr);
    BOOST_REQUIRE(left1->op == TokenType::PLUS);
    
    auto* right2 = dynamic_cast<BinaryExpression*>(left1->rex.get());
    BOOST_REQUIRE(right2 != nullptr);
    BOOST_REQUIRE(right2->op == TokenType::MULTI);
    
    auto* right2_left1 = dynamic_cast<LiteralExpression*>(right2->lex.get());
    BOOST_REQUIRE(right2_left1 != nullptr);
    BOOST_REQUIRE_EQUAL(std::get<int>(right2_left1->literal), 2);

    auto* right2_right1 = dynamic_cast<LiteralExpression*>(right2->rex.get());
    BOOST_REQUIRE(right2_right1 != nullptr);
    BOOST_REQUIRE_EQUAL(std::get<int>(right2_right1->literal), 3);

    auto* left2 = dynamic_cast<LiteralExpression*>(left1->lex.get());
    BOOST_REQUIRE(left2 != nullptr);
    BOOST_REQUIRE_EQUAL(std::get<int>(left2->literal), 1);
}

BOOST_AUTO_TEST_CASE(addidtion_expression_inside)
{
    std::wstring code = L"numer x to 1 razy 2 plus 3 przez 4.";
    
    StringReader reader;
    reader.openString(code);
    Lexer lexer(reader);
    Parser parser(lexer);

    std::optional<Program> program = parser.tryParseProgram();
    BOOST_REQUIRE(program.has_value());
    BOOST_REQUIRE_EQUAL(program->statements.size(), 1);

    const auto& stmtPtr = program->statements[0];
    auto* varDecl = dynamic_cast<VariableDeclaration*>(stmtPtr.get());
    const auto& expr = varDecl->value;

    auto* binExpr1 = dynamic_cast<BinaryExpression*>(expr.get());
    BOOST_REQUIRE(binExpr1 != nullptr);
    BOOST_REQUIRE(binExpr1->op == TokenType::PLUS);

    auto* right1 = dynamic_cast<BinaryExpression*>(binExpr1->rex.get());
    BOOST_REQUIRE(right1 != nullptr);
    BOOST_REQUIRE(right1->op == TokenType::DIVIDE);

    auto* left1 = dynamic_cast<BinaryExpression*>(binExpr1->lex.get());
    BOOST_REQUIRE(left1 != nullptr);
    BOOST_REQUIRE(left1->op == TokenType::MULTI);
}

BOOST_AUTO_TEST_CASE(recursive_addition_expression)
{
    std::wstring code = L"numer x to 1 plus 2 minus 3 złącz 4.";
    StringReader reader;
    reader.openString(code);
    Lexer lexer(reader);
    Parser parser(lexer);

    std::optional<Program> program = parser.tryParseProgram();
    BOOST_REQUIRE(program.has_value());
    BOOST_REQUIRE_EQUAL(program->statements.size(), 1);

    const auto& stmtPtr = program->statements[0];
    auto* varDecl = dynamic_cast<VariableDeclaration*>(stmtPtr.get());
    const auto& expr = varDecl->value;

    auto* binExpr1 = dynamic_cast<BinaryExpression*>(expr.get());
    BOOST_REQUIRE(binExpr1 != nullptr);
    BOOST_REQUIRE(binExpr1->op == TokenType::CONNECT);

    auto* right1 = dynamic_cast<LiteralExpression*>(binExpr1->rex.get());
    BOOST_REQUIRE_EQUAL(std::get<int>(right1->literal), 4);

    auto* left1 = dynamic_cast<BinaryExpression*>(binExpr1->lex.get());
    BOOST_REQUIRE(left1 != nullptr);
    BOOST_REQUIRE(left1->op == TokenType::MINUS);
    
    auto* right2 = dynamic_cast<LiteralExpression*>(left1->rex.get());
    BOOST_REQUIRE_EQUAL(std::get<int>(right2->literal), 3);

    auto* left2 = dynamic_cast<BinaryExpression*>(left1->lex.get());
    BOOST_REQUIRE(left2 != nullptr);
    BOOST_REQUIRE(left2->op == TokenType::PLUS);
    
    auto* right3 = dynamic_cast<LiteralExpression*>(left2->rex.get());
    BOOST_REQUIRE_EQUAL(std::get<int>(right3->literal), 2);

    auto* left3 = dynamic_cast<LiteralExpression*>(left2->lex.get());
    BOOST_REQUIRE(left3 != nullptr);
    BOOST_REQUIRE_EQUAL(std::get<int>(left3->literal), 1);
}

BOOST_AUTO_TEST_CASE(relational_expression_outside)
{
    std::wstring code = L"numer x to 1 nierówne 2 plus 3 równe 4.";
    
    StringReader reader;
    reader.openString(code);
    Lexer lexer(reader);
    Parser parser(lexer);

    std::optional<Program> program = parser.tryParseProgram();
    BOOST_REQUIRE(program.has_value());
    BOOST_REQUIRE_EQUAL(program->statements.size(), 1);

    const auto& stmtPtr = program->statements[0];
    auto* varDecl = dynamic_cast<VariableDeclaration*>(stmtPtr.get());
    const auto& expr = varDecl->value;

    auto* binExpr1 = dynamic_cast<BinaryExpression*>(expr.get());
    BOOST_REQUIRE(binExpr1 != nullptr);
    BOOST_REQUIRE(binExpr1->op == TokenType::EQ);

    auto* right1 = dynamic_cast<LiteralExpression*>(binExpr1->rex.get());
    BOOST_REQUIRE_EQUAL(std::get<int>(right1->literal), 4);

    auto* left1 = dynamic_cast<BinaryExpression*>(binExpr1->lex.get());
    BOOST_REQUIRE(left1 != nullptr);
    BOOST_REQUIRE(left1->op == TokenType::NEQ);
    
    auto* right2 = dynamic_cast<BinaryExpression*>(left1->rex.get());
    BOOST_REQUIRE(right2 != nullptr);
    BOOST_REQUIRE(right2->op == TokenType::PLUS);
    
    auto* right2_left1 = dynamic_cast<LiteralExpression*>(right2->lex.get());
    BOOST_REQUIRE(right2_left1 != nullptr);
    BOOST_REQUIRE_EQUAL(std::get<int>(right2_left1->literal), 2);

    auto* right2_right1 = dynamic_cast<LiteralExpression*>(right2->rex.get());
    BOOST_REQUIRE(right2_right1 != nullptr);
    BOOST_REQUIRE_EQUAL(std::get<int>(right2_right1->literal), 3);

    auto* left2 = dynamic_cast<LiteralExpression*>(left1->lex.get());
    BOOST_REQUIRE(left2 != nullptr);
    BOOST_REQUIRE_EQUAL(std::get<int>(left2->literal), 1);
}

BOOST_AUTO_TEST_CASE(relational_expression_inside)
{
    std::wstring code = L"numer x to 1 razy 2 równe 3 przez 4.";
    
    StringReader reader;
    reader.openString(code);
    Lexer lexer(reader);
    Parser parser(lexer);

    std::optional<Program> program = parser.tryParseProgram();
    BOOST_REQUIRE(program.has_value());
    BOOST_REQUIRE_EQUAL(program->statements.size(), 1);

    const auto& stmtPtr = program->statements[0];
    auto* varDecl = dynamic_cast<VariableDeclaration*>(stmtPtr.get());
    const auto& expr = varDecl->value;

    auto* binExpr1 = dynamic_cast<BinaryExpression*>(expr.get());
    BOOST_REQUIRE(binExpr1 != nullptr);
    BOOST_REQUIRE(binExpr1->op == TokenType::EQ);

    auto* right1 = dynamic_cast<BinaryExpression*>(binExpr1->rex.get());
    BOOST_REQUIRE(right1 != nullptr);
    BOOST_REQUIRE(right1->op == TokenType::DIVIDE);

    auto* left1 = dynamic_cast<BinaryExpression*>(binExpr1->lex.get());
    BOOST_REQUIRE(left1 != nullptr);
    BOOST_REQUIRE(left1->op == TokenType::MULTI);
}

BOOST_AUTO_TEST_CASE(recursive_relational_expression)
{
    std::wstring code = L"numer x to 1 większe_niż 2 mniejsze_niż 3 równe 4 nierówne 5.";
    StringReader reader;
    reader.openString(code);
    Lexer lexer(reader);
    Parser parser(lexer);

    std::optional<Program> program = parser.tryParseProgram();
    BOOST_REQUIRE(program.has_value());
    BOOST_REQUIRE_EQUAL(program->statements.size(), 1);

    const auto& stmtPtr = program->statements[0];
    auto* varDecl = dynamic_cast<VariableDeclaration*>(stmtPtr.get());
    const auto& expr = varDecl->value;

    auto* binExpr1 = dynamic_cast<BinaryExpression*>(expr.get());
    BOOST_REQUIRE(binExpr1 != nullptr);
    BOOST_REQUIRE(binExpr1->op == TokenType::NEQ);

    auto* right1 = dynamic_cast<LiteralExpression*>(binExpr1->rex.get());
    BOOST_REQUIRE_EQUAL(std::get<int>(right1->literal), 5);

    auto* left1 = dynamic_cast<BinaryExpression*>(binExpr1->lex.get());
    BOOST_REQUIRE(left1 != nullptr);
    BOOST_REQUIRE(left1->op == TokenType::EQ);
    
    auto* right2 = dynamic_cast<LiteralExpression*>(left1->rex.get());
    BOOST_REQUIRE_EQUAL(std::get<int>(right2->literal), 4);

    auto* left2 = dynamic_cast<BinaryExpression*>(left1->lex.get());
    BOOST_REQUIRE(left2 != nullptr);
    BOOST_REQUIRE(left2->op == TokenType::LESSER);

    auto* right3 = dynamic_cast<LiteralExpression*>(left2->rex.get());
    BOOST_REQUIRE_EQUAL(std::get<int>(right3->literal), 3);
    
    auto* left3 = dynamic_cast<BinaryExpression*>(left2->lex.get());
    BOOST_REQUIRE(left3 != nullptr);
    BOOST_REQUIRE(left3->op == TokenType::GREATER);
    
    auto* right4 = dynamic_cast<LiteralExpression*>(left3->rex.get());
    BOOST_REQUIRE_EQUAL(std::get<int>(right4->literal), 2);

    auto* left4 = dynamic_cast<LiteralExpression*>(left3->lex.get());
    BOOST_REQUIRE(left4 != nullptr);
    BOOST_REQUIRE_EQUAL(std::get<int>(left4->literal), 1);
}

BOOST_AUTO_TEST_CASE(logical_expression_outside)
{
    std::wstring code = L"numer x to 1 oraz 2 plus 3 lub 4.";
    
    StringReader reader;
    reader.openString(code);
    Lexer lexer(reader);
    Parser parser(lexer);

    std::optional<Program> program = parser.tryParseProgram();
    BOOST_REQUIRE(program.has_value());
    BOOST_REQUIRE_EQUAL(program->statements.size(), 1);

    const auto& stmtPtr = program->statements[0];
    auto* varDecl = dynamic_cast<VariableDeclaration*>(stmtPtr.get());
    const auto& expr = varDecl->value;

    auto* binExpr1 = dynamic_cast<BinaryExpression*>(expr.get());
    BOOST_REQUIRE(binExpr1 != nullptr);
    BOOST_REQUIRE(binExpr1->op == TokenType::OR);

    auto* right1 = dynamic_cast<LiteralExpression*>(binExpr1->rex.get());
    BOOST_REQUIRE_EQUAL(std::get<int>(right1->literal), 4);

    auto* left1 = dynamic_cast<BinaryExpression*>(binExpr1->lex.get());
    BOOST_REQUIRE(left1 != nullptr);
    BOOST_REQUIRE(left1->op == TokenType::AND);
    
    auto* right2 = dynamic_cast<BinaryExpression*>(left1->rex.get());
    BOOST_REQUIRE(right2 != nullptr);
    BOOST_REQUIRE(right2->op == TokenType::PLUS);
    
    auto* right2_left1 = dynamic_cast<LiteralExpression*>(right2->lex.get());
    BOOST_REQUIRE(right2_left1 != nullptr);
    BOOST_REQUIRE_EQUAL(std::get<int>(right2_left1->literal), 2);

    auto* right2_right1 = dynamic_cast<LiteralExpression*>(right2->rex.get());
    BOOST_REQUIRE(right2_right1 != nullptr);
    BOOST_REQUIRE_EQUAL(std::get<int>(right2_right1->literal), 3);

    auto* left2 = dynamic_cast<LiteralExpression*>(left1->lex.get());
    BOOST_REQUIRE(left2 != nullptr);
    BOOST_REQUIRE_EQUAL(std::get<int>(left2->literal), 1);
}

BOOST_AUTO_TEST_CASE(logical_expression_inside)
{
    std::wstring code = L"numer x to 1 razy 2 oraz 3 nierówne 4.";
    
    StringReader reader;
    reader.openString(code);
    Lexer lexer(reader);
    Parser parser(lexer);

    std::optional<Program> program = parser.tryParseProgram();
    BOOST_REQUIRE(program.has_value());
    BOOST_REQUIRE_EQUAL(program->statements.size(), 1);

    const auto& stmtPtr = program->statements[0];
    auto* varDecl = dynamic_cast<VariableDeclaration*>(stmtPtr.get());
    const auto& expr = varDecl->value;

    auto* binExpr1 = dynamic_cast<BinaryExpression*>(expr.get());
    BOOST_REQUIRE(binExpr1 != nullptr);
    BOOST_REQUIRE(binExpr1->op == TokenType::AND);

    auto* right1 = dynamic_cast<BinaryExpression*>(binExpr1->rex.get());
    BOOST_REQUIRE(right1 != nullptr);
    BOOST_REQUIRE(right1->op == TokenType::NEQ);

    auto* left1 = dynamic_cast<BinaryExpression*>(binExpr1->lex.get());
    BOOST_REQUIRE(left1 != nullptr);
    BOOST_REQUIRE(left1->op == TokenType::MULTI);
}

BOOST_AUTO_TEST_CASE(logical_relational_expression)
{
    std::wstring code = L"numer x to 1 oraz 2 lub 3 lub 4 oraz 5.";
    StringReader reader;
    reader.openString(code);
    Lexer lexer(reader);
    Parser parser(lexer);

    std::optional<Program> program = parser.tryParseProgram();
    BOOST_REQUIRE(program.has_value());
    BOOST_REQUIRE_EQUAL(program->statements.size(), 1);

    const auto& stmtPtr = program->statements[0];
    auto* varDecl = dynamic_cast<VariableDeclaration*>(stmtPtr.get());
    const auto& expr = varDecl->value;

    auto* binExpr1 = dynamic_cast<BinaryExpression*>(expr.get());
    BOOST_REQUIRE(binExpr1 != nullptr);
    BOOST_REQUIRE(binExpr1->op == TokenType::AND);

    auto* right1 = dynamic_cast<LiteralExpression*>(binExpr1->rex.get());
    BOOST_REQUIRE_EQUAL(std::get<int>(right1->literal), 5);

    auto* left1 = dynamic_cast<BinaryExpression*>(binExpr1->lex.get());
    BOOST_REQUIRE(left1 != nullptr);
    BOOST_REQUIRE(left1->op == TokenType::OR);
    
    auto* right2 = dynamic_cast<LiteralExpression*>(left1->rex.get());
    BOOST_REQUIRE_EQUAL(std::get<int>(right2->literal), 4);

    auto* left2 = dynamic_cast<BinaryExpression*>(left1->lex.get());
    BOOST_REQUIRE(left2 != nullptr);
    BOOST_REQUIRE(left2->op == TokenType::OR);

    auto* right3 = dynamic_cast<LiteralExpression*>(left2->rex.get());
    BOOST_REQUIRE_EQUAL(std::get<int>(right3->literal), 3);
    
    auto* left3 = dynamic_cast<BinaryExpression*>(left2->lex.get());
    BOOST_REQUIRE(left3 != nullptr);
    BOOST_REQUIRE(left3->op == TokenType::AND);
    
    auto* right4 = dynamic_cast<LiteralExpression*>(left3->rex.get());
    BOOST_REQUIRE_EQUAL(std::get<int>(right4->literal), 2);

    auto* left4 = dynamic_cast<LiteralExpression*>(left3->lex.get());
    BOOST_REQUIRE(left4 != nullptr);
    BOOST_REQUIRE_EQUAL(std::get<int>(left4->literal), 1);
}

BOOST_AUTO_TEST_CASE(grouping_expression)
{
    std::wstring code = L"numer x to (1 plus 2) razy 3.";
    StringReader reader;
    reader.openString(code);
    Lexer lexer(reader);
    Parser parser(lexer);

    std::optional<Program> program = parser.tryParseProgram();
    BOOST_REQUIRE(program.has_value());
    BOOST_REQUIRE_EQUAL(program->statements.size(), 1);

    const auto& stmtPtr = program->statements[0];
    auto* varDecl = dynamic_cast<VariableDeclaration*>(stmtPtr.get());
    const auto& expr = varDecl->value;

    auto* binExpr1 = dynamic_cast<BinaryExpression*>(expr.get());
    BOOST_REQUIRE(binExpr1 != nullptr);
    BOOST_REQUIRE(binExpr1->op == TokenType::MULTI);
}

BOOST_AUTO_TEST_CASE(recursive_grouping_expression)
{
    std::wstring code = L"numer x to (1 razy ((3 przez 4) minus 2)) plus 3.";
    StringReader reader;
    reader.openString(code);
    Lexer lexer(reader);
    Parser parser(lexer);

    std::optional<Program> program = parser.tryParseProgram();
    BOOST_REQUIRE(program.has_value());
    BOOST_REQUIRE_EQUAL(program->statements.size(), 1);

    const auto& stmtPtr = program->statements[0];
    auto* varDecl = dynamic_cast<VariableDeclaration*>(stmtPtr.get());
    const auto& expr = varDecl->value;

    auto* binExpr1 = dynamic_cast<BinaryExpression*>(expr.get());
    BOOST_REQUIRE(binExpr1 != nullptr);
    BOOST_REQUIRE(binExpr1->op == TokenType::PLUS);

    auto* binExpr2 = dynamic_cast<BinaryExpression*>(binExpr1->lex.get());
    BOOST_REQUIRE(binExpr2 != nullptr);
    BOOST_REQUIRE(binExpr2->op == TokenType::MULTI);

    auto* binExpr3 = dynamic_cast<BinaryExpression*>(binExpr2->rex.get());
    BOOST_REQUIRE(binExpr3 != nullptr);
    BOOST_REQUIRE(binExpr3->op == TokenType::MINUS);

    auto* binExpr4 = dynamic_cast<BinaryExpression*>(binExpr3->lex.get());
    BOOST_REQUIRE(binExpr4 != nullptr);
    BOOST_REQUIRE(binExpr4->op == TokenType::DIVIDE);
}

BOOST_AUTO_TEST_CASE(simple_funccall_expression)
{
    std::wstring code = L"numer x to jakas_funkcja(2 i 3).";
    StringReader reader;
    reader.openString(code);
    Lexer lexer(reader);
    Parser parser(lexer);

    std::optional<Program> program = parser.tryParseProgram();
    BOOST_REQUIRE(program.has_value());
    BOOST_REQUIRE_EQUAL(program->statements.size(), 1);

    const auto& stmtPtr = program->statements[0];
    auto* varDecl = dynamic_cast<VariableDeclaration*>(stmtPtr.get());
    BOOST_REQUIRE(varDecl != nullptr);
    const auto& expr = varDecl->value;

    auto* funcExpr1 = dynamic_cast<ValueExpression*>(expr.get());
    BOOST_REQUIRE(funcExpr1 != nullptr);

    
    auto* callee = dynamic_cast<IdentifierExpression*>(funcExpr1->callee.get());
    BOOST_REQUIRE(callee != nullptr);
    bool eq = callee->identifier == std::wstring(L"jakas_funkcja");
    BOOST_TEST(eq);

    BOOST_REQUIRE_EQUAL(funcExpr1->arguments.size(), 2);

    auto* arg1 = dynamic_cast<LiteralExpression*>(funcExpr1->arguments[0].get());
    BOOST_REQUIRE(arg1 != nullptr);
    BOOST_TEST(std::get<int>(arg1->literal) == 2);

    auto* arg2 = dynamic_cast<LiteralExpression*>(funcExpr1->arguments[1].get());
    BOOST_REQUIRE(arg2 != nullptr);
    BOOST_TEST(std::get<int>(arg2->literal) == 3);
}

BOOST_AUTO_TEST_CASE(nested_funccall_expression)
{
    std::wstring code = L"numer x to get_func(1)(2)(3)(4)(5)(6)(7).";
    StringReader reader;
    reader.openString(code);
    Lexer lexer(reader);
    Parser parser(lexer);

    std::optional<Program> program = parser.tryParseProgram();
    BOOST_REQUIRE(program.has_value());
    BOOST_REQUIRE_EQUAL(program->statements.size(), 1);

    const auto& stmtPtr = program->statements[0];
    auto* varDecl = dynamic_cast<VariableDeclaration*>(stmtPtr.get());
    BOOST_REQUIRE(varDecl != nullptr);
    const auto& expr = varDecl->value;

    const ValueExpression* currentCall = dynamic_cast<ValueExpression*>(expr.get());
    BOOST_REQUIRE(currentCall != nullptr);

    BOOST_REQUIRE_EQUAL(currentCall->arguments.size(), 1);
    {
        auto* literalArg = dynamic_cast<LiteralExpression*>(currentCall->arguments[0].get());
        BOOST_REQUIRE(literalArg != nullptr);
        BOOST_TEST(std::get<int>(literalArg->literal) == 7);
    }

    for (int expected = 6; expected >= 1; --expected)
    {
        auto* nested = dynamic_cast<ValueExpression*>(currentCall->callee.get());
        BOOST_REQUIRE(nested != nullptr);

        currentCall = nested;

        BOOST_REQUIRE_EQUAL(currentCall->arguments.size(), 1);
        {
            auto* literalArg = dynamic_cast<LiteralExpression*>(currentCall->arguments[0].get());
            BOOST_REQUIRE(literalArg != nullptr);
            BOOST_TEST(std::get<int>(literalArg->literal) == expected);
        }
    }

    auto* callee = dynamic_cast<IdentifierExpression*>(currentCall->callee.get());
    BOOST_REQUIRE(callee != nullptr);
    bool eq = callee->identifier == std::wstring(L"get_func");
    BOOST_TEST(eq);
}


BOOST_AUTO_TEST_CASE(przyklad_identifier_expression)
{
    std::wstring code = L"numer x to przykład func_name.";
    StringReader reader;
    reader.openString(code);
    Lexer lexer(reader);
    Parser parser(lexer);

    std::optional<Program> program = parser.tryParseProgram();
    BOOST_REQUIRE(program.has_value());
    BOOST_REQUIRE_EQUAL(program->statements.size(), 1);

    const auto& stmtPtr = program->statements[0];
    auto* varDecl = dynamic_cast<VariableDeclaration*>(stmtPtr.get());
    BOOST_REQUIRE(varDecl != nullptr);
    const auto& expr = varDecl->value;

    auto* przykladExpr = dynamic_cast<PrzykladExpression*>(expr.get());
    BOOST_REQUIRE(przykladExpr != nullptr);

    auto* identExpr = dynamic_cast<IdentifierExpression*>(przykladExpr->expr.get());
    BOOST_REQUIRE(identExpr != nullptr);
    bool eq = identExpr->identifier == std::wstring(L"func_name"); BOOST_TEST(eq);
}

BOOST_AUTO_TEST_CASE(przyklad_value_expression)
{
    std::wstring code = L"numer x to przykład func_name().";
    StringReader reader;
    reader.openString(code);
    Lexer lexer(reader);
    Parser parser(lexer);

    std::optional<Program> program = parser.tryParseProgram();
    BOOST_REQUIRE(program.has_value());
    BOOST_REQUIRE_EQUAL(program->statements.size(), 1);

    const auto& stmtPtr = program->statements[0];
    auto* varDecl = dynamic_cast<VariableDeclaration*>(stmtPtr.get());
    BOOST_REQUIRE(varDecl != nullptr);
    const auto& expr = varDecl->value;

    auto* przykladExpr = dynamic_cast<PrzykladExpression*>(expr.get());
    BOOST_REQUIRE(przykladExpr != nullptr);

    auto* valueExpr = dynamic_cast<ValueExpression*>(przykladExpr->expr.get());
    BOOST_REQUIRE(valueExpr != nullptr);
}

BOOST_AUTO_TEST_CASE(recursive_przyklad_value_expression)
{
    std::wstring code = L"numer x to przykład (przykład func_name)().";
    StringReader reader;
    reader.openString(code);
    Lexer lexer(reader);
    Parser parser(lexer);

    std::optional<Program> program = parser.tryParseProgram();
    BOOST_REQUIRE(program.has_value());
    BOOST_REQUIRE_EQUAL(program->statements.size(), 1);

    const auto& stmtPtr = program->statements[0];
    auto* varDecl = dynamic_cast<VariableDeclaration*>(stmtPtr.get());
    BOOST_REQUIRE(varDecl != nullptr);
    const auto& expr = varDecl->value;

    auto* outerPrzyklad = dynamic_cast<PrzykladExpression*>(expr.get());
    BOOST_REQUIRE(outerPrzyklad != nullptr);

    auto* callExpr = dynamic_cast<ValueExpression*>(outerPrzyklad->expr.get());
    BOOST_REQUIRE(callExpr != nullptr);
    BOOST_REQUIRE(callExpr->arguments.empty());

    auto* innerPrzyklad = dynamic_cast<PrzykladExpression*>(callExpr->callee.get());
    BOOST_REQUIRE(innerPrzyklad != nullptr);

    auto* ident = dynamic_cast<IdentifierExpression*>(innerPrzyklad->expr.get());
    BOOST_REQUIRE(ident != nullptr);
    bool eq = ident->identifier == std::wstring(L"func_name"); BOOST_TEST(eq);
}

BOOST_AUTO_TEST_CASE(mixed_func_op_recursive_expression)
{
    std::wstring code = L"numer x to przykład (skrócona_o (1 i 2) (przykład skrócona_o (1) funkcja()())).";
    StringReader reader;
    reader.openString(code);
    Lexer lexer(reader);
    Parser parser(lexer);

    std::optional<Program> program = parser.tryParseProgram();
    BOOST_REQUIRE(program.has_value());
    BOOST_REQUIRE_EQUAL(program->statements.size(), 1);

    const auto& stmtPtr = program->statements[0];
    auto* varDecl = dynamic_cast<VariableDeclaration*>(stmtPtr.get());
    BOOST_REQUIRE(varDecl != nullptr);

    auto* outerPrzyklad = dynamic_cast<PrzykladExpression*>(varDecl->value.get());
    BOOST_REQUIRE(outerPrzyklad != nullptr);

    auto* firstSkrocona = dynamic_cast<SkroconaExpression*>(outerPrzyklad->expr.get());
    BOOST_REQUIRE(firstSkrocona != nullptr);
    BOOST_REQUIRE_EQUAL(firstSkrocona->arguments.size(), 2);

    auto* innerPrzyklad = dynamic_cast<PrzykladExpression*>(firstSkrocona->expr.get());
    BOOST_REQUIRE(innerPrzyklad != nullptr);

    auto* secondSkrocona = dynamic_cast<SkroconaExpression*>(innerPrzyklad->expr.get());
    BOOST_REQUIRE(secondSkrocona != nullptr);
    BOOST_REQUIRE_EQUAL(secondSkrocona->arguments.size(), 1);
    auto* litExpr = dynamic_cast<LiteralExpression*>(secondSkrocona->arguments[0].get());
    BOOST_REQUIRE(litExpr != nullptr);

    auto* outerCall = dynamic_cast<ValueExpression*>(secondSkrocona->expr.get());
    BOOST_REQUIRE(outerCall != nullptr);
    BOOST_REQUIRE(outerCall->arguments.empty());

    auto* innerCall = dynamic_cast<ValueExpression*>(outerCall->callee.get());
    BOOST_REQUIRE(innerCall != nullptr);
    BOOST_REQUIRE(innerCall->arguments.empty());

    auto* ident = dynamic_cast<IdentifierExpression*>(innerCall->callee.get());
    BOOST_REQUIRE(ident != nullptr);
}

