#include <vector>
#include "lexer.h"
#include "string_reader.h"

#define BOOST_TEST_MODULE LexerTests
#include <boost/test/included/unit_test.hpp>

BOOST_AUTO_TEST_CASE(tokenize_all_keywords)
{
    std::wstring code = L".() od do powtórz razy jeżeli wtedy inaczej_gdy inaczej_gdy ostatecznie plus minus złącz przez razy równe nierówne większe_niż mniejsze_niż nie oraz lub praca daje robi dająca zwróć i przykład skrócona_o to nic stały fakt numer ułamek wyraz na prawda fałsz krotnie.";
    std::vector<Token> tokens;
    StringReader reader;
    reader.openString(code);
    Lexer lexer(reader);
    while (true)
    {
        tokens.push_back(lexer.nextToken());

        if (lexer.isEOF()) break;
    }

    BOOST_TEST(tokens[0].getType() == TokenType::DOT);
    BOOST_TEST(tokens[1].getType() == TokenType::LROUND);
    BOOST_TEST(tokens[2].getType() == TokenType::RROUND);
    BOOST_TEST(tokens[3].getType() == TokenType::LSCOPE);
    BOOST_TEST(tokens[4].getType() == TokenType::RSCOPE);
    BOOST_TEST(tokens[5].getType() == TokenType::FOR);
    BOOST_TEST(tokens[6].getType() == TokenType::MULTI);
    BOOST_TEST(tokens[7].getType() == TokenType::IF);
    BOOST_TEST(tokens[8].getType() == TokenType::THEN);
    BOOST_TEST(tokens[9].getType() == TokenType::ELIF);
    BOOST_TEST(tokens[10].getType() == TokenType::ELIF);
    BOOST_TEST(tokens[11].getType() == TokenType::ELSE);
    BOOST_TEST(tokens[12].getType() == TokenType::PLUS);
    BOOST_TEST(tokens[13].getType() == TokenType::MINUS);
    BOOST_TEST(tokens[14].getType() == TokenType::CONNECT);
    BOOST_TEST(tokens[15].getType() == TokenType::DIVIDE);
    BOOST_TEST(tokens[16].getType() == TokenType::MULTI);
    BOOST_TEST(tokens[17].getType() == TokenType::EQ);
    BOOST_TEST(tokens[18].getType() == TokenType::NEQ);
    BOOST_TEST(tokens[19].getType() == TokenType::GREATER);
    BOOST_TEST(tokens[20].getType() == TokenType::LESSER);
    BOOST_TEST(tokens[21].getType() == TokenType::NOT);
    BOOST_TEST(tokens[22].getType() == TokenType::AND);
    BOOST_TEST(tokens[23].getType() == TokenType::OR);
    BOOST_TEST(tokens[24].getType() == TokenType::FUNC_DEF);
    BOOST_TEST(tokens[25].getType() == TokenType::FUNC_RET);
    BOOST_TEST(tokens[26].getType() == TokenType::FUNC_START);
    BOOST_TEST(tokens[27].getType() == TokenType::RETURN_ARROW);
    BOOST_TEST(tokens[28].getType() == TokenType::RETURN);
    BOOST_TEST(tokens[29].getType() == TokenType::SEPARATOR);
    BOOST_TEST(tokens[30].getType() == TokenType::FUNC_OP_EXAMPLE);
    BOOST_TEST(tokens[31].getType() == TokenType::FUNC_OP_BIND);
    BOOST_TEST(tokens[32].getType() == TokenType::ASSIGN);
    BOOST_TEST(tokens[33].getType() == TokenType::EMPTY_VALUE);
    BOOST_TEST(tokens[34].getType() == TokenType::CONST_T);
    BOOST_TEST(tokens[35].getType() == TokenType::BOOLEAN);
    BOOST_TEST(tokens[36].getType() == TokenType::INTEGER);
    BOOST_TEST(tokens[37].getType() == TokenType::DOUBLE);
    BOOST_TEST(tokens[38].getType() == TokenType::STRING);
    BOOST_TEST(tokens[39].getType() == TokenType::CAST);
    BOOST_TEST(tokens[40].getType() == TokenType::BOOL_VALUE);
    BOOST_TEST(std::get<bool>(tokens[40].getValue()) == true);
    BOOST_TEST(tokens[41].getType() == TokenType::BOOL_VALUE);
    BOOST_TEST(std::get<bool>(tokens[41].getValue()) == false);
    BOOST_TEST(tokens[42].getType() == TokenType::FOR_FOLLOW);
    BOOST_TEST(tokens[43].getType() == TokenType::DOT);
}

BOOST_AUTO_TEST_CASE(tokenize_numbers)
{
    std::wstring code = L"123 456,789";
    std::vector<Token> tokens;
    StringReader reader;
    reader.openString(code);
    Lexer lexer(reader);
    while (true)
    {
        tokens.push_back(lexer.nextToken());
        if (lexer.isEOF()) break;
    }

    BOOST_TEST(tokens[0].getType() == TokenType::INT_VALUE);
    BOOST_TEST(std::get<int>(tokens[0].getValue()) == 123);

    BOOST_TEST(tokens[1].getType() == TokenType::DOUBLE_VALUE);
    BOOST_TEST(std::get<double>(tokens[1].getValue()) == 456.789);
}

BOOST_AUTO_TEST_CASE(tokenize_integer_boundaries)
{
    std::wstring code = L"2147483647 2147483648";
    std::vector<Token> tokens;
    StringReader reader;
    reader.openString(code);
    Lexer lexer(reader);

    // Test INT_MAX
    BOOST_CHECK_NO_THROW(tokens.push_back(lexer.nextToken())); // 2147483647
    BOOST_TEST(tokens[0].getType() == TokenType::INT_VALUE);
    BOOST_TEST(std::get<int>(tokens[0].getValue()) == 2147483647);
    BOOST_CHECK_THROW(tokens.push_back(lexer.nextToken()), std::runtime_error); // 2147483648

    std::wstring code1 = L"-2147483647 -2147483648";
    std::vector<Token> tokens1;
    StringReader reader1;
    reader1.openString(code1);
    Lexer lexer1(reader1);

    // Test INT_MIN
    BOOST_CHECK_NO_THROW(tokens1.push_back(lexer1.nextToken())); // -2147483647
    BOOST_TEST(tokens1[0].getType() == TokenType::INT_VALUE);
    BOOST_TEST(std::get<int>(tokens1[0].getValue()) == -2147483647);
    BOOST_CHECK_THROW(tokens1.push_back(lexer1.nextToken()), std::runtime_error); // -21474836478
}

BOOST_AUTO_TEST_CASE(tokenize_comments)
{
    std::wstring code = L"PS: This is a comment\n123 commentPS: thats not a comment";
    std::vector<Token> tokens;
    StringReader reader;
    reader.openString(code);
    Lexer lexer(reader);

    // Test comment token
    BOOST_CHECK_NO_THROW(tokens.push_back(lexer.nextToken()));
    BOOST_TEST(tokens[0].getType() == TokenType::COMMENT);
    bool equal = std::get<std::wstring>(tokens[0].getValue()) == L" This is a comment";
    BOOST_TEST(equal == true); // avoiding missing << operator for wstrings in Boost

    // Test integer token after comment
    BOOST_CHECK_NO_THROW(tokens.push_back(lexer.nextToken()));
    BOOST_TEST(tokens[1].getType() == TokenType::INT_VALUE);
    BOOST_TEST(std::get<int>(tokens[1].getValue()) == 123);
    
    // Test for incorrect comment
    BOOST_CHECK_NO_THROW(tokens.push_back(lexer.nextToken()));
    BOOST_TEST(tokens[2].getType() == TokenType::IDENTIFIER);
    equal = std::get<std::wstring>(tokens[2].getValue()) == L"commentPS";
    BOOST_TEST(equal == true);
    BOOST_CHECK_THROW(tokens.push_back(lexer.nextToken()), std::runtime_error); // error on ':'
}

BOOST_AUTO_TEST_CASE(tokenize_identifiers)
{
    std::wstring code = L"zmienna żółć łukasz_dzięcioł";
    std::vector<Token> tokens;
    StringReader reader;
    reader.openString(code);
    Lexer lexer(reader);
    while (true)
    {
        tokens.push_back(lexer.nextToken());
        if (lexer.isEOF()) break;
    }

    BOOST_TEST(tokens[0].getType() == TokenType::IDENTIFIER);
    bool equal = std::get<std::wstring>(tokens[0].getValue()) == L"zmienna";
    BOOST_TEST(equal == true);

    BOOST_TEST(tokens[1].getType() == TokenType::IDENTIFIER);
    equal = std::get<std::wstring>(tokens[1].getValue()) == L"żółć";
    BOOST_TEST(equal == true);

    BOOST_TEST(tokens[2].getType() == TokenType::IDENTIFIER);
    equal = std::get<std::wstring>(tokens[2].getValue()) == L"łukasz_dzięcioł";
    BOOST_TEST(equal == true);
}

BOOST_AUTO_TEST_CASE(tokenize_strings)
{
    std::wstring code = L"\"Hello, World!\" \"PS: -= ąęóćźżł\n\t\rabc\\\\abc \" \"String with \\\"escaped quotes\\\"\"";
    std::vector<Token> tokens;
    StringReader reader;
    reader.openString(code);
    Lexer lexer(reader);
    while (true)
    {
        tokens.push_back(lexer.nextToken());
        if (lexer.isEOF()) break;
    }

    BOOST_TEST(tokens[0].getType() == TokenType::STR_VALUE);
    bool equal = std::get<std::wstring>(tokens[0].getValue()) == L"Hello, World!";
    BOOST_TEST(equal == true);

    BOOST_TEST(tokens[1].getType() == TokenType::STR_VALUE);
    equal = std::get<std::wstring>(tokens[1].getValue()) == L"PS: -= ąęóćźżł\n\t\rabc\\abc ";
    BOOST_TEST(equal == true);

    BOOST_TEST(tokens[2].getType() == TokenType::STR_VALUE);
    equal = std::get<std::wstring>(tokens[2].getValue()) == L"String with \"escaped quotes\"";
    BOOST_TEST(equal == true);
}

BOOST_AUTO_TEST_CASE(identifier_with_keyword)
{
    std::wstring code = L"prawdax";
    std::vector<Token> tokens;
    StringReader reader;
    reader.openString(code);
    Lexer lexer(reader);
    tokens.push_back(lexer.nextToken());

    BOOST_TEST(tokens[0].getType() == TokenType::IDENTIFIER);
    bool equal = std::get<std::wstring>(tokens[0].getValue()) == L"prawdax";
    BOOST_TEST(equal == true);
}

BOOST_AUTO_TEST_CASE(not_ended_string)
{
    std::wstring code = L"\"\\";
    std::vector<Token> tokens;
    StringReader reader;
    reader.openString(code);
    Lexer lexer(reader);
    BOOST_CHECK_THROW(tokens.push_back(lexer.nextToken()), std::runtime_error);
}