#pragma once

#include "abstract_lexer.h"
#include "abstract_reader.h"
#include <string>
#include <optional>


class Lexer: public ILexer
{
public:
        Lexer(AbstractReader& reader);
        Token nextToken() override;
        bool isEOF() const;
        void setIgnoreComments(bool v);
private:
        AbstractReader& reader;
        wchar_t currentChar;
        Position currentPosition;
        static const std::pair<std::wstring, TokenType> keywordPairs[];
        [[noreturn]] void throwError(const std::wstring& message);
        void advance();
        Token buildComment();
        std::optional<Token> tryBuildEOF();
        std::optional<Token> tryBuildIdentifier();
        std::optional<Token> tryBuildNumber();
        std::optional<Token> tryBuildString();
        std::optional<Token> tryBuildSingleChar();
        bool ignoreComments_ = false;
};

inline const std::pair<std::wstring, TokenType> Lexer::keywordPairs[] = {
        {L"od",                 TokenType::LSCOPE},
        {L"do",                 TokenType::RSCOPE},
        {L"na",                 TokenType::CAST},
        {L"powtórz",            TokenType::FOR},
        {L"razy",               TokenType::MULTI},
        {L"jeżeli",             TokenType::IF},
        {L"wtedy",              TokenType::THEN},
        {L"inaczej_gdy",        TokenType::ELIF},
        {L"ostatecznie",        TokenType::ELSE},
        {L"plus",               TokenType::PLUS},
        {L"minus",              TokenType::MINUS},
        {L"złącz",              TokenType::CONNECT},
        {L"przez",              TokenType::DIVIDE},
        {L"razy",               TokenType::MULTI},
        {L"krotnie",            TokenType::FOR_FOLLOW},
        {L"równe",              TokenType::EQ},
        {L"nierówne",           TokenType::NEQ},
        {L"większe_niż",        TokenType::GREATER},
        {L"mniejsze_niż",       TokenType::LESSER},
        {L"nie",                TokenType::NOT},
        {L"oraz",               TokenType::AND},
        {L"lub",                TokenType::OR},
        {L"praca",              TokenType::FUNC_DEF},
        {L"daje",               TokenType::FUNC_RET},
        {L"robi",               TokenType::FUNC_START},
        {L"dająca",             TokenType::RETURN_ARROW},
        {L"zwróć",              TokenType::RETURN},
        {L"i",                  TokenType::SEPARATOR},
        {L"przykład",           TokenType::FUNC_OP_EXAMPLE},
        {L"skrócona_o",         TokenType::FUNC_OP_BIND},
        {L"to",                 TokenType::ASSIGN},
        {L"nic",                TokenType::EMPTY_VALUE},
        {L"stały",              TokenType::CONST_T},
        {L"fakt",               TokenType::BOOLEAN},
        {L"numer",              TokenType::INTEGER},
        {L"ułamek",             TokenType::DOUBLE},
        {L"wyraz",              TokenType::STRING},
        {L"prawda",             TokenType::BOOL_VALUE},
        {L"fałsz",              TokenType::BOOL_VALUE}
    };
    
