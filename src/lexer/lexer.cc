#include "lexer.h"
#include <cwctype>
#include <sstream>
#include <limits>
#include <math.h>

Lexer::Lexer(AbstractReader& reader) 
    : reader(reader), currentChar(L'\0')
{
    advance();
}

void Lexer::throwError(const std::wstring& message)
{
    std::wcout << L"Błąd Leksera [" << currentPosition.line << L":" << currentPosition.column << L"]: " << message;
    throw std::runtime_error("");
}

void Lexer::advance() {
    if (!reader.isEOF()) {
        currentChar = reader.readChar();
        if (currentChar == L'\n') {
            currentPosition.line++;
            currentPosition.column = 0;
        } else {
            currentPosition.column++;
        }
    } else {
        currentChar = L'\0';
    }
}

Token Lexer::nextToken() {
    while (std::iswspace(currentChar)) { advance(); }

    if (auto tokenOpt = tryBuildEOF())          { return *tokenOpt; }
    if (auto tokenOpt = tryBuildSingleChar())   { return *tokenOpt; }
    if (auto tokenOpt = tryBuildIdentifier())   { return *tokenOpt; }
    if (auto tokenOpt = tryBuildNumber())       { return *tokenOpt; }
    if (auto tokenOpt = tryBuildString())       { return *tokenOpt; }

    throwError(L"Nieznany token.");
}

bool Lexer::isEOF() const {
    return currentChar == L'\0';
}

void Lexer::setIgnoreComments(bool v)
{
    ignoreComments_ = v;
}

std::optional<Token> Lexer::tryBuildEOF() {
    if (currentChar == L'\0') {
        return Token(TokenType::EOF_TOKEN, currentPosition);
    }
    return std::nullopt;
}

Token Lexer::buildComment() {
    Position startPosition = currentPosition;
    std::wstringstream ss;
    advance();
    while (currentChar != L'\n' && currentChar != L'\0') {
        ss << currentChar;
        advance();
    }
    if(ignoreComments_) return nextToken();
    return Token(TokenType::COMMENT, ss.str(), startPosition);
}

std::optional<Token> Lexer::tryBuildIdentifier() {
    if (std::iswalpha(currentChar) || currentChar == L'_') {
        Position startPosition = currentPosition;
        std::wstringstream ss;

        if (currentChar == L'P') {
            ss << currentChar;
            advance();
            if (currentChar == L'S') {
                ss << currentChar;
                advance();
                if (currentChar == L':') {
                    return buildComment();
                }
            }
        }

        while (std::iswalnum(currentChar) || currentChar == L'_') {
            ss << currentChar;
            advance();
        }

        std::wstring identifier = ss.str();

        TokenType type = TokenType::IDENTIFIER;
        for (const auto& pair : keywordPairs) {
            if (pair.first == identifier) {
                type = pair.second;
                break;
            }
        }
        if (type == TokenType::IDENTIFIER)
        {
            return Token(TokenType::IDENTIFIER, identifier, startPosition);
        }
        else if (type == TokenType::BOOL_VALUE)
        {
            if (identifier == L"prawda")
                return Token(TokenType::BOOL_VALUE, true, startPosition);
            else if(identifier == L"fałsz")
                return Token(TokenType::BOOL_VALUE, false, startPosition);
        }
        else
        {
                return Token(type, startPosition);
        }
    }
    return std::nullopt;
}

std::optional<Token> Lexer::tryBuildNumber() {
    if (std::isdigit(currentChar) || currentChar == L'-') {
        Position startPosition = currentPosition;
        bool isNegative = false;
        if (currentChar == L'-') {
            isNegative = true;
            advance();
            if (!std::isdigit(currentChar)) {
                throwError(L"Nieprawidłowo utworzona negatywna liczba.");
            }
        }

        long integerPart = 0;
        while (std::isdigit(currentChar)) {
            int digit = currentChar - L'0';
            if (integerPart > ((std::numeric_limits<int>::max() - digit) / 10)) {
                throwError(L"Numer wykroczył poza zakres.");
            }
            integerPart = integerPart * 10 + digit;
            advance();
        }

        if (currentChar == L',') {
            advance();
            if (!std::isdigit(currentChar)) {
                throwError(L"Nieprawidłowy ułamek.");
            }

            long fractionalDigits = 0;
            int fractionalLength = 0;

            while (std::isdigit(currentChar)) {
                fractionalDigits = fractionalDigits * 10 + (currentChar - L'0');
                ++fractionalLength;
                advance();
            }

            double fractionalPart = static_cast<double>(fractionalDigits) / std::pow(10.0, fractionalLength);
            double doubleValue = static_cast<double>(integerPart) + fractionalPart;
            if (isNegative) {
                doubleValue = -doubleValue;
            }

            return Token(TokenType::DOUBLE_VALUE, doubleValue, startPosition);
        }

        if (isNegative) {
            integerPart = -integerPart;
        }
        return Token(TokenType::INT_VALUE, integerPart, startPosition);
    }
    return std::nullopt;
}

std::optional<Token> Lexer::tryBuildString() {
    if (currentChar == L'"') {
        Position startPosition = currentPosition;
        std::wstringstream ss;
        advance();
        while (currentChar != L'"' && currentChar != L'\0') {
            if (currentChar == L'\\') {
                advance();
                switch (currentChar) {
                    case L'n': ss << L'\n'; break;
                    case L'r': ss << L'\r'; break;
                    case L't': ss << L'\t'; break;
                    case L'\\': ss << L'\\'; break;
                    case L'"': ss << L'"'; break;
                    default:
                        throwError(L"Nieprawidłowa ucieczka znaku.");
                }
            } else {
                ss << currentChar;
            }
            advance();
        }
        if (currentChar == L'"') {
            advance();
            return Token(TokenType::STR_VALUE, ss.str(), startPosition);
        }
        throwError(L"Niedokończony wyraz.");
    }
    return std::nullopt;
}

std::optional<Token> Lexer::tryBuildSingleChar() {
    Position startPosition = currentPosition;
    if (currentChar == L'=') {
        advance();
        if (currentChar == L'>') {
            advance();
            return Token(TokenType::RETURN_ARROW, startPosition);
        }
        currentChar = L'=';
    }
    switch (currentChar) {
        case L'.':
            advance();
            return Token(TokenType::DOT, startPosition);
        case L'(': 
            advance();
            return Token(TokenType::LROUND, startPosition);
        case L')':
            advance();
            return Token(TokenType::RROUND, startPosition);
        default:
            return std::nullopt;
    }
    return std::nullopt;
}