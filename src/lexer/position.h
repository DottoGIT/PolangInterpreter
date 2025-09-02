#pragma once

class Position
{
public:
    Position(int line, int column) : line(line), column(column) {}
    Position() : line(1), column(-1) {}
    int line;
    int column;
};