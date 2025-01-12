#include <iostream>

int expression(const char *&expr);

char peek(const char *&expr)
{
    return *expr;
}

char get(const char *&expr)
{
    return *expr++;
}

int number(const char *&expr)
{
    int result = get(expr) - '0';
    while (peek(expr) >= '0' && peek(expr) <= '9')
    {
        result = 10 * result + get(expr) - '0';
    }
    while (peek(expr) == ' ') get(expr);
    return result;
}

int factor(const char *&expr)
{
    while (peek(expr) == ' ')
        get(expr);
    if (peek(expr) >= '0' && peek(expr) <= '9')
        return number(expr);
    else if (peek(expr) == '(')
    {
        get(expr); // '('
        while (peek(expr) == ' ')
            get(expr);
        int result = expression(expr);
        while (peek(expr) == ' ')
            get(expr);
        get(expr); // ')'
        while (peek(expr) == ' ')
            get(expr);
        return result;
    }
    else if (peek(expr) == '-')
    {
        get(expr);
        return -factor(expr);
    }
    return 0; // error
}

int term(const char *&expr)
{
    while (peek(expr) == ' ')
        get(expr);
    int result = factor(expr);
    while (peek(expr) == ' ')
        get(expr);
    while (peek(expr) == '*' || peek(expr) == '/')
    {
        if (get(expr) == '*')
            result *= factor(expr);
        
        else
            result /= factor(expr);
    }
    return result;
}

int expression(const char *&expr)
{
    while (peek(expr) == ' ')
        get(expr);
    int result = term(expr);
    while (peek(expr) == ' ')
        get(expr);
    while (peek(expr) == '+' || peek(expr) == '-')
    {
        if (get(expr) == '+')
            result += term(expr);
        else
            result -= term(expr);
    }
    return result;
}

int main(int argc, char *argv[])
{
    const char *expr1 = "32 * 2 + 4 * 1 + ((11) + 9 ) * 6 ;";
    const char *expr2 = "5+8*(2+3)";

    std::cout << "Result of expr1: " << expression(expr1) << "\n";
    std::cout << "Result of expr2: " << expression(expr2) << "\n";
    return 0;
}

