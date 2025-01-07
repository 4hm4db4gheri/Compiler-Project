#include <string>
#include <iostream>
using namespace std;

namespace charinfo
{

    LLVM_READNONE inline bool isWhitespace(char c)
    {
        return c == ' ' || c == '\t' || c == '\f' ||
               c == '\v' || c == '\r' || c == '\n';
    }

    LLVM_READNONE inline bool isDigit(char c)
    {
        return c >= '0' && c <= '9';
    }

    LLVM_READNONE inline bool isLetter(char c)
    {
        return (c >= 'a' && c <= 'z') ||
               (c >= 'A' && c <= 'Z');
    }

    LLVM_READNONE inline bool isSemiColon(char c)
    {
        return c == ';';
    }

    LLVM_READNONE inline bool isEqual(char c)
    {
        return c == '=';
    }

}

class Optimizer
{
    std::vector<llvm::StringRef> Lines;
    std::vector<bool> dead_lines;
    std::string code = "";
    const char *BufferPtr;

public:
    Optimizer(const llvm::StringRef &Buffer)
    { // constructor scans the whole context

        BufferPtr = Buffer.begin();
        const char *end = BufferPtr + 1;
        while (*end)
        { // since end of context is 0 -> !0 = true -> end of context
            end++;
        }
        llvm::StringRef Context(BufferPtr, end - BufferPtr);
        code = (std::string)Context;

        const char *pointer = BufferPtr;
        const char *line_start = BufferPtr;
        while (*pointer)
        {
            while (!charinfo::isSemiColon(*pointer))
            {
                ++pointer; //  ¯\_(ツ)_/¯
            }
            llvm::StringRef Context(line_start, pointer - line_start);
            Lines.push_back(Context);
            dead_lines.push_back(true);
            llvm::errs() << "read line: " << Context << "\n";
            line_start = ++pointer;
        }
    }

public:
    void optimize()
    {
        int i = Lines.size();
        llvm::errs() << "ready for const_pul\n";
        llvm::errs() << "output has value: " << const_pul(i, "output") << "\n";
    }

    int const_pul(int j, llvm::StringRef variab)
    {
        int i = j;
        bool flag = true;
        while (i >= 0 && flag)
        {
            i--;
            llvm::StringRef corrent_line = Lines[i];
            const char *pointer = corrent_line.begin(); // ¯\_(ツ)_/¯

            while (*pointer)
            {
                while (*pointer && charinfo::isWhitespace(*pointer))
                {
                    ++pointer;
                }

                if (charinfo::isLetter(*pointer))
                {

                    const char *end = pointer + 1;

                    while (charinfo::isLetter(*end) || charinfo::isDigit(*end))
                        ++end;
                    llvm::StringRef Context(pointer, end - pointer);

                    if (Context == variab)
                    {
                        flag = false;
                        break;
                    }

                    pointer = end;
                }
                else if (charinfo::isEqual(*pointer))
                {
                    break;
                }

                ++pointer;
            }
        }
        dead_lines[i] = false;
        llvm::StringRef corrent_line = Lines[i];
        const char *pointer = corrent_line.begin();
        const char *start_exp = corrent_line.begin();
        while (!charinfo::isEqual(*start_exp))
        {
            ++start_exp;
        }
        llvm::StringRef new_line(pointer, start_exp - pointer);
        start_exp++;
        llvm::errs() << "calculating: " << variab << "\n";
        return expression(start_exp, i);

    }


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
        while (peek(expr) == ' ')
            get(expr);
        return result;
    }

    int variable(const char *&expr, int i)
    {
        const char *temp = expr;
        while (charinfo::isLetter(peek(expr)) || charinfo::isDigit(peek(expr)))
        {
            get(expr);
        }
        llvm::StringRef name(temp, expr - temp);
        while (peek(expr) == ' ')
            get(expr);
        return const_pul(i, name);
    }

    int factor(const char *&expr, int i)
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
            int result = expression(expr, i);
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
            return -factor(expr, i);
        }
        else if (charinfo::isLetter(peek(expr))){
            return variable(expr, i);
        }
        return 0; // error
    }

    int term(const char *&expr, int i)
    {
        while (peek(expr) == ' ')
            get(expr);
        int result = factor(expr, i);
        while (peek(expr) == ' ')
            get(expr);
        while (peek(expr) == '*' || peek(expr) == '/')
        {
            if (get(expr) == '*')
                result *= factor(expr, i);

            else
                result /= factor(expr, i);
        }
        return result;
    }

    int expression(const char *&expr, int i)
    {
        while (peek(expr) == ' ')
            get(expr);
        int result = term(expr, i);
        while (peek(expr) == ' ')
            get(expr);
        while (peek(expr) == '+' || peek(expr) == '-')
        {
            if (get(expr) == '+')
                result += term(expr, i);
            else
                result -= term(expr, i);
        }
        return result;
    }
};
