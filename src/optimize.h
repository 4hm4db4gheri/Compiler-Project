#include <string>
#include <iostream>

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

class Optimize
{
    std::vector<llvm::StringRef> Lines;
    std::vector<bool> dead_lines;
    std::string code = "";
    const char *BufferPtr;

public:
    Optimize(const llvm::StringRef &Buffer)
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
            daed_lines.push_back(true);
            llvm::errs() << "read line: " << Context << "\n";
            line_start = ++pointer;
        }
    }

public:
    void optimizer()
    {
        int i = Lines.size();
        const_pul(i, "output", )
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
        while (!charinfo::isEqual(*start_exp)){
            ++start_exp;
        }
        llvm::StringRef new_line(pointer, start_exp - pointer);
        start_exp++;

        

    }
};
