#include "Parser.h"
#include "TypeKind.h"

// main point is that the whole input has been consumed
Program *Parser::parse()
{
    Program *Res = parseProgram();
    return Res;
}

Program *Parser::parseProgram()
{
    llvm::SmallVector<AST *> data;

    // First parse all #define declarations
    while (Tok.is(Token::KW_define))
    {
        DeclareDefine *defineDecl = parseDefineDec();
        if (defineDecl)
            data.push_back(defineDecl);
        else
            goto _error;
        advance();
    }

    while (!Tok.is(Token::eoi))
    {

        switch (Tok.getKind())
        {
        case Token::KW_int:
        {
            DeclarationInt *d;
            d = parseIntDec(false);
            if (d)
                data.push_back(d);
            else
                goto _error;

            break;
        }
        case Token::KW_bool:
        {
            DeclarationBool *dbool;
            dbool = parseBoolDec(false);
            if (dbool)
                data.push_back(dbool);
            else
                goto _error;

            break;
        }
        case Token::KW_float:
        {
            DeclarationFloat *dfloat;
            dfloat = parseFloatDec(false);
            if (dfloat)
                data.push_back(dfloat);
            else
                goto _error;
            break;
        }
        case Token::KW_var:
        {
            DeclarationVar *dvar;
            dvar = parseVarDec(false);
            if (dvar)
                data.push_back(dvar);
            else
                goto _error;
            break;
        }
        
        case Token::ident:
        {
            Token prev_token = Tok;
            const char *prev_buffer = Lex.getBuffer();

            // 1. Attempt to parse a unary operation
            UnaryOp *u = parseUnary();
            if (u && Tok.is(Token::semicolon))
            {
                data.push_back(u);
                break;
            }
            else
            {
                // If parsing failed or no semicolon, reset lexer state
                Tok = prev_token;
                Lex.setBufferPtr(prev_buffer);
            }

            // 2. Attempt to parse a ternary assignment
            TernaryAssignment *t_assign = parseTernaryAssign();
            if (t_assign)
            {
                data.push_back(t_assign);
                break;
            }
            else
            {
                // If parsing failed or no semicolon, reset lexer state
                Tok = prev_token;
                Lex.setBufferPtr(prev_buffer);
            }

            // 3. Attempt to parse a variable assignment
            Assignment *a_var = parseVarAssign();
            if (a_var && Tok.is(Token::semicolon))
            {
                data.push_back(a_var);
                break;
            }
            else
            {
                // Reset lexer state
                Tok = prev_token;
                Lex.setBufferPtr(prev_buffer);
            }

            // 4. Attempt to parse a float assignment
            Assignment *a_float = parseFloatAssign();
            if (a_float && Tok.is(Token::semicolon))
            {
                data.push_back(a_float);
                break;
            }
            else
            {
                // Reset lexer state
                Tok = prev_token;
                Lex.setBufferPtr(prev_buffer);
            }

            // 5. Attempt to parse a boolean assignment
            Assignment *a_bool = parseBoolAssign();
            if (a_bool && Tok.is(Token::semicolon))
            {
                data.push_back(a_bool);
                break;
            }
            else
            {
                // Reset lexer state
                Tok = prev_token;
                Lex.setBufferPtr(prev_buffer);
            }

            // 6. Attempt to parse an integer assignment
            Assignment *a_int = parseIntAssign();
            if (a_int && Tok.is(Token::semicolon))
            {
                data.push_back(a_int);
                break;
            }
            else
            {
                // If all parsing attempts fail, handle error
                goto _error;
            }

            break;
        }
        
        case Token::KW_if:
        {
            IfStmt *i;
            i = parseIf();
            if (i)
                data.push_back(i);
            else
                goto _error;

            break;
        }
        case Token::KW_while:
        {
            WhileStmt *w;
            w = parseWhile();
            if (w)
                data.push_back(w);
            else
            {
                goto _error;
            }
            break;
        }
        case Token::KW_for:
        {
            ForStmt *f;
            f = parseFor();
            if (f)
                data.push_back(f);
            else
            {
                goto _error;
            }
            break;
        }
        case Token::KW_do:
        {
            DoWhileStmt *d;
            d = parseDoWhile();
            if (d)
                data.push_back(d);
            else
                goto _error;
            break;
        }
        case Token::KW_switch:
        {
            SwitchStmt *s;
            s = parseSwitch();
            if (s)
                data.push_back(s);
            else
                goto _error;
            break;
        }
        
        case Token::KW_min:
        {
            MinStmt *m;
            m = parseMin();
            if (m)
                data.push_back(m);
            else
                goto _error;
            break;
        }
        case Token::KW_max:
        {
            MaxStmt *m;
            m = parseMax();
            if (m)
                data.push_back(m);
            else
                goto _error;
            break;
        }
        case Token::KW_mean:
        {
            MeanStmt *mean;
            mean = parseMean();
            if (mean)
                data.push_back(mean);
            else
                goto _error;
            break;
        }
        case Token::KW_sqrtN:
        {
            SqrtNStmt *sqrtN;
            sqrtN = parseSqrtN();
            if (sqrtN)
                data.push_back(sqrtN);
            else
                goto _error;
            break;
        }
        
        case Token::KW_print:
        {
            PrintStmt *p;
            p = parsePrint();
            if (p)
                data.push_back(p);
            else
            {
                goto _error;
            }
            break;
        }
        case Token::KW_const:
        {
            AST *c;
            c = parseConst();
            if (c)
                data.push_back(c);
            else
            {
                goto _error;
            }
            break;
        }
        default:
        {
            

            goto _error;
            break;
        }
        }
        advance();
    }

    return new Program(data);
_error:
    while (Tok.getKind() != Token::eoi)
        advance();
    return nullptr;
}

DeclarationInt *Parser::parseIntDec(bool isConst)
{
    Expr *E = nullptr;
    llvm::SmallVector<llvm::StringRef> Vars;
    llvm::SmallVector<Expr *> Values;

    // Ensure the current token is 'int'
    if (expect(Token::KW_int))
    {
        goto _error;
    }
    advance();

    // Parse the first identifier
    if (expect(Token::ident))
    {
        goto _error;
    }
    Vars.push_back(Tok.getText());
    advance();

    // Parse additional identifiers separated by commas
    while (Tok.is(Token::comma))
    {
        advance();
        if (expect(Token::ident))
        {
            goto _error;
        }
        Vars.push_back(Tok.getText());
        advance();
    }

    // If an assignment operator is present, parse the expressions
    if (Tok.is(Token::assign))
    {
        advance();

        // TernaryAssignment *t_assign = parseTernaryAssign();
        // if (t_assign)
        // {
        //     Values.push_back(t_assign);
        //     advance();
        // }
        // Parse the first expression


        // Token prev_token = Tok;
        // const char *prev_buffer = Lex.getBuffer();
        // TernaryAssignment *t_assign = parseTernaryAssign();
        // if (t_assign)
        // {
        //     Values.push_back(t_assign);
        // }
        // else
        // {
        //     // If parsing failed or no semicolon, reset lexer state
        //     Tok = prev_token;
        //     Lex.setBufferPtr(prev_buffer);
        E = parseExpr();
        if (E)
        {
            Values.push_back(E);
        }
        else
        {
            goto _error;
        }
        // }


        

        // Parse additional expressions separated by commas
        while (Tok.is(Token::comma))
        {
            // Token prev_token = Tok;
            // const char *prev_buffer = Lex.getBuffer();
            // TernaryAssignment *t_assign = parseTernaryAssign();
            // if (t_assign)
            // {
            //     Values.push_back(t_assign);
            // }
            // else
            // {
            //     // If parsing failed or no semicolon, reset lexer state
            //     Tok = prev_token;
            //     Lex.setBufferPtr(prev_buffer);
            advance();
            E = parseExpr();
            if (E)
            {
                Values.push_back(E);
            }
            else
            {
                goto _error;
            }
            // }
        }
    }

    // Assign default values to remaining variables if needed
    while (Values.size() < Vars.size())
    {
        // Assign default value (e.g., 0) to variables without assigned values
        Values.push_back(new Final(Final::Number, llvm::StringRef("0")));
    }

    // Check that we don't have more values than variables
    if (Values.size() > Vars.size())
    {
        llvm::errs() << "Error: More values than variables in declaration.\n";
        goto _error;
    }

    // Expect a semicolon at the end
    if (expect(Token::semicolon))
    {
        goto _error;
    }

    return new DeclarationInt(Vars, Values, isConst);

_error:
    while (Tok.getKind() != Token::eoi)
        advance();
    return nullptr;
}

DeclarationBool *Parser::parseBoolDec(bool isConst)
{
    Logic *L = nullptr;
    llvm::SmallVector<llvm::StringRef> Vars;
    llvm::SmallVector<Logic *> Values;

    // Ensure the current token is 'bool'
    if (expect(Token::KW_bool))
    {
        goto _error;
    }
    advance();

    // Parse the first identifier
    if (expect(Token::ident))
    {
        goto _error;
    }
    Vars.push_back(Tok.getText());
    advance();

    // Parse additional identifiers separated by commas
    while (Tok.is(Token::comma))
    {
        advance();
        if (expect(Token::ident))
        {
            goto _error;
        }
        Vars.push_back(Tok.getText());
        advance();
    }

    // If an assignment operator is present, parse the expressions
    if (Tok.is(Token::assign))
    {
        advance();

        // Parse the first logic expression
        L = parseLogic();
        if (L)
        {
            Values.push_back(L);
        }
        else
        {
            goto _error;
        }

        // Parse additional logic expressions separated by commas
        while (Tok.is(Token::comma))
        {
            advance();
            L = parseLogic();
            if (L)
            {
                Values.push_back(L);
            }
            else
            {
                goto _error;
            }
        }
    }

    // Assign default values to remaining variables if needed
    while (Values.size() < Vars.size())
    {
        // Assign default value (e.g., false) to variables without assigned values
        Values.push_back(new Comparison(nullptr, nullptr, Comparison::False));
    }

    // Check that we don't have more values than variables
    if (Values.size() > Vars.size())
    {
        llvm::errs() << "Error: More values than variables in boolean declaration.\n";
        goto _error;
    }

    // Expect a semicolon at the end
    if (expect(Token::semicolon))
    {
        goto _error;
    }

    return new DeclarationBool(Vars, Values, isConst);

_error:
    while (Tok.getKind() != Token::eoi)
        advance();
    return nullptr;
}

// new:
DeclarationFloat *Parser::parseFloatDec(bool isConst)
{
    Expr *E = nullptr;
    llvm::SmallVector<llvm::StringRef> Vars;
    llvm::SmallVector<Expr *> Values;

    // Ensure the current token is 'float'
    if (expect(Token::KW_float))
    {
        goto _error;
    }
    advance();

    // Parse the first identifier
    if (expect(Token::ident))
    {
        goto _error;
    }
    Vars.push_back(Tok.getText());
    advance();

    // Parse additional identifiers separated by commas
    while (Tok.is(Token::comma))
    {
        advance();
        if (expect(Token::ident))
        {
            goto _error;
        }
        Vars.push_back(Tok.getText());
        advance();
    }

    // If an assignment operator is present, parse the expressions
    if (Tok.is(Token::assign))
    {
        advance();

        // Parse the first expression
        E = parseExpr();
        if (E)
        {
            Values.push_back(E);
        }
        else
        {
            goto _error;
        }

        // Parse additional expressions separated by commas
        while (Tok.is(Token::comma))
        {
            advance();
            E = parseExpr();
            if (E)
            {
                Values.push_back(E);
            }
            else
            {
                goto _error;
            }
        }
    }

    // Assign default values to remaining variables if needed
    while (Values.size() < Vars.size())
    {
        // Assign default value (e.g., 0.0) to variables without assigned values
        Values.push_back(new Final(Final::FloatNumber, llvm::StringRef("0.0")));
    }

    // Check that we don't have more values than variables
    if (Values.size() > Vars.size())
    {
        llvm::errs() << "Error: More values than variables in float declaration.\n";
        goto _error;
    }

    // Expect a semicolon at the end
    if (expect(Token::semicolon))
    {
        goto _error;
    }

    return new DeclarationFloat(Vars, Values, isConst);

_error:
    while (Tok.getKind() != Token::eoi)
        advance();
    return nullptr;
}

// new:
DeclarationVar *Parser::parseVarDec(bool isConst)
{
    llvm::SmallVector<llvm::StringRef> Vars;
    llvm::SmallVector<AST *> Values;
    llvm::SmallVector<TypeKind> Types;

    // Ensure the current token is 'var'
    if (expect(Token::KW_var))
    {
        goto _error;
    }
    advance();

    // Parse the variable names
    if (expect(Token::ident))
    {
        goto _error;
    }
    Vars.push_back(Tok.getText());
    advance();

    while (Tok.is(Token::comma))
    {
        advance();
        if (expect(Token::ident))
        {
            goto _error;
        }
        Vars.push_back(Tok.getText());
        advance();
    }

    // If an assignment operator is present, parse the expressions or logic
    if (Tok.is(Token::assign))
    {
        advance();

        // Parse the first value (could be Expr or Logic)
        AST *Value = parseValue();
        if (Value)
        {
            Values.push_back(Value);
        }
        else
        {
            goto _error;
        }

        // Parse additional values separated by commas
        while (Tok.is(Token::comma))
        {
            advance();
            Value = parseValue();
            if (Value)
            {
                Values.push_back(Value);
            }
            else
            {
                goto _error;
            }
        }
    }

    // Assign default values to remaining variables if needed
    while (Values.size() < Vars.size())
    {
        // Assign default value (e.g., 0) to variables without assigned values
        Values.push_back(new Final(Final::Number, llvm::StringRef("0")));
    }

    // Error if more values than variables
    if (Values.size() > Vars.size())
    {
        llvm::errs() << "Error: More values than variables in 'var' declaration.\n";
        goto _error;
    }

    // Infer types for each value
    for (size_t i = 0; i < Values.size(); ++i)
    {
        TypeKind type = inferType(Values[i]);
        if (type == TypeKind::Unknown)
        {
            llvm::errs() << "Error: Unable to infer type for variable '" << Vars[i] << "'.\n";
            goto _error;
        }
        Types.push_back(type);
    }

    // Expect a semicolon at the end
    if (expect(Token::semicolon))
    {
        goto _error;
    }

    return new DeclarationVar(Vars, Values, Types, isConst);

_error:
    while (Tok.getKind() != Token::eoi)
        advance();
    return nullptr;
}

AST *Parser::parseConst()
{
    // Ensure the current token is 'const'
    if (expect(Token::KW_const))
    {
        goto _error;
    }
    advance();

    // Switch based on the next token
    switch (Tok.getKind())
    {
    case Token::KW_int:
    {
        DeclarationInt *d = parseIntDec(true);
        if (d)
            return d;
        else
            goto _error;
        break;
    }
    case Token::KW_float:
    {
        DeclarationFloat *d = parseFloatDec(true);
        if (d)
            return d;
        else
            goto _error;
        break;
    }
    case Token::KW_bool:
    {
        DeclarationBool *d = parseBoolDec(true);
        if (d)
            return d;
        else
            goto _error;
        break;
    }
    case Token::KW_var:
    {
        DeclarationVar *d = parseVarDec(true);
        if (d)
            return d;
        else
            goto _error;
        break;
    }
    default:
    {
        
        goto _error;
        break;
    }
    }

_error:
    while (Tok.getKind() != Token::eoi)
        advance();
    return nullptr;
}

// new:
DeclareDefine *Parser::parseDefineDec()
{
    llvm::StringRef Name;
    Expr *Value = nullptr;

    // Ensure the current token is '#define'
    if (expect(Token::KW_define))
    {
        goto _error;
    }
    advance();

    // Parse the identifier
    if (expect(Token::ident))
    {
        goto _error;
    }
    Name = Tok.getText();
    advance();

    // Parse the value
    if (!expect(Token::number))
    {
        Value = new Final(Final::Number, Tok.getText());
    }
    else if ((!expect(Token::floatNumber)))
    {
        Value = new Final(Final::FloatNumber, Tok.getText());
    }
    else if ((!expect(Token::KW_true)) || (!expect(Token::KW_false)))
    {
        Value = new Final(Final::Bool, Tok.getText());
    }
    else if ((!expect(Token::ident)))
    {
        Value = new Final(Final::Ident, Tok.getText());
    }
    else
    {
        goto _error;
    }

    return new DeclareDefine(Name, Value);

_error:
    while (Tok.getKind() != Token::eoi)
        advance();
    return nullptr;
}

Assignment *Parser::parseBoolAssign()
{
    Final *F = nullptr;
    Assignment::AssignKind AK;
    Logic *L = nullptr;

    F = dynamic_cast<Final *>(parseFinal());
    if (F == nullptr)
    {
        goto _error;
    }

    if (Tok.is(Token::assign))
    {
        AK = Assignment::Assign;
        advance();
        L = parseLogic(); // check if expr is logical

        if (L)
        {
            if (!Tok.is(Token::semicolon))
            {
                goto _error;
            }
            return new Assignment(F, L, AK);
        }
        else
            goto _error;
    }
    else
    {
        goto _error;
    }

_error:
    while (Tok.getKind() != Token::eoi)
        advance();
    return nullptr;
}

Assignment *Parser::parseIntAssign()
{
    Expr *E = nullptr;
    Final *F = nullptr;
    Assignment::AssignKind AK;

    F = dynamic_cast<Final *>(parseFinal());
    if (F == nullptr)
    {
        goto _error;
    }

    if (Tok.is(Token::assign))
    {
        AK = Assignment::Assign;
    }
    else if (Tok.is(Token::plus_assign))
    {
        AK = Assignment::Plus_assign;
    }
    else if (Tok.is(Token::minus_assign))
    {
        AK = Assignment::Minus_assign;
    }
    else if (Tok.is(Token::star_assign))
    {
        AK = Assignment::Star_assign;
    }
    else if (Tok.is(Token::slash_assign))
    {
        AK = Assignment::Slash_assign;
    }
    else if (Tok.is(Token::mod_assign))
    {
        AK = Assignment::Mod_assign;
    }
    else
    {
        goto _error;
    }
    advance();

    E = parseExpr(); // Now handles both integer and float expressions
    if (E)
    {
        return new Assignment(F, E, AK);
    }
    else
    {
        goto _error;
    }

_error:
    while (Tok.getKind() != Token::eoi)
        advance();
    return nullptr;
}

// done
Assignment *Parser::parseFloatAssign()
{
    Expr *E = nullptr;
    Final *F = nullptr;
    Assignment::AssignKind AK;

    F = dynamic_cast<Final *>(parseFinal());
    if (F == nullptr)
    {
        goto _error;
    }

    if (Tok.is(Token::assign))
    {
        AK = Assignment::Assign;
    }
    else if (Tok.is(Token::plus_assign))
    {
        AK = Assignment::Plus_assign;
    }
    else if (Tok.is(Token::minus_assign))
    {
        AK = Assignment::Minus_assign;
    }
    else if (Tok.is(Token::star_assign))
    {
        AK = Assignment::Star_assign;
    }
    else if (Tok.is(Token::slash_assign))
    {
        AK = Assignment::Slash_assign;
    }
    else if (Tok.is(Token::mod_assign))
    {
        AK = Assignment::Mod_assign;
    }
    else
    {
        goto _error;
    }
    advance();

    E = parseExpr(); // Now handles both integer and float expressions
    if (E)
    {
        return new Assignment(F, E, AK);
    }
    else
    {
        goto _error;
    }

_error:
    while (Tok.getKind() != Token::eoi)
        advance();
    return nullptr;
}

// done
Assignment *Parser::parseVarAssign()
{
    Expr *Value1 = nullptr; // Use AST* to accommodate both Expr* and Logic*
    Logic *Value2 = nullptr;
    Final *F = nullptr;
    Assignment::AssignKind AK;
    Token prev_token;
    const char *prev_buffer;
    Expr *E = nullptr;

    F = dynamic_cast<Final *>(parseFinal());
    if (F == nullptr)
    {
        goto _error;
    }

    if (Tok.is(Token::assign))
    {
        AK = Assignment::Assign;
    }
    else if (Tok.is(Token::plus_assign))
    {
        AK = Assignment::Plus_assign;
    }
    else if (Tok.is(Token::minus_assign))
    {
        AK = Assignment::Minus_assign;
    }
    else if (Tok.is(Token::star_assign))
    {
        AK = Assignment::Star_assign;
    }
    else if (Tok.is(Token::slash_assign))
    {
        AK = Assignment::Slash_assign;
    }
    else if (Tok.is(Token::mod_assign))
    {
        AK = Assignment::Mod_assign;
    }
    else
    {
        goto _error;
    }
    advance();

    // Try parsing an arithmetic expression first
    prev_token = Tok;
    prev_buffer = Lex.getBuffer();

    E = parseExpr();
    if (E)
    {
        Value1 = E;
    }
    else
    {
        // Reset lexer state and try parsing a logical expression
        Tok = prev_token;
        Lex.setBufferPtr(prev_buffer);

        Logic *L = parseLogic();
        if (L)
        {
            Value2 = L;
        }
        else
        {
            goto _error;
        }
    }

    if (Value1)
    {
        return new Assignment(F, Value1, AK);
    }
    else if(Value2){
        return new Assignment(F, Value2, AK);
    }
    else {
        goto _error;
    }

_error:
    while (Tok.getKind() != Token::eoi)
        advance();
    return nullptr;
}

// done
TernaryAssignment *Parser::parseTernaryAssign()
{
    Final *F = nullptr;
    Expr *E1 = nullptr;
    Expr *E2 = nullptr;
    Logic *Condition = nullptr;
    Assignment::AssignKind AK;

    F = dynamic_cast<Final *>(parseFinal());
    if (F == nullptr)
    {
        goto _error;
    }

    if (Tok.is(Token::assign))
    {
        AK = Assignment::Assign;
        advance();

        Condition = parseLogic();
        if (Condition == nullptr)
            goto _error;

        if (expect(Token::questionMark))
            goto _error;
        advance();

        E1 = parseExpr();
        if (E1 == nullptr)
            goto _error;

        if (expect(Token::colonMark))
            goto _error;
        advance();

        E2 = parseExpr();
        if (E2 == nullptr)
            goto _error;

        if (expect(Token::semicolon))
            goto _error; // Consume semicolon here
        advance();

        return new TernaryAssignment(F, Condition, E1, E2);
    }
    else
    {
        goto _error;
    }

_error:
    llvm::errs() << "test\n";
    while (Tok.getKind() != Token::eoi)
        advance();
    return nullptr;
}

UnaryOp *Parser::parseUnary()
{
    UnaryOp *Res = nullptr;
    llvm::StringRef var;

    if (expect(Token::ident))
    {
        goto _error;
    }

    var = Tok.getText();
    advance();
    if (Tok.getKind() == Token::plus_plus)
    {
        Res = new UnaryOp(UnaryOp::Plus_plus, var);
    }
    else if (Tok.getKind() == Token::minus_minus)
    {
        Res = new UnaryOp(UnaryOp::Minus_minus, var);
    }
    else
    {
        goto _error;
    }

    advance();

    return Res;

_error:
    while (Tok.getKind() != Token::eoi)
        advance();
    return nullptr;
}

Expr *Parser::parseExpr()
{
    Expr *Left = parseTerm();

    if (Left == nullptr)
    {
        goto _error;
    }

    while (Tok.isOneOf(Token::plus, Token::minus))
    {
        BinaryOp::Operator Op;
        if (Tok.is(Token::plus))
            Op = BinaryOp::Plus;
        else if (Tok.is(Token::minus))
            Op = BinaryOp::Minus;
        else
        {
           
            goto _error;
        }
        advance();
        Expr *Right = parseTerm();
        if (Right == nullptr)
        {
            goto _error;
        }
        Left = new BinaryOp(Op, Left, Right);
    }
    return Left;

_error:
    while (Tok.getKind() != Token::eoi)
        advance();
    return nullptr;
}

Expr *Parser::parseTerm()
{
    Expr *Left = parseFactor();
    if (Left == nullptr)
    {
        goto _error;
    }
    while (Tok.isOneOf(Token::star, Token::mod, Token::slash))
    {
        BinaryOp::Operator Op;
        if (Tok.is(Token::star))
            Op = BinaryOp::Mul;
        else if (Tok.is(Token::slash))
            Op = BinaryOp::Div;
        else if (Tok.is(Token::mod))
            Op = BinaryOp::Mod;
        else
        {
          

            goto _error;
        }
        advance();
        Expr *Right = parseFactor();
        if (Right == nullptr)
        {
            goto _error;
        }
        Left = new BinaryOp(Op, Left, Right);
    }
    return Left;

_error:
    while (Tok.getKind() != Token::eoi)
        advance();
    return nullptr;
}

Expr *Parser::parseFactor()
{
    Expr *Left = parseFinal();
    if (Left == nullptr)
    {
        goto _error;
    }
    while (Tok.is(Token::exp))
    {
        BinaryOp::Operator Op;
        if (Tok.is(Token::exp))
            Op = BinaryOp::Exp;
        else
        {
           
            goto _error;
        }
        advance();
        Expr *Right = parseFactor();
        if (Right == nullptr)
        {
            goto _error;
        }
        Left = new BinaryOp(Op, Left, Right);
    }
    return Left;

_error:
    while (Tok.getKind() != Token::eoi)
        advance();
    return nullptr;
}

Expr *Parser::parseFinal()  
{
    Expr *Res = nullptr;
    switch (Tok.getKind())
    {
    case Token::number:
    {
        Res = new Final(Final::Number, Tok.getText());
        advance();
        break;
    }
    case Token::floatNumber: // Handle float literals
    {
        Res = new Final(Final::FloatNumber, Tok.getText());
        advance();
        break;
    }
    case Token::ident:
    {
        Res = new Final(Final::Ident, Tok.getText());
        Token prev_tok = Tok;
        const char* prev_buffer = Lex.getBuffer();
        Expr* u = parseUnary();
        if(u)
            return u;
        else{
            Tok = prev_tok;
            Lex.setBufferPtr(prev_buffer);
            advance();
        }
        break;
    }

    case Token::plus:{
        advance();
        if(Tok.getKind() == Token::number || Tok.getKind() == Token::floatNumber){
            Res = new SignedNumber(SignedNumber::Plus, Tok.getText());
            advance();
            break;
        }
        goto _error;
    }
    case Token::minus:{
        advance();
        if (Tok.getKind() == Token::number || Tok.getKind() == Token::floatNumber){
            Res = new SignedNumber(SignedNumber::Minus, Tok.getText());
            advance();
            break;
        }
        goto _error;
    }
    case Token::minus_paren:{
        advance();
        Expr *math_expr = parseExpr();
        if(math_expr == nullptr)
            goto _error;
        Res = new NegExpr(math_expr);
        if (!consume(Token::r_paren))
            break;
        
        goto _error;

    }
    case Token::l_paren:
    {
        advance();
        Res = parseExpr();
        if (Res == nullptr)
        {
            goto _error;
        }
        if (expect(Token::r_paren))
            goto _error;
        advance();
        break;
    }
    // Handle casting and parentheses as before
    case Token::KW_int:
    case Token::KW_float:
    case Token::KW_bool:
    {
        // Handle casting
        // llvm::errs() << "try casting\n";
        Token::TokenKind CastTypeToken = Tok.getKind();
        advance();
        if (expect(Token::l_paren)){
            // llvm::errs() << "no l_paren\n";
            // llvm::errs() << "token: " << Tok.getKind() <<"\n";
            goto _error;
        }
        advance();
        // llvm::errs() << "parse\n";
        AST *Inner = nullptr;
        if (CastTypeToken == Token::KW_int || CastTypeToken == Token::KW_float)
        {
            Inner = parseExpr();
        }
        else if (CastTypeToken == Token::KW_bool)
        {
            Inner = parseLogic();
        }
        else
        {
            // llvm::errs() << "wrong type\n";
            goto _error;
        }
        // llvm::errs() << "parse_done\n";
        if (!Inner){
            // llvm::errs() << "inner not null\n"; 
            goto _error;
        }
        if (expect(Token::r_paren)){
            // llvm::errs() << "no r_paren\n";
            goto _error;
        }
        advance();
        // llvm::errs() << "final\n";
        CastExpr::CastType CastType;
        if (CastTypeToken == Token::KW_int)
            CastType = CastExpr::IntCast;
        else if (CastTypeToken == Token::KW_float)
            CastType = CastExpr::FloatCast;
        else if (CastTypeToken == Token::KW_bool)
            CastType = CastExpr::BoolCast;
        else{
            // llvm::errs() << "wrong type 2\n";
            goto _error;
        }
        Res = new CastExpr(CastType, Inner);
        // llvm::errs() << "done casting\n";   
        break;
    }        
    case Token::KW_min:
    {
        MinStmt *m;
        m = parseMin();
        if (m)
            Res = m;
        else
            goto _error;
        break;
    }
    case Token::KW_max:
    {
        MaxStmt *m;
        m = parseMax();
        if (m)
            Res = m;
        else
            goto _error;
        break;
    }
    case Token::KW_mean:
    {
        MeanStmt *mean;
        mean = parseMean();
        if (mean)
            Res = mean;
        else
            goto _error;
        break;
    }
    case Token::KW_sqrtN:
    {
        SqrtNStmt *sqrtN;
        sqrtN = parseSqrtN();
        if (sqrtN)
            Res = sqrtN;
        else
            goto _error;
        break;
    }


    
    default:
    {
        llvm::errs() << Tok.getKind() << "\n"; 
        llvm::errs() << "default?\n"; 
        goto _error;
    }
    }
    return Res;

_error:
    llvm::errs() << "error while casting\n";       
    while (Tok.getKind() != Token::eoi)
        advance();
    return nullptr;
}

Logic *Parser::parseComparison()
{
    Logic *Res = nullptr;
    Final *Ident = nullptr;
    Expr *Left = nullptr;
    Expr *Right = nullptr;
    Token prev_Tok;
    Logic *temp = nullptr;
    const char *prev_buffer;
    if (Tok.is(Token::l_paren))
    {
        advance();
        Res = parseLogic();
        if (Res == nullptr)
        {
            goto _error;
        }
        if (consume(Token::r_paren))
            goto _error;
    }
    else
    {
        if (Tok.is(Token::KW_true))
        {
            Res = new Comparison(nullptr, nullptr, Comparison::True);
            advance();
            return Res;
        }
        else if (Tok.is(Token::KW_false))
        {
            Res = new Comparison(nullptr, nullptr, Comparison::False);
            advance();
            return Res;
        }
        else if (Tok.is(Token::number) && (Tok.getText() == "0" || Tok.getText() == "1"))
        {
            if (Tok.getText() == "1")
            {
                Res = new Comparison(nullptr, nullptr, Comparison::True);
            }
            else
            {
                Res = new Comparison(nullptr, nullptr, Comparison::False);
            }
            advance();
            return Res;
        }
        else if (Tok.is(Token::ident))
        {
            Ident = new Final(Final::Ident, Tok.getText());
        }
        else if (Tok.is(Token::KW_bool))
        {
            // Handle casting
            // llvm::errs() << "try casting\n";
            Token::TokenKind CastTypeToken = Tok.getKind();
            advance();
            if (expect(Token::l_paren)){
                // llvm::errs() << "no l_paren\n";
                // llvm::errs() << "token: " << Tok.getKind() <<"\n";
                goto _error;
            }
            advance();
            // llvm::errs() << "parse\n";
            AST *Inner = nullptr;
            if (CastTypeToken == Token::KW_int || CastTypeToken == Token::KW_float)
            {
                Inner = parseExpr();
            }
            else if (CastTypeToken == Token::KW_bool)
            {
                Inner = parseLogic();
            }
            else
            {
                // llvm::errs() << "wrong type\n";
                goto _error;
            }
            // llvm::errs() << "parse_done\n";
            if (!Inner){
                // llvm::errs() << "inner not null\n"; 
                goto _error;
            }
            if (expect(Token::r_paren)){
                // llvm::errs() << "no r_paren\n";
                goto _error;
            }
            advance();
            // llvm::errs() << "final\n";
            BoolCastExpr::CastType CastType;
            if (CastTypeToken == Token::KW_bool)
                CastType = BoolCastExpr::BoolCast;
            else{
                // llvm::errs() << "wrong type 2\n";
                goto _error;
            }
            Res = new BoolCastExpr(CastType, Inner);
            // llvm::errs() << "done casting\n";   
            return Res;
        }
        prev_Tok = Tok;
        prev_buffer = Lex.getBuffer();
        Left = parseExpr();
        if (Left == nullptr)
            goto _error;

        Comparison::Operator Op;
        if (Tok.is(Token::eq))
            Op = Comparison::Equal;
        else if (Tok.is(Token::neq))
            Op = Comparison::Not_equal;
        else if (Tok.is(Token::gt))
            Op = Comparison::Greater;
        else if (Tok.is(Token::lt))
            Op = Comparison::Less;
        else if (Tok.is(Token::gte))
            Op = Comparison::Greater_equal;
        else if (Tok.is(Token::lte))
            Op = Comparison::Less_equal;
        else if (Tok.is(Token::KW_in))
        {
            advance();

            if (expect(Token::l_bracket))
            {
                goto _error;
            }

            advance();

            Right = parseExpr();

            if (Right == nullptr)
            {
                goto _error;
            }

            Res = new Comparison(Left, Right, Comparison::Equal);

            while (!expect(Token::comma))
            {
                advance();

                Right = parseExpr();

                if (Right == nullptr)
                {
                    goto _error;
                }

                temp = new Comparison(Left, Right, Comparison::Equal);

                Res = new LogicalExpr(Res, temp, LogicalExpr::Or);
            }

            if (expect(Token::r_bracket))
            {
                goto _error;
            }

            advance();

            return Res;
        }
        else if (Tok.is(Token::KW_not))
        {
            advance();

            if (expect(Token::KW_in))
            {
                goto _error;
            }

            advance();

            if (expect(Token::l_bracket))
            {
                goto _error;
            }

            advance();

            Right = parseExpr();

            if (Right == nullptr)
            {
                goto _error;
            }

            Res = new Comparison(Left, Right, Comparison::Not_equal);

            while (Tok.is(Token::comma))
            {
                advance();

                Right = parseExpr();

                if (Right == nullptr)
                {
                    goto _error;
                }

                temp = new Comparison(Left, Right, Comparison::Not_equal);

                Res = new LogicalExpr(Res, temp, LogicalExpr::And);
            }

            if (expect(Token::r_bracket))
            {
                goto _error;
            }

            advance();

            return Res;
        }
        else
        {
            if (Ident)
            {
                Tok = prev_Tok;
                Lex.setBufferPtr(prev_buffer);
                Res = new Comparison(Ident, nullptr, Comparison::Ident);
                advance();
                return Res;
            }
            goto _error;
        }
        advance();
        Right = parseExpr();
        if (Right == nullptr)
        {
            goto _error;
        }

        Res = new Comparison(Left, Right, Op);
    }

    // if (x in [1,2,3])

    return Res;

_error:
    while (Tok.getKind() != Token::eoi)
        advance();
    return nullptr;
}

Logic *Parser::parseLogic()
{
    Logic *Left = parseComparison();
    Logic *Right;
    if (Left == nullptr)
    {
            // TODO: might be a problem
            goto _error;
    }
    while (Tok.isOneOf(Token::KW_and, Token::KW_or, Token::KW_xor))
    {
        LogicalExpr::Operator Op;
        if (Tok.is(Token::KW_and))
            Op = LogicalExpr::And;
        else if (Tok.is(Token::KW_or))
            Op = LogicalExpr::Or;
        else if (Tok.is(Token::KW_xor)) // Handle xor token
            Op = LogicalExpr::Xor;
        else
        {
       
            goto _error;
        }
        advance();
        Right = parseComparison();
        if (Right == nullptr)
        {
            goto _error;
        }
        Left = new LogicalExpr(Left, Right, Op);
    }
    return Left;

_error:
    while (Tok.getKind() != Token::eoi)
        advance();
    return nullptr;
}

IfStmt *Parser::parseIf()
{
    llvm::SmallVector<AST *> ifStmts;
    llvm::SmallVector<AST *> elseStmts;
    llvm::SmallVector<elifStmt *> elifStmts;
    llvm::SmallVector<AST *> Stmts;
    Logic *Cond = nullptr;
    Token prev_token_if;
    const char *prev_buffer_if;
    Token prev_token_elif;
    const char *prev_buffer_elif;
    bool hasElif = false;
    bool hasElse = false;

    if (expect(Token::KW_if))
    {
        goto _error;
    }

    advance();

    if (expect(Token::l_paren))
    {
        goto _error;
    }

    advance();

    Cond = parseLogic();
    if (Cond == nullptr)
    {
        goto _error;
    }

    if (expect(Token::r_paren))
    {
        goto _error;
    }

    advance();

    if (expect(Token::l_brace))
    {
        goto _error;
    }

    advance();

    ifStmts = getBody();

    if (ifStmts.empty())
        goto _error;

    prev_token_if = Tok;
    prev_buffer_if = Lex.getBuffer();

    advance();

    while (true)
    {
        if (Tok.is(Token::KW_else))
        {
            advance();
            if (Tok.is(Token::KW_if))
            {
                hasElif = true;
                advance();

                if (expect(Token::l_paren))
                {
                    goto _error;
                }

                advance();

                Logic *Cond = parseLogic();

                if (Cond == nullptr)
                {
                    goto _error;
                }

                if (expect(Token::r_paren))
                {
                    goto _error;
                }

                advance();

                if (expect(Token::l_brace))
                {
                    goto _error;
                }

                advance();

                Stmts = getBody();
                prev_token_elif = Tok;
                prev_buffer_elif = Lex.getBuffer();

                if (!Stmts.empty())
                    advance();
                else
                    goto _error;

                elifStmt *elif = new elifStmt(Cond, Stmts);
                elifStmts.push_back(elif);
            }
            else
            {
                hasElse = true;

                if (expect(Token::l_brace))
                {
                    goto _error;
                }

                advance();

                elseStmts = getBody();

                if (elseStmts.empty())
                    goto _error;

                break;
            }
        }
        else
            break;
    }

    if (hasElif && !hasElse)
    {
        Tok = prev_token_elif;
        Lex.setBufferPtr(prev_buffer_elif);
    }
    else if (!hasElif && !hasElse)
    {
        Tok = prev_token_if;
        Lex.setBufferPtr(prev_buffer_if);
    }

    return new IfStmt(Cond, ifStmts, elseStmts, elifStmts);

_error:
    while (Tok.getKind() != Token::eoi)
        advance();
    return nullptr;
}

PrintStmt *Parser::parsePrint()
{
    // llvm::StringRef Var;
    Expr *Value = nullptr;
    if (expect(Token::KW_print))
    {
        goto _error;
    }
    advance();
    if (expect(Token::l_paren))
    {
        goto _error;
    }
    advance();
    // if (expect(Token::ident))
    // {
    //     goto _error;
    // }
    // Var = Tok.getText();
    Value = parseExpr();
    // if (!expect(Token::number))
    // {
    //     Value = new Final(Final::Number, Tok.getText());
    // }
    // else if ((!expect(Token::floatNumber)))
    // {
    //     Value = new Final(Final::FloatNumber, Tok.getText());
    // }
    // else if ((!expect(Token::KW_true)) || (!expect(Token::KW_false)))
    // {
    //     Value = new Final(Final::Bool, Tok.getText());
    // }
    // else if ((!expect(Token::ident)))
    // {
    //     Value = new Final(Final::Ident, Tok.getText());
    // }
    // else
    // {
    //     goto _error;
    // }

    // advance();
    if (expect(Token::r_paren))
    {
        goto _error;
    }
    advance();
    if (expect(Token::semicolon))
    {
        goto _error;
    }
    return new PrintStmt(Value);

_error:
    while (Tok.getKind() != Token::eoi)
        advance();
    return nullptr;
}

WhileStmt *Parser::parseWhile()
{
    llvm::SmallVector<AST *> Body;
    Logic *Cond = nullptr;

    if (expect(Token::KW_while))
    {
        goto _error;
    }

    advance();

    if (expect(Token::l_paren))
    {
        goto _error;
    }

    advance();

    Cond = parseLogic();
    if (Cond == nullptr)
    {
        goto _error;
    }
    if (expect(Token::r_paren))
    {
        goto _error;
    }

    advance();

    if (expect(Token::l_brace))
    {
        goto _error;
    }

    advance();

    Body = getBody();
    if (Body.empty())
        goto _error;

    return new WhileStmt(Cond, Body);

_error:
    while (Tok.getKind() != Token::eoi)
        advance();
    return nullptr;
}

ForStmt *Parser::parseFor()
{
    Assignment *First = nullptr;
    Logic *Second = nullptr;
    Assignment *ThirdAssign = nullptr;
    UnaryOp *ThirdUnary = nullptr;
    llvm::SmallVector<AST *> Body;
    Token prev_token;
    const char *prev_buffer;

    if (expect(Token::KW_for))
    {
        goto _error;
    }

    advance();

    if (expect(Token::l_paren))
    {
        goto _error;
    }

    advance();

    First = parseIntAssign();

    if (First == nullptr)
        goto _error;

    if (First->getAssignKind() != Assignment::Assign) // The first part can only have a '=' sign
        goto _error;

    if (expect(Token::semicolon))
    {
        goto _error;
    }

    advance();

    Second = parseLogic();

    if (Second == nullptr)
        goto _error;

    if (expect(Token::semicolon))
    {
        goto _error;
    }

    advance();

    prev_token = Tok;
    prev_buffer = Lex.getBuffer();

    ThirdAssign = parseIntAssign();

    if (ThirdAssign == nullptr)
    {
        Tok = prev_token;
        Lex.setBufferPtr(prev_buffer);

        ThirdUnary = parseUnary();
        if (ThirdUnary == nullptr)
        {
            goto _error;
        }
    }
    else
    {
        if (ThirdAssign->getAssignKind() == Assignment::Assign) // The third part cannot have only '=' sign
            goto _error;
    }

    if (expect(Token::r_paren))
    {
        goto _error;
    }

    advance();

    if (expect(Token::l_brace))
    {
        goto _error;
    }

    advance();
    // llvm::errs() << "for pre body done\n";
    Body = getBody();

    if (Body.empty())
        goto _error;

    // llvm::errs() << "for body done\n";
    return new ForStmt(First, Second, ThirdAssign, ThirdUnary, Body);

_error:
    llvm::errs() << "ERROR IN FOR\n";
    while (Tok.getKind() != Token::eoi)
        advance();
    return nullptr;
}

// new:
DoWhileStmt *Parser::parseDoWhile()
{
    llvm::SmallVector<AST *> Body;
    Logic *Cond = nullptr;

    if (expect(Token::KW_do))
    {
        goto _error;
    }
    advance();

    if (expect(Token::l_brace))
    {
        goto _error;
    }
    advance();

    // llvm::errs() << "prebody\n";
    Body = getBody();
    if (Body.empty())
        goto _error;
    // llvm::errs() << "body done\n";

    advance(); // Consume '}'

    if (expect(Token::KW_while))
    {
        goto _error;
    }
    advance();

    if (expect(Token::l_paren))
    {
        goto _error;
    }
    advance();

    // llvm::errs() << "pre logic\n";
    Cond = parseLogic();
    if (Cond == nullptr)
    {
        goto _error;
    }

    // llvm::errs() << "logic done\n";
    
    if (expect(Token::r_paren))
    {
        // llvm::errs() << Tok.getKind() << "\n";
        goto _error;
    }
    advance();
    // llvm::errs() << "dowhile done\n";
    return new DoWhileStmt(Cond, Body);

_error:
    llvm::errs() << "error in dowhile";
    llvm::errs() << "\n";
    while (Tok.getKind() != Token::eoi)
        advance();
    return nullptr;
}

// new:

// switch (expression)
// {
// case
//     break;

// default:
//     break;
// }
SwitchStmt *Parser::parseSwitch()
{
    Expr *SwitchExpr = nullptr;
    llvm::SmallVector<CaseStmt *> Cases;
    DefaultStmt *DefaultCase = nullptr;

    if (expect(Token::KW_switch))
    {
        llvm::errs() << "error in KW_switch";
        goto _error;
    }
    advance();

    if (expect(Token::l_paren))
    {
        llvm::errs() << "error in l_paren";
        goto _error;
    }
    advance();

    SwitchExpr = parseExpr();
    if (SwitchExpr == nullptr)
    {
        llvm::errs() << "error in SwitchExpr";
        goto _error;
    }

    if (expect(Token::r_paren))
    {
        llvm::errs() << "error in r_paren";
        goto _error;
    }
    advance();

    if (expect(Token::l_brace))
    {
        llvm::errs() << "error in l_brace";
        goto _error;
    }
    advance();

    while (!Tok.is(Token::r_brace))
    {
        if (Tok.is(Token::KW_case))
        {
            advance();

            Expr *CaseExpr = parseExpr();
            if (CaseExpr == nullptr)
            {
                llvm::errs() << "error in CaseExpr";

                goto _error;
            }

            if (expect(Token::colonMark))
            {
                llvm::errs() << "error in colonMark-case";
                goto _error;
            }
            advance();

            llvm::SmallVector<AST *> Body = getCaseBody();

            Cases.push_back(new CaseStmt(CaseExpr, Body));
        }
        else if (Tok.is(Token::KW_default))
        {
            advance();

            if (expect(Token::colonMark))
            {
                llvm::errs() << "error in colonMark-default";
                goto _error;
            }
            advance();

            llvm::SmallVector<AST *> Body = getCaseBody();

            DefaultCase = new DefaultStmt(Body);
        }
        else
        {

            llvm::errs() << Tok.getKind() << "\n";
            llvm::errs() << "error bad key word\n";
            goto _error;
        }
    }

    if (expect(Token::r_brace))
    {
        llvm::errs() << "error in r_brace";
        goto _error;
    }

    advance();
    return new SwitchStmt(SwitchExpr, Cases, DefaultCase);

_error:
    while (Tok.getKind() != Token::eoi)
        advance();
    return nullptr;
}

// new:
MinStmt *Parser::parseMin()
{
    Expr *E1 = nullptr;
    Expr *E2 = nullptr;

    if (expect(Token::KW_min))
    {
        goto _error;
    }
    advance();

    if (expect(Token::l_paren))
    {
        goto _error;
    }
    advance();

    E1 = parseExpr();
    if (E1 == nullptr)
        goto _error;

    if (expect(Token::comma))
    {
        goto _error;
    }
    advance();

    E2 = parseExpr();
    if (E2 == nullptr)
        goto _error;

    if (expect(Token::r_paren))
    {
        goto _error;
    }
    advance();

    if (expect(Token::semicolon))
    {
        goto _error;
    }

    return new MinStmt(E1, E2);

_error:
    while (Tok.getKind() != Token::eoi)
        advance();
    return nullptr;
}

// new:
MaxStmt *Parser::parseMax()
{
    Expr *E1 = nullptr;
    Expr *E2 = nullptr;

    if (expect(Token::KW_max))
    {
        goto _error;
    }
    advance();

    if (expect(Token::l_paren))
    {
        goto _error;
    }
    advance();

    E1 = parseExpr();
    if (E1 == nullptr)
        goto _error;

    if (expect(Token::comma))
    {
        goto _error;
    }
    advance();

    E2 = parseExpr();
    if (E2 == nullptr)
        goto _error;

    if (expect(Token::r_paren))
    {
        goto _error;
    }
    advance();

    if (expect(Token::semicolon))
    {
        goto _error;
    }

    return new MaxStmt(E1, E2);

_error:
    while (Tok.getKind() != Token::eoi)
        advance();
    return nullptr;
}

// new:
MeanStmt *Parser::parseMean()
{
    Expr *E1 = nullptr;
    Expr *E2 = nullptr;

    if (expect(Token::KW_mean))
    {
        goto _error;
    }
    advance();

    if (expect(Token::l_paren))
    {
        goto _error;
    }
    advance();

    E1 = parseExpr();
    if (E1 == nullptr)
        goto _error;

    if (expect(Token::comma))
    {
        goto _error;
    }
    advance();

    E2 = parseExpr();
    if (E2 == nullptr)
        goto _error;

    if (expect(Token::r_paren))
    {
        goto _error;
    }
    advance();

    if (expect(Token::semicolon))
    {
        goto _error;
    }

    return new MeanStmt(E1, E2);

_error:
    while (Tok.getKind() != Token::eoi)
        advance();
    return nullptr;
}

// new:
SqrtNStmt *Parser::parseSqrtN()
{
    Expr *Base = nullptr;
    Expr *NthRoot = nullptr;

    if (expect(Token::KW_sqrtN))
    {
        goto _error;
    }
    advance();

    if (expect(Token::l_paren))
    {
        goto _error;
    }
    advance();

    Base = parseExpr();
    if (Base == nullptr)
        goto _error;

    if (expect(Token::comma))
    {
        goto _error;
    }
    advance();

    NthRoot = parseExpr();
    if (NthRoot == nullptr)
        goto _error;

    if (expect(Token::r_paren))
    {
        goto _error;
    }
    advance();

    if (expect(Token::semicolon))
    {
        goto _error;
    }

    return new SqrtNStmt(Base, NthRoot);

_error:
    while (Tok.getKind() != Token::eoi)
        advance();
    return nullptr;
}

llvm::SmallVector<AST *> Parser::getCaseBody()
{
    llvm::SmallVector<AST *> body;
    while (!Tok.is(Token::KW_case) && !Tok.is(Token::KW_default) && !Tok.is(Token::r_brace))
    {
        switch (Tok.getKind())
        {

        case Token::ident:
        {
            Token prev_token = Tok;
            const char *prev_buffer = Lex.getBuffer();

            // 1. Attempt to parse a unary operation
            UnaryOp *u = parseUnary();
            if (u && Tok.is(Token::semicolon))
            {
                body.push_back(u);
                break;
            }
            else
            {
                // Reset lexer state
                Tok = prev_token;
                Lex.setBufferPtr(prev_buffer);
            }

            // 2. Attempt to parse a ternary assignment
            TernaryAssignment *t_assign = parseTernaryAssign();
            if (t_assign && Tok.is(Token::semicolon))
            {
                body.push_back(t_assign);
                break;
            }
            else
            {
                // Reset lexer state
                Tok = prev_token;
                Lex.setBufferPtr(prev_buffer);
            }

            // 3. Attempt to parse a variable assignment
            Assignment *a_var = parseVarAssign();
            if (a_var && Tok.is(Token::semicolon))
            {
                body.push_back(a_var);
                break;
            }
            else
            {
                // Reset lexer state
                Tok = prev_token;
                Lex.setBufferPtr(prev_buffer);
            }

            // 4. Attempt to parse a float assignment
            Assignment *a_float = parseFloatAssign();
            if (a_float && Tok.is(Token::semicolon))
            {
                body.push_back(a_float);
                break;
            }
            else
            {
                // Reset lexer state
                Tok = prev_token;
                Lex.setBufferPtr(prev_buffer);
            }

            // 5. Attempt to parse a boolean assignment
            Assignment *a_bool = parseBoolAssign();
            if (a_bool && Tok.is(Token::semicolon))
            {
                body.push_back(a_bool);
                break;
            }
            else
            {
                // Reset lexer state
                Tok = prev_token;
                Lex.setBufferPtr(prev_buffer);
            }

            // 6. Attempt to parse an integer assignment
            Assignment *a_int = parseIntAssign();
            if (a_int && Tok.is(Token::semicolon))
            {
                body.push_back(a_int);
                break;
            }
            else
            {
                // Handle error if all parsing attempts fail
                goto _error;
            }

            break;
        }

        case Token::KW_if:
        {
            IfStmt *i;
            i = parseIf();
            if (i)
                body.push_back(i);
            else
                goto _error;

            break;
        }
        case Token::KW_while:
        {
            WhileStmt *w;
            w = parseWhile();
            if (w)
                body.push_back(w);
            else
            {
                goto _error;
            }
            break;
        }
        case Token::KW_for:
        {
            ForStmt *f;
            f = parseFor();
            if (f)
                body.push_back(f);
            else
            {
                goto _error;
            }
            break;
        }

        case Token::KW_do:
        {
            DoWhileStmt *d;
            d = parseDoWhile();
            if (d)
                body.push_back(d);
            else
                goto _error;
            break;
        }
        case Token::KW_switch:
        {
            SwitchStmt *s;
            s = parseSwitch();
            if (s)
                body.push_back(s);
            else
                goto _error;
            break;
        }
        case Token::KW_print:
        {
            PrintStmt *p;
            p = parsePrint();
            if (p)
                body.push_back(p);
            else
            {
                goto _error;
            }
            break;
        }
        case Token::KW_min:
        {
            MinStmt *m;
            m = parseMin();
            if (m)
                body.push_back(m);
            else
                goto _error;
            break;
        }
        case Token::KW_max:
        {
            MaxStmt *m;
            m = parseMax();
            if (m)
                body.push_back(m);
            else
                goto _error;
            break;
        }
        case Token::KW_mean:
        {
            MeanStmt *mean;
            mean = parseMean();
            if (mean)
                body.push_back(mean);
            else
                goto _error;
            break;
        }
        case Token::KW_sqrtN:
        {
            SqrtNStmt *sqrtN;
            sqrtN = parseSqrtN();
            if (sqrtN)
                body.push_back(sqrtN);
            else
                goto _error;
            break;
        }
        case Token::KW_break:
        {
            advance();
            if (expect(Token::semicolon))
            {
                goto _error;
            }
            BreakStmt *breaks;
            body.push_back(breaks);
            break;
        }
        default:
        {
           

            goto _error;
            break;
        }
        }
        advance();
    }
    if (Tok.is(Token::KW_case) || Tok.is(Token::KW_default) || Tok.is(Token::r_brace))
    {
        return body;
    }

_error:
    while (Tok.getKind() != Token::eoi)
        advance();
    return body;
}

llvm::SmallVector<AST *> Parser::getBody()
{
    llvm::SmallVector<AST *> body;
    while (!Tok.is(Token::r_brace))
    {
        // llvm::errs() << Tok.getKind() << "\n";
        switch (Tok.getKind())
        {

        case Token::ident:
        {
            Token prev_token = Tok;
            const char *prev_buffer = Lex.getBuffer();

            // 1. Attempt to parse a unary operation
            UnaryOp *u = parseUnary();
            if (u && Tok.is(Token::semicolon))
            {
                body.push_back(u);
                break;
            }
            else
            {
                // Reset lexer state
                Tok = prev_token;
                Lex.setBufferPtr(prev_buffer);
            }

            // 2. Attempt to parse a ternary assignment
            TernaryAssignment *t_assign = parseTernaryAssign();
            if (t_assign && Tok.is(Token::semicolon))
            {
                body.push_back(t_assign);
                break;
            }
            else
            {
                // Reset lexer state
                Tok = prev_token;
                Lex.setBufferPtr(prev_buffer);
            }

            // 3. Attempt to parse a variable assignment
            Assignment *a_var = parseVarAssign();
            if (a_var && Tok.is(Token::semicolon))
            {
                body.push_back(a_var);
                break;
            }
            else
            {
                // Reset lexer state
                Tok = prev_token;
                Lex.setBufferPtr(prev_buffer);
            }

            // 4. Attempt to parse a float assignment
            Assignment *a_float = parseFloatAssign();
            if (a_float && Tok.is(Token::semicolon))
            {
                body.push_back(a_float);
                break;
            }
            else
            {
                // Reset lexer state
                Tok = prev_token;
                Lex.setBufferPtr(prev_buffer);
            }

            // 5. Attempt to parse a boolean assignment
            Assignment *a_bool = parseBoolAssign();
            if (a_bool && Tok.is(Token::semicolon))
            {
                body.push_back(a_bool);
                break;
            }
            else
            {
                // Reset lexer state
                Tok = prev_token;
                Lex.setBufferPtr(prev_buffer);
            }

            // 6. Attempt to parse an integer assignment
            Assignment *a_int = parseIntAssign();
            if (a_int && Tok.is(Token::semicolon))
            {
                body.push_back(a_int);
                break;
            }
            else
            {
                // Handle error if all parsing attempts fail
                goto _error;
            }

            break;
        }

        case Token::KW_if:
        {
            IfStmt *i;
            i = parseIf();
            if (i)
                body.push_back(i);
            else
                goto _error;

            break;
        }
        case Token::KW_while:
        {
            WhileStmt *w;
            w = parseWhile();
            if (w)
                body.push_back(w);
            else
            {
                goto _error;
            }
            break;
        }
        case Token::KW_for:
        {
            ForStmt *f;
            f = parseFor();
            if (f)
                body.push_back(f);
            else
            {
                goto _error;
            }
            break;
        }
        case Token::KW_do:
        {
            DoWhileStmt *d;
            d = parseDoWhile();
            if (d)
                body.push_back(d);
            else
                goto _error;
            break;
        }
        case Token::KW_switch:
        {
            SwitchStmt *s;
            s = parseSwitch();
            if (s)
                body.push_back(s);
            else
                goto _error;
            break;
        }
        case Token::KW_print:
        {
            PrintStmt *p;
            p = parsePrint();
            if (p)
                body.push_back(p);
            else
            {
                goto _error;
            }
            break;
        }
        case Token::KW_min:
        {
            MinStmt *m;
            m = parseMin();
            if (m)
                body.push_back(m);
            else
                goto _error;
            break;
        }
        case Token::KW_max:
        {
            MaxStmt *m;
            m = parseMax();
            if (m)
                body.push_back(m);
            else
                goto _error;
            break;
        }
        case Token::KW_mean:
        {
            MeanStmt *mean;
            mean = parseMean();
            if (mean)
                body.push_back(mean);
            else
                goto _error;
            break;
        }
        case Token::KW_sqrtN:
        {
            SqrtNStmt *sqrtN;
            sqrtN = parseSqrtN();
            if (sqrtN)
                body.push_back(sqrtN);
            else
                goto _error;
            break;
        }
        case Token::KW_break:
        {
            advance();
            if (expect(Token::semicolon))
            {
                goto _error;
            }
            BreakStmt *breaks;
            body.push_back(breaks);
            break;
        }
        case Token::KW_continue:
        {
            advance();
            if (expect(Token::semicolon))
            {
                goto _error;
            }
            ContinueStmt *continues;
            body.push_back(continues);
            break;
        }
        
        default:
        {
           

            goto _error;
            break;
        }
        }
        advance();
    }
    if (Tok.is(Token::r_brace))
    {
        return body;
    }

_error:
    llvm::errs() << "getbody error\n";
    while (Tok.getKind() != Token::eoi)
        advance();
    return body;
}

// handles var
AST *Parser::parseValue()
{
    // Try parsing an arithmetic expression
    Token prev_token = Tok;
    const char *prev_buffer = Lex.getBuffer();

    Expr *E = parseExpr();
    if (E != nullptr)
    {
        return E;
    }

    // Reset the lexer state
    Tok = prev_token;
    Lex.setBufferPtr(prev_buffer);

    // Try parsing a logical expression
    Logic *L = parseLogic();
    if (L != nullptr)
    {
        return L;
    }

    // Parsing failed
    return nullptr;
}

TypeKind Parser::inferType(AST *Value)
{
    if (auto *E = dynamic_cast<Expr *>(Value))
    {
        if (auto *F = dynamic_cast<Final *>(E))
        {
            if (F->getKind() == Final::Number)
                return TypeKind::Int;
            else if (F->getKind() == Final::FloatNumber)
                return TypeKind::Float;
            else if (F->getKind() == Final::Ident)
            {
                // Lookup identifier type in the symbol table
                // Placeholder: assume int
                return TypeKind::Int;
            }
        }
        else if (auto *C = dynamic_cast<CastExpr *>(E))
        {
            switch (C->getCastType())
            {
            case CastExpr::IntCast:
                return TypeKind::Int;
            case CastExpr::FloatCast:
                return TypeKind::Float;
            case CastExpr::BoolCast:
                return TypeKind::Bool;
            }
        }
        else if (auto *B = dynamic_cast<BinaryOp *>(E))
        {
            // Infer type based on operands
            TypeKind LeftType = inferType(B->getLeft());
            TypeKind RightType = inferType(B->getRight());
            if (LeftType == TypeKind::Float || RightType == TypeKind::Float)
                return TypeKind::Float;
            else
                return TypeKind::Int;
        }
        // Add more cases as needed
    }
    else if (auto *L = dynamic_cast<Logic *>(Value))
    {
        // Logic expressions are of type Bool
        return TypeKind::Bool;
    }
    // If unable to infer type
    return TypeKind::Unknown;
}


