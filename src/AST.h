#ifndef AST_H
#define AST_H

#include "llvm/ADT/SmallVector.h"
#include "llvm/ADT/StringRef.h"
#include "TypeKind.h"


// Forward declarations of classes used in the AST
class AST;
class Expr;
class Program;
class DeclarationInt;
class DeclarationBool;
class Final;
class BinaryOp;
class UnaryOp;
class SignedNumber;
class NegExpr;
class Assignment;
class Logic;
class Comparison;
class LogicalExpr;
class IfStmt;
class WhileStmt;
class elifStmt;
class ForStmt;
class PrintStmt;

class CastExpr;
class DeclarationFloat;
class DeclarationVar;
class DeclareDefine;
class TernaryAssignment;
class DoWhileStmt;
class SwitchStmt;
class CaseStmt;
class DefaultStmt;
class MinStmt;
class MaxStmt;
class MeanStmt;
class SqrtNStmt;
class BreakStmt;

// ASTVisitor class defines a visitor pattern to traverse the AST
class ASTVisitor
{
public:
  // Virtual visit functions for each AST node type
  virtual void visit(AST &) {}               // Visit the base AST node
  virtual void visit(Expr &) {}              // Visit the expression node
  virtual void visit(Logic &) {}             // Visit the Logic node
  virtual void visit(Program &) {};          // Visit the group of expressions node
  virtual void visit(Final &) = 0;           // Visit the Final node
  virtual void visit(BinaryOp &) = 0;        // Visit the binary operation node
  virtual void visit(UnaryOp &) = 0;
  virtual void visit(SignedNumber &) = 0;
  virtual void visit(NegExpr &) = 0;
  virtual void visit(Assignment &) = 0;      // Visit the assignment expression node
  virtual void visit(DeclarationInt &) = 0;     // Visit the variable declaration node
  virtual void visit(DeclarationBool &) = 0;     // Visit the variable declaration node
  virtual void visit(Comparison &) = 0;      // Visit the Comparison node
  virtual void visit(LogicalExpr &) = 0;     // Visit the LogicalExpr node
  virtual void visit(IfStmt &) = 0;          // Visit the IfStmt node
  virtual void visit(WhileStmt &) = 0;        // Visit the IterStmt node
  virtual void visit(elifStmt &) = 0;        // Visit the elifStmt node
  virtual void visit(ForStmt &) = 0;
  virtual void visit(PrintStmt &) = 0;
  virtual void visit(CastExpr &) = 0;

    virtual void visit(DeclarationFloat &) = 0;
    virtual void visit(DeclarationVar &) = 0;
    virtual void visit(DeclareDefine &) = 0;
    virtual void visit(TernaryAssignment &) = 0;
    virtual void visit(DoWhileStmt &) = 0;
    virtual void visit(SwitchStmt &) = 0;
    virtual void visit(CaseStmt &) = 0;
    virtual void visit(DefaultStmt &) = 0;
    virtual void visit(MinStmt &) = 0;
    virtual void visit(MaxStmt &) = 0;
    virtual void visit(MeanStmt &) = 0;
    virtual void visit(SqrtNStmt &) = 0;
};

// AST class serves as the base class for all AST nodes
class AST
{
public:
  virtual ~AST() {}
  virtual void accept(ASTVisitor &V) = 0;    // Accept a visitor for traversal
};

// Expr class represents an expression in the AST
class Expr : public AST
{
public:
  Expr() {}
};

class Logic : public AST
{
public:
  Logic() {}
};

// Program class represents a group of expressions in the AST
class Program : public AST
{
  using dataVector = llvm::SmallVector<AST *>;

private:
  dataVector data;                          // Stores the list of expressions

public:
  Program(llvm::SmallVector<AST *> data) : data(data) {}
  Program() = default;

  llvm::SmallVector<AST *> getdata() { return data; }

  dataVector::const_iterator begin() { return data.begin(); }

  dataVector::const_iterator end() { return data.end(); }

  virtual void accept(ASTVisitor &V) override
  {
    V.visit(*this);
  }
};

// Declaration class represents a variable declaration with an initializer in the AST
class DeclarationInt : public Program
{
  using VarVector = llvm::SmallVector<llvm::StringRef>;
  using ValueVector = llvm::SmallVector<Expr *>;
  VarVector Vars;                           // Stores the list of variables
  ValueVector Values;                       // Stores the list of initializers

public:
  // Declaration(llvm::SmallVector<llvm::StringRef> Vars, Expr *E) : Vars(Vars), E(E) {}
  DeclarationInt(llvm::SmallVector<llvm::StringRef> Vars, llvm::SmallVector<Expr *> Values) : Vars(Vars), Values(Values) {}

  VarVector::const_iterator varBegin() { return Vars.begin(); }

  VarVector::const_iterator varEnd() { return Vars.end(); }

  ValueVector::const_iterator valBegin() { return Values.begin(); }

  ValueVector::const_iterator valEnd() { return Values.end(); }

  virtual void accept(ASTVisitor &V) override
  {
    V.visit(*this);
  }
};

// Declaration class represents a variable declaration with an initializer in the AST
class DeclarationBool : public Program
{
  using VarVector = llvm::SmallVector<llvm::StringRef>;
  using ValueVector = llvm::SmallVector<Logic *>;
  VarVector Vars;                           // Stores the list of variables
  ValueVector Values;                       // Stores the list of initializers

public:
  // Declaration(llvm::SmallVector<llvm::StringRef> Vars, Expr *E) : Vars(Vars), E(E) {}
  DeclarationBool(llvm::SmallVector<llvm::StringRef> Vars, llvm::SmallVector<Logic *> Values) : Vars(Vars), Values(Values) {}

  VarVector::const_iterator varBegin() { return Vars.begin(); }

  VarVector::const_iterator varEnd() { return Vars.end(); }

  ValueVector::const_iterator valBegin() { return Values.begin(); }

  ValueVector::const_iterator valEnd() { return Values.end(); }

  virtual void accept(ASTVisitor &V) override
  {
    V.visit(*this);
  }
};

//new:
// DeclarationFloat class represents a float variable declaration in the AST
class DeclarationFloat : public Program
{
    using VarVector = llvm::SmallVector<llvm::StringRef>;
    using ValueVector = llvm::SmallVector<Expr *>;
    VarVector Vars;                           // Stores the list of variables
    ValueVector Values;                       // Stores the list of initializers

public:
    DeclarationFloat(VarVector Vars, ValueVector Values) : Vars(Vars), Values(Values) {}

    VarVector::const_iterator varBegin() { return Vars.begin(); }
    VarVector::const_iterator varEnd() { return Vars.end(); }
    ValueVector::const_iterator valBegin() { return Values.begin(); }
    ValueVector::const_iterator valEnd() { return Values.end(); }

    virtual void accept(ASTVisitor &V) override
    {
        V.visit(*this);
    }
};

//new:
class DeclarationVar : public Program
{
    using VarVector = llvm::SmallVector<llvm::StringRef>;
    using ValueVector = llvm::SmallVector<AST *>;
    using TypeVector = llvm::SmallVector<TypeKind>;
    VarVector Vars;
    ValueVector Values;
    TypeVector Types;

public:
    DeclarationVar(VarVector Vars, ValueVector Values, TypeVector Types)
        : Vars(Vars), Values(Values), Types(Types) {}

    VarVector::const_iterator varBegin() const { return Vars.begin(); }
    VarVector::const_iterator varEnd() const { return Vars.end(); }
    ValueVector::const_iterator valBegin() const { return Values.begin(); }
    ValueVector::const_iterator valEnd() const { return Values.end(); }
    TypeVector::const_iterator typeBegin() const { return Types.begin(); }
    TypeVector::const_iterator typeEnd() const { return Types.end(); }

    virtual void accept(ASTVisitor &V) override
    {
        V.visit(*this);
    }
};

//new:
// DeclareDefine class represents a #define directive in the AST
class DeclareDefine : public Program
{
private:
    llvm::StringRef Name;   // Name of the constant
    Expr *Value;            // Value of the constant

public:
    DeclareDefine(llvm::StringRef Name, Expr *Value) : Name(Name), Value(Value) {}

    llvm::StringRef getName() { return Name; }
    Expr *getValue() { return Value; }

    virtual void accept(ASTVisitor &V) override
    {
        V.visit(*this);
    }
};

//new:
// TernaryAssignment class represents a ternary assignment in the AST
class TernaryAssignment : public Program
{
private:
    Final *Variable;        // Variable being assigned
    Logic *Condition;       // Condition expression
    Expr *TrueExpr;         // Expression if condition is true
    Expr *FalseExpr;        // Expression if condition is false

public:
    TernaryAssignment(Final *Variable, Logic *Condition, Expr *TrueExpr, Expr *FalseExpr)
        : Variable(Variable), Condition(Condition), TrueExpr(TrueExpr), FalseExpr(FalseExpr) {}

    Final *getVariable() { return Variable; }
    Logic *getCondition() { return Condition; }
    Expr *getTrueExpr() { return TrueExpr; }
    Expr *getFalseExpr() { return FalseExpr; }

    virtual void accept(ASTVisitor &V) override
    {
        V.visit(*this);
    }
};


class Final : public Expr
{
public:
    enum ValueKind
    {
        Ident,
        Number,
        FloatNumber,  // Represents float literals
        Bool 
    };

private:
    ValueKind Kind;
    llvm::StringRef Val;

public:
    Final(ValueKind Kind, llvm::StringRef Val) : Kind(Kind), Val(Val) {}

    ValueKind getKind() const { return Kind; }
    llvm::StringRef getVal() const { return Val; }

    virtual void accept(ASTVisitor &V) override
    {
        V.visit(*this);
    }
};



// BinaryOp class represents a binary operation in the AST (plus, minus, multiplication, division)
class BinaryOp : public Expr
{
public:
  enum Operator
  {
    Plus,
    Minus,
    Mul,
    Div,
    Mod,
    Exp
  };

private:
  Expr *Left;                               // Left-hand side expression
  Expr *Right;                              // Right-hand side expression
  Operator Op;                              // Operator of the binary operation

public:
  BinaryOp(Operator Op, Expr *L, Expr *R) : Op(Op), Left(L), Right(R) {}

  Expr *getLeft() { return Left; }

  Expr *getRight() { return Right; }

  Operator getOperator() { return Op; }

  virtual void accept(ASTVisitor &V) override
  {
    V.visit(*this);
  }
};

// naryOp class represents a unary operation in the AST (plus plus, minus minus)
class UnaryOp : public Expr
{
public:
  enum Operator
  {
    Plus_plus,
    Minus_minus
  };

private:
  llvm::StringRef Ident;                      
  Operator Op;                              // Operator of the unary operation

public:
  UnaryOp(Operator Op, llvm::StringRef I) : Op(Op), Ident(I) {}

  llvm::StringRef getIdent() { return Ident; }

  Operator getOperator() { return Op; }

  virtual void accept(ASTVisitor &V) override
  {
    V.visit(*this);
  }
};

class SignedNumber : public Expr
{
public:
  enum Sign
  {
    Plus,
    Minus
  };

private:
  llvm::StringRef Value;                              
  Sign s;                              

public:
  SignedNumber(Sign S, llvm::StringRef V) : s(S), Value(V) {}

  llvm::StringRef getValue() { return Value; }

  Sign getSign() { return s; }

  virtual void accept(ASTVisitor &V) override
  {
    V.visit(*this);
  }
};

class NegExpr : public Expr
{

private:
  Expr *expr;                              

public:
  NegExpr(Expr *E) : expr(E) {}

  Expr *getExpr() { return expr; }

  virtual void accept(ASTVisitor &V) override
  {
    V.visit(*this);
  }
};

// AST.h

class Assignment : public Program {
public:
    enum AssignKind {
        Assign,
        Plus_assign,
        Minus_assign,
        Star_assign,
        Slash_assign,
        Mod_assign
    };

private:
    Final *Variable;   // The variable being assigned to
    AST *Value;        // The value being assigned (Expr* or Logic*)
    AssignKind OpKind; // The kind of assignment operator

public:
    Assignment(Final *Variable, AST *Value, AssignKind OpKind)
        : Variable(Variable), Value(Value), OpKind(OpKind) {}

    Final *getVariable() { return Variable; }
    AST *getValue() { return Value; }
    AssignKind getAssignKind() { return OpKind; }

    virtual void accept(ASTVisitor &V) override {
        V.visit(*this);
    }
};


// Comparison class represents a comparison expression in the AST
class Comparison : public Logic
{
  public:
  enum Operator
  {
    Equal,          // ==
    Not_equal,      // !=
    Greater,        // >
    Less,           // <
    Greater_equal,  // >=
    Less_equal,     // <=
    True,           //CHECK???
    False,
    Ident           // only one boolean ident
  };
    
private:
  Expr *Left;                                // Left-hand side expression
  Expr *Right;                               // Right-hand side expression
  Operator Op;                               // Kind of assignment

public:
  Comparison(Expr *L, Expr *R, Operator Op) : Left(L), Right(R), Op(Op) {}

  Expr *getLeft() { return Left; }

  Expr *getRight() { return Right; }

  Operator getOperator() { return Op; }

  virtual void accept(ASTVisitor &V) override
  {
    V.visit(*this);
  }
};

// LogicalExpr class represents a logical expression in the AST
class LogicalExpr : public Logic
{
  public:
  enum Operator
  {
    And,          // &&
    Or,           // ||
    Xor           
  };

private:
  Logic *Left;                                // Left-hand side expression
  Logic *Right;                               // Right-hand side expression
  Operator Op;                                // Kind of assignment

public:
  LogicalExpr(Logic *L, Logic *R, Operator Op) : Left(L), Right(R), Op(Op) {}

  Logic *getLeft() { return Left; }

  Logic *getRight() { return Right; }

  Operator getOperator() { return Op; }

  virtual void accept(ASTVisitor &V) override
  {
    V.visit(*this);
  }
};

class elifStmt : public Program
{
  using Stmts = llvm::SmallVector<AST *>;

private:
  Stmts S;
  Logic *Cond;

public:
  elifStmt(Logic *Cond, llvm::SmallVector<AST *> S) : Cond(Cond), S(S) {}

  Logic *getCond() { return Cond; }

  Stmts::const_iterator begin() { return S.begin(); }

  Stmts::const_iterator end() { return S.end(); }

  virtual void accept(ASTVisitor &V) override
  {
    V.visit(*this);
  }

};

class IfStmt : public Program
{
using BodyVector = llvm::SmallVector<AST *>;
using elifVector = llvm::SmallVector<elifStmt *>;

private:
  BodyVector ifStmts;
  elifVector elifStmts;
  BodyVector elseStmts;
  Logic *Cond;

public:
  IfStmt(Logic *Cond, llvm::SmallVector<AST *> ifStmts, llvm::SmallVector<AST *> elseStmts, llvm::SmallVector<elifStmt *> elifStmts) : Cond(Cond), ifStmts(ifStmts), elseStmts(elseStmts), elifStmts(elifStmts) {}

  Logic *getCond() { return Cond; }

  BodyVector::const_iterator begin() { return ifStmts.begin(); }

  BodyVector::const_iterator end() { return ifStmts.end(); }

  BodyVector::const_iterator beginElse() { return elseStmts.begin(); }

  BodyVector::const_iterator endElse() { return elseStmts.end(); }

  elifVector::const_iterator beginElif() { return elifStmts.begin(); }

  elifVector::const_iterator endElif() { return elifStmts.end(); }

  virtual void accept(ASTVisitor &V) override
  {
    V.visit(*this);
  }
};

class WhileStmt : public Program
{
using BodyVector = llvm::SmallVector<AST *>;
BodyVector Body;

private:
  Logic *Cond;

public:
  WhileStmt(Logic *Cond, llvm::SmallVector<AST *> Body) : Cond(Cond), Body(Body) {}

  Logic *getCond() { return Cond; }

  BodyVector::const_iterator begin() { return Body.begin(); }

  BodyVector::const_iterator end() { return Body.end(); }

  virtual void accept(ASTVisitor &V) override
  {
    V.visit(*this);
  }
};

//new:
// DoWhileStmt class represents a do-while loop in the AST
class DoWhileStmt : public Program
{
    using BodyVector = llvm::SmallVector<AST *>;
    BodyVector Body;
    Logic *Cond;

public:
    DoWhileStmt(Logic *Cond, BodyVector Body) : Cond(Cond), Body(Body) {}

    Logic *getCond() { return Cond; }
    BodyVector::const_iterator begin() { return Body.begin(); }
    BodyVector::const_iterator end() { return Body.end(); }

    virtual void accept(ASTVisitor &V) override
    {
        V.visit(*this);
    }
};



class ForStmt : public Program
{
using BodyVector = llvm::SmallVector<AST *>;
BodyVector Body;

private:
  Assignment *First;
  Logic *Second;
  Assignment *ThirdAssign;
  UnaryOp *ThirdUnary;


public:
  ForStmt(Assignment *First, Logic *Second, Assignment *ThirdAssign, UnaryOp* ThirdUnary, llvm::SmallVector<AST *> Body) : First(First), Second(Second), ThirdAssign(ThirdAssign), ThirdUnary(ThirdUnary), Body(Body) {}

  Assignment *getFirst() { return First; }

  Logic *getSecond() { return Second; }

  Assignment *getThirdAssign() { return ThirdAssign; }

  UnaryOp *getThirdUnary() { return ThirdUnary; }

  BodyVector::const_iterator begin() { return Body.begin(); }

  BodyVector::const_iterator end() { return Body.end(); }

  virtual void accept(ASTVisitor &V) override
  {
    V.visit(*this);
  }
};

class PrintStmt : public Program
{
private:
  Expr *Value = nullptr;
  
public:
  PrintStmt(Expr *Value) : Value(Value) {}

  Expr* getVar() { return Value; }

  virtual void accept(ASTVisitor &V) override
  {
    V.visit(*this);
  }
};

class CastExpr : public Expr
{
public:
    enum CastType
    {
        IntCast,
        BoolCast,
        FloatCast
    };

private:
    CastType Type;
    AST *Inner;

public:
    CastExpr(CastType Type, AST *Inner) : Type(Type), Inner(Inner) {}

    CastType getCastType() { return Type; }
    AST *getInner() { return Inner; }

    virtual void accept(ASTVisitor &V) override
    {
        V.visit(*this);
    }
};

//new:
// SwitchStmt class represents a switch statement in the AST
class SwitchStmt : public Program
{
    Expr *SwitchExpr;                           // Expression being switched on
    llvm::SmallVector<CaseStmt *> Cases;        // List of case statements
    DefaultStmt *DefaultCase;                   // Default case (optional)

public:
    SwitchStmt(Expr *SwitchExpr, llvm::SmallVector<CaseStmt *> Cases, DefaultStmt *DefaultCase)
        : SwitchExpr(SwitchExpr), Cases(Cases), DefaultCase(DefaultCase) {}

    Expr *getSwitchExpr() { return SwitchExpr; }
    llvm::SmallVector<CaseStmt *>::const_iterator caseBegin() { return Cases.begin(); }
    llvm::SmallVector<CaseStmt *>::const_iterator caseEnd() { return Cases.end(); }
    DefaultStmt *getDefaultCase() { return DefaultCase; }

    virtual void accept(ASTVisitor &V) override
    {
        V.visit(*this);
    }
};

// CaseStmt class represents a case within a switch statement in the AST
class CaseStmt : public Program
{
    Expr *CaseExpr;                          // Expression for the case
    llvm::SmallVector<AST *> Body;           // Statements within the case

public:
    CaseStmt(Expr *CaseExpr, llvm::SmallVector<AST *> Body)
        : CaseExpr(CaseExpr), Body(Body) {}

    Expr *getCaseExpr() { return CaseExpr; }
    llvm::SmallVector<AST *>::const_iterator begin() { return Body.begin(); }
    llvm::SmallVector<AST *>::const_iterator end() { return Body.end(); }

    virtual void accept(ASTVisitor &V) override
    {
        V.visit(*this);
    }
};

// DefaultStmt class represents the default case within a switch statement in the AST
class DefaultStmt : public Program
{
    llvm::SmallVector<AST *> Body;           // Statements within the default case

public:
    DefaultStmt(llvm::SmallVector<AST *> Body) : Body(Body) {}

    llvm::SmallVector<AST *>::const_iterator begin() { return Body.begin(); }
    llvm::SmallVector<AST *>::const_iterator end() { return Body.end(); }

    virtual void accept(ASTVisitor &V) override
    {
        V.visit(*this);
    }
};

// MinStmt class represents a call to the min function in the AST
class MinStmt : public Expr
{
    Expr *Left;       // First argument
    Expr *Right;      // Second argument

public:
    MinStmt(Expr *Left, Expr *Right) : Left(Left), Right(Right) {}

    Expr *getLeft() { return Left; }
    Expr *getRight() { return Right; }

    virtual void accept(ASTVisitor &V) override
    {
        V.visit(*this);
    }
};

// MaxStmt class represents a call to the max function in the AST
class MaxStmt : public Expr
{
    Expr *Left;       // First argument
    Expr *Right;      // Second argument

public:
    MaxStmt(Expr *Left, Expr *Right) : Left(Left), Right(Right) {}

    Expr *getLeft() { return Left; }
    Expr *getRight() { return Right; }

    virtual void accept(ASTVisitor &V) override
    {
        V.visit(*this);
    }
};

// MeanStmt class represents a call to the mean function in the AST
class MeanStmt : public Expr
{
    llvm::SmallVector<Expr *> Values;  // Arguments to the mean function

public:
    MeanStmt(llvm::SmallVector<Expr *> Values) : Values(Values) {}

    llvm::SmallVector<Expr *>::const_iterator begin() { return Values.begin(); }
    llvm::SmallVector<Expr *>::const_iterator end() { return Values.end(); }

    virtual void accept(ASTVisitor &V) override
    {
        V.visit(*this);
    }
};

// SqrtNStmt class represents a call to the sqrtN function in the AST
class SqrtNStmt : public Expr
{
    Expr *Base;       // Base value
    Expr *NthRoot;    // Nth root

public:
    SqrtNStmt(Expr *Base, Expr *NthRoot) : Base(Base), NthRoot(NthRoot) {}

    Expr *getBase() { return Base; }
    Expr *getNthRoot() { return NthRoot; }

    virtual void accept(ASTVisitor &V) override
    {
        V.visit(*this);
    }
};

class BreakStmt : public Program
{
private:
  
public:
  BreakStmt(){}

  virtual void accept(ASTVisitor &V) override
  {
    V.visit(*this);
  }
};

#endif
