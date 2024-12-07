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
class DeclarationFloat;
class DeclarationVar;
class DeclareDefine;
class TernaryAssignment;
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
class elifStmt;
class WhileStmt;
class ForStmt;
class DoWhileStmt;
class SwitchStmt;
class CaseStmt;
class DefaultStmt;
class PrintStmt;
class CastExpr;
class BoolCastExpr;
class MinStmt;
class MaxStmt;
class MeanStmt;
class SqrtNStmt;
class BreakStmt;
class ContinueStmt;

// ASTVisitor class defines a visitor pattern to traverse the AST
class ASTVisitor {
public:
    // Virtual visit functions for each AST node type
    virtual void visit(AST &) {}               // Visit the base AST node
    virtual void visit(Expr &) {}              // Visit the expression node
    virtual void visit(Logic &) {}             // Visit the Logic node
    virtual void visit(Program &) {}           // Visit the group of expressions node

    // Pure virtual functions (must be implemented by derived classes)
    virtual void visit(Final &) = 0;
    virtual void visit(BinaryOp &) = 0;
    virtual void visit(UnaryOp &) = 0;
    virtual void visit(SignedNumber &) = 0;
    virtual void visit(NegExpr &) = 0;
    virtual void visit(Assignment &) = 0;
    virtual void visit(DeclarationInt &) = 0;
    virtual void visit(DeclarationBool &) = 0;
    virtual void visit(DeclarationFloat &) = 0;
    virtual void visit(DeclarationVar &) = 0;
    virtual void visit(DeclareDefine &) = 0;
    virtual void visit(TernaryAssignment &) = 0;
    virtual void visit(Comparison &) = 0;
    virtual void visit(LogicalExpr &) = 0;
    virtual void visit(IfStmt &) = 0;
    virtual void visit(WhileStmt &) = 0;
    virtual void visit(DoWhileStmt &) = 0;
    virtual void visit(elifStmt &) = 0;
    virtual void visit(ForStmt &) = 0;
    virtual void visit(SwitchStmt &) = 0;
    virtual void visit(CaseStmt &) = 0;
    virtual void visit(DefaultStmt &) = 0;
    virtual void visit(PrintStmt &) = 0;
    virtual void visit(CastExpr &) = 0;
    virtual void visit(BoolCastExpr &) = 0;
    virtual void visit(MinStmt &) = 0;
    virtual void visit(MaxStmt &) = 0;
    virtual void visit(MeanStmt &) = 0;
    virtual void visit(SqrtNStmt &) = 0;
    virtual void visit(BreakStmt &) = 0;
    virtual void visit(ContinueStmt &) = 0;
};

class AST
{
public:
  virtual ~AST() {}
  virtual void accept(ASTVisitor &V) = 0;    // Accept a visitor for traversal
};

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

class DeclarationInt : public Program
{
  using VarVector = llvm::SmallVector<llvm::StringRef>;
  using ValueVector = llvm::SmallVector<Expr *>;
  VarVector Vars;                           // Stores the list of variables
  ValueVector Values;                       // Stores the list of initializers
   bool isConst;

public:
  // Declaration(llvm::SmallVector<llvm::StringRef> Vars, Expr *E) : Vars(Vars), E(E) {}
  DeclarationInt(llvm::SmallVector<llvm::StringRef> Vars, llvm::SmallVector<Expr *> Values, bool isConst) : Vars(Vars), Values(Values), isConst(isConst) {}

  VarVector::const_iterator varBegin() { return Vars.begin(); }

  VarVector::const_iterator varEnd() { return Vars.end(); }

  ValueVector::const_iterator valBegin() { return Values.begin(); }

  ValueVector::const_iterator valEnd() { return Values.end(); }

  const ValueVector &getValue() const {return Values;}

  virtual void accept(ASTVisitor &V) override
  {
    V.visit(*this);
  }
};

class DeclarationBool : public Program
{
  using VarVector = llvm::SmallVector<llvm::StringRef>;
  using ValueVector = llvm::SmallVector<Logic *>;
  VarVector Vars;                           // Stores the list of variables
  ValueVector Values;                       // Stores the list of initializers
  bool isConst;

public:
  // Declaration(llvm::SmallVector<llvm::StringRef> Vars, Expr *E) : Vars(Vars), E(E) {}
  DeclarationBool(llvm::SmallVector<llvm::StringRef> Vars, llvm::SmallVector<Logic *> Values, bool isConst) : Vars(Vars), Values(Values), isConst(isConst){}

  VarVector::const_iterator varBegin() { return Vars.begin(); }

  VarVector::const_iterator varEnd() { return Vars.end(); }

  ValueVector::const_iterator valBegin() { return Values.begin(); }

  ValueVector::const_iterator valEnd() { return Values.end(); }

  virtual void accept(ASTVisitor &V) override
  {
    V.visit(*this);
  }
};

class DeclarationFloat : public Program
{
    using VarVector = llvm::SmallVector<llvm::StringRef>;
    using ValueVector = llvm::SmallVector<Expr *>;
    VarVector Vars;                           // Stores the list of variables
    ValueVector Values;                       // Stores the list of initializers
     bool isConst;

public:
    DeclarationFloat(VarVector Vars, ValueVector Values, bool isConst) : Vars(Vars), Values(Values), isConst(isConst){}

    VarVector::const_iterator varBegin() { return Vars.begin(); }
    VarVector::const_iterator varEnd() { return Vars.end(); }
    ValueVector::const_iterator valBegin() { return Values.begin(); }
    ValueVector::const_iterator valEnd() { return Values.end(); }

    virtual void accept(ASTVisitor &V) override
    {
        V.visit(*this);
    }
};

class DeclarationVar : public Program
{
    using VarVector = llvm::SmallVector<llvm::StringRef>;
    using ValueVector = llvm::SmallVector<AST *>;
    using TypeVector = llvm::SmallVector<TypeKind>;
    VarVector Vars;
    ValueVector Values;
    TypeVector Types;
    

public:
    DeclarationVar(VarVector Vars, ValueVector Values, TypeVector Types, bool isConst)
        : Vars(Vars), Values(Values), Types(Types) , isConst(isConst) {}

    VarVector::const_iterator varBegin() const { return Vars.begin(); }
    VarVector::const_iterator varEnd() const { return Vars.end(); }
    ValueVector::const_iterator valBegin() const { return Values.begin(); }
    ValueVector::const_iterator valEnd() const { return Values.end(); }
    TypeVector::const_iterator typeBegin() const { return Types.begin(); }
    TypeVector::const_iterator typeEnd() const { return Types.end(); }
    bool isConst;
    virtual void accept(ASTVisitor &V) override
    {
        V.visit(*this);
    }
};

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

class TernaryAssignment : public Expr
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

class Final : public Expr {
public:
    enum ValueKind {
        Ident,
        Number,
        FloatNumber,
        Bool
    };

private:
    ValueKind Kind;
    llvm::StringRef Val;

public:
    Final(ValueKind Kind, llvm::StringRef Val) : Kind(Kind), Val(Val) {}

    ValueKind getKind() const { return Kind; }
    llvm::StringRef getVal() const { return Val; }

    virtual void accept(ASTVisitor &V) override {
        V.visit(*this);
    }
};

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


class Assignment : public AST {
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
    Expr *RightExpr;   // The right-hand side expression (if any)
    Logic *RightLogic; // The right-hand side logic expression (if any)
    AssignKind OpKind; // The kind of assignment operator

public:
    // Constructor for expression assignment
    Assignment(Final *Variable, Expr *Value, AssignKind OpKind)
        : Variable(Variable), RightExpr(Value), RightLogic(nullptr), OpKind(OpKind) {}

    // Constructor for logic assignment
    Assignment(Final *Variable, Logic *Value, AssignKind OpKind)
        : Variable(Variable), RightExpr(nullptr), RightLogic(Value), OpKind(OpKind) {}

    Final *getVariable() const { return Variable; }
    Expr *getRightExpr() const { return RightExpr; }
    Logic *getRightLogic() const { return RightLogic; }
    AssignKind getAssignKind() const { return OpKind; }

    virtual void accept(ASTVisitor &V) override {
        V.visit(*this);
    }
};


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

  BodyVector getBody(){return Body;}

  virtual void accept(ASTVisitor &V) override
  {
    V.visit(*this);
  }
};

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
    BodyVector getBody(){return Body;}

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

  BodyVector getBody(){return Body;}

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

class BoolCastExpr : public Logic
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
    BoolCastExpr(CastType Type, AST *Inner) : Type(Type), Inner(Inner) {}

    CastType getCastType() { return Type; }
    AST *getInner() { return Inner; }

    virtual void accept(ASTVisitor &V) override
    {
        V.visit(*this);
    }
};

class SwitchStmt : public AST {
    Expr *SwitchExpr;
    llvm::SmallVector<CaseStmt *> Cases;
    DefaultStmt *DefaultCase;

public:
    SwitchStmt(Expr *SwitchExpr, llvm::SmallVector<CaseStmt *> Cases, DefaultStmt *DefaultCase)
        : SwitchExpr(SwitchExpr), Cases(Cases), DefaultCase(DefaultCase) {}

    Expr *getSwitchExpr() { return SwitchExpr; }
    const llvm::SmallVector<CaseStmt *> &getCases() const { return Cases; } // Add this method
    DefaultStmt *getDefaultCase() { return DefaultCase; }

    virtual void accept(ASTVisitor &V) override {
        V.visit(*this);
    }
};


class CaseStmt : public AST {
    Expr *CaseExpr;
    llvm::SmallVector<AST *> Body;

public:
    CaseStmt(Expr *CaseExpr, llvm::SmallVector<AST *> Body)
        : CaseExpr(CaseExpr), Body(Body) {}

    Expr *getCaseExpr() { return CaseExpr; }
    const llvm::SmallVector<AST *> &getBody() const { return Body; } // Add this method

    virtual void accept(ASTVisitor &V) override {
        V.visit(*this);
    }
};


class DefaultStmt : public AST {
    llvm::SmallVector<AST *> Body;

public:
    DefaultStmt(llvm::SmallVector<AST *> Body) : Body(Body) {}

    const llvm::SmallVector<AST *> &getBody() const { return Body; } // Add this method

    virtual void accept(ASTVisitor &V) override {
        V.visit(*this);
    }
};

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

class MeanStmt : public Expr
{
    Expr *Left;       // First argument
    Expr *Right;      // Second argument

public:
    MeanStmt(Expr *Left, Expr *Right) : Left(Left), Right(Right) {}

    Expr *getLeft() { return Left; }
    Expr *getRight() { return Right; }

    virtual void accept(ASTVisitor &V) override
    {
        V.visit(*this);
    }
};

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
class ContinueStmt : public Program
{
private:
  
public:
  ContinueStmt(){}

  virtual void accept(ASTVisitor &V) override
  {
    V.visit(*this);
  }
};
#endif
