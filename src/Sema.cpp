#include "Sema.h"
#include "llvm/ADT/StringSet.h"
#include "llvm/Support/raw_ostream.h"
#include <unordered_map>

namespace nms {
class InputCheck : public ASTVisitor {
    llvm::StringSet<> IntScope;
    llvm::StringSet<> BoolScope;
    llvm::StringSet<> FloatScope;
    std::unordered_map<std::string, TypeKind> VarScope;
    llvm::StringSet<> ConstScope;
    llvm::StringSet<> DefineScope;
    bool HasError;

    enum ErrorType { Twice, Not };

    void error(ErrorType ET, llvm::StringRef V) {
        llvm::errs() << "Variable '" << V << "' is "
                     << (ET == Twice ? "already" : "not")
                     << " declared.\n";
        HasError = true;
    }

public:
    InputCheck() : HasError(false) {}

    bool hasError() { return HasError; }

    virtual void visit(Program &Node) override {
        for (auto *stmt : Node.getdata()) {
            stmt->accept(*this);
        }
    }

    virtual void visit(AST &Node) override {
        Node.accept(*this);
    }

    virtual void visit(Final &Node) override {
        if (Node.getKind() == Final::Ident) {
            llvm::StringRef varName = Node.getVal();
            if (IntScope.find(varName) == IntScope.end() &&
                BoolScope.find(varName) == BoolScope.end() &&
                FloatScope.find(varName) == FloatScope.end() &&
                VarScope.find(varName.str()) == VarScope.end() &&
                ConstScope.find(varName) == ConstScope.end() &&
                DefineScope.find(varName) == DefineScope.end()) {
                error(Not, varName);
            }
        }
    }

    virtual void visit(BinaryOp &Node) override {
        Node.getLeft()->accept(*this);
        Node.getRight()->accept(*this);

        TypeKind leftType = inferType(Node.getLeft());
        TypeKind rightType = inferType(Node.getRight());

        if (leftType != rightType) {
            llvm::errs() << "Type mismatch in binary operation.\n";
            HasError = true;
        }

        if (leftType == TypeKind::Bool || rightType == TypeKind::Bool) {
            llvm::errs() << "Cannot perform arithmetic operations on boolean variables.\n";
            HasError = true;
        }

        // Division by zero check
        if (Node.getOperator() == BinaryOp::Div || Node.getOperator() == BinaryOp::Mod) {
            if (auto *rightFinal = dynamic_cast<Final *>(Node.getRight())) {
                if (rightFinal->getKind() == Final::Number && rightFinal->getVal() == "0") {
                    llvm::errs() << "Division by zero is not allowed.\n";
                    HasError = true;
                }
            }
        }
    }

    virtual void visit(Assignment &Node) override {
        Final *dest = Node.getVariable();
        dest->accept(*this);

        llvm::StringRef varName = dest->getVal();

        if (ConstScope.find(varName) != ConstScope.end() ||
            DefineScope.find(varName) != DefineScope.end()) {
            llvm::errs() << "Cannot assign to constant variable: " << varName << "\n";
            HasError = true;
            return;
        }

        TypeKind destType = getVariableType(varName);

        // Infer the type of the right-hand side
        AST *value = nullptr;
        if (Node.getRightExpr()) {
            value = Node.getRightExpr();
        } else if (Node.getRightLogic()) {
            value = Node.getRightLogic();
        } else {
            llvm::errs() << "Assignment has no right-hand side.\n";
            HasError = true;
            return;
        }

        value->accept(*this);
        TypeKind valueType = inferType(value);

        // Check for explicit cast
        bool isCast = dynamic_cast<CastExpr *>(value) != nullptr;

        if (destType != valueType && !isCast) {
            llvm::errs() << "Type mismatch in assignment to '" << varName << "'.\n";
            HasError = true;
        }
    }

    virtual void visit(DeclarationInt &Node) override {
        for (auto I = Node.varBegin(), E = Node.varEnd(); I != E; ++I) {
            llvm::StringRef varName = *I;
            if (BoolScope.find(varName) != BoolScope.end() ||
                FloatScope.find(varName) != FloatScope.end() ||
                VarScope.find(varName.str()) != VarScope.end() ||
                ConstScope.find(varName) != ConstScope.end() ||
                DefineScope.find(varName) != DefineScope.end()) {
                llvm::errs() << "Variable '" << varName << "' is already declared.\n";
                HasError = true;
            } else {
                if (!IntScope.insert(varName).second) {
                    error(Twice, varName);
                }
            }
        }

        for (auto *expr : Node.getValue()) {
            expr->accept(*this);
        }
    }


    virtual void visit(DeclarationBool &Node) override {
    for (auto I = Node.varBegin(), E = Node.varEnd(); I != E; ++I) {
        llvm::StringRef varName = *I;
        if (IntScope.find(varName) != IntScope.end() ||
            FloatScope.find(varName) != FloatScope.end() ||
            VarScope.find(varName.str()) != VarScope.end() || // Convert to std::string
            ConstScope.find(varName) != ConstScope.end() ||
            DefineScope.find(varName) != DefineScope.end()) {
            llvm::errs() << "Variable " << varName << " is already declared.\n";
            HasError = true;
        } else {
            if (!BoolScope.insert(varName).second) {
                error(Twice, varName);
            }
        }
    }
}

    virtual void visit(DeclarationFloat &Node) override {
    for (auto I = Node.varBegin(), E = Node.varEnd(); I != E; ++I) {
        llvm::StringRef varName = *I;
        if (IntScope.find(varName) != IntScope.end() ||
            BoolScope.find(varName) != BoolScope.end() ||
            VarScope.find(varName.str()) != VarScope.end() ||
            ConstScope.find(varName) != ConstScope.end() ||
            DefineScope.find(varName) != DefineScope.end()) {
            llvm::errs() << "Variable " << varName << " is already declared.\n";
            HasError = true;
        } else {
            if (!FloatScope.insert(varName).second) {
                error(Twice, varName);
            }
        }
    }
}

    virtual void visit(DeclarationVar &Node) override {
    auto varIt = Node.varBegin();
    auto valIt = Node.valBegin();
    auto typeIt = Node.typeBegin();
    for (; varIt != Node.varEnd(); ++varIt, ++valIt, ++typeIt) {
        llvm::StringRef varName = *varIt;
        TypeKind varType = *typeIt;
        if (IntScope.find(varName) != IntScope.end() ||
            BoolScope.find(varName) != BoolScope.end() ||
            FloatScope.find(varName) != FloatScope.end() ||
            ConstScope.find(varName) != ConstScope.end() ||
            DefineScope.find(varName) != DefineScope.end()) {
            llvm::errs() << "Variable " << varName << " is already declared.\n";
            HasError = true;
        } else {
            if (!VarScope.emplace(varName.str(), varType).second) {
                error(Twice, varName);
            }
        }
        // Visit the value to ensure correctness
        (*valIt)->accept(*this);
    }
}

    virtual void visit(PrintStmt &Node) override {
        Expr *value = Node.getVar();
        Final *finalValue = dynamic_cast<Final *>(value);
        if (!finalValue || finalValue->getKind() != Final::Ident) {
            llvm::errs() << "Print statement expects an identifier.\n";
            HasError = true;
            return;
        }

        llvm::StringRef varName = finalValue->getVal();

        if (IntScope.find(varName) == IntScope.end() &&
            BoolScope.find(varName) == BoolScope.end() &&
            FloatScope.find(varName) == FloatScope.end() &&
            VarScope.find(varName.str()) == VarScope.end() &&
            ConstScope.find(varName) == ConstScope.end() &&
            DefineScope.find(varName) == DefineScope.end()) {
            error(Not, varName);
        }
    }

  virtual void visit(Comparison &Node) override {
    if(Node.getLeft()){
      Node.getLeft()->accept(*this);
    }
    if(Node.getRight()){
      Node.getRight()->accept(*this);
    }
   
    if (Node.getOperator() != Comparison::True && Node.getOperator() != Comparison::False && Node.getOperator() != Comparison::Ident){
      Final* L = (Final*)(Node.getLeft());
      if(L){
        if (L->getKind() == Final::ValueKind::Ident && IntScope.find(L->getVal()) == IntScope.end()) {
          llvm::errs() << "you can only compare a defined integer variable: "<< L->getVal() << "\n";
          HasError = true;
        } 
      }
      
      Final* R = (Final*)(Node.getRight());
      if(R){
        if (R->getKind() == Final::ValueKind::Ident && IntScope.find(R->getVal()) == IntScope.end()) {
          llvm::errs() << "you can only compare a defined integer variable: "<< R->getVal() << "\n";
          HasError = true;
        } 
      }
    }
  };

  virtual void visit(LogicalExpr &Node) override {
    if(Node.getLeft()){
      Node.getLeft()->accept(*this);
    }
    if(Node.getRight()){
      Node.getRight()->accept(*this);
    }
  };

  virtual void visit(UnaryOp &Node) override {
    if (IntScope.find(Node.getIdent()) == IntScope.end()){
      llvm::errs() << "Variable "<<Node.getIdent() << " is not a defined integer variable." << "\n";
      HasError = true;
    }
  };

  virtual void visit(NegExpr &Node) override {
    Expr *expr = Node.getExpr();
    (*expr).accept(*this);
  };

  virtual void visit(IfStmt &Node) override {
    Logic *l = Node.getCond();
    (*l).accept(*this);

    for (llvm::SmallVector<AST *>::const_iterator I = Node.begin(), E = Node.end(); I != E; ++I) {
      (*I)->accept(*this);
    }
    for (llvm::SmallVector<AST *>::const_iterator I = Node.beginElse(), E = Node.endElse(); I != E; ++I){
      (*I)->accept(*this);
    }
    for (llvm::SmallVector<elifStmt *>::const_iterator I = Node.beginElif(), E = Node.endElif(); I != E; ++I){
      (*I)->accept(*this);
    }
  };

  virtual void visit(elifStmt &Node) override {
    Logic* l = Node.getCond();
    (*l).accept(*this);

    for (llvm::SmallVector<AST *>::const_iterator I = Node.begin(), E = Node.end(); I != E; ++I) {
      (*I)->accept(*this);
    }
  };

  virtual void visit(WhileStmt &Node) override {
    Logic* l = Node.getCond();
    (*l).accept(*this);

    for (llvm::SmallVector<AST *>::const_iterator I = Node.begin(), E = Node.end(); I != E; ++I) {
      (*I)->accept(*this);
    }
  };

  virtual void visit(ForStmt &Node) override {
    Assignment *first = Node.getFirst();
    (*first).accept(*this);

    Logic *second = Node.getSecond();
    (*second).accept(*this);

    Assignment *assign = Node.getThirdAssign();
    if(assign)
      (*assign).accept(*this);
    else{
      UnaryOp *unary = Node.getThirdUnary();
      (*unary).accept(*this);
    }
      

    for (llvm::SmallVector<AST *>::const_iterator I = Node.begin(), E = Node.end(); I != E; ++I) {
      (*I)->accept(*this);
    }
  };

  virtual void visit(SignedNumber &Node) override {
  };

    // Implement the virtual methods to prevent the class from being abstract
    virtual void visit(CastExpr &Node) override {
        Node.getInner()->accept(*this);
    }

    virtual void visit(DeclareDefine &Node) override {
        llvm::StringRef varName = Node.getName();
        if (!DefineScope.insert(varName).second) {
            error(Twice, varName);
        }
    }

    virtual void visit(TernaryAssignment &Node) override {
        Node.getVariable()->accept(*this);
        Node.getCondition()->accept(*this);
        Node.getTrueExpr()->accept(*this);
        Node.getFalseExpr()->accept(*this);
    }

    virtual void visit(DoWhileStmt &Node) override {
        Node.getCond()->accept(*this);
        for (auto *stmt : Node.getBody()) {
            stmt->accept(*this);
        }
    }

    virtual void visit(SwitchStmt &Node) override {
    Node.getSwitchExpr()->accept(*this);
    for (auto *caseStmt : Node.getCases()) {
        caseStmt->accept(*this);
    }
    if (Node.getDefaultCase()) {
        Node.getDefaultCase()->accept(*this);
    }
}


   virtual void visit(CaseStmt &Node) override {
    Node.getCaseExpr()->accept(*this);
    for (auto *stmt : Node.getBody()) {
        stmt->accept(*this);
    }
}


   virtual void visit(DefaultStmt &Node) override {
    for (auto *stmt : Node.getBody()) {
        stmt->accept(*this);
    }
}


    virtual void visit(MinStmt &Node) override {
        Node.getLeft()->accept(*this);
        Node.getRight()->accept(*this);
    }

    virtual void visit(MaxStmt &Node) override {
        Node.getLeft()->accept(*this);
        Node.getRight()->accept(*this);
    }

    virtual void visit(MeanStmt &Node) override {
        Node.getLeft()->accept(*this);
        Node.getRight()->accept(*this);
    }

    virtual void visit(SqrtNStmt &Node) override {
        Node.getBase()->accept(*this);
        Node.getNthRoot()->accept(*this);
    }

    virtual void visit(BreakStmt &Node) override {
    // Implement any necessary logic for break statements
    // If break statements are not used in your language, you can leave this empty
    }
    // Helper Functions

    TypeKind inferType(AST *Node) {
        if (auto *finalNode = dynamic_cast<Final *>(Node)) {
            if (finalNode->getKind() == Final::Number)
                return TypeKind::Int;
            else if (finalNode->getKind() == Final::FloatNumber)
                return TypeKind::Float;
            else if (finalNode->getKind() == Final::Ident) {
                llvm::StringRef varName = finalNode->getVal();
                if (IntScope.find(varName) != IntScope.end())
                    return TypeKind::Int;
                if (BoolScope.find(varName) != BoolScope.end())
                    return TypeKind::Bool;
                if (FloatScope.find(varName) != FloatScope.end())
                    return TypeKind::Float;
                auto varIt = VarScope.find(varName.str());
                if (varIt != VarScope.end())
                    return varIt->second;
            }
        } else if (auto *binaryOp = dynamic_cast<BinaryOp *>(Node)) {
            TypeKind leftType = inferType(binaryOp->getLeft());
            TypeKind rightType = inferType(binaryOp->getRight());
            if (leftType == rightType)
                return leftType;
            return TypeKind::Unknown;
        } else if (auto *castExpr = dynamic_cast<CastExpr *>(Node)) {
            switch (castExpr->getCastType()) {
                case CastExpr::IntCast:
                    return TypeKind::Int;
                case CastExpr::FloatCast:
                    return TypeKind::Float;
                case CastExpr::BoolCast:
                    return TypeKind::Bool;
            }
        }
        return TypeKind::Unknown;
    }

    TypeKind getVariableType(llvm::StringRef varName) {
        if (IntScope.find(varName) != IntScope.end())
            return TypeKind::Int;
        if (BoolScope.find(varName) != BoolScope.end())
            return TypeKind::Bool;
        if (FloatScope.find(varName) != FloatScope.end())
            return TypeKind::Float;
        auto varIt = VarScope.find(varName.str());
        if (varIt != VarScope.end())
            return varIt->second;
        return TypeKind::Unknown;
    }
};
}

bool Sema::semantic(Program *Tree) {
    if (!Tree)
        return false;
    nms::InputCheck Check;
    Tree->accept(Check);
    return Check.hasError();
}
