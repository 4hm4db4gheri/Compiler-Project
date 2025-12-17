#include "CodeGen.h"
#include "llvm/ADT/StringMap.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/LLVMContext.h"
#include "llvm/Support/raw_ostream.h"
#include "llvm/ADT/SmallVector.h"
#include "llvm/IR/Constants.h"

using namespace llvm;

// Define a visitor class for generating LLVM IR from the AST.
namespace
ns{
  class ToIRVisitor : public ASTVisitor
  {
    Module *M;
    IRBuilder<> Builder;
    Type *VoidTy;
    Type *Int1Ty;
    Type *Int32Ty;
    Type *Int8PtrTy;
    Type *Int8PtrPtrTy;
    Constant *Int32Zero;
    Constant *Int32One;
    Constant *Int1False;
    Constant *Int1True;

    Value *V;
   StringMap<llvm::Value *> nameMapInt;
   StringMap<llvm::Value *> nameMapBool;
   StringMap<llvm::Value *> nameMapFloat;


    FunctionType *PrintIntFnTy;
    Function *PrintIntFn;

    FunctionType *PrintBoolFnTy;
    Function *PrintBoolFn;
    
    StringMap<llvm::Value *> globalNameMapInt;
    StringMap<llvm::Value *> globalNameMapBool;
    StringMap<llvm::Value *> globalNameMapFloat;

   std::vector<llvm::BasicBlock *> BreakTargets;
    
  Type *FloatTy;


  public:
    // Constructor for the visitor class.
    ToIRVisitor(Module *M) : M(M), Builder(M->getContext())
    {
      

      // Initialize LLVM types and constants.
      VoidTy = Type::getVoidTy(M->getContext());
      Int1Ty = Type::getInt1Ty(M->getContext());
      Int32Ty = Type::getInt32Ty(M->getContext());
      Int8PtrTy = Type::getInt8PtrTy(M->getContext());
      Int8PtrPtrTy = Int8PtrTy->getPointerTo();
      FloatTy = Type::getFloatTy(M->getContext());


      Int1False = ConstantInt::getFalse(Int1Ty);
      Int1True = ConstantInt::getTrue(Int1Ty);
      Int32Zero = ConstantInt::get(Int32Ty, 0, true);
      Int32One = ConstantInt::get(Int32Ty, 1, true);

      
      PrintIntFnTy = FunctionType::get(VoidTy, {Int32Ty}, false);
      // Create a function declaration for the "compiler_write" function.
      PrintIntFn = Function::Create(PrintIntFnTy, GlobalValue::ExternalLinkage, "print_int", M);

      PrintBoolFnTy = FunctionType::get(VoidTy, {Int1Ty}, false);
      // Create a function declaration for the "compiler_write" function.
      PrintBoolFn = Function::Create(PrintBoolFnTy, GlobalValue::ExternalLinkage, "print_bool", M);
    }

    // Entry point for generating LLVM IR from the AST.
    void run(Program *Tree)
    {
      // Create the main function with the appropriate function type.
      FunctionType *MainFty = FunctionType::get(Int32Ty, {Int32Ty, Int8PtrPtrTy}, false);
      Function *MainFn = Function::Create(MainFty, GlobalValue::ExternalLinkage, "main", M);

      // Create a basic block for the entry point of the main function.
      BasicBlock *BB = BasicBlock::Create(M->getContext(), "entry", MainFn);
      Builder.SetInsertPoint(BB);

      // Visit the root node of the AST to generate IR.
      Tree->accept(*this);

      // Create a return instruction at the end of the main function.
      Builder.CreateRet(Int32Zero);
    }

    // Visit function for the Program node in the AST.
    virtual void visit(Program &Node) override
    {
      // Iterate over the children of the Program node and visit each child.
      for (llvm::SmallVector<AST *>::const_iterator I = Node.begin(), E = Node.end(); I != E; ++I)
    {
      (*I)->accept(*this); // Visit each child node
    }
    };

    virtual void visit(DeclarationInt &Node) override
    {
      llvm::SmallVector<Value *, 8> vals;

      llvm::SmallVector<Expr *, 8>::const_iterator E = Node.valBegin();
      for (llvm::SmallVector<llvm::StringRef, 8>::const_iterator Var = Node.varBegin(), End = Node.varEnd(); Var != End; ++Var){
        if (E<Node.valEnd() && *E != nullptr)
        {
          (*E)->accept(*this); // If the Declaration node has an expression, recursively visit the expression node
          vals.push_back(V);
        }
        else 
        {
          vals.push_back(nullptr);
        }
        E++;
      }
      StringRef Var;
      Value* val;
      llvm::SmallVector<Value *, 8>::const_iterator itVal = vals.begin();
      for (llvm::SmallVector<llvm::StringRef, 8>::const_iterator S = Node.varBegin(), End = Node.varEnd(); S != End; ++S){
        
        Var = *S;

        // Create an alloca instruction to allocate memory for the variable.
        nameMapInt[Var] = Builder.CreateAlloca(Int32Ty);
        
        // Store the initial value (if any) in the variable's memory location.
        if (*itVal != nullptr)
        {
          Builder.CreateStore(*itVal, nameMapInt[Var]);
        }
        else
        {
          Builder.CreateStore(Int32Zero, nameMapInt[Var]);
        }
        itVal++;
      }
    };

    virtual void visit(DeclarationBool &Node) override
    {
      llvm::SmallVector<Value *, 8> vals;

      llvm::SmallVector<Logic *, 8>::const_iterator L = Node.valBegin();
      for (llvm::SmallVector<llvm::StringRef, 8>::const_iterator Var = Node.varBegin(), End = Node.varEnd(); Var != End; ++Var){
        if (L<Node.valEnd() && *L != nullptr)
        {
          (*L)->accept(*this); // If the Declaration node has an expression, recursively visit the expression node
          vals.push_back(V);
        }
        else 
        {
          vals.push_back(nullptr);
        }
        L++;
      }
      StringRef Var;
      Value* val;
      llvm::SmallVector<Value *, 8>::const_iterator itVal = vals.begin();
      for (llvm::SmallVector<llvm::StringRef, 8>::const_iterator S = Node.varBegin(), End = Node.varEnd(); S != End; ++S){
        
        Var = *S;

        // Create an alloca instruction to allocate memory for the variable.
        nameMapBool[Var] = Builder.CreateAlloca(Int1Ty);
        
        // Store the initial value (if any) in the variable's memory location.
        if (*itVal != nullptr)
        {
          Builder.CreateStore(*itVal, nameMapBool[Var]);
        }
        else
        {
          Builder.CreateStore(Int1False, nameMapBool[Var]);
        }
        itVal++;
      }
    };
    
    virtual void visit(DeclarationFloat &Node) override {
    llvm::SmallVector<Value *, 8> vals;

    llvm::SmallVector<Expr *, 8>::const_iterator E = Node.valBegin();
    for (llvm::SmallVector<llvm::StringRef, 8>::const_iterator Var = Node.varBegin(), End = Node.varEnd(); Var != End; ++Var){
        if (E < Node.valEnd() && *E != nullptr)
        {
            (*E)->accept(*this); // Visit the initializer expression
            vals.push_back(V);
        }
        else 
        {
            vals.push_back(nullptr);
        }
        E++;
    }
    StringRef Var;
    Value* val;
    llvm::SmallVector<Value *, 8>::const_iterator itVal = vals.begin();
    for (llvm::SmallVector<llvm::StringRef, 8>::const_iterator S = Node.varBegin(), End = Node.varEnd(); S != End; ++S){
        Var = *S;

        // Create an alloca instruction to allocate memory for the variable.
        nameMapFloat[Var] = Builder.CreateAlloca(FloatTy);

        // Store the initial value (if any) in the variable's memory location.
        if (*itVal != nullptr)
        {
            Builder.CreateStore(*itVal, nameMapFloat[Var]);
        }
        else
        {
            // Initialize with 0.0
            Builder.CreateStore(ConstantFP::get(FloatTy, 0.0), nameMapFloat[Var]);
        }
        itVal++;
    }
}

    virtual void visit(Assignment &Node) override {
    llvm::StringRef varName = Node.getVariable()->getVal();

    if (Node.getRightExpr() != nullptr)
        Node.getRightExpr()->accept(*this);
    else if (Node.getRightLogic() != nullptr)
        Node.getRightLogic()->accept(*this);
    else
        llvm::errs() << "Assignment node has neither RightExpr nor RightLogic\n";

    Value *val = V;

    if (isInt(varName)) {
        // Handle integer assignment
        if (val->getType()->isFloatingPointTy())
            val = Builder.CreateFPToSI(val, Int32Ty);

        Value *varVal = Builder.CreateLoad(Int32Ty, nameMapInt[varName]);
        switch (Node.getAssignKind())
        {
        case Assignment::Assign:
            break;
        case Assignment::Plus_assign:
            val = Builder.CreateAdd(varVal, val);
            break;
        case Assignment::Minus_assign:
            val = Builder.CreateSub(varVal, val);
            break;
        case Assignment::Star_assign:
            val = Builder.CreateMul(varVal, val);
            break;
        case Assignment::Slash_assign:
            val = Builder.CreateSDiv(varVal, val);
            break;
        case Assignment::Mod_assign:
            val = Builder.CreateSRem(varVal, val);
            break;
        default:
            break;
        }
        Builder.CreateStore(val, nameMapInt[varName]);
    }
    else if (isFloat(varName)) {
        // Handle float assignment
        if (val->getType()->isIntegerTy())
            val = Builder.CreateSIToFP(val, FloatTy);

        Value *varVal = Builder.CreateLoad(FloatTy, nameMapFloat[varName]);
        switch (Node.getAssignKind())
        {
        case Assignment::Assign:
            break;
        case Assignment::Plus_assign:
            val = Builder.CreateFAdd(varVal, val);
            break;
        case Assignment::Minus_assign:
            val = Builder.CreateFSub(varVal, val);
            break;
        case Assignment::Star_assign:
            val = Builder.CreateFMul(varVal, val);
            break;
        case Assignment::Slash_assign:
            val = Builder.CreateFDiv(varVal, val);
            break;
        default:
            break;
        }
        Builder.CreateStore(val, nameMapFloat[varName]);
    }
    else if (isBool(varName)) {
        // Handle boolean assignment
        if (val->getType()->isIntegerTy(32))
            val = Builder.CreateICmpNE(val, ConstantInt::get(Int32Ty, 0));
        else if (val->getType()->isFloatingPointTy())
            val = Builder.CreateFCmpONE(val, ConstantFP::get(FloatTy, 0.0));

        Builder.CreateStore(val, nameMapBool[varName]);
    }
    else {
        llvm::errs() << "Undefined variable in assignment: " << varName << "\n";
    }
}

  virtual void visit(Final &Node) override {
    if (Node.getKind() == Final::Ident)
    {   
        // Retrieve the variable pointer
        llvm::StringRef varName = Node.getVal();
        llvm::Value *varPtr = getVariablePointer(varName);

        if (varPtr == nullptr) {
            llvm::errs() << "Undefined variable: " << varName << "\n";
            V = nullptr;
            return;
        }

        // Load the value from the variable's storage
        llvm::Type *varType = varPtr->getType()->getPointerElementType();
        V = Builder.CreateLoad(varType, varPtr);
    }
    else
    {
        // Handle literals
        switch (Node.getKind())
        {
        case Final::Number:
        {
            int64_t intval;
            if (Node.getVal().getAsInteger(10, intval)) {
                llvm::errs() << "Invalid integer literal: " << Node.getVal() << "\n";
                V = nullptr;
            } else {
                V = ConstantInt::get(Int32Ty, intval, true);
            }
            break;
        }
        case Final::FloatNumber:
        {
            double floatval;
            if (Node.getVal().getAsDouble(floatval)) {
                llvm::errs() << "Invalid float literal: " << Node.getVal() << "\n";
                V = nullptr;
            } else {
                V = ConstantFP::get(FloatTy, floatval);
            }
            break;
        }
        case Final::Bool:
        {
            if (Node.getVal() == "true")
                V = Int1True;
            else if (Node.getVal() == "false")
                V = Int1False;
            else {
                llvm::errs() << "Invalid boolean literal: " << Node.getVal() << "\n";
                V = nullptr;
            }
            break;
        }
        default:
            llvm::errs() << "Unknown Final kind: " << Node.getKind() << "\n";
            V = nullptr;
            break;
        }
    }
}

    virtual void visit(BinaryOp &Node) override {
    Node.getLeft()->accept(*this);
    Value *Left = V;

    Node.getRight()->accept(*this);
    Value *Right = V;

    Type *LeftTy = Left->getType();
    Type *RightTy = Right->getType();

    // Promote integer to float if necessary
    if (LeftTy->isIntegerTy() && RightTy->isFloatingPointTy()) {
        Left = Builder.CreateSIToFP(Left, FloatTy);
        LeftTy = FloatTy;
    }
    else if (LeftTy->isFloatingPointTy() && RightTy->isIntegerTy()) {
        Right = Builder.CreateSIToFP(Right, FloatTy);
        RightTy = FloatTy;
    }

    if (LeftTy->isIntegerTy() && RightTy->isIntegerTy()) {
        // Integer operations
        switch (Node.getOperator())
        {
        case BinaryOp::Plus:
            V = Builder.CreateNSWAdd(Left, Right);
            break;
        case BinaryOp::Minus:
            V = Builder.CreateNSWSub(Left, Right);
            break;
        case BinaryOp::Mul:
            V = Builder.CreateNSWMul(Left, Right);
            break;
        case BinaryOp::Div:
            V = Builder.CreateSDiv(Left, Right);
            break;
        case BinaryOp::Mod:
            V = Builder.CreateSRem(Left, Right);
            break;
        case BinaryOp::Exp:
            V = CreateExp(Left, Right);
            break;
        default:
            llvm::errs() << "Unknown binary operator\n";
            V = nullptr;
            break;
        }
    }
    else if (LeftTy->isFloatingPointTy() && RightTy->isFloatingPointTy()) {
        // Floating-point operations
        switch (Node.getOperator())
        {
        case BinaryOp::Plus:
            V = Builder.CreateFAdd(Left, Right);
            break;
        case BinaryOp::Minus:
            V = Builder.CreateFSub(Left, Right);
            break;
        case BinaryOp::Mul:
            V = Builder.CreateFMul(Left, Right);
            break;
        case BinaryOp::Div:
            V = Builder.CreateFDiv(Left, Right);
            break;
        default:
            llvm::errs() << "Unsupported operator for floats\n";
            V = nullptr;
            break;
        }
    }
    else {
        llvm::errs() << "Type mismatch in binary operation\n";
        V = nullptr;
    }
}

    Value* CreateExp(Value *Left, Value *Right)
    {
      AllocaInst* counterAlloca = Builder.CreateAlloca(Int32Ty);
      AllocaInst* resultAlloca = Builder.CreateAlloca(Int32Ty);
      Builder.CreateStore(Int32Zero, counterAlloca);
      Builder.CreateStore(Int32One, resultAlloca);

      llvm::BasicBlock* ForCondBB = llvm::BasicBlock::Create(M->getContext(), "exp.cond", Builder.GetInsertBlock()->getParent());
      llvm::BasicBlock* ForBodyBB = llvm::BasicBlock::Create(M->getContext(), "exp.body", Builder.GetInsertBlock()->getParent());
      llvm::BasicBlock* AfterForBB = llvm::BasicBlock::Create(M->getContext(), "after.exp", Builder.GetInsertBlock()->getParent());

      Builder.CreateBr(ForCondBB); //?

      Builder.SetInsertPoint(ForCondBB);
      Value* counterLoad = Builder.CreateLoad(counterAlloca->getAllocatedType(), counterAlloca);


      Value *cond = Builder.CreateICmpSLT(counterLoad, Right);
      Builder.CreateCondBr(cond, ForBodyBB, AfterForBB);

      Builder.SetInsertPoint(ForBodyBB);
      Value* resultLoad = Builder.CreateLoad(resultAlloca->getAllocatedType(), resultAlloca);


      Value* resultMul = Builder.CreateMul(resultLoad, Left);
      Value* counterInc = Builder.CreateAdd(counterLoad, Int32One);
      Builder.CreateStore(resultMul, resultAlloca);
      Builder.CreateStore(counterInc, counterAlloca);

      Builder.CreateBr(ForCondBB);
      Builder.SetInsertPoint(AfterForBB);

      Value* result = Builder.CreateLoad(resultAlloca->getAllocatedType(), resultAlloca);

      return result;
    }

    virtual void visit(UnaryOp &Node) override
    {
      // Visit the left-hand side of the binary operation and get its value.
      Value *Left = Builder.CreateLoad(Int32Ty, nameMapInt[Node.getIdent()]);;

      // Perform the binary operation based on the operator type and create the corresponding instruction.
      switch (Node.getOperator())
      {
      case UnaryOp::Plus_plus:
        V = Builder.CreateNSWAdd(Left, Int32One);
        break;
      case UnaryOp::Minus_minus:
        V = Builder.CreateNSWSub(Left, Int32One);
      default:
        break;
      }
      
      Builder.CreateStore(V, nameMapInt[Node.getIdent()]);
    };

    virtual void visit(SignedNumber &Node) override
    {
      int intval;
      Node.getValue().getAsInteger(10, intval);
      V = ConstantInt::get(Int32Ty, (Node.getSign() == SignedNumber::Minus) ? -intval : intval, true);
    };

    virtual void visit(NegExpr &Node) override
    {
      Node.getExpr()->accept(*this);
      V = Builder.CreateNeg(V);
    };

    virtual void visit(LogicalExpr &Node) override{
      // Visit the left-hand side of the Logical operation and get its value.
      Node.getLeft()->accept(*this);
      Value *Left = V;

      if (Node.getRight() == nullptr)
      {
        V = Left;
        return; 
      }
      // Visit the right-hand side of the Logical operation and get its value.
      Node.getRight()->accept(*this);
      Value *Right = V;

      switch (Node.getOperator())
      {
      case LogicalExpr::And:
        V = Builder.CreateAnd(Left, Right);
        break;
      case LogicalExpr::Or:
        V = Builder.CreateOr(Left, Right);
        break;
      default:
        break;
      }
    };

    virtual void visit(Comparison &Node) override{
      // Visit the left-hand side of the Comparison operation and get its value.
      if (Node.getRight() == nullptr)
      {
        switch (Node.getOperator())
        {
        case Comparison::True:
          V = Int1True;
          break;
        case Comparison::False:
          V = Int1False;
          break;
        case Comparison::Ident: 
          if(isBool(((Final*)Node.getLeft())->getVal())){
            V = Builder.CreateLoad(Int1Ty, nameMapBool[((Final*)Node.getLeft())->getVal()]);
            break;
          }
          
          V = Builder.CreateLoad(Int32Ty, nameMapInt[((Final*)Node.getLeft())->getVal()]);
          break;
        
        default:
          break;
        }
        return;
      }
      Node.getLeft()->accept(*this);
      Value *Left = V;

      // Visit the right-hand side of the Comparison operation and get its value.
      Node.getRight()->accept(*this);
      Value *Right = V;

      switch (Node.getOperator())
      {
      case Comparison::Equal:
        V = Builder.CreateICmpEQ(Left, Right);
        break;
      case Comparison::Not_equal:
        V = Builder.CreateICmpNE(Left, Right);
        break;
      case Comparison::Less:
        V = Builder.CreateICmpSLT(Left, Right);
        break;
      case Comparison::Greater:
        V = Builder.CreateICmpSGT(Left, Right);
        break;
      case Comparison::Less_equal:
        V = Builder.CreateICmpSLE(Left, Right);
        break;
      case Comparison::Greater_equal:
        V = Builder.CreateICmpSGE(Left, Right);
        break;
      default:
        break;
      }
    };

    virtual void visit(PrintStmt &Node) override {
    // Evaluate the expression to print
    Node.getVar()->accept(*this);
    Value *val = V;

    // Determine the type of the value
    Type *valType = val->getType();

    if (valType->isIntegerTy(1)) {
        // Boolean value
        CallInst *Call = Builder.CreateCall(PrintBoolFnTy, PrintBoolFn, {val});
    } else if (valType->isIntegerTy(32)) {
        // Integer value
        CallInst *Call = Builder.CreateCall(PrintIntFnTy, PrintIntFn, {val});
    }else {
        llvm::errs() << "Unsupported type in PrintStmt.\n";
    }
}

    virtual void visit(WhileStmt &Node) override {
    llvm::Function *TheFunction = Builder.GetInsertBlock()->getParent();

    // Create blocks for the loop condition, body, and after the loop
    llvm::BasicBlock *WhileCondBB = llvm::BasicBlock::Create(M->getContext(), "while.cond", TheFunction);
    llvm::BasicBlock *WhileBodyBB = llvm::BasicBlock::Create(M->getContext(), "while.body", TheFunction);
    llvm::BasicBlock *AfterWhileBB = llvm::BasicBlock::Create(M->getContext(), "after.while", TheFunction);

    // Insert a branch to the loop condition
    Builder.CreateBr(WhileCondBB);

    // Start insertion in the loop condition block
    Builder.SetInsertPoint(WhileCondBB);

    // Evaluate the loop condition
    Node.getCond()->accept(*this);
    llvm::Value *CondV = V;

    // Create the conditional branch based on the loop condition
    Builder.CreateCondBr(CondV, WhileBodyBB, AfterWhileBB);

    // Start insertion in the loop body block
    Builder.SetInsertPoint(WhileBodyBB);

    // **Push the break target before visiting the loop body**
    BreakTargets.push_back(AfterWhileBB);

    // Generate code for the loop body
    for (auto *Stmt : Node.getBody()) {
        Stmt->accept(*this);
    }

    // **Pop the break target after visiting the loop body**
    BreakTargets.pop_back();

    // After the loop body, branch back to the loop condition
    Builder.CreateBr(WhileCondBB);

    // Continue insertion in the block after the loop
    Builder.SetInsertPoint(AfterWhileBB);
}

    virtual void visit(ForStmt &Node) override {
    llvm::Function *TheFunction = Builder.GetInsertBlock()->getParent();

    // Create blocks for the loop components
    llvm::BasicBlock *ForCondBB = llvm::BasicBlock::Create(M->getContext(), "for.cond", TheFunction);
    llvm::BasicBlock *ForBodyBB = llvm::BasicBlock::Create(M->getContext(), "for.body", TheFunction);
    llvm::BasicBlock *ForIncBB = llvm::BasicBlock::Create(M->getContext(), "for.inc", TheFunction);
    llvm::BasicBlock *AfterForBB = llvm::BasicBlock::Create(M->getContext(), "after.for", TheFunction);

    // Evaluate the initialization statement
    if (Node.getFirst())
        Node.getFirst()->accept(*this);

    // Branch to the loop condition
    Builder.CreateBr(ForCondBB);

    // Loop condition
    Builder.SetInsertPoint(ForCondBB);
    Node.getSecond()->accept(*this);
    llvm::Value *CondV = V;

    // Conditional branch to loop body or after loop
    Builder.CreateCondBr(CondV, ForBodyBB, AfterForBB);

    // Loop body
    Builder.SetInsertPoint(ForBodyBB);

    // **Push the break target**
    BreakTargets.push_back(AfterForBB);

    // Generate code for the loop body
    for (auto *Stmt : Node.getBody()) {
        Stmt->accept(*this);
    }

    // **Pop the break target**
    BreakTargets.pop_back();

    // After loop body, branch to increment block
    Builder.CreateBr(ForIncBB);

    // Loop increment
    Builder.SetInsertPoint(ForIncBB);
    if (Node.getThirdAssign())
        Node.getThirdAssign()->accept(*this);
    else if (Node.getThirdUnary())
        Node.getThirdUnary()->accept(*this);

    // Branch back to loop condition
    Builder.CreateBr(ForCondBB);

    // After loop
    Builder.SetInsertPoint(AfterForBB);
}

    virtual void visit(IfStmt &Node) override{
      llvm::BasicBlock* IfCondBB = llvm::BasicBlock::Create(M->getContext(), "if.cond", Builder.GetInsertBlock()->getParent());
      llvm::BasicBlock* IfBodyBB = llvm::BasicBlock::Create(M->getContext(), "if.body", Builder.GetInsertBlock()->getParent());
      llvm::BasicBlock* AfterIfBB = llvm::BasicBlock::Create(M->getContext(), "after.if", Builder.GetInsertBlock()->getParent());

      Builder.CreateBr(IfCondBB); //?
      Builder.SetInsertPoint(IfCondBB);
      Node.getCond()->accept(*this);
      Value* IfCondVal=V;

      Builder.SetInsertPoint(IfBodyBB);

      for (llvm::SmallVector<AST* >::const_iterator I = Node.begin(), E = Node.end(); I != E; ++I)
        {
            (*I)->accept(*this);
        }

      Builder.CreateBr(AfterIfBB);

      llvm::BasicBlock* PreviousCondBB = IfCondBB;
      llvm::BasicBlock* PreviousBodyBB = IfBodyBB;
      Value* PreviousCondVal = IfCondVal;

      for (llvm::SmallVector<elifStmt *, 8>::const_iterator I = Node.beginElif(), E = Node.endElif(); I != E; ++I)
      {
        llvm::BasicBlock* ElifCondBB = llvm::BasicBlock::Create(M->getContext(), "elif.cond", Builder.GetInsertBlock()->getParent());
        llvm::BasicBlock* ElifBodyBB = llvm::BasicBlock::Create(M->getContext(), "elif.body", Builder.GetInsertBlock()->getParent());

        Builder.SetInsertPoint(PreviousCondBB);
        Builder.CreateCondBr(PreviousCondVal, PreviousBodyBB, ElifCondBB);

        Builder.SetInsertPoint(ElifCondBB);
        (*I)->getCond()->accept(*this);
        Value* ElifCondVal = V;

        Builder.SetInsertPoint(ElifBodyBB);
        (*I)->accept(*this);
        Builder.CreateBr(AfterIfBB);

        PreviousCondBB = ElifCondBB;
        PreviousCondVal = ElifCondVal;
        PreviousBodyBB = ElifBodyBB;
      }
      if (Node.beginElse() != Node.endElse()) {
        llvm::BasicBlock* ElseBB = llvm::BasicBlock::Create(M->getContext(), "else.body", Builder.GetInsertBlock()->getParent());
        Builder.SetInsertPoint(ElseBB);
        for (llvm::SmallVector<AST* >::const_iterator I = Node.beginElse(), E = Node.endElse(); I != E; ++I)
        {
            (*I)->accept(*this);
        }
        Builder.CreateBr(AfterIfBB);

        Builder.SetInsertPoint(PreviousCondBB);
        Builder.CreateCondBr(PreviousCondVal, PreviousBodyBB, ElseBB);
      }
      else {
        Builder.SetInsertPoint(PreviousCondBB);
        Builder.CreateCondBr(IfCondVal, PreviousBodyBB, AfterIfBB);
      }

      Builder.SetInsertPoint(AfterIfBB);
    };

    virtual void visit(elifStmt &Node) override{
      for (llvm::SmallVector<AST* >::const_iterator I = Node.begin(), E = Node.end(); I != E; ++I)
        {
            (*I)->accept(*this);
        }
    };
  
    virtual void visit(DeclareDefine &Node) override {
    llvm::StringRef varName = Node.getName();
    Node.getValue()->accept(*this);
    Value *val = V;

    // Since it's a constant, define it as a global constant
    if (val->getType()->isIntegerTy(32)) {
        GlobalVariable *gVar = new GlobalVariable(
            *M,
            Int32Ty,
            true, // isConstant
            GlobalValue::PrivateLinkage,
            dyn_cast<ConstantInt>(val),
            varName);
        nameMapInt[varName] = gVar;
    } else if (val->getType()->isFloatingPointTy()) {
        GlobalVariable *gVar = new GlobalVariable(
            *M,
            FloatTy,
            true,
            GlobalValue::PrivateLinkage,
            dyn_cast<ConstantFP>(val),
            varName);
        nameMapFloat[varName] = gVar;
    } else if (val->getType()->isIntegerTy(1)) {
        GlobalVariable *gVar = new GlobalVariable(
            *M,
            Int1Ty,
            true,
            GlobalValue::PrivateLinkage,
            dyn_cast<ConstantInt>(val),
            varName);
        nameMapBool[varName] = gVar;
    } else {
        llvm::errs() << "Unsupported type in DeclareDefine\n";
    }
}

    virtual void visit(DoWhileStmt &Node) override {
    llvm::Function *TheFunction = Builder.GetInsertBlock()->getParent();

    // Create blocks for the loop body, condition, and after the loop
    llvm::BasicBlock *LoopBodyBB = llvm::BasicBlock::Create(M->getContext(), "dowhile.body", TheFunction);
    llvm::BasicBlock *LoopCondBB = llvm::BasicBlock::Create(M->getContext(), "dowhile.cond", TheFunction);
    llvm::BasicBlock *AfterLoopBB = llvm::BasicBlock::Create(M->getContext(), "after.dowhile", TheFunction);

    // Branch to the loop body
    Builder.CreateBr(LoopBodyBB);

    // Loop body
    Builder.SetInsertPoint(LoopBodyBB);

    // **Push the break target**
    BreakTargets.push_back(AfterLoopBB);

    // Generate code for the loop body
    for (auto *Stmt : Node.getBody()) {
        Stmt->accept(*this);
    }

    // **Pop the break target**
    BreakTargets.pop_back();

    // After the body, branch to the condition
    Builder.CreateBr(LoopCondBB);

    // Loop condition
    Builder.SetInsertPoint(LoopCondBB);
    Node.getCond()->accept(*this);
    llvm::Value *CondV = V;
    Builder.CreateCondBr(CondV, LoopBodyBB, AfterLoopBB);

    // Continue after the loop
    Builder.SetInsertPoint(AfterLoopBB);
}

    virtual void visit(SwitchStmt &Node) override {
    Node.getSwitchExpr()->accept(*this);
    Value *SwitchValue = V;

    Function *TheFunction = Builder.GetInsertBlock()->getParent();
    BasicBlock *AfterSwitchBB = BasicBlock::Create(M->getContext(), "after.switch", TheFunction);

    SwitchInst *Switch = Builder.CreateSwitch(SwitchValue, AfterSwitchBB, Node.getCases().size());

    std::vector<BasicBlock *> CaseBBs;
    auto CaseIt = Node.getCases().begin();
    for (CaseStmt *CaseNode : Node.getCases()) {
        BasicBlock *CaseBB = BasicBlock::Create(M->getContext(), "case", TheFunction);
        CaseBBs.push_back(CaseBB);

        // Get the case value
        CaseNode->getCaseExpr()->accept(*this);
        Value *CaseValue = V;
        if (ConstantInt *CI = dyn_cast<ConstantInt>(CaseValue)) {
            Switch->addCase(CI, CaseBB);
        } else {
            llvm::errs() << "Case value is not a constant integer\n";
        }
    }

    if (Node.getDefaultCase()) {
        BasicBlock *DefaultBB = BasicBlock::Create(M->getContext(), "default", TheFunction);
        Switch->setDefaultDest(DefaultBB);

        Builder.SetInsertPoint(DefaultBB);
        for (AST *stmt : Node.getDefaultCase()->getBody()) {
            stmt->accept(*this);
        }
        Builder.CreateBr(AfterSwitchBB);
    } else {
        Switch->setDefaultDest(AfterSwitchBB);
    }

    // Generate code for each case
    CaseIt = Node.getCases().begin();
    for (BasicBlock *CaseBB : CaseBBs) {
        Builder.SetInsertPoint(CaseBB);
        for (AST *stmt : (*CaseIt)->getBody()) {
            stmt->accept(*this);
        }
        Builder.CreateBr(AfterSwitchBB);
        ++CaseIt;
    }

    // Continue after switch
    Builder.SetInsertPoint(AfterSwitchBB);
}

    virtual void visit(CastExpr &Node) override {
    Node.getInner()->accept(*this);
    Value *InnerValue = V;
    Type *TargetType = nullptr;

    switch (Node.getCastType()) {
    case CastExpr::IntCast:
        TargetType = Int32Ty;
        break;
    case CastExpr::BoolCast:
        TargetType = Int1Ty;
        break;
    case CastExpr::FloatCast:
        TargetType = FloatTy;
        break;
    default:
        llvm::errs() << "Unknown cast type\n";
        V = nullptr;
        return;
    }

    Type *InnerType = InnerValue->getType();
    if (InnerType == TargetType) {
        V = InnerValue;
    } else if (InnerType->isIntegerTy() && TargetType->isFloatingPointTy()) {
        V = Builder.CreateSIToFP(InnerValue, TargetType);
    } else if (InnerType->isFloatingPointTy() && TargetType->isIntegerTy()) {
        V = Builder.CreateFPToSI(InnerValue, TargetType);
    } else if (InnerType->isIntegerTy(1) && TargetType->isIntegerTy(32)) {
        V = Builder.CreateZExt(InnerValue, TargetType);
    } else if (InnerType->isIntegerTy(32) && TargetType->isIntegerTy(1)) {
        V = Builder.CreateICmpNE(InnerValue, ConstantInt::get(Int32Ty, 0));
    } else {
        llvm::errs() << "Unsupported cast from " << *InnerType << " to " << *TargetType << "\n";
        V = nullptr;
    }
}

    virtual void visit(BoolCastExpr &Node) override {
    Node.getInner()->accept(*this);
    Value *InnerValue = V;
    Type *TargetType = nullptr;

    switch (Node.getCastType()) {
    case BoolCastExpr::IntCast:
        TargetType = Int32Ty;
        break;
    case BoolCastExpr::BoolCast:
        TargetType = Int1Ty;
        break;
    case BoolCastExpr::FloatCast:
        TargetType = FloatTy;
        break;
    default:
        llvm::errs() << "Unknown cast type\n";
        V = nullptr;
        return;
    }

    Type *InnerType = InnerValue->getType();
    if (InnerType == TargetType) {
        V = InnerValue;
    } else if (InnerType->isIntegerTy() && TargetType->isFloatingPointTy()) {
        V = Builder.CreateSIToFP(InnerValue, TargetType);
    } else if (InnerType->isFloatingPointTy() && TargetType->isIntegerTy()) {
        V = Builder.CreateFPToSI(InnerValue, TargetType);
    } else if (InnerType->isIntegerTy(1) && TargetType->isIntegerTy(32)) {
        V = Builder.CreateZExt(InnerValue, TargetType);
    } else if (InnerType->isIntegerTy(32) && TargetType->isIntegerTy(1)) {
        V = Builder.CreateICmpNE(InnerValue, ConstantInt::get(Int32Ty, 0));
    } else {
        llvm::errs() << "Unsupported cast from " << *InnerType << " to " << *TargetType << "\n";
        V = nullptr;
    }
}

    virtual void visit(DefaultStmt &Node) override {
    // Generate code for the default case body
    for (auto *Stmt : Node.getBody()) {
        Stmt->accept(*this);
    }
}
  
    virtual void visit(BreakStmt &Node) override {
    if (BreakTargets.empty()) {
        llvm::errs() << "Break statement not within a loop or switch\n";
        return;
    }
    Builder.CreateBr(BreakTargets.back());
    // Optionally, insert an unreachable block
    BasicBlock *UnreachableBB = BasicBlock::Create(M->getContext(), "unreachable", Builder.GetInsertBlock()->getParent());
    Builder.SetInsertPoint(UnreachableBB);
}
  
    virtual void visit(ContinueStmt &Node) override {
    if (BreakTargets.empty()) {
        llvm::errs() << "Break statement not within a loop or switch\n";
        return;
    }
    Builder.CreateBr(BreakTargets.back());
    // Optionally, insert an unreachable block
    // BasicBlock *UnreachableBB = BasicBlock::Create(M->getContext(), "unreachable", Builder.GetInsertBlock());
    Builder.SetInsertPoint(Builder.GetInsertBlock());
}

    virtual void visit(DeclarationVar &Node) override {
    auto VarIt = Node.varBegin();
    auto ValIt = Node.valBegin();
    auto TypeIt = Node.typeBegin();

    for (; VarIt != Node.varEnd(); ++VarIt, ++ValIt, ++TypeIt) {
        llvm::StringRef varName = *VarIt;
        AST *valueAST = *ValIt; // May be nullptr
        TypeKind varTypeKind = *TypeIt;

        llvm::Type *llvmType = nullptr;
        Value *initialValue = nullptr;

        switch (varTypeKind) {
        case TypeKind::Int:
            llvmType = Int32Ty;
            if (valueAST != nullptr) {
                valueAST->accept(*this);
                initialValue = V;
                // Cast to int if necessary
                if (initialValue->getType()->isFloatingPointTy())
                    initialValue = Builder.CreateFPToSI(initialValue, Int32Ty);
                else if (initialValue->getType()->isIntegerTy(1))
                    initialValue = Builder.CreateZExt(initialValue, Int32Ty);
            } else {
                initialValue = Int32Zero;
            }
            if (Node.isConst) {
                // For constants, define as global constant
                auto *gVar = new GlobalVariable(
                    *M,
                    llvmType,
                    true, // isConstant
                    GlobalValue::PrivateLinkage,
                    dyn_cast<ConstantInt>(initialValue),
                    varName);
                globalNameMapInt[varName] = gVar;
            } else {
                // Local variable
                AllocaInst *alloca = Builder.CreateAlloca(Int32Ty, nullptr, varName);
                nameMapInt[varName] = alloca;
                Builder.CreateStore(initialValue, alloca);
            }
            break;
        case TypeKind::Float:
            llvmType = FloatTy;
            if (valueAST != nullptr) {
                valueAST->accept(*this);
                initialValue = V;
                // Cast to float if necessary
                if (initialValue->getType()->isIntegerTy())
                    initialValue = Builder.CreateSIToFP(initialValue, FloatTy);
            } else {
                initialValue = ConstantFP::get(FloatTy, 0.0);
            }
            if (Node.isConst) {
                auto *gVar = new GlobalVariable(
                    *M,
                    llvmType,
                    true,
                    GlobalValue::PrivateLinkage,
                    dyn_cast<ConstantFP>(initialValue),
                    varName);
                globalNameMapFloat[varName] = gVar;
            } else {
                AllocaInst *alloca = Builder.CreateAlloca(FloatTy, nullptr, varName);
                nameMapFloat[varName] = alloca;
                Builder.CreateStore(initialValue, alloca);
            }
            break;
        case TypeKind::Bool:
            llvmType = Int1Ty;
            if (valueAST != nullptr) {
                valueAST->accept(*this);
                initialValue = V;
                // Ensure it's boolean
                if (initialValue->getType()->isIntegerTy(32))
                    initialValue = Builder.CreateICmpNE(initialValue, Int32Zero);
                else if (initialValue->getType()->isFloatingPointTy())
                    initialValue = Builder.CreateFCmpUNE(initialValue, ConstantFP::get(FloatTy, 0.0));
            } else {
                initialValue = Int1False;
            }
            if (Node.isConst) {
                auto *gVar = new GlobalVariable(
                    *M,
                    llvmType,
                    true,
                    GlobalValue::PrivateLinkage,
                    dyn_cast<ConstantInt>(initialValue),
                    varName);
                globalNameMapBool[varName] = gVar;
            } else {
                AllocaInst *alloca = Builder.CreateAlloca(Int1Ty, nullptr, varName);
                nameMapBool[varName] = alloca;
                Builder.CreateStore(initialValue, alloca);
            }
            break;
        default:
            llvm::errs() << "Unknown type in DeclarationVar\n";
            break;
        }
    }
}

    virtual void visit(TernaryAssignment &Node) override {
    // Evaluate the condition
    Node.getCondition()->accept(*this);
    Value *CondV = V;

    // Create basic blocks for true and false cases and the merge block
    Function *TheFunction = Builder.GetInsertBlock()->getParent();

    BasicBlock *TrueBB = BasicBlock::Create(M->getContext(), "ternary.true", TheFunction);
    BasicBlock *FalseBB = BasicBlock::Create(M->getContext(), "ternary.false");
    BasicBlock *MergeBB = BasicBlock::Create(M->getContext(), "ternary.end");

    // Conditional branch based on the condition
    Builder.CreateCondBr(CondV, TrueBB, FalseBB);

    // Emit true block
    Builder.SetInsertPoint(TrueBB);
    Node.getTrueExpr()->accept(*this);
    Value *TrueV = V;
    Builder.CreateBr(MergeBB);
    TrueBB = Builder.GetInsertBlock(); // Update after possible new blocks

    // Emit false block
    FalseBB->insertInto(TheFunction);

    Builder.SetInsertPoint(FalseBB);
    Node.getFalseExpr()->accept(*this);
    Value *FalseV = V;
    Builder.CreateBr(MergeBB);
    FalseBB = Builder.GetInsertBlock();

    // Emit merge block
    MergeBB->insertInto(TheFunction);

    Builder.SetInsertPoint(MergeBB);
    PHINode *PN = Builder.CreatePHI(TrueV->getType(), 2, "ternary.result");
    PN->addIncoming(TrueV, TrueBB);
    PN->addIncoming(FalseV, FalseBB);

    // Assign PN to Variable
    llvm::StringRef varName = Node.getVariable()->getVal();
    llvm::Value *varPtr = getVariablePointer(varName);

    if (varPtr == nullptr) {
        llvm::errs() << "Undefined variable in TernaryAssignment: " << varName << "\n";
        return;
    }
    Builder.CreateStore(PN, varPtr);
}

    virtual void visit(MinStmt &Node) override {
    Node.getLeft()->accept(*this);
    Value *Left = V;
    Node.getRight()->accept(*this);
    Value *Right = V;

    if (Left->getType()->isFloatingPointTy()) {
        Value *Cmp = Builder.CreateFCmpOLT(Left, Right);
        V = Builder.CreateSelect(Cmp, Left, Right);
    } else {
        Value *Cmp = Builder.CreateICmpSLT(Left, Right);
        V = Builder.CreateSelect(Cmp, Left, Right);
    }
}

    virtual void visit(MaxStmt &Node) override {
    Node.getLeft()->accept(*this);
    Value *Left = V;
    Node.getRight()->accept(*this);
    Value *Right = V;

    if (Left->getType()->isFloatingPointTy()) {
        Value *Cmp = Builder.CreateFCmpOGT(Left, Right);
        V = Builder.CreateSelect(Cmp, Left, Right);
    } else {
        Value *Cmp = Builder.CreateICmpSGT(Left, Right);
        V = Builder.CreateSelect(Cmp, Left, Right);
    }
}

    virtual void visit(MeanStmt &Node) override {
    Node.getLeft()->accept(*this);
    Value *Left = V;
    Node.getRight()->accept(*this);
    Value *Right = V;

    if (Left->getType()->isFloatingPointTy()) {
        Value *Sum = Builder.CreateFAdd(Left, Right);
        V = Builder.CreateFDiv(Sum, ConstantFP::get(Left->getType(), 2.0));
    } else {
        Value *Sum = Builder.CreateAdd(Left, Right);
        V = Builder.CreateSDiv(Sum, ConstantInt::get(Int32Ty, 2));
    }
}

    virtual void visit(SqrtNStmt &Node) override {
    Node.getBase()->accept(*this);
    Value *Base = V;
    Node.getNthRoot()->accept(*this);
    Value *NthRoot = V;

    if (!Base->getType()->isFloatingPointTy()) {
        Base = Builder.CreateSIToFP(Base, FloatTy);
    }
    if (!NthRoot->getType()->isFloatingPointTy()) {
        NthRoot = Builder.CreateSIToFP(NthRoot, FloatTy);
    }

    Function *PowFn = Intrinsic::getDeclaration(M, Intrinsic::pow, FloatTy);
    Value *One = ConstantFP::get(FloatTy, 1.0);
    Value *Exponent = Builder.CreateFDiv(One, NthRoot);
    V = Builder.CreateCall(PowFn, {Base, Exponent});
}

    virtual void visit(CaseStmt &Node) override {
    for (auto *Stmt : Node.getBody()) {
        Stmt->accept(*this);
    }
}



    llvm::Value* getVariablePointer(llvm::StringRef varName) {
    if (nameMapInt.count(varName))
        return nameMapInt[varName];
    else if (globalNameMapInt.count(varName))
        return globalNameMapInt[varName];
    else if (nameMapFloat.count(varName))
        return nameMapFloat[varName];
    else if (globalNameMapFloat.count(varName))
        return globalNameMapFloat[varName];
    else if (nameMapBool.count(varName))
        return nameMapBool[varName];
    else if (globalNameMapBool.count(varName))
        return globalNameMapBool[varName];
    else
        return nullptr;
}


    bool isInt(llvm::StringRef Var) {
    return nameMapInt.count(Var) || globalNameMapInt.count(Var);
}

    bool isFloat(llvm::StringRef Var) {
    return nameMapFloat.count(Var) || globalNameMapFloat.count(Var);
}

    bool isBool(llvm::StringRef Var) {
    return nameMapBool.count(Var) || globalNameMapBool.count(Var);
}


  };
}; // namespace

void CodeGen::compile(Program *Tree)
{
  // Create an LLVM context and a module.
  LLVMContext Ctx;
  Module *M = new Module("simple-compiler", Ctx);

  // Create an instance of the ToIRVisitor and run it on the AST to generate LLVM IR.
  ns::ToIRVisitor *ToIR = new ns::ToIRVisitor(M);


  ToIR->run(Tree);

  // Print the generated module to the standard output.
  M->print(outs(), nullptr);
}
