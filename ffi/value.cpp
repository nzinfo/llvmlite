#include "core.h"
#include "llvm-c/Core.h"
#include <string>

#include <iostream>

// the following is needed for WriteGraph()
#include "llvm/Analysis/CFGPrinter.h"
#include "llvm/IR/DebugInfoMetadata.h"
#include "llvm/IR/Attributes.h"
#include <llvm/IR/Operator.h>
#include <llvm/Demangle/Demangle.h>
#include "llvm/Support/GraphWriter.h"

/* An iterator around a attribute list, including the stop condition */
struct AttributeListIterator {
    typedef llvm::AttributeList::iterator const_iterator;
    const_iterator cur;
    const_iterator end;

    unsigned       has_ret;
    unsigned       has_fn;

    AttributeListIterator(const_iterator cur, const_iterator end)
        : cur(cur), end(end) {}
};
/*
struct AttributeListIndexedIterator {
    typedef llvm::AttributeList::index_iterator const_iterator;
    const_iterator cur;
    const_iterator end;

    AttributeListIndexedIterator(const_iterator cur, const_iterator end)
        : cur(cur), end(end) {}
};
*/

struct OpaqueAttributeListIterator;
typedef OpaqueAttributeListIterator *LLVMAttributeListIteratorRef;

/* An iterator around a attribute set, including the stop condition */
struct AttributeSetIterator {
    typedef llvm::AttributeSet::iterator const_iterator;
    const_iterator cur;
    const_iterator end;

    AttributeSetIterator(const_iterator cur, const_iterator end)
        : cur(cur), end(end) {}
};

struct OpaqueAttributeSetIterator;
typedef OpaqueAttributeSetIterator *LLVMAttributeSetIteratorRef;

/* An iterator around a function's blocks, including the stop condition */
struct BlocksIterator {
    typedef llvm::Function::const_iterator const_iterator;
    const_iterator cur;
    const_iterator end;

    BlocksIterator(const_iterator cur, const_iterator end)
        : cur(cur), end(end) {}
};

struct OpaqueBlocksIterator;
typedef OpaqueBlocksIterator *LLVMBlocksIteratorRef;

struct PredBlocksIterator {
    typedef llvm::pred_iterator const_iterator;
    const_iterator cur;
    const_iterator end;

    PredBlocksIterator(const_iterator cur, const_iterator end)
        : cur(cur), end(end) {}
};

struct OpaquePredBlocksIterator;
typedef OpaquePredBlocksIterator *LLVMPredBlocksIteratorRef;

/* An iterator around a function's arguments, including the stop condition */
struct ArgumentsIterator {
    typedef llvm::Function::const_arg_iterator const_iterator;
    const_iterator cur;
    const_iterator end;

    ArgumentsIterator(const_iterator cur, const_iterator end)
        : cur(cur), end(end) {}
};

struct OpaqueArgumentsIterator;
typedef OpaqueArgumentsIterator *LLVMArgumentsIteratorRef;

/* An iterator around a basic block's instructions, including the stop condition
 */
struct InstructionsIterator {
    typedef llvm::BasicBlock::const_iterator const_iterator;
    const_iterator cur;
    const_iterator end;

    InstructionsIterator(const_iterator cur, const_iterator end)
        : cur(cur), end(end) {}
};

struct OpaqueInstructionsIterator;
typedef OpaqueInstructionsIterator *LLVMInstructionsIteratorRef;

/* An iterator around a instruction's operands, including the stop condition */
struct OperandsIterator {
    typedef llvm::Instruction::const_op_iterator const_iterator;
    const_iterator cur;
    const_iterator end;

    OperandsIterator(const_iterator cur, const_iterator end)
        : cur(cur), end(end) {}
};

struct OpaqueOperandsIterator;
typedef OpaqueOperandsIterator *LLVMOperandsIteratorRef;

namespace llvm {

static LLVMAttributeListIteratorRef wrap(AttributeListIterator *GI) {
    return reinterpret_cast<LLVMAttributeListIteratorRef>(GI);
}

static AttributeListIterator *unwrap(LLVMAttributeListIteratorRef GI) {
    return reinterpret_cast<AttributeListIterator *>(GI);
}

static LLVMAttributeSetIteratorRef wrap(AttributeSetIterator *GI) {
    return reinterpret_cast<LLVMAttributeSetIteratorRef>(GI);
}

static AttributeSetIterator *unwrap(LLVMAttributeSetIteratorRef GI) {
    return reinterpret_cast<AttributeSetIterator *>(GI);
}

static LLVMBlocksIteratorRef wrap(BlocksIterator *GI) {
    return reinterpret_cast<LLVMBlocksIteratorRef>(GI);
}

static BlocksIterator *unwrap(LLVMBlocksIteratorRef GI) {
    return reinterpret_cast<BlocksIterator *>(GI);
}

static LLVMPredBlocksIteratorRef wrap(PredBlocksIterator *GI) {
    return reinterpret_cast<LLVMPredBlocksIteratorRef>(GI);
}

static PredBlocksIterator *unwrap(LLVMPredBlocksIteratorRef GI) {
    return reinterpret_cast<PredBlocksIterator *>(GI);
}

static LLVMArgumentsIteratorRef wrap(ArgumentsIterator *GI) {
    return reinterpret_cast<LLVMArgumentsIteratorRef>(GI);
}

static ArgumentsIterator *unwrap(LLVMArgumentsIteratorRef GI) {
    return reinterpret_cast<ArgumentsIterator *>(GI);
}

static LLVMInstructionsIteratorRef wrap(InstructionsIterator *GI) {
    return reinterpret_cast<LLVMInstructionsIteratorRef>(GI);
}

static InstructionsIterator *unwrap(LLVMInstructionsIteratorRef GI) {
    return reinterpret_cast<InstructionsIterator *>(GI);
}

static LLVMOperandsIteratorRef wrap(OperandsIterator *GI) {
    return reinterpret_cast<LLVMOperandsIteratorRef>(GI);
}

static OperandsIterator *unwrap(LLVMOperandsIteratorRef GI) {
    return reinterpret_cast<OperandsIterator *>(GI);
}

} // namespace llvm

extern "C" {

API_EXPORT(LLVMAttributeListIteratorRef)
LLVMPY_FunctionAttributesIter(LLVMValueRef F) {
    using namespace llvm;
    Function *func = unwrap<Function>(F);
    AttributeList attrs = func->getAttributes();
    // printf("has ret %d, has fn %d.\n", attrs.hasRetAttrs(), attrs.hasFnAttrs());
    auto itr = new AttributeListIterator(attrs.begin(), attrs.end());
    itr->has_fn = attrs.hasFnAttrs();
    itr->has_ret = attrs.hasRetAttrs();
    return wrap(itr);
}

API_EXPORT(LLVMAttributeSetIteratorRef)
LLVMPY_ArgumentAttributesIter(LLVMValueRef A) {
    using namespace llvm;
    Argument *arg = unwrap<Argument>(A);
    unsigned argno = arg->getArgNo();
#if LLVM_VERSION_MAJOR >=14    
    AttributeSet attrs =
        arg->getParent()->getAttributes().getParamAttrs(argno);
#else    
    AttributeSet attrs =
        arg->getParent()->getAttributes().getParamAttributes(argno);
#endif    
    return wrap(new AttributeSetIterator(attrs.begin(), attrs.end()));
}

API_EXPORT(LLVMAttributeListIteratorRef)
LLVMPY_CallInstAttributesIter(LLVMValueRef C) {
    using namespace llvm;
    CallBase  *inst = unwrap<CallBase >(C);
    AttributeList attrs = inst->getAttributes();
    auto iter = new AttributeListIterator(attrs.begin(), attrs.end());
    iter -> has_ret = attrs.hasRetAttrs();
    iter -> has_fn = attrs.hasRetAttrs(); //hasAttributes(AttributeList::FunctionIndex);
    return wrap(iter);
}

API_EXPORT(LLVMAttributeListIteratorRef)
LLVMPY_InvokeInstAttributesIter(LLVMValueRef C) {
    using namespace llvm;
    InvokeInst *inst = unwrap<InvokeInst>(C);
    AttributeList attrs = inst->getAttributes();
    return wrap(new AttributeListIterator(attrs.begin(), attrs.end()));
}

API_EXPORT(LLVMAttributeSetIteratorRef)
LLVMPY_GlobalAttributesIter(LLVMValueRef G) {
    using namespace llvm;
    GlobalVariable *g = unwrap<GlobalVariable>(G);
    AttributeSet attrs = g->getAttributes();
    return wrap(new AttributeSetIterator(attrs.begin(), attrs.end()));
}

API_EXPORT(LLVMBlocksIteratorRef)
LLVMPY_FunctionBlocksIter(LLVMValueRef F) {
    using namespace llvm;
    Function *func = unwrap<Function>(F);
    // printf("init block iter %p -> %p\n", func->begin(), func->end());
    return wrap(new BlocksIterator(func->begin(), func->end()));
}


API_EXPORT(LLVMPredBlocksIteratorRef)
LLVMPY_BlockPredsIter(LLVMValueRef B) {
    using namespace llvm;
    BasicBlock *block = unwrap<BasicBlock>(B);
    return wrap(new PredBlocksIterator(llvm::pred_begin(block), llvm::pred_end(block)));
}

API_EXPORT(LLVMArgumentsIteratorRef)
LLVMPY_FunctionArgumentsIter(LLVMValueRef F) {
    using namespace llvm;
    Function *func = unwrap<Function>(F);
    return wrap(new ArgumentsIterator(func->arg_begin(), func->arg_end()));
}

API_EXPORT(LLVMTypeRef)
LLVMPY_FunctionReturnType(LLVMValueRef F) { 
    using namespace llvm;
    // Ref: https://lists.llvm.org/pipermail/llvm-dev/2008-May/014874.html
    Function *func = unwrap<Function>(F);
    return wrap(func->getReturnType());
}

API_EXPORT(LLVMTypeRef)
LLVMPY_GetBuiltinTypeForName(LLVMModuleRef M, const char *name, size_t len) {
    /* 返回 LLVM 系统中预置的类型，用于 构造函数类型、进行代码生成
       Ref: https://llvm.org/doxygen/Type_8h_source.html#l00140
     */
    using namespace llvm;
    // LLVMContextRef ctx = LLVMGetModuleContext(
    LLVMContext* C = llvm::unwrap(LLVMGetModuleContext(M));
    
    std::string k(name, len);
    if(k == "void") 
        return wrap(llvm::Type::getVoidTy(*C));
    if(k == "label") 
        return wrap(llvm::Type::getLabelTy(*C));
    if(k == "halt" || k == "f16") 
        return wrap(llvm::Type::getHalfTy(*C));
    if(k == "bfloat") 
        return wrap(llvm::Type::getBFloatTy(*C));
    if(k == "float" || k == "f32") 
        return wrap(llvm::Type::getFloatTy(*C));
    if(k == "double" || k == "f64") 
        return wrap(llvm::Type::getDoubleTy(*C));
    if(k == "metadata") 
        return wrap(llvm::Type::getMetadataTy(*C));
    if(k == "x86_fp80") 
        return wrap(llvm::Type::getX86_FP80Ty(*C));
    if(k == "fp128") 
        return wrap(llvm::Type::getFP128Ty(*C));
    if(k == "ppc_fp128") 
        return wrap(llvm::Type::getPPC_FP128Ty(*C));
    if(k == "x86_mmx") 
        return wrap(llvm::Type::getX86_MMXTy(*C));
    if(k == "x86_amx") 
        return wrap(llvm::Type::getX86_AMXTy(*C));
    if(k == "token") 
        return wrap(llvm::Type::getTokenTy(*C));
    //if(k == "intN") 
    //    return wrap(llvm::Type::getIntNTy(*C));
    if(k == "i1" || k == "int1")  
        return wrap(llvm::Type::getInt1Ty(*C));
    if(k == "i8" || k == "int8") 
        return wrap(llvm::Type::getInt8Ty(*C));
    if(k == "i16" || k == "int16") 
        return wrap(llvm::Type::getInt16Ty(*C));
    if(k == "i32" || k == "int32") 
        return wrap(llvm::Type::getInt32Ty(*C));
    if(k == "i64" || k == "int64")  
        return wrap(llvm::Type::getInt64Ty(*C));
    if(k == "i128" || k == "int128") 
        return wrap(llvm::Type::getInt128Ty(*C));
 
    return nullptr;
}

API_EXPORT(LLVMTypeRef)
LLVMPY_GetPointerType(LLVMTypeRef type) {
    llvm::Type *ty = llvm::unwrap(type);
    //llvm::PointerType *ty = llvm::dyn_cast<llvm::PointerType>(unwrapped);
    if (ty != nullptr) {
        return llvm::wrap(ty->getPointerTo());
    }
    return nullptr;
}

API_EXPORT(LLVMTypeRef)
LLVMPY_GetFunctionType(LLVMTypeRef returnTy, size_t argc, LLVMTypeRef* args, bool isVarArg)
{
    // 虽然单纯的 初始化函数不需要 参数，但是声明的谓词函数（eg. 抽象类型关联）均需要参数
    using namespace llvm;

    std::vector<llvm::Type *> arg_types;
    for(size_t i=0; i<argc; i++) 
        arg_types.push_back(llvm::unwrap(args[i]));
    
    llvm::FunctionType *ftype = llvm::FunctionType::get(
		llvm::unwrap(returnTy), arg_types, isVarArg);

    return llvm::wrap(ftype); 
}

API_EXPORT(LLVMValueRef)
LLVMPY_CreateFunction(LLVMModuleRef M, LLVMTypeRef FT, const char *name, size_t len) 
{
    // Function *func = unwrap<Function>(F);
    llvm::FunctionType* fnTy = llvm::unwrap<llvm::FunctionType>(FT);
    if(fnTy) {
        llvm::Function *fn = llvm::Function::Create(fnTy, llvm::Function::ExternalLinkage, std::string(name, len), llvm::unwrap(M));
        return llvm::wrap(fn);
    }
    return nullptr; 
}

API_EXPORT(LLVMValueRef) //LLVMBasicBlockRef) 
LLVMPY_AppendBasicBlock(LLVMValueRef Fn, const char *name, size_t len)
{
    // 接口上保留 长度，实际调用忽略
    using namespace llvm;
    LLVMBasicBlockRef BRef = LLVMAppendBasicBlock(Fn, name);
    return LLVMBasicBlockAsValue(BRef);
}

API_EXPORT(void) 
LLVMPY_DeleteBasicBlock(LLVMValueRef B) 
{   
    if(LLVMValueIsBasicBlock(B))
        LLVMDeleteBasicBlock(LLVMValueAsBasicBlock(B));
}

API_EXPORT(void) 
LLVMPY_BuilderSetToBasicBlock(LLVMBuilderRef Builder, LLVMValueRef B) // LLVMBasicBlockRef Block)
{   
    using namespace llvm;
    BasicBlock *block = unwrap<BasicBlock>(B);
    LLVMPositionBuilderAtEnd(Builder, wrap(block));
}

API_EXPORT(LLVMValueRef) // LLVMBasicBlockRef) 
LLVMPY_BuilderGetBlock(LLVMBuilderRef Builder)
{
    // llvmlite binding 部分，目前无法封装一个 LLVMValueRef 上下文信息不足，此方法保留为空 不实现
    // using namespace llvm;
    // BasicBlock *block = unwrap<BasicBlock>(B);
    // return LLVMGetInsertBlock(Builder);
    return nullptr;
}
API_EXPORT(LLVMValueRef) 
LLVMPY_BuildCallInst(LLVMBuilderRef B, LLVMTypeRef T, LLVMValueRef Fn,
                            LLVMValueRef *Args, unsigned NumArgs,
                            const char *name, size_t len) {
    return LLVMBuildCall2(B, T, Fn, Args, NumArgs, name);
}

API_EXPORT(LLVMValueRef) 
LLVMPY_ConstIntValue(LLVMTypeRef IntTy, unsigned long long N,
                          LLVMBool SignExtend) {
    return LLVMConstInt(IntTy, N, SignExtend);                       
}

API_EXPORT(LLVMInstructionsIteratorRef)
LLVMPY_BlockInstructionsIter(LLVMValueRef B) {
    using namespace llvm;
    BasicBlock *block = unwrap<BasicBlock>(B);
    return wrap(new InstructionsIterator(block->begin(), block->end()));
}

API_EXPORT(LLVMOperandsIteratorRef)
LLVMPY_InstructionOperandsIter(LLVMValueRef I) {
    using namespace llvm;
    Instruction *inst = unwrap<Instruction>(I);
    return wrap(new OperandsIterator(inst->op_begin(), inst->op_end()));
}

API_EXPORT(LLVMAttributeSetIteratorRef)
LLVMPY_AttributeListIterNext(LLVMAttributeListIteratorRef GI) {
    using namespace llvm;
    AttributeListIterator *iter = unwrap(GI);
    if (iter->cur != iter->end) {
        const AttributeSet * attrs = (&*iter->cur++);
        return wrap(new AttributeSetIterator(attrs->begin(), attrs->end()));
        // return LLVMPY_CreateString(attr_set->getAsString().c_str());
    } else {
        return NULL;
    }
}

API_EXPORT(unsigned)
LLVMPY_AttributeListAttr(LLVMAttributeListIteratorRef GI, unsigned ty) {
    AttributeListIterator *iter = llvm::unwrap(GI);
    if(ty == 0)
        return iter->has_ret;
    else 
        return iter->has_fn;
}

API_EXPORT(const char *)
LLVMPY_AttributeSetIterNext(LLVMAttributeSetIteratorRef GI) {
    using namespace llvm;
    // printf("ret idx %d, fn idx %d\n", AttributeList::AttrIndex::ReturnIndex, AttributeList::AttrIndex::FunctionIndex);
    AttributeSetIterator *iter = unwrap(GI);
    if (iter->cur != iter->end) {
        return LLVMPY_CreateString((&*iter->cur++)->getAsString().c_str());
    } else {
        return NULL;
    }
}

API_EXPORT(LLVMValueRef)
LLVMPY_BlocksIterNext(LLVMBlocksIteratorRef GI) {
    using namespace llvm;
    BlocksIterator *iter = unwrap(GI);
    if (iter->cur != iter->end) {
        const Value * v_ptr = static_cast<const Value *>(&*iter->cur++);
        // printf("bb v_ptr=%p\n", v_ptr);
        return wrap(v_ptr);
    } else {
        return NULL;
    }
}

API_EXPORT(LLVMValueRef)
LLVMPY_PredBlocksIterNext(LLVMPredBlocksIteratorRef GI) {
    using namespace llvm;
    PredBlocksIterator *iter = unwrap(GI);
    if (iter->cur != iter->end) {
        return wrap(static_cast<const Value *>(*iter->cur++));
    } else {
        return NULL;
    }
}

API_EXPORT(LLVMValueRef)
LLVMPY_ArgumentsIterNext(LLVMArgumentsIteratorRef GI) {
    using namespace llvm;
    ArgumentsIterator *iter = unwrap(GI);
    if (iter->cur != iter->end) {
        return wrap(&*iter->cur++);
    } else {
        return NULL;
    }
}

API_EXPORT(LLVMValueRef)
LLVMPY_InstructionsIterNext(LLVMInstructionsIteratorRef GI) {
    using namespace llvm;
    InstructionsIterator *iter = unwrap(GI);
    if (iter->cur != iter->end) {
        return wrap(&*iter->cur++);
    } else {
        return NULL;
    }
}

API_EXPORT(LLVMValueRef)
LLVMPY_OperandsIterNext(LLVMOperandsIteratorRef GI) {
    using namespace llvm;
    OperandsIterator *iter = unwrap(GI);
    if (iter->cur != iter->end) {
        return wrap((&*iter->cur++)->get());
    } else {
        return NULL;
    }
}

API_EXPORT(void)
LLVMPY_DisposeAttributeListIter(LLVMAttributeListIteratorRef GI) {
    delete llvm::unwrap(GI);
}

API_EXPORT(void)
LLVMPY_DisposeAttributeSetIter(LLVMAttributeSetIteratorRef GI) {
    delete llvm::unwrap(GI);
}

API_EXPORT(void)
LLVMPY_DisposeBlocksIter(LLVMBlocksIteratorRef GI) { delete llvm::unwrap(GI); }

API_EXPORT(void)
LLVMPY_DisposePredBlocksIter(LLVMPredBlocksIteratorRef GI) { delete llvm::unwrap(GI); }


API_EXPORT(void)
LLVMPY_DisposeArgumentsIter(LLVMArgumentsIteratorRef GI) {
    delete llvm::unwrap(GI);
}

API_EXPORT(void)
LLVMPY_DisposeInstructionsIter(LLVMInstructionsIteratorRef GI) {
    delete llvm::unwrap(GI);
}

API_EXPORT(void)
LLVMPY_DisposeOperandsIter(LLVMOperandsIteratorRef GI) {
    delete llvm::unwrap(GI);
}

API_EXPORT(void)
LLVMPY_PrintValueToString(LLVMValueRef Val, const char **outstr) {
    *outstr = LLVMPrintValueToString(Val);
}

API_EXPORT(void)
LLVMPY_PrintValueToStringAsOperand(LLVMValueRef Val, bool print_type, const char **outstr) {
    // printAsOperand(rso, / * PrintType * / false);
    using namespace llvm;

    std::string buf;
    raw_string_ostream os(buf);
 
    if (unwrap(Val))
        unwrap(Val)->printAsOperand(os, print_type);
    else
        os << "Printing <null> Value";
    
    os.flush();
    
    *outstr = strdup(buf.c_str());
}

API_EXPORT(const char *)
LLVMPY_GetInstrOptimizationInfo(LLVMValueRef Val) { 
    using namespace llvm;

    std::string buf;
    raw_string_ostream os(buf);

    if (const FPMathOperator *fpo = dyn_cast<const FPMathOperator>(unwrap(Val))) {

        if (fpo->isFast()) {
            os << "fast ";
        }
        else {
            if (fpo->hasNoNaNs())
                os << "nnan ";

            if (fpo->hasNoInfs())
                os << "ninf ";

            if (fpo->hasNoSignedZeros())
                os << "nsz ";

            if (fpo->hasAllowReciprocal())
                os << "arcp ";
                
            if (fpo->hasAllowContract())
                os << "acon ";

            if (fpo->hasApproxFunc())
                os << "apfn ";
        }
    }

    if (const OverflowingBinaryOperator *obo = dyn_cast<OverflowingBinaryOperator>(unwrap(Val))) {

        if (obo->hasNoUnsignedWrap())
            os << "nuw ";
            // gen.writeFact(pred::instruction::flag, iref, "nuw");

        if (obo->hasNoSignedWrap())
            os << "nsw ";
    }
    else if (const PossiblyExactOperator *div = dyn_cast<PossiblyExactOperator>(unwrap(Val))) {

        if (div->isExact())
            os << "exact ";
    }

    os.flush();
    return LLVMPY_CreateByteString(buf.data(), buf.size());
}

API_EXPORT(const char *)
LLVMPY_GetValueName(LLVMValueRef Val) { 
    const char* p = LLVMGetValueName(Val);
    // printf("name: %s, %d\n", p,  llvm::unwrap(Val)->hasName());
    return p;
}

API_EXPORT(size_t)
LLVMPY_GetValueAddressAsID(LLVMValueRef Val) { 
    /**
     * On many platforms (an exception is systems with segmented addressing) 
     * std::size_t can safely store the value of any non-member pointer, 
     * in which case it is synonymous with std::uintptr_t.
     */
    return (size_t)Val; 
}

API_EXPORT(const char *)
LLVMPY_ItaniumDemangle(const char *mangled_name, size_t len) { 

  char *Demangled = llvm::itaniumDemangle(mangled_name, nullptr, nullptr, nullptr);
  if (!Demangled)
    return LLVMPY_CreateString("");;

  return LLVMPY_CreateString(Demangled);
}

API_EXPORT(const char *)
LLVMPY_GetDbgValueName(LLVMValueRef Val) { 
    // return LLVMGetValueName(Val);
    using namespace llvm;

    if (const auto *GV = dyn_cast<GlobalVariable>(unwrap(Val))) {
        SmallVector<DIGlobalVariableExpression *, 1> GVEs;
        GV->getDebugInfo(GVEs);
        if (GVEs.size())
            if (const DIGlobalVariable *DGV = GVEs[0]->getVariable()) {
                StringRef name = DGV->getName();     // DIVariable
                return LLVMPY_CreateByteString(name.data(), name.size());
            }
    } else if (const Function *func = unwrap<Function>(Val)) {
        DISubprogram * diSub = func->getSubprogram();
        if(diSub) {
            StringRef name = diSub->getName();     
            return LLVMPY_CreateByteString(name.data(), name.size());
        }
    }     

    return LLVMPY_CreateString("");
}

API_EXPORT(const char *)
LLVMPY_GetDbgValueTypeName(LLVMValueRef Val) { 
    // return LLVMGetValueName(Val);
    using namespace llvm;
    
    if (const auto *GV = dyn_cast<GlobalVariable>(unwrap(Val))) {
        SmallVector<DIGlobalVariableExpression *, 1> GVEs;
        GV->getDebugInfo(GVEs);
        if (GVEs.size())
            if (const DIGlobalVariable *DGV = GVEs[0]->getVariable()) {
                StringRef name = DGV->getType() -> getName();     // DIType 
                return LLVMPY_CreateString(name.data());
            }
    } // else if ()     

    return LLVMPY_CreateString("");
}

API_EXPORT(const char *)
LLVMPY_GetDbgFile(LLVMValueRef Val)
{   
    using namespace llvm;
    /*
    auto *val = unwrap<Value>(Val);
    if (auto *I = dyn_cast<Instruction>(val)) {
        if (auto &D = I->getDebugLoc()) {
            return LLVMPY_CreateString(D.get()->getFilename().str().c_str());
        }
    }

    return LLVMPY_CreateString("");
    */
    unsigned len = 0;
    const char * ptr_dir = LLVMGetDebugLocDirectory(Val, &len);
    std::string s(ptr_dir, len);

    len = 0;
    const char * ptr = LLVMGetDebugLocFilename(Val, &len);
    if(len) {
        s += std::string(ptr, len);
        return LLVMPY_CreateByteString(s.data(), s.length());
    } else
        return LLVMPY_CreateString("");
}

API_EXPORT(unsigned)
LLVMPY_GetDbgLine(LLVMValueRef Val)
{   /*
    using namespace llvm;
    auto *val = unwrap<Value>(Val);
    if (auto *I = dyn_cast<Instruction>(val)) {
        if (auto &D = I->getDebugLoc()) {
            return D.getLine();
        }
    }

    return 0; */
    return LLVMGetDebugLocLine(Val);
}


API_EXPORT(unsigned)
LLVMPY_GetDbgCol(LLVMValueRef Val)
{
    /* using namespace llvm;
    auto *val = unwrap<Value>(Val);
    if (auto *I = dyn_cast<Instruction>(val)) {
        if (auto &D = I->getDebugLoc()) {
            return D.getCol();
        }
    }

    return 0;   */
    return LLVMGetDebugLocColumn(Val);
}

API_EXPORT(void)
LLVMPY_SetValueName(LLVMValueRef Val, const char *Name) {
    LLVMSetValueName(Val, Name);
}

API_EXPORT(LLVMModuleRef)
LLVMPY_GetGlobalParent(LLVMValueRef Val) { return LLVMGetGlobalParent(Val); }

API_EXPORT(LLVMTypeRef)
LLVMPY_TypeOf(LLVMValueRef Val) { return LLVMTypeOf(Val); }

API_EXPORT(const char *)
LLVMPY_PrintType(LLVMTypeRef type) {
    char *str = LLVMPrintTypeToString(type);
    const char *out = LLVMPY_CreateString(str);
    LLVMDisposeMessage(str);
    return out;
}

API_EXPORT(const char *)
LLVMPY_GetTypeName(LLVMTypeRef type) {
    // try to convert to a struct type, works for other derived
    // types too
    llvm::Type *unwrapped = llvm::unwrap(type);
    llvm::StructType *ty = llvm::dyn_cast<llvm::StructType>(unwrapped);
    if (ty && !ty->isLiteral()) {
        return LLVMPY_CreateString(ty->getStructName().str().c_str());
    }
    return LLVMPY_CreateString("");
}

API_EXPORT(bool)
LLVMPY_TypeIsPointer(LLVMTypeRef type) {
    return llvm::unwrap(type)->isPointerTy();
}

API_EXPORT(bool)
LLVMPY_TypeIsArray(LLVMTypeRef type) {
    return llvm::unwrap(type)->isArrayTy();
}

API_EXPORT(unsigned)
LLVMPY_TypeGetArrayNumElements(LLVMTypeRef type)
{
    llvm::Type* unwrapped = llvm::unwrap(type);
    if (auto* ty = llvm::dyn_cast<llvm::ArrayType>(unwrapped)) {
        return ty->getNumElements();
    }
    return 0;
}


API_EXPORT(bool)
LLVMPY_TypeIsStruct(LLVMTypeRef type)
{
    return llvm::unwrap(type)->isStructTy();
}

API_EXPORT(bool)
LLVMPY_TypeIsVector(LLVMTypeRef type)
{
    return llvm::unwrap(type)->isVectorTy();
}

// begin isFunctionTy, llvm::Type 也可能是 Function Type
API_EXPORT(bool)
LLVMPY_TypeIsFunctionType(LLVMTypeRef type)
{
    return llvm::unwrap(type)->isFunctionTy();
}

// isFunctionVarArg
API_EXPORT(bool)
LLVMPY_TypeIsFunctionVarArg(LLVMTypeRef type)
{
    return llvm::unwrap(type)->isFunctionVarArg();
}

API_EXPORT(LLVMTypeRef)
LLVMPY_GetFunctionParamType(LLVMTypeRef type, unsigned n) {
     return llvm::wrap(llvm::unwrap(type)->getFunctionParamType(n));
}
 
API_EXPORT(unsigned)
LLVMPY_GetFunctionNumParams(LLVMTypeRef type) {
    return llvm::unwrap(type)->getFunctionNumParams();
}

API_EXPORT(LLVMTypeRef)
LLVMPY_GetFunctionReturnType(LLVMTypeRef type) {
    llvm::Type* unwrapped = llvm::unwrap(type);
    if (auto* ty = llvm::dyn_cast<llvm::FunctionType>(unwrapped)) {
        return llvm::wrap(ty->getReturnType());
    }
    return nullptr;
}

// end isFunctionTy

API_EXPORT(LLVMTypeRef)
LLVMPY_GetElementType(LLVMTypeRef type) {
    llvm::Type *unwrapped = llvm::unwrap(type);
    //llvm::PointerType *ty = llvm::dyn_cast<llvm::PointerType>(unwrapped);
    //if (ty != nullptr) {
    if (auto* ty = llvm::dyn_cast<llvm::PointerType>(unwrapped)) {
        // Ref: LLVMGetElementType
#if LLVM_VERSION_MAJOR > 13        
        return llvm::wrap(ty->getNonOpaquePointerElementType());
#else
        return llvm::wrap(ty->getElementType());
#endif 
    }
    if (auto* ty = llvm::dyn_cast<llvm::ArrayType>(unwrapped)) {
         return llvm::wrap(ty->getElementType());
    }

    if (auto* ty = llvm::dyn_cast<llvm::VectorType>(unwrapped)) {
        return llvm::wrap(ty->getElementType());
    }
    return nullptr;
}


API_EXPORT(unsigned)
LLVMPY_GetStructNumElements(LLVMTypeRef type)
{
    llvm::Type* unwrapped = llvm::unwrap(type);
    if (auto* ty = llvm::dyn_cast<llvm::StructType>(unwrapped)) {
        return ty->getNumElements();
    }
    return 0;
}

API_EXPORT(LLVMTypeRef)
LLVMPY_GetStructElementType(LLVMTypeRef type, unsigned n)
{
    llvm::Type* unwrapped = llvm::unwrap(type);
    if (auto* ty = llvm::dyn_cast<llvm::StructType>(unwrapped)) {
        assert (n < ty->getNumElements() && "Invalid element number");
        return llvm::wrap(ty->getElementType(n));
    }
    return nullptr;
}

API_EXPORT(void)
LLVMPY_SetLinkage(LLVMValueRef Val, int Linkage) {
    LLVMSetLinkage(Val, (LLVMLinkage)Linkage);
}

API_EXPORT(int)
LLVMPY_GetLinkage(LLVMValueRef Val) { return (int)LLVMGetLinkage(Val); }

API_EXPORT(void)
LLVMPY_SetAlignment(LLVMValueRef Val, unsigned Bytes) {
    LLVMSetAlignment(Val, Bytes);
}

API_EXPORT(unsigned)
LLVMPY_GetAlignment(LLVMValueRef Val) { return LLVMGetAlignment(Val); }

// thread_local
API_EXPORT(int)
LLVMPY_GetThreadLocalMode(LLVMValueRef Val) { return (int)LLVMGetThreadLocalMode(Val); }

API_EXPORT(const char *)
LLVMPY_GetSection(LLVMValueRef Val) {
    return LLVMPY_CreateString(
        LLVMGetSection(Val)
    );
}

API_EXPORT(void)
LLVMPY_SetVisibility(LLVMValueRef Val, int Visibility) {
    LLVMSetVisibility(Val, (LLVMVisibility)Visibility);
}

API_EXPORT(int)
LLVMPY_GetVisibility(LLVMValueRef Val) { return (int)LLVMGetVisibility(Val); }

API_EXPORT(void)
LLVMPY_SetDLLStorageClass(LLVMValueRef Val, int DLLStorageClass) {
    LLVMSetDLLStorageClass(Val, (LLVMDLLStorageClass)DLLStorageClass);
}

API_EXPORT(int)
LLVMPY_GetDLLStorageClass(LLVMValueRef Val) {
    return (int)LLVMGetDLLStorageClass(Val);
}

API_EXPORT(unsigned)
LLVMPY_GetEnumAttributeKindForName(const char *name, size_t len) {
    /* zero is returned if no match */
    return LLVMGetEnumAttributeKindForName(name, len);
}

API_EXPORT(void)
LLVMPY_AddFunctionAttr(LLVMValueRef Fn, unsigned AttrKind) {
    LLVMContextRef ctx = LLVMGetModuleContext(LLVMGetGlobalParent(Fn));
    LLVMAttributeRef attr_ref = LLVMCreateEnumAttribute(ctx, AttrKind, 0);
    LLVMAddAttributeAtIndex(Fn, LLVMAttributeReturnIndex, attr_ref);
}

API_EXPORT(void)
LLVMPY_SetFunctionStringAttr(LLVMValueRef Fn, const char *k, size_t klen) // , const char *v, size_t vlen) 
{
    using namespace llvm;
    Function *F = unwrap<Function>(Fn);
    F->addFnAttr(std::string(k, klen));
    // LLVMContextRef ctx = LLVMGetModuleContext(LLVMGetGlobalParent(Fn));
    // LLVMAttributeRef attrRef =	LLVMCreateStringAttribute ( ctx, k, klen, v, vlen)
}

/*
API_EXPORT(LLVMValueRef)
LLVMPY_GetPersonalityFn(LLVMValueRef Fn)  
{
    using namespace llvm;

    Function *F = unwrap<Function>(Fn);
    if (F->hasPersonalityFn()) {
        llvm::Constant *pers_fn = F->getPersonalityFn();
        return wrap(pers_fn);
    }
    return nullptr;
}
*/

API_EXPORT(int)
LLVMPY_GetCallingConv(LLVMValueRef Fn) { 
    using namespace llvm;

    Function *F = unwrap<Function>(Fn); // FIXME: check pointer ?
    if(F)
        return (int)F->getCallingConv(); 
    CallInst *inst = unwrap<CallInst>(Fn);
    if(inst)
        return (int)inst->getCallingConv(); 
}

API_EXPORT(LLVMValueRef)
LLVMPY_GetPersonalityFn(LLVMValueRef Fn)  
{
    using namespace llvm;

    Function *F = unwrap<Function>(Fn);
    if (F->hasPersonalityFn()) {
        llvm::Constant *pers_fn = F->getPersonalityFn();
        return wrap(pers_fn);
    }
    return nullptr;
}

/*
API_EXPORT(bool)
LLVMPY_HasFunctionStringAttr(LLVMValueRef Fn, const char *k, size_t klen) {
    // 仅检查是否存在这个属性

}
*/

API_EXPORT(int)
LLVMPY_IsDeclaration(LLVMValueRef GV) { return LLVMIsDeclaration(GV); }

API_EXPORT(int)
LLVMPY_IsConstant(LLVMValueRef V)
{
    using namespace llvm;
    return isa<Constant>(unwrap<Value>(V));
}


API_EXPORT(int)
LLVMPY_IsConstantExpr(LLVMValueRef V)
{
    using namespace llvm;
    return isa<ConstantExpr>(unwrap<Value>(V));
}

API_EXPORT(int)
LLVMPY_IsGlobalVariableConstant(LLVMValueRef V)
{
    using namespace llvm;
    auto *GV = dyn_cast<GlobalVariable>(unwrap<Value>(V));
    return GV && GV->isConstant();
}

API_EXPORT(int)
LLVMPY_IsGlobalVariable(LLVMValueRef V)
{
    using namespace llvm;
    return isa<GlobalValue>(unwrap<Value>(V));
}

API_EXPORT(LLVMValueRef)
LLVMPY_GlobalGetInitializer(LLVMValueRef G)
{
    return LLVMGetInitializer(G);
}

API_EXPORT(LLVMValueRef)
LLVMPY_GlobalGetAliasee(LLVMValueRef V)
{
    // check GlobalAlias
    using namespace llvm;
    auto *GV = dyn_cast<GlobalAlias>(unwrap<Value>(V));
    if(GV)
        return wrap(GV->getAliasee());
    return nullptr;
}

API_EXPORT(LLVMValueRef)
LLVMPY_ConstantExprAsInst(LLVMValueRef CE)
{
    using namespace llvm;
    return wrap(cast<ConstantExpr>(unwrap<Value>(CE))->getAsInstruction());
}

API_EXPORT(LLVMValueRef)
LLVMPY_GlobalGetInlineAsm(LLVMValueRef V)
{
    // check GlobalAlias
    using namespace llvm;
    auto *GV = dyn_cast<InlineAsm>(unwrap<Value>(V));
    if(GV)
        return V;
    return nullptr;
}

API_EXPORT(unsigned)
LLVMPY_PhiCountIncoming(LLVMValueRef P)
{
    return LLVMCountIncoming(P);
}

API_EXPORT(LLVMValueRef)
LLVMPY_PhiGetIncomingValue(LLVMValueRef P, unsigned Idx)
{
    return LLVMGetIncomingValue(P, Idx);
}

API_EXPORT(LLVMValueRef) // LLVMBasicBlockRef)
LLVMPY_PhiGetIncomingBlock(LLVMValueRef P, unsigned Idx)
{
    return LLVMBasicBlockAsValue(LLVMGetIncomingBlock(P, Idx));
}

API_EXPORT(void)
LLVMPY_WriteCFG(LLVMValueRef Fval, const char **OutStr, int ShowInst) {
    using namespace llvm;
    Function *F = unwrap<Function>(Fval);
    std::string buffer;
    raw_string_ostream stream(buffer);
    DOTFuncInfo CFGInfo(F, nullptr, nullptr, 0);
    WriteGraph(stream, &CFGInfo, !ShowInst);
    *OutStr = LLVMPY_CreateString(stream.str().c_str());
}

API_EXPORT(const char *)
LLVMPY_GetOpcodeName(LLVMValueRef Val) {
    // try to convert to an instruction value, works for other derived
    // types too
    llvm::Value *unwrapped = llvm::unwrap(Val);
    llvm::Instruction *inst = llvm::dyn_cast<llvm::Instruction>(unwrapped);
    if (inst) {
        return LLVMPY_CreateString(inst->getOpcodeName());
    }
    return LLVMPY_CreateString("");
}

//////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
//  遍历 Inst 所需要的 额外的接口信息
//      
//      - LLVMPY_ReturnInst_GetReturnValue
//      - LLVMPY_BranchInst_GetCondition
//////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

// BranchInst
API_EXPORT(LLVMValueRef)
LLVMPY_BranchInst_GetCondition(LLVMValueRef V)
{
    using namespace llvm;
    BranchInst *inst = unwrap<BranchInst>(V);
    if(inst->isConditional())
        return wrap(inst->getCondition());
    return nullptr;
}

// SwitchInst, 
// switch <intty> <value>, label <defaultdest> [ <intty> <val>, label <dest> ... ]
API_EXPORT(unsigned)
LLVMPY_SwitchInst_GetNumCases(LLVMValueRef V)
{
    using namespace llvm;
    SwitchInst *inst = unwrap<SwitchInst>(V);
    return inst->getNumCases();
}

// IndirectBrInst
// indirectbr <somety>* <address>, [ label <dest1>, label <dest2>, ... ]

// InvokeInst

// CallInst
API_EXPORT(unsigned)
LLVMPY_CallBase_GetNumArgOperands(LLVMValueRef V)
{
    using namespace llvm;
    const CallBase  *inst = unwrap<CallBase >(V);
#if LLVM_VERSION_MAJOR <13  // 先写之前版本的调用方式有助于代码工具给提示
    return inst->getNumArgOperands();
#else
    return inst->arg_size();
#endif
}

API_EXPORT(LLVMValueRef)
LLVMPY_CallBase_GetArgOperand(LLVMValueRef V, unsigned idx)
{
    using namespace llvm;
    const CallBase *inst = unwrap<CallBase>(V);
    return wrap(inst->getArgOperand(idx));
}

API_EXPORT(LLVMValueRef)
LLVMPY_CallBase_GetCalledFunction(LLVMValueRef V)
{
    using namespace llvm;
    const CallBase *inst = unwrap<CallBase>(V);

    if (auto *F = dyn_cast_or_null<Function>(inst->getCalledOperand()))
        if (F->getValueType() == inst->getFunctionType())
            return wrap(F);
    return nullptr;
}

API_EXPORT(unsigned)
LLVMPY_CallInst_IsTailCall(LLVMValueRef V)
{
    using namespace llvm;
    const CallInst *inst = unwrap<CallInst>(V);
    return inst->isTailCall();
}

API_EXPORT(LLVMValueRef)
LLVMPY_CallInst_GetCalledValue(LLVMValueRef V)
{
    using namespace llvm;
    const CallInst *inst = unwrap<CallInst>(V);
#if LLVM_VERSION_MAJOR <13
    return wrap(inst->getCalledValue());
#else
    return wrap(inst->getCalledOperand());
#endif
}

API_EXPORT(LLVMValueRef)
LLVMPY_CallInst_GetCalledFunction(LLVMValueRef V)
{
    using namespace llvm;
    const CallInst *inst = unwrap<CallInst>(V);
    if(inst ->isInlineAsm())
        return nullptr;
    return wrap(inst->getCalledFunction());
}


/*
    根据 Factgen 的使用情况，以下为 需要额外处理 的 opcode
    ret    getReturnValue
    br     getCondition / isConditional
    switch cases, getCaseValue / getCaseSuccessor , getNumCases
    invoke getCalledFunction  / getCalledValue / getNormalDest / getUnwindDest
    resume getValue
    alloc  getAllocatedType   , isArrayAllocation / getArraySize, getAlignment
    load   getPointerOperand / isVolatile / getAlignment / isAtomic
    vaarg  getPointerOperand
    extractValue    getNumIndices
    store  getValueOperand / getPointerOperand / isVolatile / getAlignment / isAtomic
    AtomicCmpXchg  getPointerOperand / getCompareOperand / getNewValOperand / isVolatile / getSuccessOrdering / getFailureOrdering
                   getSyncScopeID
    AtomicRMW    getPointerOperand / getValOperand / isVolatile / getOperation / getAlignment
    GEP     getPointerOperand / getNumIndices / getOperand / isInBounds
    phi     getNumIncomingValues / getIncomingValue / getIncomingBlock
    call   getCalledFunction  / getCalledValue / getNumArgOperands / getArgOperand
    insertValue idx_begin  / idx_end
    landingpad   isCleanup / getNumClauses / getClause / getPersonalityFn
    call    isInlineAsm / getCalledFunction / getCalledValue / getNumArgOperands / getArgOperand
            isTailCall / getCallingConv / getAttributes
    DbgDeclare TODO: debug info
    DbgValue TODO: debug info
    ExtractElement getVectorOperand / getIndexOperand
 */
} // end extern "C"
