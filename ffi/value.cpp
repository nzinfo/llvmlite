#include "core.h"
#include "llvm-c/Core.h"
#include <string>

#include <iostream>

// the following is needed for WriteGraph()
#include "llvm/Analysis/CFGPrinter.h"

/* An iterator around a attribute list, including the stop condition */
struct AttributeListIterator {
    typedef llvm::AttributeList::iterator const_iterator;
    const_iterator cur;
    const_iterator end;

    AttributeListIterator(const_iterator cur, const_iterator end)
        : cur(cur), end(end) {}
};

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
    return wrap(new AttributeListIterator(attrs.begin(), attrs.end()));
}

API_EXPORT(LLVMAttributeSetIteratorRef)
LLVMPY_ArgumentAttributesIter(LLVMValueRef A) {
    using namespace llvm;
    Argument *arg = unwrap<Argument>(A);
    unsigned argno = arg->getArgNo();
#if LLVM_VERSION_MAJOR >13    
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
    CallInst *inst = unwrap<CallInst>(C);
    AttributeList attrs = inst->getAttributes();
    return wrap(new AttributeListIterator(attrs.begin(), attrs.end()));
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
    return wrap(new BlocksIterator(func->begin(), func->end()));
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

API_EXPORT(const char *)
LLVMPY_AttributeListIterNext(LLVMAttributeListIteratorRef GI) {
    using namespace llvm;
    AttributeListIterator *iter = unwrap(GI);
    if (iter->cur != iter->end) {
        return LLVMPY_CreateString((&*iter->cur++)->getAsString().c_str());
    } else {
        return NULL;
    }
}

API_EXPORT(const char *)
LLVMPY_AttributeSetIterNext(LLVMAttributeSetIteratorRef GI) {
    using namespace llvm;
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
        return wrap(static_cast<const Value *>(&*iter->cur++));
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

API_EXPORT(const char *)
LLVMPY_GetValueName(LLVMValueRef Val) { return LLVMGetValueName(Val); }

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

API_EXPORT(LLVMTypeRef)
LLVMPY_GetElementType(LLVMTypeRef type) {
    llvm::Type *unwrapped = llvm::unwrap(type);
    llvm::PointerType *ty = llvm::dyn_cast<llvm::PointerType>(unwrapped);
    if (ty != nullptr) {
        return llvm::wrap(ty->getElementType());
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

API_EXPORT(int)
LLVMPY_IsDeclaration(LLVMValueRef GV) { return LLVMIsDeclaration(GV); }

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

} // end extern "C"
