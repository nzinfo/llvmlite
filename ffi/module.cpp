#include "llvm/IR/Module.h"
#include "core.h"
#include "llvm-c/Analysis.h"
#include "llvm-c/Core.h"
#include "llvm/IR/TypeFinder.h"
#include "llvm/IR/IntrinsicInst.h"
#include "llvm/IR/DebugInfoMetadata.h"
#include "llvm/IR/Metadata.h"
#include <clocale>
#include <string>

/* An iterator around a module's globals, including the stop condition */
struct GlobalsIterator {
    typedef llvm::Module::global_iterator iterator;
    iterator cur;
    iterator end;

    GlobalsIterator(iterator cur, iterator end) : cur(cur), end(end) {}
};

struct OpaqueGlobalsIterator;
typedef OpaqueGlobalsIterator *LLVMGlobalsIteratorRef;

/* An iterator around a module's functions, including the stop condition */
struct FunctionsIterator {
    typedef llvm::Module::const_iterator const_iterator;
    const_iterator cur;
    const_iterator end;

    FunctionsIterator(const_iterator cur, const_iterator end)
        : cur(cur), end(end) {}
};

struct OpaqueFunctionsIterator;
typedef OpaqueFunctionsIterator *LLVMFunctionsIteratorRef;

/* module types iterator */
class TypesIterator {
  private:
    llvm::TypeFinder finder;
    using const_iterator = llvm::TypeFinder::const_iterator;
    const_iterator cur;

  public:
    TypesIterator(llvm::Module &m, bool namedOnly)
        : finder(llvm::TypeFinder()) {
        finder.run(m, namedOnly);
        cur = finder.begin();
    }
    const llvm::Type *next() {
        if (cur != finder.end()) {
            return *cur++;
        }
        return nullptr;
    }
};

typedef TypesIterator *LLVMTypesIteratorRef;

/* An iterator around a module's metadata, including the stop condition */
struct NamedMetaIterator {
    typedef llvm::Module::named_metadata_iterator iterator;
    iterator cur;
    iterator end;

    NamedMetaIterator(iterator cur, iterator end) : cur(cur), end(end) {}
};

struct OpaqueNamedMetaIterator;
typedef OpaqueNamedMetaIterator *LLVMNamedMetaIteratorRef;


//
// Local helper functions
//

namespace llvm {

static LLVMGlobalsIteratorRef wrap(GlobalsIterator *GI) {
    return reinterpret_cast<LLVMGlobalsIteratorRef>(GI);
}

static GlobalsIterator *unwrap(LLVMGlobalsIteratorRef GI) {
    return reinterpret_cast<GlobalsIterator *>(GI);
}

static LLVMFunctionsIteratorRef wrap(FunctionsIterator *GI) {
    return reinterpret_cast<LLVMFunctionsIteratorRef>(GI);
}

static FunctionsIterator *unwrap(LLVMFunctionsIteratorRef GI) {
    return reinterpret_cast<FunctionsIterator *>(GI);
}

static LLVMTypesIteratorRef wrap(TypesIterator *TyI) {
    return reinterpret_cast<LLVMTypesIteratorRef>(TyI);
}

static TypesIterator *unwrap(LLVMTypesIteratorRef TyI) {
    return reinterpret_cast<TypesIterator *>(TyI);
}

static LLVMNamedMetaIteratorRef wrap(NamedMetaIterator *GI) {
    return reinterpret_cast<LLVMNamedMetaIteratorRef>(GI);
}

static NamedMetaIterator *unwrap(LLVMNamedMetaIteratorRef GI) {
    return reinterpret_cast<NamedMetaIterator *>(GI);
}


} // end namespace llvm

//
// Exported API
//

extern "C" {

API_EXPORT(void)
LLVMPY_DisposeModule(LLVMModuleRef m) { return LLVMDisposeModule(m); }

API_EXPORT(void)
LLVMPY_DisposeBuilder(LLVMBuilderRef irb) { return LLVMDisposeBuilder(irb); }


API_EXPORT(void)
LLVMPY_PrintModuleToString(LLVMModuleRef M, const char **outstr) {
    // Change the locale to en_US before calling LLVM to print the module
    // due to a LLVM bug https://llvm.org/bugs/show_bug.cgi?id=12906
    char *old_locale = strdup(setlocale(LC_ALL, NULL));
    setlocale(LC_ALL, "C");

    *outstr = LLVMPrintModuleToString(M);

    // Revert locale
    setlocale(LC_ALL, old_locale);
    free(old_locale);
}

API_EXPORT(const char *)
LLVMPY_GetModuleSourceFileName(LLVMModuleRef M) {
    return llvm::unwrap(M)->getSourceFileName().c_str();
}

API_EXPORT(const char *)
LLVMPY_GetModuleName(LLVMModuleRef M) {
    return llvm::unwrap(M)->getModuleIdentifier().c_str();
}

API_EXPORT(void)
LLVMPY_SetModuleName(LLVMModuleRef M, const char *Name) {
    llvm::unwrap(M)->setModuleIdentifier(Name);
}

API_EXPORT(LLVMValueRef)
LLVMPY_ParseDbgDeclareAddr(LLVMValueRef I)
{
    using namespace llvm;
    if (auto *DD = dyn_cast<DbgDeclareInst>(llvm::unwrap<Value>(I))) {
        return wrap(DD->getAddress());
    }
    if (auto *DD = dyn_cast<DbgValueInst>(llvm::unwrap<Value>(I))) {
        return wrap(DD->getValue());
    }
    return nullptr;
}


API_EXPORT(const char *)
LLVMPY_ParseDbgDeclareVar(LLVMValueRef I)
{
    using namespace llvm;
    if (auto *DD = dyn_cast<DbgDeclareInst>(llvm::unwrap<Value>(I))) {
        return DD->getVariable()->getName().str().c_str();
    }
    if (auto *DD = dyn_cast<DbgValueInst>(llvm::unwrap<Value>(I))) {
        return DD->getVariable()->getName().str().c_str();
    }
    return "";
}

API_EXPORT(const char *)
LLVMPY_ParseDbgDeclareType(LLVMValueRef I)
{
    using namespace llvm;
    if (auto *DD = dyn_cast<DbgDeclareInst>(llvm::unwrap<Value>(I))) {
        return DD->getVariable()->getType()->getName().str().c_str();
    }
    if (auto *DD = dyn_cast<DbgValueInst>(llvm::unwrap<Value>(I))) {
        return DD->getVariable()->getType()->getName().str().c_str();
    }
    return "";
}

API_EXPORT(LLVMValueRef)
LLVMPY_GetNamedFunction(LLVMModuleRef M, const char *Name) {
    return LLVMGetNamedFunction(M, Name);
}

API_EXPORT(LLVMValueRef)
LLVMPY_GetNamedGlobalVariable(LLVMModuleRef M, const char *Name) {
    using namespace llvm;
    return wrap(unwrap(M)->getGlobalVariable(Name));
}

API_EXPORT(size_t)
LLVMPY_TypeSize(LLVMModuleRef mod, LLVMTypeRef type)
{
    llvm::Module *m = llvm::unwrap(mod);
    return m->getDataLayout().getTypeSizeInBits(llvm::unwrap(type));
}

API_EXPORT(size_t)
LLVMPY_TypeStoreSize(LLVMModuleRef mod, LLVMTypeRef type)
{
    llvm::Module *m = llvm::unwrap(mod);
    return m->getDataLayout().getTypeStoreSizeInBits(llvm::unwrap(type));
}


API_EXPORT(LLVMTypeRef)
LLVMPY_GetNamedStructType(LLVMModuleRef M, const char *Name) {
    return LLVMGetTypeByName(M, Name);
}

API_EXPORT(int)
LLVMPY_VerifyModule(LLVMModuleRef M, char **OutMsg) {
    return LLVMVerifyModule(M, LLVMReturnStatusAction, OutMsg);
}

API_EXPORT(void)
LLVMPY_GetDataLayout(LLVMModuleRef M, const char **DL) {
    *DL = LLVMGetDataLayoutStr(M);
}

API_EXPORT(void)
LLVMPY_SetDataLayout(LLVMModuleRef M, const char *DL) {
    LLVMSetDataLayout(M, DL);
}

API_EXPORT(void)
LLVMPY_GetTarget(LLVMModuleRef M, const char **Triple) {
    *Triple = LLVMGetTarget(M);
}

API_EXPORT(void)
LLVMPY_SetTarget(LLVMModuleRef M, const char *Triple) {
    LLVMSetTarget(M, Triple);
}

// Iteration APIs

API_EXPORT(LLVMGlobalsIteratorRef)
LLVMPY_ModuleGlobalsIter(LLVMModuleRef M) {
    using namespace llvm;
    Module *mod = unwrap(M);
    return wrap(new GlobalsIterator(mod->global_begin(), mod->global_end()));
}

API_EXPORT(LLVMFunctionsIteratorRef)
LLVMPY_ModuleFunctionsIter(LLVMModuleRef M) {
    using namespace llvm;
    Module *mod = unwrap(M);
    return wrap(new FunctionsIterator(mod->begin(), mod->end()));
}

API_EXPORT(LLVMTypesIteratorRef)
LLVMPY_ModuleTypesIter(LLVMModuleRef M) {
    llvm::Module *mod = llvm::unwrap(M);
    auto *iter = new TypesIterator(*mod, false);
    return llvm::wrap(iter);
}

API_EXPORT(LLVMNamedMetaIteratorRef)
LLVMPY_ModuleNamedMetaIter(LLVMModuleRef M) {
    using namespace llvm;
    Module *mod = unwrap(M);
    return wrap(new NamedMetaIterator(mod->named_metadata_begin(), mod->named_metadata_end()));
}

/*
  These functions return NULL if we are at the end
*/
API_EXPORT(LLVMValueRef)
LLVMPY_GlobalsIterNext(LLVMGlobalsIteratorRef GI) {
    using namespace llvm;
    GlobalsIterator *iter = unwrap(GI);
    if (iter->cur != iter->end) {
        return wrap(&*iter->cur++);
    } else {
        return NULL;
    }
}

API_EXPORT(LLVMValueRef)
LLVMPY_FunctionsIterNext(LLVMFunctionsIteratorRef GI) {
    using namespace llvm;
    FunctionsIterator *iter = unwrap(GI);
    if (iter->cur != iter->end) {
        return wrap(&*iter->cur++);
    } else {
        return NULL;
    }
}

API_EXPORT(LLVMTypeRef)
LLVMPY_TypesIterNext(LLVMTypesIteratorRef TyI) {
    return llvm::wrap(llvm::unwrap(TyI)->next());
}

API_EXPORT(LLVMNamedMDNodeRef)
LLVMPY_NamedMetaIterNext(LLVMNamedMetaIteratorRef GI) {
    using namespace llvm;
    NamedMetaIterator *iter = unwrap(GI);
    if (iter->cur != iter->end) {
        return wrap(&*iter->cur++);
    } else {
        return NULL;
    }
}

API_EXPORT(void)
LLVMPY_DisposeGlobalsIter(LLVMGlobalsIteratorRef GI) {
    delete llvm::unwrap(GI);
}

API_EXPORT(void)
LLVMPY_DisposeFunctionsIter(LLVMFunctionsIteratorRef GI) {
    delete llvm::unwrap(GI);
}

API_EXPORT(void)
LLVMPY_DisposeTypesIter(LLVMTypesIteratorRef TyI) { delete llvm::unwrap(TyI); }

API_EXPORT(void)
LLVMPY_DisposeNamedMetaIter(LLVMNamedMetaIteratorRef NMI) {
    delete llvm::unwrap(NMI);
}

API_EXPORT(const char *)
LLVMPY_GetMDNodeName(LLVMNamedMDNodeRef Val) { 
    size_t name_len = 0;
    const char* s_ptr = LLVMGetNamedMetadataName (Val, &name_len);
    // return s_ptr;
    return LLVMPY_CreateByteString(s_ptr, name_len); 
    //printf("ssss: %s, (%p, %p)", s_ptr, s_ptr, p);
    // return LLVMPY_CreateString("cccc");
    //return p;
}

API_EXPORT(unsigned)
LLVMPY_GetMDNodeOperandCount(LLVMNamedMDNodeRef Val) {
    using namespace llvm;

    NamedMDNode * MDNode = unwrap<NamedMDNode>(Val);
    return MDNode->getNumOperands();
}

API_EXPORT(LLVMValueRef)
LLVMPY_GetMDNodeOperand(LLVMNamedMDNodeRef Val, unsigned idx) {
    using namespace llvm;

    // FIXME: 此处的时间复杂度会变为 n^2 当遍历时。    
    NamedMDNode *N = unwrap<NamedMDNode>(Val);
    if (!N)
        return NULL;
    if (idx>=N->getNumOperands())
        return NULL;

    // LLVMValueRef dest;
    LLVMValueRef* dest = new LLVMValueRef[N->getNumOperands()];
    LLVMGetNamedMetadataOperands(wrap(N->getParent()), LLVMPY_GetMDNodeName(Val), dest);
    LLVMValueRef rs = dest[idx];
    delete[] dest;
    return rs;
    //dest = wrap(llvm::dyn_cast<llvm::Value>(N->getOperand(idx)));
    // dest = wrap(cast< ValueAsMetadata >(N->getOperand(idx))->getValue());
    //dest = (LLVMValueRef)wrap(N->getOperand(idx));
    //return dest;
}
/*
void LLVMGetNamedMetadataOperands(LLVMModuleRef M, const char* name, LLVMValueRef *Dest)
{
  NamedMDNode *N = unwrap(M)->getNamedMetadata(name);
  if (!N)
    return;
  for (unsigned i=0;i<N->getNumOperands();i++)
    Dest[i] = wrap(N->getOperand(i));
}
*/

API_EXPORT(const char*)
LLVMPY_GetMDString(LLVMValueRef Val) {
    unsigned str_len = 0;
    return LLVMGetMDString(Val, &str_len);
}
 
API_EXPORT(int)
LLVMPY_GetNumOperands(LLVMValueRef Val) {
    return LLVMGetNumOperands(Val);
}

API_EXPORT(LLVMValueRef)
LLVMPY_GetOperand(LLVMValueRef Val, unsigned idx) {
    return LLVMGetOperand(Val, idx);
}

API_EXPORT(LLVMModuleRef)
LLVMPY_CloneModule(LLVMModuleRef M) { return LLVMCloneModule(M); }

// 代码生成相关的接口
API_EXPORT(LLVMBuilderRef)
LLVMPY_CreateIRBuilder(LLVMModuleRef M) { 
    return LLVMCreateBuilderInContext(LLVMGetModuleContext(M));
}

} // end extern "C"
