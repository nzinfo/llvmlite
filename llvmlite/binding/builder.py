from ctypes import POINTER, c_char_p, c_int, c_size_t, c_uint, c_bool, c_ulonglong
import enum

from llvmlite.binding import ffi
from llvmlite.binding.common import _decode_string, _encode_string

class BuilderRef(ffi.ObjectRef):

    def __init__(self, ptr):
        super().__init__(ptr)
        self._block = None

    def append_to_block(self, bb):
        if not bb.is_block:
            raise ValueError('expected block value, got %s' % (self._kind,))
        self._block = bb
        ffi.lib.LLVMPY_BuilderSetToBasicBlock(self, bb)
    
    @property
    def block(self):
        return self._block
    
    # 进行指令的操作
    def add_callinst(self, fnTy, fn, argsV, target_var):
        # argsVPtr = [x._ptr for x in argsV] # unwarp to raw ptr.
        args = (ffi.LLVMValueRef * len(argsV))(*argsV)
        return ffi.lib.LLVMPY_BuildCallInst(self, fnTy, fn, args, len(argsV), _encode_string(target_var), len(target_var))
    
    # 构造常数 作为 操作数 
    def int_const(self, intTy, N, signed=False):
        return ffi.lib.LLVMPY_ConstIntValue(intTy, N, signed)

    # 如果约束需要使用 LLVM IR 中已经存在的变量，则可以通过指令遍历时获取对应的引用
    
    # 为降低接口的复杂性 不提供修改 BasicBlock 的方法 ，只能删除后重新构造. BB.delete_block()
    pass

# =============================================================================
# Set function FFI

ffi.lib.LLVMPY_BuilderSetToBasicBlock.argtypes = [ffi.LLVMBuilderRef, ffi.LLVMValueRef]

ffi.lib.LLVMPY_BuilderGetBlock.argtypes = [ffi.LLVMBuilderRef]
ffi.lib.LLVMPY_BuilderGetBlock.restype = ffi.LLVMValueRef

ffi.lib.LLVMPY_BuildCallInst.argtypes = [ffi.LLVMBuilderRef, ffi.LLVMTypeRef, ffi.LLVMValueRef, POINTER(ffi.LLVMValueRef), c_uint, c_char_p, c_size_t]
ffi.lib.LLVMPY_BuildCallInst.restype = ffi.LLVMValueRef

ffi.lib.LLVMPY_ConstIntValue.argtypes = [ffi.LLVMTypeRef, c_ulonglong, c_bool]
ffi.lib.LLVMPY_ConstIntValue.restype = ffi.LLVMValueRef

# ffi.lib.LLVMPY_BuilderGetBlock.restype = ffi.LLVMValueRef void 
