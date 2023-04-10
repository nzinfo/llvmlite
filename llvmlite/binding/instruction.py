from ctypes import POINTER, c_char_p, c_int, c_size_t, c_uint, c_bool, c_void_p
import enum

from llvmlite.binding import ffi, ValueRef
from llvmlite.binding.common import _decode_string, _encode_string


class ReturnInst(object):
    """
    A ReturnInst represents a return instruction.
    Ref: https://llvm.org/doxygen/Instructions_8h_source.html
    """

    def __init__(self, value):
        self._value = value
        # 解析 return value.
        self._ret_value = None
        if self._value.operands_num:
            for op in self._value.operands:
                self._ret_value = op
                break
    @property
    def value(self):
        return self._value

    @property
    def return_value(self):
        return self._ret_value

    def __repr__(self):
        return self._value.__repr__()


class BranchInst(object):
    def __init__(self, value):
        self._value = value

        n = self._value.operands_num
        self._is_conditional = (n == 3)
        assert n == 3 or n == 1

        # 尝试计算 condition
        self._condition_value = None
        operands = [x for x in self._value.operands]
        self._condition_value = operands[-3] if n == 3 else None

    @property
    def is_conditional(self):
        return self._is_conditional

    @property
    def condition(self):
        return self._condition_value


class CallBase(object):

    def __init__(self, value):
        self._value = value
        # The last operand is the called operand.
        self._called_value = None
        operands = [x for x in self._value.operands]
        # 由于有 bound arg 存在，无法直接利用 operand 计算参数

    @property
    def called_value(self):
        return self._called_value

    # 对参数进行迭代
    @property
    def args_num(self):
        return ffi.lib.LLVMPY_CallBase_GetNumArgOperands(self)

    def arg(self, index):
        return ValueRef(
            ffi.lib.LLVMPY_CallBase_GetArgOperand(self, index)
            , 'operand', self._value._parents)

    @property
    def called_fn(self):
        v = ffi.lib.LLVMPY_CallBase_GetCalledFunction(self._value)
        if v:
            return ValueRef(v, 'function', self._value._parents)
        return None

    @property
    def is_tail_call(self):
        return bool(ffi.lib.LLVMPY_CallInst_IsTailCall(self))

    @property
    def is_inline_asm(self):
        if self.called_value:
            return self.called_value.is_inline_asm
        return False


class CallInst(CallBase):
    pass


g_instr_map = {
    'ret': ReturnInst,
    'br':  BranchInst,
    'call': CallInst,
}


def parse_instruction(value):
    """
    Parse a textual LLVM IR instruction.
    """
    global g_instr_map

    k = value.opcode
    if k in g_instr_map:
        return g_instr_map[k](value)
    raise NotImplementedError


def value_to_instr(value):
    """
        cast to instruction by opcode
    """
    if not value.is_instruction:
        raise ValueError('expected instruction, got %s' % (value._kind,))

    return parse_instruction(value)

# FFI

# 注意 GEP , Call/Invoke 是需要额外制作接口反而比较简单指令。
ffi.lib.LLVMPY_CallBase_GetNumArgOperands.argtypes = [ffi.LLVMValueRef]
ffi.lib.LLVMPY_CallBase_GetNumArgOperands.restype = c_uint

ffi.lib.LLVMPY_CallBase_GetArgOperand.argtypes = [ffi.LLVMValueRef, c_uint]
ffi.lib.LLVMPY_CallBase_GetArgOperand.restype = ffi.LLVMValueRef

ffi.lib.LLVMPY_CallInst_IsTailCall.argtypes = [ffi.LLVMValueRef]
ffi.lib.LLVMPY_CallInst_IsTailCall.restype = c_uint

ffi.lib.LLVMPY_CallBase_GetCalledFunction.argtypes = [ffi.LLVMValueRef]
ffi.lib.LLVMPY_CallBase_GetCalledFunction.restype = ffi.LLVMValueRef

# EOF
