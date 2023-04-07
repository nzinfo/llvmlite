from ctypes import POINTER, c_char_p, c_int, c_size_t, c_uint, c_bool, c_void_p
import enum

from llvmlite.binding import ffi
from llvmlite.binding.common import _decode_string, _encode_string


class Linkage(enum.IntEnum):
    # The LLVMLinkage enum from llvm-c/Core.h

    external = 0
    available_externally = 1
    linkonce_any = 2
    linkonce_odr = 3
    linkonce_odr_autohide = 4
    weak_any = 5
    weak_odr = 6
    appending = 7
    internal = 8
    private = 9
    dllimport = 10
    dllexport = 11
    external_weak = 12
    ghost = 13
    common = 14
    linker_private = 15
    linker_private_weak = 16

    def __repr__(self) -> str:
        str_repr = {
            Linkage.external: "external",
            Linkage.available_externally: "available_externally",
            Linkage.linkonce_any: "linkonce",
            Linkage.linkonce_odr: "linkonce_odr",
            Linkage.linkonce_odr_autohide: "external",
            Linkage.weak_any: "weak",
            Linkage.weak_odr: "weak_odr",
            Linkage.appending: "appending",
            Linkage.internal: "internal",
            Linkage.private: "private",
            # Linkage.dllimport: "external",
            # Linkage.dllexport: "external",
            Linkage.external_weak: "extern_weak",
            # Linkage.ghost: "external",
            Linkage.common: "common",
            # Linkage.linker_private: "external",
            # Linkage.linker_private_weak: "external",
        }
        if self._value_ not in str_repr:
            raise NotImplementedError
        return str_repr[self._value_]


class ThreadLocal(enum.IntEnum):
    NotThreadLocal = 0
    GeneralDynamicTLSModel = 1
    LocalDynamicTLSModel = 2
    InitialExecTLSModel = 3
    LocalExecTLSModel = 4

    def __repr__(self) -> str:
        str_repr = {
            ThreadLocal.NotThreadLocal: "",
            ThreadLocal.GeneralDynamicTLSModel: "thread_local",
            ThreadLocal.LocalDynamicTLSModel: "thread_local(localdynamic)",
            ThreadLocal.InitialExecTLSModel: "thread_local(initialexec)",
            ThreadLocal.LocalExecTLSModel: "thread_local(localexec)",
        }
        if self._value_ not in str_repr:
            raise NotImplementedError
        return str_repr[self._value_]


class Visibility(enum.IntEnum):
    # The LLVMVisibility enum from llvm-c/Core.h

    default = 0
    hidden = 1
    protected = 2

    def __repr__(self) -> str:
        str_repr = {
            Visibility.default: "default",
            Visibility.hidden: "hidden",
            Visibility.protected: "protected",
        }
        if self._value_ not in str_repr:
            raise NotImplementedError
        return str_repr[self._value_]


class CallingConv(enum.IntEnum):
    C = 0
    Fast = 8 
    Cold = 9 
    GHC = 10
    HiPE = 11
    WebKit_JS = 12 
    AnyReg = 13
    PreserveMost = 14
    PreserveAll = 15  
    Swift = 16
    CXX_FAST_TLS = 17
    Tail = 18 
    
    CFGuard_Check = 19 
    SwiftTail = 20 
    FirstTargetCC = 64 
    X86_StdCall = 64 

    X86_FastCall = 65 
    ARM_APCS = 66 
    ARM_AAPCS = 67 
    ARM_AAPCS_VFP = 68 

    MSP430_INTR = 69 
    X86_ThisCall = 70 
    PTX_Kernel = 71 
    PTX_Device = 72 

    SPIR_FUNC = 75 
    SPIR_KERNEL = 76 
    Intel_OCL_BI = 77 
    X86_64_SysV = 78 

    Win64 = 79 
    X86_VectorCall = 80 
    DUMMY_HHVM = 81 
    DUMMY_HHVM_C = 82 

    X86_INTR = 83 
    AVR_INTR = 84 
    AVR_SIGNAL = 85 
    AVR_BUILTIN = 86 

    AMDGPU_VS = 87 
    AMDGPU_GS = 88 
    AMDGPU_PS = 89 
    AMDGPU_CS = 90 

    AMDGPU_KERNEL = 91 
    X86_RegCall = 92 
    AMDGPU_HS = 93 
    MSP430_BUILTIN = 94 

    AMDGPU_LS = 95 
    AMDGPU_ES = 96 
    AArch64_VectorCall = 97 
    AArch64_SVE_VectorCall = 98 

    WASM_EmscriptenInvoke = 99 
    AMDGPU_Gfx = 100 
    M68k_INTR = 101 
    AArch64_SME_ABI_Support_Routines_PreserveMost_From_X0 = 102 

    AArch64_SME_ABI_Support_Routines_PreserveMost_From_X2 = 103 
    MaxID = 1023

    def __repr__(self) -> str:
        str_repr = {
            CallingConv.Fast: "fastcc",
            CallingConv.Cold: "coldcc",
            CallingConv.X86_FastCall: "x86_fastcallcc",
            CallingConv.X86_StdCall: "x86_stdcallcc",
            CallingConv.X86_ThisCall: "x86_thiscallcc",
            CallingConv.Intel_OCL_BI: "intel_ocl_bicc",
            CallingConv.ARM_AAPCS: "arm_aapcscc",
            CallingConv.ARM_AAPCS_VFP: "arm_aapcs_vfpcc",
            CallingConv.ARM_APCS: "arm_apcscc",
            CallingConv.MSP430_INTR: "msp430_intrcc",
            CallingConv.PTX_Device: "tx_device",
            CallingConv.PTX_Kernel: "ptx_kernel",
        }
        if self._value_ not in str_repr:
            return "cc" + str(self._value_)
            # raise NotImplementedError
        return str_repr[self._value_]


class AtomicOrdering(enum.IntEnum):
    NotAtomic = 0,
    Unordered = 1,
    Monotonic = 2,      # Equivalent to C++'s relaxed.
    # Consume = 3,  #  Not specified yet.
    Acquire = 4,
    Release = 5,
    AcquireRelease = 6,
    SequentiallyConsistent = 7,
    LAST = SequentiallyConsistent

    def __repr__(self) -> str:
        str_repr = {
            AtomicOrdering.NotAtomic: "",
            AtomicOrdering.Unordered: "unordered",
            AtomicOrdering.Monotonic: "monotonic",
            AtomicOrdering.Acquire: "acquire",
            AtomicOrdering.Release: "release",
            AtomicOrdering.AcquireRelease: "acq_rel",
            AtomicOrdering.SequentiallyConsistent: "seq_cst",
            Visibility.hidden: "hidden",
            Visibility.protected: "protected",
        }
        if self._value_ not in str_repr:
            raise NotImplementedError
        return str_repr[self._value_]


class StorageClass(enum.IntEnum):
    # The LLVMDLLStorageClass enum from llvm-c/Core.h

    default = 0
    dllimport = 1
    dllexport = 2


class TypeRef(ffi.ObjectRef):
    """A weak reference to a LLVM type
    """
    @property
    def name(self):
        """
        Get type name
        """
        return ffi.ret_string(ffi.lib.LLVMPY_GetTypeName(self))

    @property
    def is_void_ty(self)->bool:
        if self.name:
            return False
        
        if self.is_pointer or self.is_array or self.is_function or self.is_struct or self.is_vector:
            return False
        
        return str(self) == "void"

    @property
    def is_integer_ty(self)->bool:
        if self.name:
            return False
        
        if self.is_pointer or self.is_array or self.is_function or self.is_struct or self.is_vector:
            return False
        s_expr = str(self)
        return s_expr[0] == 'i'

    @property
    def is_pointer(self):
        """
        Returns true is the type is a pointer type.
        """
        return ffi.lib.LLVMPY_TypeIsPointer(self)

    @property
    def is_vector(self):
        """
        Returns true is the type is a vector type.
        """
        return ffi.lib.LLVMPY_TypeIsVector(self)

    @property
    def is_array(self):
        """
        Returns true is the type is an array type.
        """
        return ffi.lib.LLVMPY_TypeIsArray(self)

    @property
    def is_struct(self):
        """
        Returns true is the type is an array type.
        """
        return ffi.lib.LLVMPY_TypeIsStruct(self)

    @property
    def is_function(self) -> bool:
        """
        Returns true is the type is a function type.
        """
        return ffi.lib.LLVMPY_TypeIsFunctionType(self)

    @property
    def is_function_vararg(self) -> bool:
        """
        Returns true is the type is a function type.
        """
        if not self.is_function:
            raise ValueError(f"Type {self} not a function.")
        return ffi.lib.LLVMPY_TypeIsFunctionVarArg(self)

    @property
    def element_type(self):
        """
        Returns the pointed-to type. When the type is not a pointer,
        raises exception.
        """
        if self.is_function:
            return TypeRef(ffi.lib.LLVMPY_GetFunctionReturnType(self))
        #if not self.is_pointer:
        #    raise ValueError("Type {} is not a pointer".format(self))
        if not (self.is_pointer or self.is_array or self.is_vector):
            raise ValueError(f"Type {self} has no elements")
        return TypeRef(ffi.lib.LLVMPY_GetElementType(self))
    
    @property
    def array_num_elements(self):
        if not self.is_array:
            raise ValueError(f"Type {self} has no elements")
        return ffi.lib.LLVMPY_TypeGetArrayNumElements(self)

    @property
    def struct_num_elements(self):
        """
        Returns the pointed-to type. When the type is not a pointer,
        raises exception.
        """
        # 与函数复用
        if self.is_function:
            return ffi.lib.LLVMPY_GetFunctionNumParams(self)
        if not self.is_struct:
            raise ValueError(f"Type {self} has no elements")
        return ffi.lib.LLVMPY_GetStructNumElements(self)

    def struct_element_type(self, n):
        """
        Returns the nth type in struct. When the type is not a pointer,
        raises exception.
        """
        # 与函数复用
        if self.is_function:
            return TypeRef(ffi.lib.LLVMPY_GetFunctionParamType(self, n))
        if not self.is_struct:
            raise ValueError(f"Type {self} has no elements")
        assert n < self.struct_num_elements, f"Invalid type index: {n}"
        return TypeRef(ffi.lib.LLVMPY_GetStructElementType(self, n))

    def __str__(self):
        return ffi.ret_string(ffi.lib.LLVMPY_PrintType(self))


class ValueRef(ffi.ObjectRef):
    """A weak reference to a LLVM value.
    """

    def __init__(self, ptr, kind, parents):
        self._kind = kind
        self._parents = parents
        ffi.ObjectRef.__init__(self, ptr)

    def __str__(self):
        with ffi.OutputString() as outstr:
            ffi.lib.LLVMPY_PrintValueToString(self, outstr)
            return str(outstr)

    def as_operand(self, with_ty=False):
        with ffi.OutputString() as outstr:
            ffi.lib.LLVMPY_PrintValueToStringAsOperand(self, with_ty, outstr)
            return str(outstr)
        
    @property
    def module(self):
        """
        The module this function or global variable value was obtained from.
        """
        return self._parents.get('module')

    @property
    def function(self):
        """
        The function this argument or basic block value was obtained from.
        """
        return self._parents.get('function')

    @property
    def block(self):
        """
        The block this instruction value was obtained from.
        """
        return self._parents.get('block')

    @property
    def instruction(self):
        """
        The instruction this operand value was obtained from.
        """
        return self._parents.get('instruction')

    @property
    def is_global(self):
        if self._kind == 'global':
            return True
        else:
            return ffi.lib.LLVMPY_IsGlobalVariable(self) != 0
    
    @property
    def is_alias(self):
        return self._kind == 'alias'
    
    @property
    def is_function(self):
        return self._kind == 'function'

    @property
    def is_block(self):
        return self._kind == 'block'

    @property
    def is_argument(self):
        return self._kind == 'argument'

    @property
    def is_instruction(self):
        return self._kind == 'instruction'

    @property
    def is_operand(self):
        return self._kind == 'operand'
    
    @property
    def is_metadata(self):
        return self._kind == 'metadata'

    @property
    def is_constant(self):
        """
        Whether this operand is a contant expr
        """
        return ffi.lib.LLVMPY_IsConstant(self) != 0

    def is_global_constant(self):
        """
        Whether this value is global variable and the memory it references
        is constant.
        """
        return ffi.lib.LLVMPY_IsGlobalVariableConstant(self) != 0

    @property
    def is_constantexpr(self):
        """
        Whether this operand is a contant expr
        """
        return ffi.lib.LLVMPY_IsConstantExpr(self) != 0

    @property
    def name(self):
        return _decode_string(ffi.lib.LLVMPY_GetValueName(self))
    
    @property
    def unique_id(self):
        return ffi.lib.LLVMPY_GetValueAddressAsID(self)

    @property
    def dbg_name(self):
        return _decode_string(ffi.lib.LLVMPY_GetDbgValueName(self))
    
    @property
    def dbg_type_name(self):
        return _decode_string(ffi.lib.LLVMPY_GetDbgValueTypeName(self))
    

    @property
    def dbg_loc(self):
        return (_decode_string(ffi.lib.LLVMPY_GetDbgFile(self)),
                ffi.lib.LLVMPY_GetDbgLine(self),
                ffi.lib.LLVMPY_GetDbgCol(self))

    @name.setter
    def name(self, val):
        ffi.lib.LLVMPY_SetValueName(self, _encode_string(val))
    
    @property
    def demangled_name(self):
        name = self.name
        return _decode_string(ffi.lib.LLVMPY_ItaniumDemangle(_encode_string(name), len(name)))

    @property
    def linkage(self):
        return Linkage(ffi.lib.LLVMPY_GetLinkage(self))
    
    @linkage.setter
    def linkage(self, value):
        if not isinstance(value, Linkage):
            value = Linkage[value]
        ffi.lib.LLVMPY_SetLinkage(self, value)

    @property
    def thread_local(self):
        return ThreadLocal(ffi.lib.LLVMPY_GetThreadLocalMode(self))
    
    @property
    def section(self):
        assert self.is_global
        v = ffi.lib.LLVMPY_GetSection(self)
        if v:
            return _decode_string(v)
        else:
            return None
    
    @property
    def callingconv(self):
        return CallingConv(ffi.lib.LLVMPY_GetCallingConv(self))
    
    @property
    def alignment(self):
        return ffi.lib.LLVMPY_GetAlignment(self)

    @alignment.setter
    def alignment(self, value):
        ffi.lib.LLVMPY_SetAlignment(self, value)

    @property
    def visibility(self):
        return Visibility(ffi.lib.LLVMPY_GetVisibility(self))

    @visibility.setter
    def visibility(self, value):
        if not isinstance(value, Visibility):
            value = Visibility[value]
        ffi.lib.LLVMPY_SetVisibility(self, value)

    @property
    def storage_class(self):
        return StorageClass(ffi.lib.LLVMPY_GetDLLStorageClass(self))

    @storage_class.setter
    def storage_class(self, value):
        if not isinstance(value, StorageClass):
            value = StorageClass[value]
        ffi.lib.LLVMPY_SetDLLStorageClass(self, value)

    def add_function_attribute(self, attr):
        """Only works on function value

        Parameters
        -----------
        attr : str
            attribute name
        """
        if not self.is_function:
            raise ValueError('expected function value, got %s' % (self._kind,))
        attrname = str(attr)
        attrval = ffi.lib.LLVMPY_GetEnumAttributeKindForName(
            _encode_string(attrname), len(attrname))
        if attrval == 0:
            # 处理成字符串属性
            ffi.lib.LLVMPY_SetFunctionStringAttr(self, _encode_string(attrname), len(attrname))
            # raise ValueError('no such attribute {!r}'.format(attrname))
        ffi.lib.LLVMPY_AddFunctionAttr(self, attrval)

    @property
    def type(self):
        """
        This value's LLVM type.
        """
        # XXX what does this return?
        return TypeRef(ffi.lib.LLVMPY_TypeOf(self))

    @property
    def is_declaration(self):
        """
        Whether this value (presumably global) is defined in the current
        module.
        """
        if not (self.is_global or self.is_function):
            raise ValueError('expected global or function value, got %s'
                             % (self._kind,))
        return ffi.lib.LLVMPY_IsDeclaration(self)

    @property
    def attributes(self):
        """
        Return an iterator over this value's attributes.
        The iterator will yield a string for each attribute.
        """
        itr = iter(())
        if self.is_function:
            it = ffi.lib.LLVMPY_FunctionAttributesIter(self)
            itr = _AttributeListIterator(it)
        elif self.is_instruction:
            if self.opcode == 'call':
                it = ffi.lib.LLVMPY_CallInstAttributesIter(self)
                itr = _AttributeListIterator(it)
            elif self.opcode == 'invoke':
                it = ffi.lib.LLVMPY_InvokeInstAttributesIter(self)
                itr = _AttributeListIterator(it)
        elif self.is_global:
            it = ffi.lib.LLVMPY_GlobalAttributesIter(self)
            itr = _AttributeSetIterator(it)
        elif self.is_argument:
            it = ffi.lib.LLVMPY_ArgumentAttributesIter(self)
            itr = _AttributeSetIterator(it)
        return itr

    @property
    def blocks(self):
        """
        Return an iterator over this function's blocks.
        The iterator will yield a ValueRef for each block.
        """
        if not self.is_function:
            raise ValueError('expected function value, got %s' % (self._kind,))
        it = ffi.lib.LLVMPY_FunctionBlocksIter(self)
        parents = self._parents.copy()
        parents.update(function=self)
        return _BlocksIterator(it, parents)

    @property
    def predecessors(self):
        if not self.is_block:
            raise ValueError('expected block value, got %s' % (self._kind,))
        
        it = ffi.lib.LLVMPY_BlockPredsIter(self)
        parents = self._parents.copy()
        parents.update(function=self)
        return _PredBlocksIterator(it, parents)
    
    @property
    def arguments(self):
        """
        Return an iterator over this function's arguments.
        The iterator will yield a ValueRef for each argument.
        """
        if not self.is_function:
            raise ValueError('expected function value, got %s' % (self._kind,))
        it = ffi.lib.LLVMPY_FunctionArgumentsIter(self)
        parents = self._parents.copy()
        parents.update(function=self)
        return _ArgumentsIterator(it, parents)

    @property
    def return_type(self):
        """
        Function's return type.
        """
        if not self.is_function:
            raise ValueError('expected function value, got %s' % (self._kind,))
        return TypeRef(ffi.lib.LLVMPY_FunctionReturnType(self))
    
    @property
    def personality_fn(self):
        if not self.is_function:
            raise ValueError('expected function value, got %s' % (self._kind,))
        fn = ffi.lib.LLVMPY_GetPersonalityFn(self)
        if fn:
            return ValueRef(fn, 'function', self._parents)
        else:
            return None


    """    
    @property
    def function_set_attr(self, attr):
        '''
        Function's return type.
        '''
        if not self.is_function:
            raise ValueError('expected function value, got %s' % (self._kind,))
        return ffi.lib.LLVMPY_SetFunctionStringAttr(self, )
    """

    @property
    def instructions(self):
        """
        Return an iterator over this block's instructions.
        The iterator will yield a ValueRef for each instruction.
        """
        if not self.is_block:
            raise ValueError('expected block value, got %s' % (self._kind,))
        it = ffi.lib.LLVMPY_BlockInstructionsIter(self)
        parents = self._parents.copy()
        parents.update(block=self)
        return _InstructionsIterator(it, parents)

    @property
    def operands(self):
        """
        Return an iterator over this instruction's operands.
        The iterator will yield a ValueRef for each operand.
        """
        if self.is_instruction:
            #raise ValueError('expected instruction value, got %s'
            #                 % (self._kind,))
            it = ffi.lib.LLVMPY_InstructionOperandsIter(self)
            parents = self._parents.copy()
            parents.update(instruction=self)
            return _OperandsIterator(it, parents)
        
        # 处理标准的 operands
        o_list = []
        for i in range(0, ffi.lib.LLVMPY_GetNumOperands(self)):
            op = ffi.lib.LLVMPY_GetOperand(self, i)
            v = ValueRef(op, 'metadata', self._parents)
            o_list.append(v)
        return o_list

    @property
    def opcode(self):
        if not self.is_instruction:
            raise ValueError('expected instruction value, got %s'
                             % (self._kind,))
        return ffi.ret_string(ffi.lib.LLVMPY_GetOpcodeName(self))

    @property
    def initializer(self):
        """
        This value's initializer
        """
        if not self.is_global:
            raise ValueError('expected global value, got %s'
                             % (self._kind,))
        v = ffi.lib.LLVMPY_GlobalGetInitializer(self)
        if not v:
            return None
        return ValueRef(v, self._kind, self._parents)

    @property
    def aliasee(self):
        """
        This value's aliasee
        """
        if not self.is_global:
            raise ValueError('expected global value, got %s'
                             % (self._kind,))
        v = ffi.lib.LLVMPY_GlobalGetAliasee(self)
        if not v:
            return None
        return ValueRef(v, self._kind, self._parents)

    # function 相关的函数
    def append_basic_block(self, name):
        if not self.is_function:
            raise ValueError('expected function value, got %s' % (self._kind,))
        p = ffi.lib.LLVMPY_AppendBasicBlock(self, _encode_string(name), len(name))
        return ValueRef(p, 'block', self)

    def delete_block(self):
        if not self.is_block:
            raise ValueError('expected block value, got %s' % (self._kind,))
        ffi.lib.LLVMPY_DeleteBasicBlock(self)

    @property
    def ce_as_inst(self):
        """
        ConstantExpr as Instruction
        """
        if not self.is_constantexpr:
            raise ValueError('expected constant expr, got %s' % (self._kind,))
        return ValueRef(ffi.lib.LLVMPY_ConstantExprAsInst(self),
                        'instruction', self._parents)

    @property
    def phi_incoming_count(self):
        """
        Get incoming value and block of a PHI node
        """
        if not self.opcode == 'phi':
            raise ValueError('expected phi instruction, got %s'
                             % (self.opcode()))
        return ffi.lib.LLVMPY_PhiCountIncoming(self)

    def phi_incoming(self, idx):
        """
        Get incoming value and block of a PHI node
        """
        if not self.opcode == 'phi':
            raise ValueError('expected phi instruction, got %s'
                             % (self.opcode()))
        if idx >= self.phi_incoming_count:
            raise ValueError('index of phi instruction out of bounds')

        # FIXME: we screw up the parents here
        return (ValueRef(ffi.lib.LLVMPY_PhiGetIncomingValue(self, idx),
                         'operand', self._parents),
                ValueRef(ffi.lib.LLVMPY_PhiGetIncomingBlock(self, idx),
                         'block', self._parents))


class NamedMetaRef(ffi.ObjectRef):
    """A weak reference to a LLVM value.
    """

    def __init__(self, ptr, kind, parents):
        self._kind = kind
        self._parents = parents
        ffi.ObjectRef.__init__(self, ptr)

    @property
    def name(self):
        """
        Get type name
        """
        return _decode_string(ffi.lib.LLVMPY_GetMDNodeName(self))
    
    @property
    def operands(self):
        # FIXME: 目前的代码实现非常非常低效
        v_list = []
        for i in range(0, ffi.lib.LLVMPY_GetMDNodeOperandCount(self)):
            v_list.append(
                ValueRef(ffi.lib.LLVMPY_GetMDNodeOperand(self, i),
                        'metadata', self._parents)
            )
        return v_list
    

class _ValueIterator(ffi.ObjectRef):

    kind = None  # derived classes must specify the Value kind value
    # as class attribute

    def __init__(self, ptr, parents):
        ffi.ObjectRef.__init__(self, ptr)
        # Keep parent objects (module, function, etc) alive
        self._parents = parents
        if self.kind is None:
            raise NotImplementedError('%s must specify kind attribute'
                                      % (type(self).__name__,))

    def __next__(self):
        vp = self._next()
        if vp:
            return ValueRef(vp, self.kind, self._parents)
        else:
            raise StopIteration

    next = __next__

    def __iter__(self):
        return self


class _AttributeIterator(ffi.ObjectRef):

    def __next__(self):
        vp = self._next()
        if vp:
            return vp
        else:
            raise StopIteration

    next = __next__

    def __iter__(self):
        return self


class _AttributeListIterator(_AttributeIterator):

    def _dispose(self):
        self._capi.LLVMPY_DisposeAttributeListIter(self)

    def _next(self):
        itr = ffi.lib.LLVMPY_AttributeListIterNext(self)
        if itr:
            return _AttributeSetIterator(itr)
        return None
    
    @property
    def has_fn_attr(self):
        return ffi.lib.LLVMPY_AttributeListAttr(self, 1)

    @property
    def has_ret_attr(self):
        return ffi.lib.LLVMPY_AttributeListAttr(self, 0)


class _AttributeSetIterator(_AttributeIterator):

    def _dispose(self):
        self._capi.LLVMPY_DisposeAttributeSetIter(self)

    def _next(self):
        return ffi.ret_bytes(ffi.lib.LLVMPY_AttributeSetIterNext(self))


class _BlocksIterator(_ValueIterator):

    kind = 'block'

    def _dispose(self):
        self._capi.LLVMPY_DisposeBlocksIter(self)

    def _next(self):
        return ffi.lib.LLVMPY_BlocksIterNext(self)

class _PredBlocksIterator(_ValueIterator):

    kind = 'block'

    def _dispose(self):
        self._capi.LLVMPY_DisposePredBlocksIter(self)

    def _next(self):
        return ffi.lib.LLVMPY_PredBlocksIterNext(self)

class _ArgumentsIterator(_ValueIterator):

    kind = 'argument'

    def _dispose(self):
        self._capi.LLVMPY_DisposeArgumentsIter(self)

    def _next(self):
        return ffi.lib.LLVMPY_ArgumentsIterNext(self)


class _InstructionsIterator(_ValueIterator):

    kind = 'instruction'

    def _dispose(self):
        self._capi.LLVMPY_DisposeInstructionsIter(self)

    def _next(self):
        return ffi.lib.LLVMPY_InstructionsIterNext(self)


class _OperandsIterator(_ValueIterator):

    kind = 'operand'

    def _dispose(self):
        self._capi.LLVMPY_DisposeOperandsIter(self)

    def _next(self):
        return ffi.lib.LLVMPY_OperandsIterNext(self)


# FFI

ffi.lib.LLVMPY_PrintValueToString.argtypes = [
    ffi.LLVMValueRef,
    POINTER(c_char_p)
]

ffi.lib.LLVMPY_PrintValueToStringAsOperand.argtypes = [
    ffi.LLVMValueRef,
    c_bool,
    POINTER(c_char_p)
]

ffi.lib.LLVMPY_GetGlobalParent.argtypes = [ffi.LLVMValueRef]
ffi.lib.LLVMPY_GetGlobalParent.restype = ffi.LLVMModuleRef

ffi.lib.LLVMPY_GetValueName.argtypes = [ffi.LLVMValueRef]
ffi.lib.LLVMPY_GetValueName.restype = c_char_p

ffi.lib.LLVMPY_GetValueAddressAsID.argtypes = [ffi.LLVMValueRef]
ffi.lib.LLVMPY_GetValueAddressAsID.restype = c_size_t

ffi.lib.LLVMPY_GetMDNodeName.argtypes = [ffi.LLVMNamedMDNodeRef]
ffi.lib.LLVMPY_GetMDNodeName.restype = c_char_p

ffi.lib.LLVMPY_GetMDNodeOperandCount.argtypes = [ffi.LLVMNamedMDNodeRef]
ffi.lib.LLVMPY_GetMDNodeOperandCount.restype = c_uint

ffi.lib.LLVMPY_GetMDNodeOperand.argtypes = [ffi.LLVMNamedMDNodeRef, c_uint]
ffi.lib.LLVMPY_GetMDNodeOperand.restype = ffi.LLVMValueRef

ffi.lib.LLVMPY_GetNumOperands.argtypes = [ffi.LLVMValueRef]
ffi.lib.LLVMPY_GetNumOperands.restype = c_int

ffi.lib.LLVMPY_GetOperand.argtypes = [ffi.LLVMValueRef, c_int]
ffi.lib.LLVMPY_GetOperand.restype = ffi.LLVMValueRef

ffi.lib.LLVMPY_ItaniumDemangle.argtypes = [c_char_p, c_size_t]
ffi.lib.LLVMPY_ItaniumDemangle.restype = c_char_p

ffi.lib.LLVMPY_GetDbgValueName.argtypes = [ffi.LLVMValueRef]
ffi.lib.LLVMPY_GetDbgValueName.restype = c_char_p

ffi.lib.LLVMPY_GetDbgValueTypeName.argtypes = [ffi.LLVMValueRef]
ffi.lib.LLVMPY_GetDbgValueTypeName.restype = c_char_p

ffi.lib.LLVMPY_GetDbgFile.argtypes = [ffi.LLVMValueRef]
ffi.lib.LLVMPY_GetDbgFile.restype = c_char_p

ffi.lib.LLVMPY_SetValueName.argtypes = [ffi.LLVMValueRef, c_char_p]

ffi.lib.LLVMPY_TypeOf.argtypes = [ffi.LLVMValueRef]
ffi.lib.LLVMPY_TypeOf.restype = ffi.LLVMTypeRef


ffi.lib.LLVMPY_PrintType.argtypes = [ffi.LLVMTypeRef]
ffi.lib.LLVMPY_PrintType.restype = c_void_p

ffi.lib.LLVMPY_TypeIsPointer.argtypes = [ffi.LLVMTypeRef]
ffi.lib.LLVMPY_TypeIsPointer.restype = c_bool

ffi.lib.LLVMPY_TypeIsArray.argtypes = [ffi.LLVMTypeRef]
ffi.lib.LLVMPY_TypeIsArray.restype = c_bool

ffi.lib.LLVMPY_TypeGetArrayNumElements.argtypes = [ffi.LLVMTypeRef]
ffi.lib.LLVMPY_TypeGetArrayNumElements.restype = c_uint

ffi.lib.LLVMPY_TypeIsStruct.argtypes = [ffi.LLVMTypeRef]
ffi.lib.LLVMPY_TypeIsStruct.restype = c_bool

ffi.lib.LLVMPY_TypeIsVector.argtypes = [ffi.LLVMTypeRef]
ffi.lib.LLVMPY_TypeIsVector.restype = c_bool

ffi.lib.LLVMPY_TypeIsFunctionType.argtypes = [ffi.LLVMTypeRef]
ffi.lib.LLVMPY_TypeIsFunctionType.restype = c_bool

ffi.lib.LLVMPY_TypeIsFunctionVarArg.argtypes = [ffi.LLVMTypeRef]
ffi.lib.LLVMPY_TypeIsFunctionVarArg.restype = c_bool

ffi.lib.LLVMPY_GetFunctionParamType.argtypes = [ffi.LLVMTypeRef, c_uint]
ffi.lib.LLVMPY_GetFunctionParamType.restype = ffi.LLVMTypeRef

ffi.lib.LLVMPY_GetFunctionNumParams.argtypes = [ffi.LLVMTypeRef]
ffi.lib.LLVMPY_GetFunctionNumParams.restype = c_uint

ffi.lib.LLVMPY_GetFunctionReturnType.argtypes = [ffi.LLVMTypeRef]
ffi.lib.LLVMPY_GetFunctionReturnType.restype = ffi.LLVMTypeRef

ffi.lib.LLVMPY_GetPersonalityFn.argtypes = [ffi.LLVMValueRef]
ffi.lib.LLVMPY_GetPersonalityFn.restype = ffi.LLVMValueRef

ffi.lib.LLVMPY_GetElementType.argtypes = [ffi.LLVMTypeRef]
ffi.lib.LLVMPY_GetElementType.restype = ffi.LLVMTypeRef

ffi.lib.LLVMPY_GetStructNumElements.argtypes = [ffi.LLVMTypeRef]
ffi.lib.LLVMPY_GetStructNumElements.restype = c_uint

ffi.lib.LLVMPY_GetStructElementType.argtypes = [ffi.LLVMTypeRef, c_uint]
ffi.lib.LLVMPY_GetStructElementType.restype = ffi.LLVMTypeRef

ffi.lib.LLVMPY_GetTypeName.argtypes = [ffi.LLVMTypeRef]
ffi.lib.LLVMPY_GetTypeName.restype = c_void_p

ffi.lib.LLVMPY_GetDbgLine.argtypes = [ffi.LLVMValueRef]
ffi.lib.LLVMPY_GetDbgLine.restype = c_uint

ffi.lib.LLVMPY_GetDbgCol.argtypes = [ffi.LLVMValueRef]
ffi.lib.LLVMPY_GetDbgCol.restype = c_uint

ffi.lib.LLVMPY_GetLinkage.argtypes = [ffi.LLVMValueRef]
ffi.lib.LLVMPY_GetLinkage.restype = c_int

ffi.lib.LLVMPY_SetLinkage.argtypes = [ffi.LLVMValueRef, c_int]

ffi.lib.LLVMPY_GetAlignment.argtypes = [ffi.LLVMValueRef]
ffi.lib.LLVMPY_GetAlignment.restype = c_uint

ffi.lib.LLVMPY_SetAlignment.argtypes = [ffi.LLVMValueRef, c_uint]

ffi.lib.LLVMPY_GetVisibility.argtypes = [ffi.LLVMValueRef]
ffi.lib.LLVMPY_GetVisibility.restype = c_int

ffi.lib.LLVMPY_SetVisibility.argtypes = [ffi.LLVMValueRef, c_int]

ffi.lib.LLVMPY_GetThreadLocalMode.argtypes = [ffi.LLVMValueRef]
ffi.lib.LLVMPY_GetThreadLocalMode.restype = c_int

ffi.lib.LLVMPY_GetCallingConv.argtypes = [ffi.LLVMValueRef]
ffi.lib.LLVMPY_GetCallingConv.restype = c_int

ffi.lib.LLVMPY_GetDLLStorageClass.argtypes = [ffi.LLVMValueRef]
ffi.lib.LLVMPY_GetDLLStorageClass.restype = c_int

ffi.lib.LLVMPY_SetDLLStorageClass.argtypes = [ffi.LLVMValueRef, c_int]

ffi.lib.LLVMPY_GetEnumAttributeKindForName.argtypes = [c_char_p, c_size_t]
ffi.lib.LLVMPY_GetEnumAttributeKindForName.restype = c_uint

ffi.lib.LLVMPY_AddFunctionAttr.argtypes = [ffi.LLVMValueRef, c_uint]

ffi.lib.LLVMPY_IsDeclaration.argtypes = [ffi.LLVMValueRef]
ffi.lib.LLVMPY_IsDeclaration.restype = c_int

ffi.lib.LLVMPY_IsConstant.argtypes = [ffi.LLVMValueRef]
ffi.lib.LLVMPY_IsConstant.restype = c_int

ffi.lib.LLVMPY_IsConstantExpr.argtypes = [ffi.LLVMValueRef]
ffi.lib.LLVMPY_IsConstantExpr.restype = c_int

ffi.lib.LLVMPY_IsGlobalVariableConstant.argtypes = [ffi.LLVMValueRef]
ffi.lib.LLVMPY_IsGlobalVariableConstant.restype = c_int

ffi.lib.LLVMPY_IsGlobalVariable.argtypes = [ffi.LLVMValueRef]
ffi.lib.LLVMPY_IsGlobalVariable.restype = c_int

ffi.lib.LLVMPY_FunctionAttributesIter.argtypes = [ffi.LLVMValueRef]
ffi.lib.LLVMPY_FunctionAttributesIter.restype = ffi.LLVMAttributeListIterator

ffi.lib.LLVMPY_SetFunctionStringAttr.argtypes = [ffi.LLVMValueRef, c_char_p, c_size_t]
# ffi.lib.LLVMPY_FunctionAttributesIter.restype = ffi.LLVMAttributeListIterator

ffi.lib.LLVMPY_FunctionReturnType.argtypes = [ffi.LLVMValueRef]
ffi.lib.LLVMPY_FunctionReturnType.restype = ffi.LLVMTypeRef

ffi.lib.LLVMPY_AppendBasicBlock.argtypes = [ffi.LLVMValueRef, c_char_p, c_size_t] 
ffi.lib.LLVMPY_AppendBasicBlock.restype = ffi.LLVMValueRef

ffi.lib.LLVMPY_CallInstAttributesIter.argtypes = [ffi.LLVMValueRef]
ffi.lib.LLVMPY_CallInstAttributesIter.restype = ffi.LLVMAttributeListIterator

ffi.lib.LLVMPY_InvokeInstAttributesIter.argtypes = [ffi.LLVMValueRef]
ffi.lib.LLVMPY_InvokeInstAttributesIter.restype = ffi.LLVMAttributeListIterator

ffi.lib.LLVMPY_GlobalAttributesIter.argtypes = [ffi.LLVMValueRef]
ffi.lib.LLVMPY_GlobalAttributesIter.restype = ffi.LLVMAttributeSetIterator

ffi.lib.LLVMPY_ArgumentAttributesIter.argtypes = [ffi.LLVMValueRef]
ffi.lib.LLVMPY_ArgumentAttributesIter.restype = ffi.LLVMAttributeSetIterator

ffi.lib.LLVMPY_FunctionBlocksIter.argtypes = [ffi.LLVMValueRef]
ffi.lib.LLVMPY_FunctionBlocksIter.restype = ffi.LLVMBlocksIterator

ffi.lib.LLVMPY_FunctionArgumentsIter.argtypes = [ffi.LLVMValueRef]
ffi.lib.LLVMPY_FunctionArgumentsIter.restype = ffi.LLVMArgumentsIterator

ffi.lib.LLVMPY_BlockInstructionsIter.argtypes = [ffi.LLVMValueRef]
ffi.lib.LLVMPY_BlockInstructionsIter.restype = ffi.LLVMInstructionsIterator

ffi.lib.LLVMPY_InstructionOperandsIter.argtypes = [ffi.LLVMValueRef]
ffi.lib.LLVMPY_InstructionOperandsIter.restype = ffi.LLVMOperandsIterator

ffi.lib.LLVMPY_DisposeAttributeListIter.argtypes = [
    ffi.LLVMAttributeListIterator]

ffi.lib.LLVMPY_DisposeAttributeSetIter.argtypes = [ffi.LLVMAttributeSetIterator]

ffi.lib.LLVMPY_DisposeBlocksIter.argtypes = [ffi.LLVMBlocksIterator]

ffi.lib.LLVMPY_DisposeInstructionsIter.argtypes = [ffi.LLVMInstructionsIterator]

ffi.lib.LLVMPY_DisposeOperandsIter.argtypes = [ffi.LLVMOperandsIterator]

ffi.lib.LLVMPY_AttributeListIterNext.argtypes = [ffi.LLVMAttributeListIterator]
ffi.lib.LLVMPY_AttributeListIterNext.restype = ffi.LLVMAttributeSetIterator

ffi.lib.LLVMPY_AttributeListAttr.argtypes = [ffi.LLVMAttributeListIterator, c_uint]
ffi.lib.LLVMPY_AttributeListAttr.restype = c_uint

ffi.lib.LLVMPY_AttributeSetIterNext.argtypes = [ffi.LLVMAttributeSetIterator]
ffi.lib.LLVMPY_AttributeSetIterNext.restype = c_void_p

ffi.lib.LLVMPY_BlocksIterNext.argtypes = [ffi.LLVMBlocksIterator]
ffi.lib.LLVMPY_BlocksIterNext.restype = ffi.LLVMValueRef

ffi.lib.LLVMPY_ArgumentsIterNext.argtypes = [ffi.LLVMArgumentsIterator]
ffi.lib.LLVMPY_ArgumentsIterNext.restype = ffi.LLVMValueRef

ffi.lib.LLVMPY_InstructionsIterNext.argtypes = [ffi.LLVMInstructionsIterator]
ffi.lib.LLVMPY_InstructionsIterNext.restype = ffi.LLVMValueRef

ffi.lib.LLVMPY_OperandsIterNext.argtypes = [ffi.LLVMOperandsIterator]
ffi.lib.LLVMPY_OperandsIterNext.restype = ffi.LLVMValueRef

ffi.lib.LLVMPY_GetOpcodeName.argtypes = [ffi.LLVMValueRef]
ffi.lib.LLVMPY_GetOpcodeName.restype = c_void_p

ffi.lib.LLVMPY_GlobalGetInitializer.argtypes = [ffi.LLVMValueRef]
ffi.lib.LLVMPY_GlobalGetInitializer.restype = ffi.LLVMValueRef

ffi.lib.LLVMPY_GlobalGetAliasee.argtypes = [ffi.LLVMValueRef]
ffi.lib.LLVMPY_GlobalGetAliasee.restype = ffi.LLVMValueRef

ffi.lib.LLVMPY_DeleteBasicBlock.argtypes = [ffi.LLVMValueRef]
# ffi.lib.LLVMPY_DeleteBasicBlock.restype = ffi.LLVMBlocksIterator

ffi.lib.LLVMPY_ConstantExprAsInst.argtypes = [ffi.LLVMValueRef]
ffi.lib.LLVMPY_ConstantExprAsInst.restype = ffi.LLVMValueRef

ffi.lib.LLVMPY_PhiCountIncoming.argtypes = [ffi.LLVMValueRef]
ffi.lib.LLVMPY_PhiCountIncoming.restype = c_uint

ffi.lib.LLVMPY_PhiGetIncomingValue.argtypes = [ffi.LLVMValueRef, c_uint]
ffi.lib.LLVMPY_PhiGetIncomingValue.restype = ffi.LLVMValueRef

ffi.lib.LLVMPY_PhiGetIncomingBlock.argtypes = [ffi.LLVMValueRef, c_uint]
ffi.lib.LLVMPY_PhiGetIncomingBlock.restype = ffi.LLVMValueRef

ffi.lib.LLVMPY_BlockPredsIter.argtypes = [ffi.LLVMValueRef]
ffi.lib.LLVMPY_BlockPredsIter.restype = ffi.LLVMPredBlocksIterator

ffi.lib.LLVMPY_DisposePredBlocksIter.argtypes = [ffi.LLVMPredBlocksIterator]

ffi.lib.LLVMPY_PredBlocksIterNext.argtypes = [ffi.LLVMPredBlocksIterator]
ffi.lib.LLVMPY_PredBlocksIterNext.restype = ffi.LLVMValueRef

ffi.lib.LLVMPY_GetSection.argtypes = [ffi.LLVMValueRef]
ffi.lib.LLVMPY_GetSection.restype = c_char_p
