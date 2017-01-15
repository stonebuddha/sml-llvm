signature LLVM_EXECUTIONENGINE =
sig

(** JIT Interpreter.

    This interface provides an OCaml API for LLVM execution engine (JIT/
    interpreter), the classes in the [ExecutionEngine] library. *)

(** [initialize ()] initializes the backend corresponding to the host.
    Returns [true] if initialization is successful; [false] indicates
    that there is no such backend or it is unable to emit object code
    via MCJIT. *)
val initialize : unit -> bool

(** An execution engine is either a JIT compiler or an interpreter, capable of
    directly loading an LLVM module and executing its functions without first
    invoking a static compiler and generating a native executable. *)
type llexecutionengine

type llgenericvalue

(** MCJIT compiler options. See [llvm::TargetOptions]. *)
type llcompileroptions = {
  opt_level : int,
  code_model : LlvmTarget.CodeModel.t,
  no_framepointer_elim : bool,
  enable_fast_isel : bool
}

(** Default MCJIT compiler options:
    [{ opt_level = 0; code_model = CodeModel.JIT_default;
       no_framepointer_elim = false; enable_fast_isel = false }] *)
val default_compiler_options : llcompileroptions

(** [create m optlevel] creates a new MCJIT just-in-time compiler, taking
    ownership of the module [m] if successful with the desired optimization
    level [optlevel]. Raises [Error msg] if an error occurrs. The execution
    engine is not garbage collected and must be destroyed with [dispose ee].

    Run {!initialize} before using this function.

    See the function [llvm::EngineBuilder::create]. *)
val create : llcompileroptions option -> LlvmCore.llmodule -> llexecutionengine

(** [dispose ee] releases the memory used by the execution engine and must be
    invoked to avoid memory leaks. *)
val dispose : llexecutionengine -> unit

(** [add_module m ee] adds the module [m] to the execution engine [ee]. *)
val add_module : LlvmCore.llmodule -> llexecutionengine -> unit

(** [remove_module m ee] removes the module [m] from the execution engine
    [ee]. Raises [Error msg] if an error occurs. *)
val remove_module : LlvmCore.llmodule -> llexecutionengine -> LlvmCore.llmodule

(** [run_static_ctors ee] executes the static constructors of each module in
    the execution engine [ee]. *)
val run_static_ctors : llexecutionengine -> unit

(** [run_static_dtors ee] executes the static destructors of each module in
    the execution engine [ee]. *)
val run_static_dtors : llexecutionengine -> unit

(** [data_layout ee] is the data layout of the execution engine [ee]. *)
val data_layout : llexecutionengine -> LlvmTarget.DataLayout.t

(** [add_global_mapping gv ptr ee] tells the execution engine [ee] that
    the global [gv] is at the specified location [ptr], which must outlive
    [gv] and [ee].
    All uses of [gv] in the compiled code will refer to [ptr]. *)
val add_global_mapping : LlvmCore.llvalue -> 'a C.ptr -> llexecutionengine -> unit

(** [get_global_value_address id typ ee] returns a pointer to the
    identifier [id] as type [typ], which will be a pointer type for a
    value, and which will be live as long as [id] and [ee]
    are. Caution: this function finalizes, i.e. forces code
    generation, all loaded modules.  Further modifications to the
    modules will not have any effect. *)
val get_global_value_address : string -> 'a C.ptr C.T.typ -> llexecutionengine -> 'a C.ptr

(** [get_function_address fn typ ee] returns a pointer to the function
    [fn] as type [typ], which will be a pointer type for a function
    (e.g. [(int -> int) typ]), and which will be live as long as [fn]
    and [ee] are. Caution: this function finalizes, i.e. forces code
    generation, all loaded modules.  Further modifications to the
    modules will not have any effect. *)
val get_function_address : string -> 'a C.fptr C.T.typ -> llexecutionengine -> 'a C.fptr

(* val create_generic_value_of_int : LlvmCore.lltype -> Int64.int -> bool -> llgenericvalue *)

val create_generic_value_of_pointer : C.voidptr -> llgenericvalue

val create_generic_value_of_float : LlvmCore.lltype -> real -> llgenericvalue

val generic_value_int_width : llgenericvalue -> int

(* val generic_value_to_int : llgenericvalue -> bool -> Int64.int *)

val generic_value_to_pointer : llgenericvalue -> C.voidptr

val generic_value_to_float : LlvmCore.lltype -> llgenericvalue -> real

val dispose_generic_value : llgenericvalue -> unit

val run_function_as_main : llexecutionengine -> LlvmCore.llvalue -> int -> string list -> string list -> int

val run_function : llexecutionengine -> LlvmCore.llvalue -> int -> llgenericvalue array -> llgenericvalue

end
