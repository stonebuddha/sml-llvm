signature LLVM_TARGET =
sig

(** Target Information.

    This interface provides an OCaml API for LLVM target information,
    the classes in the Target library. *)

type llmodule

type lltype

type llmemorybuffer

type llcontext

type llvalue

structure PassManager :
          sig
              type 'a t
          end

structure Endian :
          sig
              datatype t =
                       Big
                       | Little
          end

structure CodeGenOptLevel :
          sig
              datatype t =
                       None
                       | Less
                       | Default
                       | Aggressive
          end

structure RelocMode :
          sig
              datatype t =
                       Default
                       | Static
                       | PIC
                       | DynamicNoPIC
          end

structure CodeModel :
          sig
              datatype t =
                       Default
                       | JITDefault
                       | Small
                       | Kernel
                       | Medium
                       | Large

              val toInt : t -> int
          end

structure CodeGenFileType :
          sig
              datatype t =
                       AssemblyFile
                       | ObjectFile
          end

(** {6 Data Layout} *)

structure DataLayout :
          sig
              type t

              (** [of_string rep] parses the data layout string representation [rep].
                  See the constructor [llvm::DataLayout::DataLayout]. *)
              val of_string : string -> t

              (** [as_string dl] is the string representation of the data layout [dl].
                  See the method [llvm::DataLayout::getStringRepresentation]. *)
              val as_string : t -> string

              (** Returns the byte order of a target, either [Endian.Big] or
                  [Endian.Little].
                  See the method [llvm::DataLayout::isLittleEndian]. *)
              val byte_order : t -> Endian.t

              (** Returns the pointer size in bytes for a target.
                  See the method [llvm::DataLayout::getPointerSize]. *)
              val pointer_size : t -> int

              (** Returns the integer type that is the same size as a pointer on a target.
                  See the method [llvm::DataLayout::getIntPtrType]. *)
              val intptr_type : llcontext -> t -> lltype

              (** Returns the pointer size in bytes for a target in a given address space.
                  See the method [llvm::DataLayout::getPointerSize]. *)
              val qualified_pointer_size : int -> t -> int

              (** Returns the integer type that is the same size as a pointer on a target
                  in a given address space.
                  See the method [llvm::DataLayout::getIntPtrType]. *)
              val qualified_intptr_type : llcontext -> int -> t -> lltype

              (** Computes the size of a type in bits for a target.
                  See the method [llvm::DataLayout::getTypeSizeInBits]. *)
              (* FIXME: this can not compile under SML/NJ *)
              (* val size_in_bits : lltype -> t -> Int64.int *)

              (** Computes the storage size of a type in bytes for a target.
                  See the method [llvm::DataLayout::getTypeStoreSize]. *)
              (* FIXME: this can not compile under SML/NJ *)
              (* val store_size : lltype -> t -> Int64.int *)

              (** Computes the ABI size of a type in bytes for a target.
                  See the method [llvm::DataLayout::getTypeAllocSize]. *)
              (* FIXME: this can not compile under SML/NJ *)
              (* val abi_size : lltype -> t -> Int64.int *)

              (** Computes the ABI alignment of a type in bytes for a target.
                  See the method [llvm::DataLayout::getTypeABISize]. *)
              val abi_align : lltype -> t -> int

              (** Computes the call frame alignment of a type in bytes for a target.
                  See the method [llvm::DataLayout::getTypeABISize]. *)
              val stack_align : lltype -> t -> int

              (** Computes the preferred alignment of a type in bytes for a target.
                  See the method [llvm::DataLayout::getTypeABISize]. *)
              val preferred_align : lltype -> t -> int

              (** Computes the preferred alignment of a global variable in bytes for
                  a target. See the method [llvm::DataLayout::getPreferredAlignment]. *)
              val preferred_align_of_global : llvalue -> t -> int

              (** Computes the structure element that contains the byte offset for a target.
                  See the method [llvm::StructLayout::getElementContainingOffset]. *)
              (* FIXME: this can not compile under SML/NJ *)
              (* val element_at_offset : lltype -> Int64.int -> t -> int *)

              (** Computes the byte offset of the indexed struct element for a target.
                  See the method [llvm::StructLayout::getElementContainingOffset]. *)
              (* FIXME: this can not compile under SML/NJ *)
              (* val offset_of_element : lltype -> int -> t -> Int64.int *)
          end

(** {6 Target} *)

structure Target :
          sig
              type t

              (** [default_triple ()] returns the default target triple for current
                  platform. *)
              val default_triple : unit -> string

              (** [first ()] returns the first target in the registered targets
                  list, or [None]. *)
              val first : unit -> t option

              (** [succ t] returns the next target after [t], or [None]
                  if [t] was the last target. *)
              val succ : t -> t option

              (** [all ()] returns a list of known targets. *)
              val all : unit -> t list

              (** [by_name name] returns [Some t] if a target [t] named [name] is
                  registered, or [None] otherwise. *)
              val by_name : string -> t option

              (** [by_triple triple] returns a target for a triple [triple], or raises
                  [Error] if [triple] does not correspond to a registered target. *)
              val by_triple : string -> t

              (** Returns the name of a target. See [llvm::Target::getName]. *)
              val name : t -> string

              (** Returns the description of a target.
                  See [llvm::Target::getDescription]. *)
              val description : t -> string

              (** Returns [true] if the target has a JIT. *)
              val has_jit : t -> bool

              (** Returns [true] if the target has a target machine associated. *)
              val has_target_machine : t -> bool

              (** Returns [true] if the target has an ASM backend (required for
                  emitting output). *)
              val has_asm_backend : t -> bool
          end

(** {6 Target Machine} *)

structure TargetMachine :
          sig
              type t

              (** Creates a new target machine.
                  See [llvm::Target::createTargetMachine]. *)
              val create : string -> string option -> string option -> CodeGenOptLevel.t option -> RelocMode.t option -> CodeModel.t option -> Target.t -> t

              (** Returns the Target used in a TargetMachine *)
              val target : t -> Target.t

              (** Returns the triple used while creating this target machine. See
                  [llvm::TargetMachine::getTriple]. *)
              val triple : t -> string

              (** Returns the CPU used while creating this target machine. See
                  [llvm::TargetMachine::getCPU]. *)
              val cpu : t -> string

              (** Returns the data layout of this target machine. *)
              val data_layout : t -> DataLayout.t

              (** Returns the feature string used while creating this target machine. See
                  [llvm::TargetMachine::getFeatureString]. *)
              val features : t -> string

              (** Adds the target-specific analysis passes to the pass manager.
                  See [llvm::TargetMachine::addAnalysisPasses]. *)
              val add_analysis_passes : 'a PassManager.t -> t -> unit

              (** Sets the assembly verbosity of this target machine.
                  See [llvm::TargetMachine::setAsmVerbosity]. *)
              val set_verbose_asm : bool -> t -> unit

              (** Emits assembly or object data for the given module to the given
                  file or raise [Error]. *)
              val emit_to_file : llmodule -> CodeGenFileType.t -> string -> t -> unit

              (** Emits assembly or object data for the given module to a fresh memory
                  buffer or raise [Error]. *)
              val emit_to_memory_buffer : llmodule -> CodeGenFileType.t -> t -> llmemorybuffer
          end

end
