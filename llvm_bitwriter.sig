signature LLVM_BITWRITER =
sig

(** Bitcode writer.

    This interface provides an OCaml API for the LLVM bitcode writer, the
    classes in the Bitwriter library. *)

type llmodule

type llmemorybuffer

(** [write_bitcode_file m path] writes the bitcode for module [m] to the file at
    [path]. Returns [true] if successful, [false] otherwise. *)
val write_bitcode_file : llmodule -> string -> bool

(** [write_bitcode_to_fd ~unbuffered fd m] writes the bitcode for module
    [m] to the channel [c]. If [unbuffered] is [true], after every write the fd
    will be flushed. Returns [true] if successful, [false] otherwise. *)
val write_bitcode_to_fd : bool option -> llmodule -> Posix.FileSys.file_desc -> bool

(** [write_bitcode_to_memory_buffer m] returns a memory buffer containing
    the bitcode for module [m]. *)
val write_bitcode_to_memory_buffer : llmodule -> llmemorybuffer

(** [output_bitcode ~unbuffered c m] writes the bitcode for module [m]
    to the channel [c]. If [unbuffered] is [true], after every write the fd
    will be flushed. Returns [true] if successful, [false] otherwise. *)
val output_bitcode : bool option -> BinIO.outstream -> llmodule -> bool

end
