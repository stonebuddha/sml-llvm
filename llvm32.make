FFI32/llvm.cm: llvm_sml.h
	rm -rf FFI32
	ml-nlffigen -include ../stub32.sml -libhandle Stub.libh -dir FFI32 -cmfile llvm.cm $^
