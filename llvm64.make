FFI64/llvm.mlb: llvm_sml.h
	rm -rf FFI64
	mlnlffigen -include ../stub64.sml -libhandle Stub.libh -dir FFI64 -mlbfile llvm.mlb $^
