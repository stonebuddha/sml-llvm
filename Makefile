.PHONY: default all stub32 stub64 dynamic32 shared64 clean

default:

all: stub32 stub64 dynamic32 shared64

stub32: FFI32/llvm.cm

stub64: FFI64/llvm.mlb

dynamic32: llvm32.dylib

shared64: llvm64.o

clean:
	rm -rf .cm FFI32 FFI64 *.o *.dylib

FFI32/llvm.cm: llvm_sml.h
	rm -rf FFI32
	ml-nlffigen -include ../stub32.sml -libhandle Stub.libh -dir FFI32 -cmfile llvm.cm $^

FFI64/llvm.mlb: llvm_sml.h
	rm -rf FFI64
	mlnlffigen -linkage shared -dir FFI64 -mlbfile llvm.mlb $^

llvm32.dylib: llvm_sml.cpp
	g++ -m32 -dynamiclib `$(LLVMBIN32)/llvm-config --cxxflags --ldflags --system-libs --libs core` -o $@ $^

llvm64.o: llvm_sml.cpp
	g++ -c `$(LLVMBIN64)/llvm-config --cxxflags` -o $@ $^
