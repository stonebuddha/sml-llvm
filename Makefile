.PHONY: default all smlnj mlton clean run-smlnj run-mlton

FILES = \
llvm_core.sig \
llvm_core.sml \
test.sml \
mlton-test.sml

default: smlnj

all: smlnj mlton

smlnj: llvm32.dylib FFI32/llvm.cm
	ml-build -Ccompiler-mc.error-non-exhaustive-match=true -Ccompiler-mc.error-non-exhaustive-bind=true llvm.cm Test.main test-image

mlton: test

clean:
	rm -rf test test-image* *.dylib *.dSYM FFI32 FFI64

run-smlnj: smlnj
	sml @SMLload=test-image $*

run-mlton: test llvm64.dylib
	./test

test: llvm.mlb FFI64/llvm.mlb $(FILES)
	mlton -output test llvm.mlb

llvm32.dylib: llvm_sml.cpp
	g++ -m32 -dynamiclib `~/Experiments/llvm/build32/bin/llvm-config --cxxflags --ldflags --system-libs --libs core` -o llvm32.dylib llvm_sml.cpp

llvm64.dylib: llvm_sml.cpp
	g++ -dynamiclib `llvm-config --cxxflags --ldflags --system-libs --libs core` -o llvm64.dylib llvm_sml.cpp

FFI32/llvm.cm: llvm_sml.h
	rm -rf FFI32
	ml-nlffigen -include ../stub32.sml -libhandle Stub.libh -dir FFI32 -cmfile llvm.cm llvm_sml.h

FFI64/llvm.mlb: llvm_sml.h
	rm -rf FFI64
	mlnlffigen -include ../stub64.sml -libhandle Stub.libh -dir FFI64 -mlbfile llvm.mlb llvm_sml.h
