.PHONY: default smlnj mlton clean

FILES = \
test.sml \
mlton-main.sml

default: smlnj

smlnj:
	cp ../llvm.dylib .
	ml-build -Ccompiler-mc.error-non-exhaustive-match=true -Ccompiler-mc.error-non-exhaustive-bind=true test.cm Test.main test-image

mlton: test

test: test.mlb $(FILES)
	mlton -link-opt "-lstdc++ `llvm-config --ldflags --system-libs --libs all`" test.mlb ../llvm_core.o ../llvm_bitwriter.o

clean:
	rm -rf .cm test test-image* *.dylib
