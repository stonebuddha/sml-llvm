.PHONY: default smlnj mlton clean

FILES = \
test.sml \
mlton-main.sml

default: smlnj

smlnj:
	cp ../llvm32.dylib .
	ml-build -Ccompiler-mc.error-non-exhaustive-match=true -Ccompiler-mc.error-non-exhaustive-bind=true test.cm Test.main test-image

mlton: test

test: test.mlb $(FILES)
	mlton -link-opt "-lstdc++ `llvm-config --ldflags --system-libs --libs core`" test.mlb ../llvm64.o

clean:
	rm -rf .cm test test-image* *.dylib
