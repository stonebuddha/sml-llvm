.PHONY: default smlnj mlton run-smlnj run-mlton clean

FILES = \
test.sml \
mlton-main.sml

default: smlnj

smlnj:
	ml-build -Ccompiler-mc.error-non-exhaustive-match=true -Ccompiler-mc.error-non-exhaustive-bind=true test.cm Test.main test-image

mlton: test

test: test.mlb $(FILES)
	mlton test.mlb

run-smlnj: smlnj
	cp ../llvm32.dylib .
	sml @SMLload=test-image

run-mlton: mlton
	cp ../llvm64.dylib .
	./test

clean:
	rm -rf .cm test test-image* *.dylib
