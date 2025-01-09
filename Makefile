all: build

build:
	cabal build
	cp dist-newstyle/build/x86_64-linux/ghc-*/COMP-0.1.0.0/x/COMP/build/COMP/COMP ./latc_llvm
	cp latc_llvm latc

clean:
	cabal clean
	rm -f latc_llvm

