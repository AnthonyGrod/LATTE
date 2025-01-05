all: build

build:
	cabal build
	cp dist-newstyle/build/x86_64-linux/ghc-*/INTP-0.1.0.0/x/INTP/build/INTP/INTP ./latc

clean:
	cabal clean
	rm -f latc

