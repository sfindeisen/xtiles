.PHONY: default
default: all

.PHONY: all
all: xtiles.out

xtiles.out: xtiles.hs
	ghc -XBangPatterns -Wall --make -o xtiles.out xtiles.hs

.PHONY: clean
clean:
	rm xtiles.out
	rm xtiles.hi
	rm xtiles.o
