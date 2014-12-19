DIST=dist
BUILD=$(DIST)/build
DOC=$(DIST)/doc
SRC=src
TMP=tmp

.PHONY: default
default: all

.PHONY: mkdirs
mkdirs:
	mkdir -p $(BUILD)
	mkdir -p $(DOC)

.PHONY: all
all: mkdirs $(BUILD)/xtiles.out

$(BUILD)/xtiles.out: $(SRC)/xtiles.hs
	ghc -XBangPatterns -Wall -odir $(TMP) -hidir $(TMP) --make -o $(BUILD)/xtiles.out $(SRC)/xtiles.hs

.PHONY: clean
clean:
	@rm -v -rf $(DIST)
	@rm -v $(TMP)/*.hi
	@rm -v $(TMP)/*.o
