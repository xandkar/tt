PKG_NAME := tt
EXE_FILE := $(PKG_NAME)
SRC_FILE := $(EXE_FILE).rkt
PREFIX   := $(HOME)

.PHONY: build
build: $(EXE_FILE)

$(EXE_FILE): $(SRC_FILE)
	raco exe -o $@ $<

.PHONY: install
install: $(EXE_FILE)
	mkdir -p $(PREFIX)/bin/
	cp $(EXE_FILE) $(PREFIX)/bin/

.PHONY: test
test:
	raco test ./$(SRC_FILE)

.PHONY: remove
remove:
	raco pkg remove $(PKG_NAME)

.PHONY: clean
clean:
	rm -f $(EXE_FILE)

### dev helpers ###
# scmindent.rkt : https://github.com/ds26gte/scmindent
# sponge        : https://joeyh.name/code/moreutils
.PHONY: indent
indent:
	scmindent.rkt < $(SRC_FILE) | sponge $(SRC_FILE)
