SRC_FILE := tt.rkt

.PHONY: install
install:
	raco pkg install --deps search-auto

.PHONY: test
test:
	raco test ./$(SRC_FILE)

.PHONY: remove
remove:
	raco pkg remove tt

.PHONY: clean
clean:
	rm -rf compiled

### dev helpers ###
# scmindent.rkt : https://github.com/ds26gte/scmindent
# sponge        : https://joeyh.name/code/moreutils
.PHONY: indent
indent:
	scmindent.rkt < $(SRC_FILE) | sponge $(SRC_FILE)
