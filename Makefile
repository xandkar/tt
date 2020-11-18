.PHONY: install
install:
	raco pkg install

.PHONY: remove
remove:
	raco pkg remove tt

.PHONY: clean
clean:
	rm -rf compiled
