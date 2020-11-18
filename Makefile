.PHONY: install
install:
	raco pkg install

.PHONY: test
test:
	raco test ./tt.rkt

.PHONY: remove
remove:
	raco pkg remove tt

.PHONY: clean
clean:
	rm -rf compiled
