default:
	stack build .
test:

clean:

distclean: clean
	stack clean

.PHONY: default test clean distclean
