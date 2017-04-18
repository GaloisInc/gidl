include stack.mk

IVORY_REPO             ?= ../ivory
TOWER_REPO             ?= ../tower
IVORY_TOWER_STM32_REPO ?= ../ivory-tower-stm32

default:
	stack build .

test: haskell-backend-test
test: ivory-backend-test
test: tower-backend-test
test: rpc-backend-test
test: elm-backend-test

haskell-backend-test: default
	stack exec -- gidl -b haskell \
			   --debug \
			   -i tests/example.gidl \
			   -o tests/gidl-haskell-backend-test \
			   -p gidl-haskell-backend-test \
			   -n Gidl.Haskell.Test
	make -C tests/gidl-haskell-backend-test test

haskell-backend-test-clean:
	-rm -rf tests/gidl-haskell-backend-test

ivory-backend-test: default
	stack exec -- gidl -b ivory \
			   --debug \
			   -i tests/example.gidl \
			   -o tests/gidl-ivory-backend-test \
			   -p gidl-ivory-backend-test \
			   -n Gidl.Ivory.Test \
			   --ivory-repo=$(IVORY_REPO)
	make -C tests/gidl-ivory-backend-test test

ivory-backend-test-clean:
	-rm -rf tests/gidl-ivory-backend-test

tower-backend-test: default
	stack exec -- gidl -b tower \
			   --debug \
			   -i tests/example.gidl \
			   -o tests/gidl-tower-backend-test \
			   -p gidl-tower-backend-test \
			   -n Gidl.Test \
			   --ivory-repo=$(IVORY_REPO) \
			   --tower-repo=$(TOWER_REPO) \
			   --ivory-tower-stm32-repo=$(IVORY_TOWER_STM32_REPO)
	make -C tests/gidl-tower-backend-test test

tower-backend-test-clean:
	-rm -rf tests/gidl-tower-backend-test

rpc-backend-test: default
	stack exec -- gidl -b haskell-rpc \
			   --debug \
			   -i tests/example.gidl \
			   -o tests/gidl-rpc-backend-test \
			   -p gidl-rpc-backend-test \
			   -n Gidl.Test
	make -C tests/gidl-rpc-backend-test
	#make -C tests/gidl-rpc-backend-test test

rpc-backend-test-clean:
	-rm -rf tests/gidl-rpc-backend-test

elm-backend-test: default
	stack exec -- gidl -b elm \
			   --debug \
			   -i tests/example.gidl \
			   -o tests/gidl-elm-backend-test \
			   -p gidl-elm-backend-test \
			   -n Gidl.Test
	make -C tests/gidl-elm-backend-test

elm-backend-test-clean:
	-rm -rf tests/gidl-elm-backend-test

clean: ivory-backend-test-clean
clean: tower-backend-test-clean
clean: haskell-backend-test-clean
clean: rpc-backend-test-clean
clean: elm-backend-test-clean

TRAVIS_STACK ?= stack --no-terminal

travis-test:
	$(TRAVIS_STACK) setup
	$(TRAVIS_STACK) build --test --no-run-tests --haddock --no-haddock-deps --pedantic
	make test
