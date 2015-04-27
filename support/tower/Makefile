IVORY_REPO ?= ../../../ivory
TOWER_REPO ?= ../../../tower
BSP_REPO ?= ../../../ivory-tower-stm32
include Makefile.sandbox

test:
	cabal run -- --src-dir=codegen-out
	make -C codegen-out
