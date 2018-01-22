ifeq ($(WATCH),true)
STACK_FLAGS += --file-watch
endif
ifeq ($(NOTIFY),true)
STACK_FLAGS += --exec scripts/notify-build-success
endif

build:
	stack build $(STACK_FLAGS)

doc: STACK_FLAGS += --haddock
doc: build

install: STACK_FLAGS += --copy-bins
install: doc
