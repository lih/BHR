ifeq ($(WATCH),true)
STACK_FLAGS += --file-watch
endif

build:
	stack build $(STACK_FLAGS)

doc: STACK_FLAGS += --haddock
doc: build

install: STACK_FLAGS += --copy-bins
install: doc
