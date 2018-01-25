Curly.url := https://coiffiem.gricad-pages.univ-grenoble-alpes.fr/curly/pkg/curly.tar.xz
Curly.flags := 

ifeq ($(OS),Windows_NT)
    Curly.platform := Windows
else
    Curly.platform := $(shell sh -c 'uname -s 2>/dev/null || echo other')
endif
ifeq ($(THIS_PLATFORM),Windows)
  Curly.which := where
else
  Curly.which := which
endif

Curly.curly := $(shell $(Curly.which) curly 2>/dev/null)
ifeq ($(Curly.curly),)
  $(error "Could not find a 'curly' command anywhere. For Linux platforms, you can try installing it from $(Curly.url)")
endif

.PHONY: Curly.target
Curly.target:
	$(Curly.curly) $(Curly.flags)

clean: Curly.flags = %clean
clean: Curly.target
