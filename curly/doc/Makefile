SOURCES := $(wildcard *.md)
FONTS := $(wildcard *.woff2) $(wildcard *.ttf)

all: bundle.tar.gz

bundle.tar.gz: bundle/style.css $(SOURCES:%.md=bundle/%.html) $(FONTS:%=bundle/%) bundle/install-curly.sh
	tar -czf $@ -C bundle $(^:bundle/%=%)

bundle:
	mkdir $@

bundle/%.sh: %.sh | bundle
	cp $< $@
bundle/%.woff2: %.woff2 | bundle
	cp $< $@
bundle/%.ttf: %.ttf | bundle
	cp $< $@
bundle/style.css: style.scss | bundle
	sassc $< > $@
bundle/%.html: %.md template.html | bundle
	pandoc -s --toc --template=template.html $< --css style.css -o $@ 

clean:
	rm -rf bundle
