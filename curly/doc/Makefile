SOURCES := $(wildcard *.md)
FONTS := $(wildcard *.woff2) $(wildcard *.ttf)

all: bundle.tar.gz

bundle.tar.gz: bundle/style.css bundle/bootstrap.min.css bundle/bootstrap-theme.min.css $(SOURCES:%.md=bundle/%.html) $(FONTS:%=bundle/%) bundle/install-curly.sh bundle/favicon.png
	tar -czf $@ -C bundle $(^:bundle/%=%)

bundle:
	mkdir -p $@

bundle/%.sh: %.sh | bundle
	cp -a $< $@
bundle/%.png: %.png | bundle
	cp -a $< $@
bundle/%.woff2: %.woff2 | bundle
	cp -a $< $@
bundle/%.ttf: %.ttf | bundle
	cp -a $< $@
bundle/style.css: style.scss | bundle
	sassc $< > $@
bundle/bootstrap%.css: bootstrap%.css | bundle
	cp $< $@

bundle/%.html: %.md template.html links.mdi | bundle
	pandoc -s --toc --template=template.html $< links.mdi --css bootstrap.min.css --css bootstrap-theme.min.css --css style.css -o $@ 

clean:
	rm -rf bundle
