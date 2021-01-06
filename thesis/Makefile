SRCDIR=src
SRCFILE=thesis
PDF=$(SRCFILE).pdf
CITATIONS=citations.bib
OUTPUT=../build

export TEXINPUTS := ifimaster/:duoforside/:$(TEXINPUTS)

all: $(PDF)

$(PDF): $(SRCDIR)/$(SRCFILE).tex $(SRCDIR)/$(CITATIONS)
	cd $(SRCDIR) && latexmk --output-directory=$(OUTPUT) --shell-escape -pdf -use-make $(SRCFILE) && mv $(OUTPUT)/$(PDF) ..

clean:
	rm $(PDF) && cd $(SRCDIR) && latexmk -CA && rm $(SRCFILE).bbl $(SRCFILE).run.xml

watch:
	fswatch -o --event Updated src/thesis.tex src/citations.bib | while read _; do clear; make; done;

