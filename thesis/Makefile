SRCDIR=src
SRCFILE=thesis
PDF=$(SRCFILE).pdf
CITATIONS=citations.bib

export TEXINPUTS := ifimaster/:duoforside/:$(TEXINPUTS)

all: $(PDF)

$(PDF): $(SRCDIR)/$(SRCFILE).tex $(SRCDIR)/$(CITATIONS)
	cd $(SRCDIR) && latexmk --shell-escape -pdf -use-make $(SRCFILE) && mv $(PDF) ..

clean:
	rm $(PDF) && cd $(SRCDIR) && latexmk -CA && rm $(SRCFILE).bbl $(SRCFILE).run.xml

watch:
	fswatch -o --event Updated src | while read _; do clear; make; done;

