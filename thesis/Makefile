SRCDIR=src
SRCFILE=thesis
PDF=$(SRCFILE).pdf
CITATIONS=citations.bib
OUTPUT=build

export TEXINPUTS := ifimaster/:duoforside/:$(TEXINPUTS)

all: $(PDF)

$(PDF): $(SRCDIR)/$(SRCFILE).tex $(SRCDIR)/$(CITATIONS) $(SRCDIR)/chapters
	cd $(SRCDIR) && latexmk --output-directory=../$(OUTPUT) --shell-escape -pdf -use-make $(SRCFILE) && cd .. && mv $(OUTPUT)/$(PDF) .

clean:
	rm $(PDF) && rm -rf $(OUTPUT)
	# rm $(PDF) && cd $(SRCDIR) && latexmk -CA && rm $(SRCFILE).bbl $(SRCFILE).run.xml

watch:
	fswatch -o --event Updated src/chapters/*.tex src/thesis.tex src/citations.bib | while read _; do clear; make; done;
	src/thesis.tex src/citations.bib | while read _; do clear; make; done;

