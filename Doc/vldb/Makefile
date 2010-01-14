all: btree.pdf

%.pdf: %.tex
	pdflatex $^
	bibtex $*
	pdflatex $^
	pdflatex $^

#singlecol: *.tex
#grep -v multicol article.tex >singlecol.tex
#pdflatex singlecol.tex
#bibtex singlecol
#pdflatex singlecol.tex
#pdflatex singlecol.tex

clean:
	rm -f *.aux *.log *.toc *.snm *.out *.nav *.blg *.bbl


