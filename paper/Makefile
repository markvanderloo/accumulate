
article.pdf: article.Rnw refs.bib
	R -s -e "Sweave('article.Rnw')"
	pdflatex article.tex
	bibtex article
	pdflatex article.tex
	pdflatex article.tex
	xdg-open article.pdf&

article.R: article.Rnw
	R -s -e "Stangle('article.Rnw')"

clean:
	rm -f *.aux *.log *.blg *.bbl *.out

