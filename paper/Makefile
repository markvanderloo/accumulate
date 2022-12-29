
article.pdf: article.Rnw
	R -s -e "Sweave('article.Rnw')"
	pdflatex article.tex
	bibtex article
	pdflatex article.tex
	pdflatex article.tex
	xdg-open article.pdf&

clean:
	rm -f *.aux *.log *.blg *.bbl *.out

