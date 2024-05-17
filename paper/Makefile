
jss5097.pdf: jss5097.Rnw jss5097.bib
	R -s -e "Sweave('jss5097.Rnw')"
	pdflatex jss5097.tex
	bibtex jss5097
	pdflatex jss5097.tex
	pdflatex jss5097.tex
	evince jss5097.pdf&

jss5097.R: jss5097.Rnw
	R -s -e "Stangle('jss5097.Rnw')"

clean:
	rm -f *.aux *.log *.blg *.bbl *.out

