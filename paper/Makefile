
paper.pdf: paper.tex
	pdflatex paper.tex
	#bibtex paper
	#pdflatex paper.tex
	#pdflatex paper.tex
	xdg-open paper.pdf&

clean:
	rm -f *.aux *.log *.blg *.bbl

