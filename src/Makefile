SLATEX = '/Applications/Racket v7.3/bin/pdf-slatex'

book:
	${SLATEX} book.tex
	${SLATEX} book.tex

exercises: book
	${SLATEX} exercises.tex

clean:
	rm *.aux *.log .Z*.tex book.log book.explore book.toc book.out exercises-generated.tex