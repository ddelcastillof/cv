.PHONY: build clean

build:
	Rscript preprocess.R
	pandoc build/cv.md \
		--pdf-engine=xelatex \
		--template=templates/cv-template.tex \
		--lua-filter=filters/bold-author.lua \
		-o cv.pdf

clean:
	rm -f build/cv.md cv.pdf
