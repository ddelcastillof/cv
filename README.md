# Academic CV

Academic CV built directly with XeLaTeX and biber via latexmk. Content is stored in version-controlled LaTeX files and a Paperpile-synced BibTeX file — no Google Sheets or internet connection required to build.

## Build

Requires TeX Live (XeLaTeX, biber, biblatex-vancouver, fontspec,
fontawesome5) and poppler (`pdftotext`) for `make check`.

    make build   # latexmk: XeLaTeX + biber → build/cv.pdf → cv.pdf
    make check   # build + smoke-test the PDF text
    make clean   # remove build/ artifacts and cv.pdf

Content: `cv.tex` (preamble) + `content/*.tex` (one file per section).
References: `bib/references.bib` is synced by Paperpile — never edit by
hand. To add a publication: add a `\cvpub{<category>}{<citekey>}` line in
`content/publications.tex`.

The previous R + Pandoc pipeline lives on branch `legacy/yaml+makefile`.

## Structure

```
cv.tex          # preamble: packages, bibliography setup, section order
content/        # one .tex file per CV section, \input by cv.tex
bib/            # references.bib synced by Paperpile (do not edit manually)
.latexmkrc      # latexmk config: XeLaTeX + biber, output to build/
Makefile        # build / check / clean targets
build/          # generated files (gitignored)
```

## Adding Publications

Paperpile syncs `bib/references.bib` automatically. To make a new entry
appear in the CV, add a `\cvpub{<category>}{<citekey>}` line to
`content/publications.tex` (category is one of the `\DeclareBibliographyCategory`
names declared in `cv.tex`), then `make build`.

## License

[![License: GPL v3](https://img.shields.io/badge/License-GPLv3-blue.svg)](https://www.gnu.org/licenses/gpl-3.0)