# CV Académico (versión en español)

Versión en español del CV, generada con LaTeX puro y compilada con `latexmk`
(XeLaTeX + biber, estilo `biblatex-vancouver`). Es el equivalente en español
de la rama `main`; ambas comparten la misma arquitectura y difieren solo en el
idioma del contenido.

## Compilación

Requiere TeX Live (XeLaTeX, biber, biblatex-vancouver, babel-spanish,
fontspec, fontawesome5) y poppler (`pdftotext`) para `make check`.

    make build   # latexmk: XeLaTeX + biber → build/cv.pdf → cv.pdf
    make check   # compila y verifica el texto del PDF
    make clean   # elimina los artefactos de build/ y cv.pdf

El contenido vive en `cv.tex` (preámbulo) + `content/*.tex` (una sección por
archivo). `bib/references.bib` lo sincroniza Paperpile — nunca editar a mano.

## Estructura

```
cv.tex             # preámbulo + lista de \input (idioma: babel spanish)
content/*.tex      # una sección por archivo
bib/references.bib # sincronizado por Paperpile (no editar a mano)
.latexmkrc         # $pdf_mode=5 (xelatex), salida en build/
Makefile           # build / check / clean
```

## Agregar publicaciones

Paperpile sincroniza `bib/references.bib` automáticamente. Para que una entrada
nueva aparezca en el CV, agregue una línea `\cvpub{<categoría>}{<clave>}` en
`content/publications.tex`.

La versión en inglés vive en la rama `main`; el pipeline anterior en Quarto se
conserva en la rama `legacy/spanish-quarto`.

## Licencia

[![License: GPL v3](https://img.shields.io/badge/License-GPLv3-blue.svg)](https://www.gnu.org/licenses/gpl-3.0)
