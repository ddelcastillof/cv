# Academic CV

Dynamic academic CV built with Quarto and R. Automatically pulls publications from Google Sheets and citation metrics from Google Scholar. The bib folder containt BibTeX files from publications imported with [Paperpile](https://paperpile.com/).

## Build

```bash
quarto render cv-ddcf.qmd
```

## Requirements

- Quarto
- R with packages: `pander`, `stringr`, `dplyr`, `googlesheets4`, `lubridate`, `kableExtra`, `xml2`, `rvest`, `httr`
- XeLaTeX

## Features

- Publications from Google Sheets
- Live citation metrics from Google Scholar
- Professional LaTeX formatting
- Version controlled
