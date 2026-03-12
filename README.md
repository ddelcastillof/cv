# Academic CV

Academic CV built with R and Pandoc. Content is stored in version-controlled YAML files and a Paperpile-synced BibTeX file — no Google Sheets or internet connection required to build.

## Build

```bash
make build
```

Runs `preprocess.R` (YAML + BibTeX → `build/cv.md`) then Pandoc + XeLaTeX → `cv.pdf`. Completes in under 20 seconds.

```bash
make clean   # remove build/cv.md and cv.pdf
```

## Requirements

- R with packages: `yaml`, `bib2df`, `stringr`
- Pandoc
- XeLaTeX (with `fontspec`, `fontawesome5`, `longtable`, `titlesec`)

## Structure

```
data/          # one YAML file per CV section (edit here to update content)
bib/           # references.bib synced by Paperpile (do not edit manually)
filters/       # bold-author.lua — bolds author name in publication entries
templates/     # cv-template.tex — Pandoc LaTeX template
tests/         # testthat unit tests for preprocess.R
build/         # generated files (gitignored)
```

## Adding Publications

Paperpile syncs `bib/references.bib` automatically. To make a new entry appear in the CV, add its cite key to the appropriate category in `data/pub_categories.yaml`.
