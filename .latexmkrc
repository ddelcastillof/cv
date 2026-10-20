# .latexmkrc — build cv.tex with XeLaTeX + biber, aux/output in build/
$pdf_mode = 5;              # xelatex
$out_dir  = 'build';
@default_files = ('cv.tex');
$bibtex_use = 2;            # run biber as needed; latexmk -C removes .bbl

# TeX's default errorstopmode prompts on stdin when it hits an error. Editor- and
# CI-driven builds (LaTeX Workshop pipes stdin) never answer that prompt, so
# xelatex blocks forever, holds the build lock, and leaves a half-written .aux
# that breaks the next run. nonstopmode makes errors exit non-zero instead.
$xelatex = 'xelatex -interaction=nonstopmode %O %S';
