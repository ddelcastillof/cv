.PHONY: build clean check

build:
	latexmk
	cp build/cv.pdf cv.pdf

clean:
	latexmk -C
	rm -f cv.pdf

check: build
	@txt=$$(pdftotext cv.pdf - | tr '\n' ' '); \
	for s in \
	  "Int J Epidemiol" \
	  "An Fac med" \
	  "BMJ Glob Health" \
	  "Am J Epidemiol" \
	  "Endocrinol Metab Clin North Am" \
	  "FORMACI" \
	  "ADICIONAL" \
	  "EXPERIENCIA PROFESIONAL" \
	  "COMPETENCIAS" \
	  "DISTINCIONES" \
	  "EXPERIENCIA DOCENTE" \
	  "CONTRIBUCIONES DE INVESTIGACI" \
	  "COLEGIATURAS Y CERTIFICACIONES" \
	  "AFILIACIONES A ORGANIZACIONES PROFESIONALES" \
	  "Entre los autores: Del Castillo" \
	; do \
	  echo "$$txt" | grep -qF "$$s" || { echo "make check FAIL — falta: $$s"; exit 1; }; \
	done; \
	n=$$(pdftotext cv.pdf - | grep -cE '^\[?[0-9]+[].]'); \
	[ "$$n" -ge 6 ] || { echo "make check FAIL — número de publicaciones $$n < 6"; exit 1; }; \
	echo "make check: todas las verificaciones pasan"
