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
	  "EDUCATION" \
	  "ADDITIONAL EDUCATION" \
	  "PROFESSIONAL EXPERIENCE" \
	  "SKILLS" \
	  "ACADEMIC HONOURS AND AWARDS" \
	  "TEACHING EXPERIENCE" \
	  "RESEARCH CONTRIBUTIONS" \
	  "LICENSURE AND CERTIFICATION" \
	  "MEMBERSHIPS IN PROFESSIONAL ORGANIZATIONS" \
	  "Among authors: Del Castillo" \
	  "Open Materials" \
	  "Preregistered+" \
	; do \
	  echo "$$txt" | grep -qF "$$s" || { echo "make check FAIL — missing: $$s"; exit 1; }; \
	done; \
	n=$$(pdftotext cv.pdf - | grep -cE '^\[?[0-9]+[].]'); \
	[ "$$n" -ge 6 ] || { echo "make check FAIL — publication count $$n < 6"; exit 1; }; \
	echo "make check: all assertions pass"
