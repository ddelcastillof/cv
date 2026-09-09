.PHONY: build clean check

# defernumbers=true assigns each entry's label number once and caches it in
# build/cv.aux (\abx@aux@number). biblatex refuses to mint a number for a key
# that has no cached one whenever the .aux already holds any number and
# \nocite{*} is not used (biblatex.sty, \blx@addlabelnumber) — so every newly
# added \cvpub key would render as "0." until the .aux is discarded. Removing
# it here forces a from-scratch numbering pass on every build.
build:
	rm -f build/cv.aux
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
	  "RESEARCH EXPERIENCE" \
	  "FELLOWSHIPS, AWARDS, AND HONOURS" \
	  "TEACHING EXPERIENCE" \
	  "MENTORING EXPERIENCE" \
	  "LICENSURE AND CERTIFICATION" \
	  "RESEARCH CONTRIBUTIONS" \
	  "Peer-Reviewed Publications" \
	  "Conference Abstracts" \
	  "Conference Presentations" \
	  "Among authors: Del Castillo" \
	  "Rapid-fire oral presentation" \
	  "Rapid-fire presentation at:" \
	; do \
	  echo "$$txt" | grep -qF "$$s" || { echo "make check FAIL — missing: $$s"; exit 1; }; \
	done; \
	n=$$(pdftotext cv.pdf - | grep -cE '^\[?[0-9]+[].]'); \
	[ "$$n" -ge 6 ] || { echo "make check FAIL — publication count $$n < 6"; exit 1; }; \
	pdftotext cv.pdf - | grep -qE '^\[?0[].]' \
	  && { echo "make check FAIL — entry numbered 0 (stale build/cv.aux vs defernumbers)"; exit 1; }; \
	echo "make check: all assertions pass"
