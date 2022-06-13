PACKAGE=$(shell awk '/^Package: / { print $$2 }' DESCRIPTION)
VERSION=$(shell awk '/^Version: / { print $$2 }' DESCRIPTION)
TARBALL=$(PACKAGE)_$(VERSION).tar.gz

all: check

install:
	R CMD INSTALL --install-tests --html --example .

# Some things aren't installed by "make install", vignettes for example.
# This is slower, but more accurate.
full-install: build
	R CMD INSTALL --install-tests --html --example "$(TARBALL)"

build:
	R CMD build .

examples: install
	Rscript -e 'devtools::run_examples(run_donttest = FALSE, run_dontrun = FALSE, document = FALSE)'

vignettes: install
	Rscript -e 'tools::buildVignettes(dir=".")'

test: install
	for f in tests/test-*.R; do echo "=== $$f ============="; G3_TEST_TMB="" Rscript $$f || exit 1; done

inttest: install
	for f in tests/test-*.R; do echo "=== $$f ============="; G3_TEST_TMB="y" Rscript $$f || exit 1; done
	for f in inttest/*/run.R; do echo "=== $$f ============="; G3_TEST_TMB="y" Rscript $$f || exit 1; done

check: build
	R CMD check "$(TARBALL)"

check-as-cran: build
	G3_TEST_TMB="" R CMD check --as-cran "$(TARBALL)"

coverage:
	# NB: TMB transpiling and covr clash badly
	G3_TEST_TMB="" R --vanilla -e 'covr::package_coverage(type = "all", line_exclusions = list("R/run_tmb.R"))'

wincheck: build
	# See https://win-builder.r-project.org/ for more information
	curl --no-epsv -# -T "$(TARBALL)" ftp://win-builder.r-project.org/R-devel/

serve-docs:
	[ -d docs ] && rm -r docs || true
	echo 'pkgdown::build_site()' | R --vanilla
	cd docs && python2 -m SimpleHTTPServer

.PHONY: all install build test examples vignettes inttest check check-as-cran coverage wincheck serve-docs
