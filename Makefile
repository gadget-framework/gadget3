PACKAGE=$(shell awk '/^Package: / { print $$2 }' DESCRIPTION)
VERSION=$(shell awk '/^Version: / { print $$2 }' DESCRIPTION)
TARBALL=$(PACKAGE)_$(VERSION).tar.gz

all:
	make test
	make test G3_TEST_TMB=1
	make inttest G3_TEST_TMB=1
	make check-as-cran

install:
	R CMD INSTALL --install-tests --html --example .

# Some things aren't installed by "make install", vignettes for example.
# This is slower, but more accurate.
full-install: build
	R CMD INSTALL --install-tests --html --example "$(TARBALL)"

build:
	R CMD build .

check: build
	R CMD check "$(TARBALL)"

check-as-cran: build
	R CMD check --as-cran "$(TARBALL)"

wincheck: build
	# See https://win-builder.r-project.org/ for more information
	curl -# -T "$(TARBALL)" ftp://win-builder.r-project.org/R-devel/
	# https://www.mail-archive.com/r-package-devel@r-project.org/msg05040.html
	curl -s ftp://win-builder.r-project.org/R-devel/ | sed -E 's/(AM|PM)/\t\1/g' | sort -k 1 -k 3 -k 2

examples: install
	Rscript -e 'devtools::run_examples(run_donttest = TRUE, run_dontrun = FALSE, document = FALSE)'

vignettes: install
	Rscript -e 'tools::buildVignettes(dir=".")'
	Rscript -e 'for (f in Sys.glob("./vignettes/articles/*.Rmd")) tools::buildVignette(f, dir="vignettes/")'

serve-docs:
	[ -d docs ] && rm -r docs || true
	Rscript -e "options(pkgdown.internet = FALSE) ; pkgdown::build_site(override=list(url='http://localhost:8000/')) ; servr::httd(dir='docs', host='0.0.0.0', port='8000')"

test: install
	parallel -j 8 --halt now,fail=1 Rscript ::: tests/test*.R

inttest: install
	parallel -j 8 --halt now,fail=1 Rscript ::: inttest/*/run.R
	make vignettes G3_TEST_TMB=1

coverage:
	# NB: TMB transpiling and covr clash badly
	G3_TEST_TMB="" R --vanilla -e 'covr::package_coverage(type = "all", line_exclusions = list("R/run_tmb.R"))'

release: release-description release-news
	git commit -m "Release version $(NEW_VERSION)" $(wildcard DESCRIPTION ChangeLog NEWS.md)
	git tag -am "Release version $(NEW_VERSION)" v$(NEW_VERSION)
	#
	R CMD build .
	#
	sed -i 's/^Version: .*/Version: '"$(NEW_VERSION)-999"'/' DESCRIPTION
	mv NEWS.md NEWS.md.o
	/bin/echo -e "# $(PACKAGE) $(NEW_VERSION)-999:\n" > NEWS.md
	cat NEWS.md.o >> NEWS.md
	rm NEWS.md.o
	git commit -m "Development version $(NEW_VERSION)-999" $(wildcard DESCRIPTION ChangeLog NEWS.md)

release-description:
	[ -n "$(NEW_VERSION)" ]  # NEW_VERSION variable should be set
	sed -i 's/^Version: .*/Version: $(NEW_VERSION)/' DESCRIPTION
	sed -i "s/^Date: .*/Date: $$(date +%Y-%m-%d)/" DESCRIPTION
	sed -i 's/^Depends: R .*/Depends: R (>= $(shell curl -s https://api.r-hub.io/rversions/r-oldrel/3 | grep -oiE '"version":"[0-9.]+"' | grep -oE '[0-9]+\.[0-9]+\.')0)/' DESCRIPTION

release-changelog:
	[ -n "$(NEW_VERSION)" ]  # NEW_VERSION variable should be set
	mv ChangeLog ChangeLog.o || touch ChangeLog.o
	echo "$$(date +%Y-%m-%d) $$(git config user.name)  <$$(git config user.email)>" > ChangeLog
	echo "" >> ChangeLog
	echo "    Version $(NEW_VERSION)" >> ChangeLog
	echo "" >> ChangeLog
	git log --pretty=format:'    * %d %s (%h)' $(shell git describe --tags --abbrev=0)..HEAD -- ./R >> ChangeLog
	echo "" >> ChangeLog
	echo "" >> ChangeLog
	cat ChangeLog.o >> ChangeLog
	rm ChangeLog.o

release-news:
	[ -n "$(NEW_VERSION)" ]  # NEW_VERSION variable should be set
	# Remove any -999 header
	mv NEWS.md NEWS.md.o
	head -1 NEWS.md.o | grep -qE '^\# $(PACKAGE).*-999:?$$' || head -2 NEWS.md.o > NEWS.md
	tail -n +3 NEWS.md.o >> NEWS.md
	rm NEWS.md.o
	# Any news? Add new header if so
	mv NEWS.md NEWS.md.o
	head -1 NEWS.md.o | grep -qE '^\# $(PACKAGE) ' || /bin/echo -e "# $(PACKAGE) $(NEW_VERSION):\n" > NEWS.md
	cat NEWS.md.o >> NEWS.md
	rm NEWS.md.o

.PHONY: all install full-install build check check-as-cran wincheck examples vignettes serve-docs test inttest coverage release release-description release-changelog release-news
