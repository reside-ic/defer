RSCRIPT = Rscript

test:
	${RSCRIPT} -e 'library(methods); devtools::test()'

roxygen:
	@mkdir -p man
	${RSCRIPT} -e "library(methods); devtools::document()"

install:
	R CMD INSTALL .

build:
	R CMD build .

check:
	_R_CHECK_CRAN_INCOMING_=FALSE make check_all

check_all:
	${RSCRIPT} -e "rcmdcheck::rcmdcheck(args = c('--as-cran', '--no-manual'))"


## This will eventually swap out for devtools::build_vignettes(), but
## in current version it's not working when offline.  For now I'll
## just do the copy manually.
vignettes: vignettes/orderly.Rmd
	${RSCRIPT} -e 'tools::buildVignettes(dir = ".")'
	mkdir -p inst/doc
	cp vignettes/*.html vignettes/*.Rmd inst/doc

tests/testthat/montagu-reports:
	git clone git@github.com:vimc/montagu-reports $@


README.md: README.Rmd
	Rscript -e "options(warnPartialMatchArgs=FALSE); knitr::knit('$<')"
	sed -i.bak 's/[[:space:]]*$$//' README.md
	rm -f $@.bak

pkgdown:
	${RSCRIPT} -e "library(methods); pkgdown::build_site()"

website: pkgdown
	./scripts/update_web.sh

.PHONY: test roxygen install build check check_all vignettes
