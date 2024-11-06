package := adea
version := 1.5.1
R := $(wildcard R/*.R)

RmdVignettes := $(wildcard vignettes/*.Rmd)
RVignettes := $(patsubst inst/doc/%.Rmd,inst/doc/%.R,$(RmdVignettes))

RmdDocs := $(wildcard inst/doc/*.Rmd)
HtmlDocs := $(patsubst inst/doc/%.Rmd,inst/doc/%.html,$(RmdDocs))

TestsFiles := $(wildcard tests/testthat/*.R)
TestsLog := $(patsubst tests/testthat/%.R,%.testthat.log,$(TestsFiles))

## General targets
install: ~/R_LIBS/$(package)
	@echo Package $(package)_$(version) installed
check: $(package)_$(version).tar.gz
	@R CMD check --as-cran $<
check_:
	R -e "library('roxygen2'); roxygenize('.')"
	R CMD build .
	R CMD check $(package)_$(version).tar.gz
dev: dev.log
	make dev.log
tests: $(TestsLog)
	@echo All done

# Partial targets
~/R_LIBS/$(package): $(package)_$(version).tar.gz
	R CMD INSTALL $(package)_$(version).tar.gz
$(package).Rcheck: $(package)_$(version).tar.gz
	R CMD check --as-cran $(package)_$(version).tar.gz
$(package)_$(version).tar.gz: $(R) $(RVignettes) $(HtmlDocs) ChangeLog DESCRIPTION inst/po/es/LC_MESSAGES/R-$(package).mo
	R -e "library('roxygen2'); roxygenize('.')"
	R CMD build .
dev.log: makefile ~/R_LIBS/$(package)
	date > dev.log
	echo "library('adea')" > .Rprofile
	R --no-save --interactive
	rm .Rprofile
	reset
inst/po/es/LC_MESSAGES/R-$(package).mo: po/R-$(package)-es.po
	msgfmt -c -o inst/po/es/LC_MESSAGES/R-$(package).mo po/R-$(package)-es.po
po/R-$(package)-es.po: $(R) po/R-$(package).pot
	poedit po/R-$(package)-es.po
po/R-$(package).pot: $(R)
	# This target builds the pot file and update any R-$(package)-lang.po file under po dir
	R -e "library('tools'); update_pkg_po('.')"
	touch po/R-$(package).pot
inst/doc/%.html: inst/doc/%.Rmd $(R)
	R -e "rmarkdown::render('$<')"
inst/doc/index.R: $(R) $(RmdVignettes)
	R -e "devtools::build_vignettes('.', clean = FALSE, install = FALSE)"
%.testthat.log: tests/testthat/%.R
	Rscript $< | tee $@
