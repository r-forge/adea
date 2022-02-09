package := adea
version := 1.3.1
R := $(wildcard R/*.R)
Rmd := $(wildcard vignettes/*.Rmd)
html := $(patsubst vignettes/%.Rmd,inst/doc/%.html,$(Rmd))

$(package).Rcheck: $(package)_$(version).tar.gz
	R CMD check --as-cran $(package)_$(version).tar.gz
$(package)_$(version).tar.gz: $(R) $(Rmd) $(html) ChangeLog DESCRIPTION inst/po/es/LC_MESSAGES/R-$(package).mo po/R-$(package)-es.po po/R-$(package).pot 
	R -e "library('roxygen2'); roxygenize('.')"
	R CMD build --no-build-vignettes .
	R CMD INSTALL $(package)_$(version).tar.gz
inst/po/es/LC_MESSAGES/R-$(package).mo: po/R-$(package)-es.po
	msgfmt -c -o inst/po/es/LC_MESSAGES/R-$(package).mo po/R-$(package)-es.po
po/R-$(package)-es.po: $(R) po/R-$(package).pot
	poedit po/R-$(package)-es.po
po/R-$(package).pot: $(R)
	# This target builds the pot file and update any R-$(package)-lang.po file under po dir
	R -e "library('tools'); update_pkg_po('.')"
	touch po/R-$(package).pot
inst/doc/%.html: $(R) $(Rmd)
	make vignettes
vignettes: $(R) $(Rmd)
	R -e "devtools::build_vignettes('.', clean = FALSE, install = FALSE)"
	mv --force doc/* inst/doc/
	rmdir doc
