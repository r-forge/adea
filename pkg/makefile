install:
	rm -v -f adea_*.tar.gz
	R -e "library('roxygen2'); roxygenize('adea')"
	R CMD build adea
	R CMD INSTALL adea_*.tar.gz
check:
	rm -v -f adea_*.tar.gz
	R -e "library('roxygen2'); roxygenize('adea')"
	R CMD build adea
	R CMD check --as-cran adea_*.tar.gz
