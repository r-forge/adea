install:
	rm -v -f adea_*.tar.gz
	R -e "library('roxygen2'); roxygenize('pkg')"
	R CMD build pkg
	R CMD INSTALL adea_*.tar.gz
check:
	rm -v -f adea_*.tar.gz
	R -e "library('roxygen2'); roxygenize('pkg')"
	R CMD build pkg
	R CMD check --as-cran adea_*.tar.gz
