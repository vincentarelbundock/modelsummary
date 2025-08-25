.PHONY: help testall testone document documentrich check install buildsite deploysite buildpdf

help:  ## Display this help screen
	@echo -e "\033[1mAvailable commands:\033[0m\n"
	@grep -E '^[a-z.A-Z_-]+:.*?## .*$$' $(MAKEFILE_LIST) | awk 'BEGIN {FS = ":.*?## "}; {printf "  \033[36m%-18s\033[0m %s\n", $$1, $$2}' | sort

runnersup: ## hack local files to run tests
	awk '!/tinytest/' .Rbuildignore > temp && mv temp .Rbuildignore
	awk '/^run <- FALSE/{print "run <- TRUE"; next} 1' tests/tinytest.R > temp && mv temp tests/tinytest.R

runnersdown: ## unhack local files to not run tests
	git restore .Rbuildignore
	git restore tests/tinytest.R

testone: ## make testone testfile="inst/tinytest/test-aaa-warn_once.R"
	Rscript -e "pkgload::load_all();tinytest::run_test_file('$(testfile)')"

test: install runnersup ## Build and test in parallel with 8 cores
	Rscript -e "tinytest::build_install_test(ncpu = 10)"
	$(MAKE) runnersdown

document: ## devtools::document()
	quarto render NEWS.qmd
	Rscript -e "devtools::document()"

check: document ## devtools::check()
	Rscript -e "devtools::check()"

install: document ## devtools::install()
	# R CMD INSTALL .
	Rscript -e "devtools::install(dependencies = FALSE)"

deps: ## install dependencies
	Rscript -e "devtools::install(dependencies = TRUE)"
	
deploy: ## pkgdown::deploy_to_branch()
	Rscript -e "pkgdown::deploy_to_branch()"

website: install ## render vignettes and website
	rm -rf docs
	Rscript -e "altdoc::render_docs(verbose = TRUE)"
	mkdir -p docs/man/figures
	cp man/figures/* docs/man/figures/
	git restore docs/CNAME

buildpdf: document ## document + R CMD Rd2pdf .
	R CMD Rd2pdf .
