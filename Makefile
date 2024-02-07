.PHONY: help testall testone document documentrich check install buildsite deploysite buildpdf

help:  ## Display this help screen
	@echo -e "\033[1mAvailable commands:\033[0m\n"
	@grep -E '^[a-z.A-Z_-]+:.*?## .*$$' $(MAKEFILE_LIST) | awk 'BEGIN {FS = ":.*?## "}; {printf "  \033[36m%-18s\033[0m %s\n", $$1, $$2}' | sort

testall: ## tinytest::build_install_test()
	Rscript -e "tinytest::build_install_test(ncpu = 8)"

testone: ## make testone testfile="inst/tinytest/test-aaa-warn_once.R"
	Rscript -e "pkgload::load_all();tinytest::run_test_file('$(testfile)')"

document: ## devtools::document()
	Rscript -e "devtools::document()"

check: document ## devtools::check()
	Rscript -e "devtools::check()"

install: document ## devtools::install()
	Rscript -e "devtools::install(dependencies = TRUE)"

deploy: ## pkgdown::deploy_to_branch()
	Rscript -e "pkgdown::deploy_to_branch()"

document: install ## devtools::document()
	Rscript -e "devtools::document()"

website: document ## render vignettes and website
	Rscript -e "altdoc::render_docs(verbose = TRUE)"
	rm -rf _quarto

buildpdf: document ## document + R CMD Rd2pdf .
	R CMD Rd2pdf .
