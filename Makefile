.PHONY: help test testone watch document check install deps deploy website buildpdf

help:  ## Display this help screen
	@echo -e "\033[1mAvailable commands:\033[0m\n"
	@grep -E '^[a-z.A-Z_-]+:.*?## .*$$' $(MAKEFILE_LIST) | awk 'BEGIN {FS = ":.*?## "}; {printf "  \033[36m%-18s\033[0m %s\n", $$1, $$2}' | sort

test: ## Run the full tinytest suite via scrutin
	scrutin -r plain

testone: ## make testone test=test-aaa (stem or full path, .R stripped)
	scrutin -r plain --set 'filter.include=["$(basename $(notdir $(test)))*"]'

watch: ## Launch the scrutin TUI in watch mode
	scrutin

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
