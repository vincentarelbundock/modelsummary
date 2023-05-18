testall:
	Rscript -e "tinytest::build_install_test(ncpu = 8)"

testone:
	Rscript -e "pkgload::load_all();tinytest::run_test_file('$(testfile)')"

document:
	Rscript -e "Sys.setenv('pkgdown' = 'true');options('modelsummary_format_numeric_html' = 'plain');devtools::document()"

check:
	Rscript -e "devtools::check()"

install:
	Rscript -e "devtools::install()"

build:
	make document
	Rscript -e "pkgdown::build_site()"

deploy:
	make document
	Rscript -e "pkgdown::deploy_to_branch()"
