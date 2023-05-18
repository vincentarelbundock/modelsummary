testall:
	Rscript -e "tinytest::build_install_test(ncpu = 8)"

testone:
	Rscript -e "pkgload::load_all();tinytest::run_test_file('$(testfile)')"

documentplain:
	Rscript -e "devtools::document()"

documentrich:
	Rscript -e "Sys.setenv('pkgdown' = 'true');devtools::document()"

check: documentplain
	Rscript -e "devtools::check()"

install: documentplain
	Rscript -e "devtools::install()"

buildsite: documentrich
	Rscript -e "pkgdown::build_site()"

deploysite: documentrich
	Rscript -e "pkgdown::deploy_to_branch()"

buildpdf: documentplain
	R CMD Rd2pdf .
