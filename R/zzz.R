.onLoad <- function(libname, pkgname) {
    if (!'gt' %in% rownames(installed.packages())) {
        msg <- 'To take full advantage of `modelsummary`, you should install the `gt` package. Currently, `gt` is not available on CRAN.  You can install it from the Github website using the `devtools`, `remotes`, or `pak` packages:\n\nlibrary(devtools)\ninstall_github("rstudio/gt")\n\nlibrary(remotes)\ninstall_github("rstudio/gt")\n\nlibrary(pak)\npkg_install("rstudio/gt")'
        message(msg)
    }
}
