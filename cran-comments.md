This is a new package

## Test environments
* local OS X install, R 3.6.0
* win-builder (release)

## R CMD check results
There were no ERRORs or WARNINGs. 

This package suggests & can be enhanced by the {gt} package, which is only
available at rstudio/gt on github. This raises a NOTE. The Description field
tells users where they can download {gt}.

Following Martina Schmirl's suggestions:

* replaced dontrun with donttest
* examples no longer write to file (through the filename argument)

Following Swetlana Herbrandt's suggestions:

* undirected quotation marks only in Description
* software names in single quotes
* all user-facing functions have examples ('extract' and 'modelsummary')
* the examples which write to file are enclosed in a \dontrun{}
* no function writes to file by default
* unexecutable code has been fixed
* missing value and arguments tags were added to Rd-files
* use find.package instead of installed.packages
