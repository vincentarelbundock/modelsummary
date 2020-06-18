#' Function from `tables` modified to support tibbles. Remove if this gets fixed upstream.
#' Licence of the original: GPL-2 or later
#'
#' @export
#' @keywords internal
All <- function(df, numeric=TRUE, character=FALSE, logical=FALSE, factor=FALSE,
		    complex=FALSE, raw=FALSE, other=FALSE,
		    texify=getOption("tables.texify", FALSE)) {

    if (is.character(numeric)) numeric <- get(numeric, mode="function", 
    	envir=parent.frame())
    if (is.character(character)) character <- get(character, mode="function",
    	envir=parent.frame())
    if (is.character(logical)) logical <- get(logical, mode="function",
    	envir=parent.frame())
    if (is.character(factor)) factor <- get(factor, mode="function",
    	envir=parent.frame())
    if (is.character(complex)) complex <- get(complex, mode="function",
    	envir=parent.frame())
    if (is.character(complex)) complex <- get(complex, mode="function",
    	envir=parent.frame())
    if (is.character(raw)) raw <- get(raw, mode="function", 
        envir=parent.frame())
    if (is.character(other)) other <- get(other, mode="function", 
        envir=parent.frame())
        
    names <- colnames(df)
    if (texify)
    	names <- Hmisc::latexTranslate(names)
    
    f <- NULL
    for (i in seq_along(names)) {
        value <- df[[i]]
        if (is.numeric(value)) {
            if (is.function(numeric))
            	value <- numeric(value)
            else if (!isTRUE(numeric))
            	next
        } else if (is.character(value)) {
            if (is.function(character))
            	value <- character(value)
            else if (!isTRUE(character))
            	next
        }  else if (is.logical(value)) {
            if (is.function(logical)) 
            	value <- logical(value)
            else if (!isTRUE(logical))
            	next
        } else if (is.factor(value)) {
            if (is.function(factor))
            	value <- factor(value)
            else if (!isTRUE(factor))
            	next
        } else if (is.complex(value)) {
            if (is.function(complex)) 
            	value <- complex(value)
            else if (!isTRUE(complex))
            	next
        } else if (is.raw(value)) {
	    if (is.function(raw)) 
	     	value <- raw(value)
	    else if (!isTRUE(raw))
	    	next
        } else {
            if (is.function(other))
            	value <- other(value)
            else if (!isTRUE(other))
            	next
        }
        
        f1 <- call("*", call("Heading", as.name(names[i])),
                   value)
        if (is.null(f))
            f <- f1
        else
            f <- call("+", f, f1)
    }
    f
}
