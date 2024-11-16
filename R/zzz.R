.onAttach <- function(libname, pkgname) {
  if (isTRUE(config_get("startup_message"))) {
    # packageStartupMessage(msg)
  }
}
