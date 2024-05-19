.onAttach <- function(libname, pkgname){
  msg <- insight::format_message(
"`modelsummary` 2.0.0 now uses `tinytable` as its default table-drawing backend. Learn more at: https://vincentarelbundock.github.io/tinytable/",
"",
"Revert to `kableExtra` for one session:",
"",
"  options(modelsummary_factory_default = 'kableExtra')",
"  options(modelsummary_factory_latex = 'kableExtra')",
"  options(modelsummary_factory_html = 'kableExtra')",
"",
"Silence this message forever:",
"",
"  config_modelsummary(startup_message = FALSE)",
indent = ""
)
  if (isTRUE(config_get("startup_message"))) {
    packageStartupMessage(msg)
  }
}
