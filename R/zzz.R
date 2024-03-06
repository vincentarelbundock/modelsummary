.onAttach <- function(libname, pkgname){
  msg <- insight::format_message(
"`modelsummary` 2.0.0 uses `tinytable` as its default table-drawing package. Learn more at:",
"",
"  https://vincentarelbundock.github.io/tinytable/",
"",
"You can revert to `kableExtra` for one session:",
"",
"  options(modelsummary_factory_default = 'kableExtra')",
"",
"Or change the backend persistently:",
"",
"  config_modelsummary(factory_default = 'gt')",
""
)
  packageStartupMessage(msg)
}
