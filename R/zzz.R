.onAttach <- function(libname, pkgname){
  msg <- insight::format_message(
"Version 2.0.0 of `modelsummary`, to be released soon, will introduce a breaking change: The default table-drawing package will be `tinytable` instead of `kableExtra`. All currently supported table-drawing packages will continue to be supported for the foreseeable future, including `kableExtra`, `gt`, `huxtable`, `flextable, and `DT`.",
"",
"You can always call the `config_modelsummary()` function to change the default table-drawing package in persistent fashion. To try `tinytable` now:",
"",
"config_modelsummary(factory_default = 'tinytable')",
"",
"To set the default back to `kableExtra`:",
"",
"config_modelsummary(factory_default = 'kableExtra')"
)
  packageStartupMessage(msg)
}
