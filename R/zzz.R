.onAttach <- function(...) {
  pkg_desc <- utils::packageDescription("rigr")
  packageStartupMessage(paste0(
    "rigr version ", pkg_desc$Version,
    ": ", pkg_desc$Title
  ))
}