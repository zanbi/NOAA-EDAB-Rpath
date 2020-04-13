#'@useDynLib Rpath
#'@export
runRpathShiny <- function(lab) {
  # locate all the shiny app examples that exist
  validLabs <- list.files(system.file("labs", package = "Rpath"))
  validLabsMsg <-paste0("Valid Labs: '",paste(validLabs, collapse = "', '"),"'")

  # if an invalid example is given, throw an error
  if (missing(lab) || !nzchar(lab) ||
      !lab %in% validLabs) {stop(validLabsMsg, call. = F)}

  # find and launch the app
  appDir <- system.file("labs", lab, package = "Rpath")
  shiny::runApp(appDir, display.mode = "normal")
}
