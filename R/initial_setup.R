#' Set up the environment
#'
#' @description Installs the \code{bibtex} package from Github and installs the \code{tinytex} utility
#' @return NULL
#' @export
initial_setup <- function(install_tinytex = FALSE){
  
  devtools::install_github("ropensci/bibtex")
  
  if(install_tinytex) {
  
  tinytex::install_tinytex()
    
  }
  
}