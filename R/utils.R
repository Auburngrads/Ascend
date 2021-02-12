#' Helper function to find a file for a named template
#' 
#' @param template A named template folder
#' @param file the name of the file to search for within the \code{template} folder
#' @export
find_file <- function(template, file) {
  template <- system.file("rmarkdown", "templates", template, file,
                          package = "Ascend")
  if (template == "") {
    stop("Couldn't find template file ", template, "/", file, call. = FALSE)
  }

  template
}

#' Helper function to find a resource for a named template
#' 
#' @param template A named template folder
#' @param file Name of the  resource file to search for within the \code{template} folder
#' 
#' @importFrom utils getFromNamespace
#' @export

find_resource <- function(template, file) {
  find_file(template, file.path("resources", file))
}

knitr_fun <- function(name) utils::getFromNamespace(name, 'knitr')

output_asis <- knitr_fun('output_asis')

#' Render a pandoc template.
#'
#' This is a hacky way to access the pandoc templating engine.
#'
#' @param metadata A named list containing metadata to pass to template.
#' @param template Path to a pandoc template.
#' @param output Path to save output.
#' @param verbose Should extensive details be provided?
#' @return (Invisibly) The path of the generate file.

#' @importFrom rmarkdown pandoc_convert
#' @importFrom yaml as.yaml
#' @export
template_pandoc <- function(metadata, template, output, verbose = FALSE) {
  tmp <- tempfile(fileext = ".md")
  on.exit(unlink(tmp))

  cat("---\n", file = tmp)
  cat(yaml::as.yaml(metadata), file = tmp, append = TRUE)
  cat("---\n", file = tmp, append = TRUE)
  cat("\n", file = tmp, append = TRUE)

  rmarkdown::pandoc_convert(tmp, 
                            "markdown", 
                            output = output,
                            options = paste0("--template=", template), 
                            verbose = verbose)

  invisible(output)
}


#' Inherits properties from an existing pdf document
#' @description Calls rmarkdown::pdf_document and marks the return value as inheriting pdf_document
#' @importFrom bookdown pdf_document2
#' @param ... arguments sent to \code{rmarkdown::pdf_document}
inherit_pdf_document2 <- function(...) {
  fmt <- bookdown::pdf_document2(...)
  fmt$inherits <- "pdf_document2"
  fmt
}


#' Create a custom format derived from pdf_document
#' @description Helper function to create a custom format derived from pdf_document that includes a custom LaTeX template and custom CSL definition
#' 
#' @param ... arguments sent to \code{inherit_pdf_document} 
#' @param format Named format folder
#' @param template Named template folder
#' @param csl Named style file for a given \code{template} 
#' 
#' @importFrom rmarkdown pandoc_path_arg
#' 
#' @export
pdf_document_format <- function(..., format, template, csl = NULL) {

  # base format
  fmt <- inherit_pdf_document2(..., template = find_resource(format, template))

  # add csl to pandoc_args
  #' @importFrom rmarkdown pandoc_path_arg
  if(!is.null(csl)) { fmt$pandoc$args <- c(fmt$pandoc$args,
                       "--csl",
                       rmarkdown::pandoc_path_arg(find_resource(format, csl))) }

  # return format
  fmt
}


#' Inherits properties from an existing html document
#' @description Calls rmarkdown::html_document and marks the return value as inheriting html_document
#' @importFrom rmarkdown html_document
#' @param ... arguments sent to \code{rmarkdown::html_document}
inherit_html_document <- function(...) {
  fmt <- rmarkdown::html_document(...)
  fmt$inherits <- "html_document"
  fmt
}
