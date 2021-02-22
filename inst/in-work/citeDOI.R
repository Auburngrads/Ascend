#' Generate a bibTeX entry from a DOI
#'
#' @param doi 
#' @param rootURL 
#'
#' @return
#' @export
#'
#' @details 
#' References the web utility doi2bib at www.doi2bib.org and webscrapes the resulting BibTeX entry
citeDOI <- function(doi = "10.1038/sj.ejcn.1601827", 
                    rootURL = "https://www.doi2bib.org/bib"){
  
  isbn <- gsub('-', '', isbn)
  
  url <- paste(c(rootURL,doi), collapse = '/')
  
  doc <- RCurl::getURL(url)
  
  html <- htmlTreeParse(doc, useInternalNodes = TRUE)
  
  #html <- getNodeSet(doc, useInternal = TRUE)
  
  bib = xpathSApply(html, '//textarea', fun = XML::xmlValue)
  
  Start   <- regexpr('\\{', bib)[1]+1
  
}