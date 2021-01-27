#' Create a BibEntry from a DTIC document (Experimental)
#' 
#' @description Generate reference from document stored within the DTIC repository
#' 
#' @importFrom knitcitations greycite
#' @importFrom RCurl getURL
#' @importFrom XML htmlTreeParse xpathSApply xmlValue
#'
#' @param designator An AFIT thesis or dissertation designator number
#' @param access_num The DTIC access number
#' @param ... extra stuff
#'
#' @details This function references the advanced search utility on dtic.mil 
#'          to generate a bibtex.  Results returned from the search are formatted 
#'          as a bibentry-class object using \code{knitcitations::greycite}. 
#'          
#'          Note: the \code{designator} argument may contain dashes.
#' @source http://www.dtic.mil/dtic/
#' @export
citeDTIC <- function(designator = 'AFIT-ENS-DS-13-D-02', access_num = NULL,...) {
  
  url <- paste(c('http://dsearch.dtic.mil/search?client=dticol_frontend',
                 '&output=xml_no_dtd',
                 '&proxystylesheet=dticol_frontend',
                 '&ie=UTF-8',
                 '&oe=UTF-8',
                 '&filter=0',
                 '&tlen=200',
                 '&getfields=*',
                 '&q=',designator,
                 '&num=1',
                 '&site=default_collection',
                 '&as_epq=',
                 '&as_oq=',
                 '&as_eq=',
                 '&as_ft=i',
                 '&as_filetype=',
                 '&as_occt=any',
                 '&as_dt=i',
                 '&as_sitesearch=',
                 '&btnG=Search'), 
               collapse = '')
  
  doc <- RCurl::getURL(url)
  
  html <- XML::htmlTreeParse(doc, useInternalNodes = TRUE)
  
  ada <- XML::xpathSApply(html, '//td[@class="gsa_s"]', fun = XML::xmlValue)
  
  start   <- regexpr(' ', ada)[1]
  ada <- substring(ada, first = 1, last = start-1)
  ada <- paste(c('http://',ada), collapse = '')
  
  entry <- knitcitations::greycite(ada)
  
  return(entry)
}
