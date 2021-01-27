#' Create a BibEntry from an international standard book number (ISBN)
#' 
#' @importFrom RefManageR BibEntry
#' @importFrom RCurl getURL
#' @importFrom XML htmlTreeParse xpathSApply xmlValue
#'
#' @param isbn An international standard book number (ISBN-13 or ISBN-10)
#' @param preview If \code{TRUE}, the bibentry is printed to the screen via \code{cat}
#' @param bibtype The type of bibliographic element. (see \code{details})
#' @param key The bibtex key for the bibentry
#' @param author The author for the bibentry
#' @param title The title for the bibentry
#' @param publisher The publisher for the bibentry
#' @param year The year for the bibentry
#' @param address The publisher's address for the bibentry
#' @param style The output style of the bibentry (defaults to 'bibtex')
#' @param ... extra stuff
#'
#' @description This function uses ottobib.com to generate a bibtex entry for books using the 
#' ISBN.  The result is then formatted into a bibentry-class object using \code{RegManageR::BibEntry}and 
#' @source http://www.ottobib.com/
#' @details \code{isbn} may contain dashes
#' @export
citeISBN <- function(isbn = NULL, preview = TRUE, bibtype = NULL, key = NULL, author = NULL,
                     title = NULL, publisher = NULL, year = NULL, address = NULL,
                     style = 'bibtex',...) {
  
  isbn <- gsub('-', '', isbn)
  
  url <- paste(c('http://www.ottobib.com/isbn',isbn,style), collapse = '/')
  
  doc <- getURL(url)
  
  html <- htmlTreeParse(doc, useInternalNodes = TRUE)
  
  #html <- getNodeSet(doc, useInternal = TRUE)
  
  bib = xpathSApply(html, '//textarea', fun = XML::xmlValue)
  
  Start   <- regexpr('\\{', bib)[1]+1
  
  if(preview) {
    cat('\nBibEntry recorded as:\n\n')
    cat(gsub('\\\n',paste(c('\\\n',rep(' ', Start-2)),collapse = ''), bib))
    cat('\n\n')
  }

  if(is.null(bibtype)) bibtype <- tolower(substring(bib,2,regexpr('\\{', bib)[1]-1))
  
  bib <- noquote(unlist(strsplit(bib, split = ',\\\n ')))
    
  #if(!is.null(key)) bib <- gsub(substring(bib,Start,Finis), key, bib)

  if( is.null(key)) {
  
  kStart <- regexpr('\\{', bib[1])[1]+1  
  key <- substring(bib[1],kStart)
  
  }
  
  if(is.null(author) & !is.na(pmatch('author',bib))) {
    
  aStart <- regexpr('\\{', bib[pmatch('author',bib)])[1]+1
  aFinis <- regexpr('\\}', bib[pmatch('author',bib)])[1]-1
  author <- substring(bib[pmatch('author',bib)],aStart,aFinis)  
    
  }
  
  if(is.null(title) & !is.na(pmatch('title',bib))) {
    
  tStart <- regexpr('\\{', bib[pmatch('title',bib)])[1]+1
  tFinis <- regexpr('\\}', bib[pmatch('title',bib)])[1]-1
  title  <- substring(bib[pmatch('title',bib)],tStart,tFinis)  
    
  }
  
  if(is.null(publisher) & !is.na(pmatch('publisher',bib))) {
    
  pStart  <- regexpr('\\{', bib[pmatch('publisher',bib)])[1]+1
  pFinis  <- regexpr('\\}', bib[pmatch('publisher',bib)])[1]-1
  publish <- substring(bib[pmatch('publisher',bib)],pStart,pFinis)  
    
  }
  
  if(is.null(year) & !is.na(pmatch('year',bib))) {
    
  yStart <- regexpr('\\{', bib[pmatch('year',bib)])[1]+1
  yFinis <- regexpr('\\}', bib[pmatch('year',bib)])[1]-1
  year   <- substring(bib[pmatch('year',bib)],yStart,yFinis)  
    
  }
  
  if(is.null(address) & !is.na(pmatch('address',bib))) {
    
  adStart <- regexpr('\\{', bib[pmatch('address',bib)])[1]+1
  adFinis <- regexpr('\\}', bib[pmatch('address',bib)])[1]-1
  address <- substring(bib[pmatch('address',bib)],adStart,adFinis)  
    
  }
  
  entry <- RefManageR::BibEntry(bibtype = bibtype,
                       key = key,
                       author = author,
                       title = title,
                       address = address,
                       year = year,
                       publisher = publish,
                       isbn = isbn)
  # bib <- gsub('\\\n',paste(c('\\\n',rep(' ', Start-2)),collapse = ''), bib)
  # 
  # bib1 <- readLines(bib.file)
  # 
  # writeLines(c(bib1,bib), con = bib.file)
  #
  # return(knitcitations::citet(bib))
  return(entry)
}
