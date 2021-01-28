#' Ascend Innovations Whitepaper Template 
#'
#' @description Create an Ascend Innovations whitepaper using 
#'              \code{rmarkdown} and \code{bookdown}
#' 
#' @source Literally copied the format of the Ascend Innovations whitepapers
#'
#' @param ... Arguments to \code{rmarkdown::pdf_document}
#' @param keep_tex Should the raw TeX files be kept?
#' @param md_extensions Extensions passed to the Markdown engine
#' @param fig_caption Should figure captions be enabled?
#' @param toc Should a table of contents be created?
#' @param template The TeX template used
#' @param format The format of the document
#' @param highlight Highlight color template for code chunks
#' @param xtab_caption_placement Placement of captions for tables created using the \code{xtable} package 
#' @param xtab_comment Logical variable whether the \code{xtable} timestamp should be printed
#' @return R Markdown output format to pass to \code{rmarkdown::render}
#'
#' @importFrom rmarkdown pdf_document
#' @importFrom rmarkdown pandoc_highlight_args
#' @importFrom rmarkdown includes
#' @examples
#'
#' \dontrun{
#' rmarkdown::draft("my_whitepaper.Rmd", template = "whitepaper", package = "Ascend")
#' }
#'
#' @seealso \code{\link{style_guide}}
#' 
#' @export
whitepaper <- function(...,
                        keep_tex = FALSE,
                        md_extensions = c("-autolink_bare_uris"),
                        fig_caption = TRUE,
                        toc = TRUE,
                        template = 'wp_template.tex',
                        format = "whitepaper",
                        highlight = 'default',
                        xtab_caption_placement = 'top',
                        xtab_comment = FALSE) {
  
  options("xtable.caption.placement" = xtab_caption_placement)
  options("xtable.table.placement" = 'H')
  options("xtable.comment" = xtab_comment)
  
  thesis <- system.file("rmarkdown",
                        'templates',
                        format,
                        'resources',
                        template,
                        package = "Ascend")
  
  base <- inherit_pdf_document2(...,
                                template = thesis,
                                fig_caption = fig_caption,
                                keep_tex = keep_tex,
                                toc = toc,
                                citation_package = 'natbib',
                                highlight = highlight)
  
  base
  
}

