#.onLoad <- function(libname, pkgname) {
#  
  # .afit_thesis_bib <<- 'inst/rmarkdown/templates/afit_thesis/skeleton/references/my_bib.bib'
  # .afit_thesis_template <<- system.file('rmarkdown',
  #                                       'templates',
  #                                       'afit_thesis',
  #                                       'resources',
  #                                       'templateb.tex',
  #                                       package = 'DODschools')
  # }


.onUnload <- function (libpath) {
  library.dynam.unload("Ascend", libpath)
}
