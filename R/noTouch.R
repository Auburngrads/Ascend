#' Extract and parse YAML metadata
#'
#' @description Process metadata.yml content for use in a template
#' @details This function is only intended to be called from 
#'          within the YAML header of a Rmarkdown template provided 
#'          by the DODschools package. When this function in invoked, 
#'          the YAML metadata is read from \code{file}, processed and
#'          then inserted in the appropriate field as needed by the template.
#'          Calls to this function made within the YAML header of an Rmarkdown
#'          document should not be touched (hence the name).  Changes to a
#'          document's metadata should be made in the metadata.yml file.
#' 
#' @param file Path to the metadata.yaml file
#'
#' @importFrom yaml yaml.load_file as.yaml
#' @export
noTouch <- function(file = NULL) {
  
  yaml <- yaml::yaml.load_file(file)
  
  yaml$author$Fullname <- paste(c(yaml$author$firstname,
                                  try(yaml$author$middle),
                                  yaml$author$lastname),
                                collapse = ' ')
  
  yaml$author$fullname <- paste(c(yaml$author$firstname,
                                  try(yaml$author$middle),
                                  yaml$author$lastname),
                                collapse = ' ')
  
  yaml$author$trname  <- paste(c(yaml$author$lastname,
                                 yaml$author$firstname,
                                 yaml$author$middle),
                                collapse = ', ')
  
  yaml$author$sf298name  <- paste(c(yaml$author$lastname,
                                 yaml$author$firstname,
                                 yaml$author$middle,
                                 yaml$author$rank,
                                 yaml$author$service),
                                collapse = ', ')
  
  yaml$sf298$doctype <- switch(tolower(yaml$document$type),
                               'thesis' = "Master's Thesis",
                               'dissertation' = 'Doctoral Dissertation')
  
  yaml$author$dept <- switch(tolower(yaml$author$department),
                             'env' = 'Department of Systems Engineering and Management',
                             'enc' = 'Department of Mathematics and Statistics',
                             'ens' = 'Department of Operational Sciences',
                             'eng' = 'Department of Operational Sciences',
                             'eny' = 'Department of Aeronautics and Astronautics',
                             'enp' = 'Department of Engineering Physics')

  yaml$advisor$dept <- switch(tolower(yaml$advisor$department),
                             'env' = 'Department of Systems Engineering and Management',
                             'enc' = 'Department of Mathematics and Statistics',
                             'ens' = 'Department of Operational Sciences',
                             'eng' = 'Department of Operational Sciences',
                             'eny' = 'Department of Aeronautics and Astronautics',
                             'enp' = 'Department of Engineering Physics')
  
  yaml$dept_head <- switch(tolower(yaml$author$department),
                           'env' = 'Michael R. Grimaila, PhD',
                           'enc' = 'Alan V. Lair, PhD',
                           'ens' = 'Joseph J. Pignatiello, PhD',
                           'eng' = 'Kenneth M. Hopkinson, PhD',
                           'eny' = 'Bradley S. Liebst, PhD',
                           'enp' = 'Nancy C. Giles, PhD')
  
  if(yaml$distribution_statement$A) yaml$distro_state <- 'Distribution A. Approved for public release; distribution unlimited.'
  if(yaml$distribution_statement$B) yaml$distro_state <- 'Distribution B. Distribution authorized to U.S. Government Agencies'
  if(yaml$distribution_statement$C) yaml$distro_state <- 'Distribution C. Distribution authorized to U.S. Government Agencies and their contractors'
  if(yaml$distribution_statement$D) yaml$distro_state <- 'Distribution D. Distribution authorized to Department of Defense and U.S. DoD contractors only'
  if(yaml$distribution_statement$E) yaml$distro_state <- 'Distribution E. Distribution authorized to DoD components only'
  if(yaml$distribution_statement$F) yaml$distro_state <- 'Distribution F. Further dissemination only as directed'

  if(yaml$distribution_statement$A) yaml$distro_thesis <- c('A','Approved for public release; distribution unlimited.')
  if(yaml$distribution_statement$B) yaml$distro_thesis <- c('B','Distribution authorized to U.S. Government Agencies')
  if(yaml$distribution_statement$C) yaml$distro_thesis <- c('C','Distribution authorized to U.S. Government Agencies and their contractors')
  if(yaml$distribution_statement$D) yaml$distro_thesis <- c('D','Distribution authorized to Department of Defense and U.S. DoD contractors only')
  if(yaml$distribution_statement$E) yaml$distro_thesis <- c('E','Distribution authorized to DoD components only')
  if(yaml$distribution_statement$F) yaml$distro_thesis <- c('F','Further dissemination only as directed')

  
  if(!yaml$distribution_statement$F & !yaml$distribution_statement$A) {

  reason <- data.frame()
  
  if(yaml$distribution_reason$dms) reason <- c(reason,'Direct Military Support')
  if(yaml$distribution_reason$swd) reason <- c(reason,'Software Documentation')
  if(yaml$distribution_reason$fgi) reason <- c(reason,'Foreign Government Information')
  if(yaml$distribution_reason$cpe) reason <- c(reason,'Contractor Performance Evaluation')
  if(yaml$distribution_reason$pri) reason <- c(reason,'Proprietary Information')
  if(yaml$distribution_reason$crt) reason <- c(reason,'Critical Technology')
  if(yaml$distribution_reason$pre) reason <- c(reason,'Premature Dissemination')
  if(yaml$distribution_reason$aou) reason <- c(reason,'Administrative of Operational Use')
  if(yaml$distribution_reason$tne) reason <- c(reason,'Test and Evaluation')
  if(yaml$distribution_reason$aut) reason <- c(reason,'Specific Authority')
  if(yaml$distribution_reason$exp) reason <- c(reason,'Export Controlled')
  if(yaml$distribution_reason$vul) reason <- c(reason,'Vulnerability Information')
  if(yaml$distribution_reason$ops) reason <- c(reason,'Operations Security')

  yaml$distro_reason <- paste(unlist(reason), collapse = ', ')
  
  yaml$distr_extra <- 'Other requests for this document shall be referred to'
  
  }
  
  checkIT <- function(x) {
   
    if(grepl('no',yaml::as.yaml(x))) { 
      
      return(NULL)
      
    } else { 
      
      return(x)
      
    }
  }
  
  yaml$distribution_reason$dms <- checkIT(yaml$distribution_reason$dms)
  yaml$distribution_reason$swd <- checkIT(yaml$distribution_reason$swd)
  yaml$distribution_reason$fgi <- checkIT(yaml$distribution_reason$fgi)
  yaml$distribution_reason$cpe <- checkIT(yaml$distribution_reason$cpe)
  yaml$distribution_reason$pri <- checkIT(yaml$distribution_reason$pri)
  yaml$distribution_reason$crt <- checkIT(yaml$distribution_reason$crt)
  yaml$distribution_reason$pre <- checkIT(yaml$distribution_reason$pre)
  yaml$distribution_reason$aou <- checkIT(yaml$distribution_reason$aou)
  yaml$distribution_reason$tne <- checkIT(yaml$distribution_reason$tne)
  yaml$distribution_reason$aut <- checkIT(yaml$distribution_reason$aut)
  yaml$distribution_reason$exp <- checkIT(yaml$distribution_reason$exp)
  yaml$distribution_reason$vul <- checkIT(yaml$distribution_reason$vul)
  yaml$distribution_reason$ops <- checkIT(yaml$distribution_reason$ops)

  yaml$distribution_statement$A <- checkIT(yaml$distribution_statement$A)
  yaml$distribution_statement$B <- checkIT(yaml$distribution_statement$B)
  yaml$distribution_statement$C <- checkIT(yaml$distribution_statement$C)
  yaml$distribution_statement$D <- checkIT(yaml$distribution_statement$D)
  yaml$distribution_statement$E <- checkIT(yaml$distribution_statement$E)
  yaml$distribution_statement$F <- checkIT(yaml$distribution_statement$F)

  `if`(tolower(yaml$document$type)%in%c('dissertation','prospectus'),
       yaml$dissertation <- TRUE,
       yaml$dissertation <- FALSE)
  
  # yaml$dissertation <- 'no'
  # yaml$dissertation <-  'yes'
  # yaml$dissertation <- if(tolower(yaml$document$type)=='prospectus')  'yes' 
  yaml$dissertation <- checkIT(yaml$dissertation)
  
  yaml$advisor$email1 <- try(unlist(strsplit(yaml$advisor$email,'@'))[1], silent = T)
  yaml$advisor$email2 <- try(unlist(strsplit(yaml$advisor$email,'@'))[2], silent = T)
  
  yaml$author$email1 <- try(unlist(strsplit(yaml$author$email,'@'))[1], silent = T)
  yaml$author$email2 <- try(unlist(strsplit(yaml$author$email,'@'))[2], silent = T)
  
  yaml$sponsor$email1 <- try(unlist(strsplit(yaml$sponsor$email,'@'))[1], silent = T)
  yaml$sponsor$email2 <- try(unlist(strsplit(yaml$sponsor$email,'@'))[2], silent = T)
    
  #yaml <- unlist(lapply(yaml, FUN = function(x) {gsub('\"', '', x)}))

  yaml$cite_style <- `if`(yaml$numeric_citations, 'numbers', 'authoryear')
  yaml$cite_shape <- `if`(yaml$numeric_citations, 'square' , 'round')
  yaml$num_cite   <- checkIT(yaml$numeric_citations)
  
  return(yaml)
}
