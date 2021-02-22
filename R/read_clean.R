#' Read and clean data extracted from Brightview 
#'
#' @param .data A \code{data.frame}-class object
#' @param .keys Character vector of column names to use as keys for sorting and merging
#' @param .name Character vector or list of column names containing the patient's first and last name
#' @param .group Character string indicating which type of encounter to use for naming
#' @param .payer Character string indicating which column from \code{.data} contains payer information
#' @param .start Character string indicating which column from \code{.data} contains the start date of the encounter
#' @param .end   Character string indicating which column from \code{.data} contains the end date of the encounter
#' @param .description Character string indicating which column from \code{.data} contains the description of the encounter
#' @param .other   List of character string indicating which column(s) from \code{.data} contain other information about the encounter
#' 
#' 
#' @importFrom data.table data.table setDT setkeyv
#'
#' @return
#' @export
read_clean <- function(.data,
                       .keys,
                       .name = "API",
                       .type = "SUD",
                       .payer = "PAYER",
                       .start = "aDATE",
                       .end = "aDATE",
                       .description = "DXNUM_EX",
                       .id = "ENCOUNTER_ID",
                       .code = "DXNUM",
                       .category = "",
                       .other = list("bvDATE","bvID")){
  
  if(length(.other) > 2) stop("Argument .other should be either length 1 or 2")
  if(length(.other) == 2){
    
     .other_name = .other[[1]]
     .other_col  = .other[[2]]
    
  } else {
    
    .other_name <- .other_col <- .other[[1]]
    
  }
  
                         
  if(length(.name) > 2) stop("Argument .other should be either length 1 or 2")
                         
  if(!is.list(.name)) { .name = as.list(.name) }
  
  if(length(.name) == 2) {
    
     .name_data = paste(.data[[.name[[1]]]],.data[[.name[[2]]]], sep = " ")
    
  } else {
    
    .name_data = .data[[.name[[1]]]]
    
  }
  
  if(is.null(.payer) | is.na(.payer) | nchar(.payer) == 0) {
    
     PAYER = ""
    
  } else {
    
    PAYER = .data[[.payer]]
    
  }
  
  if(is.null(.id) | is.na(.id) | nchar(.id) == 0) {
    
     ID = ""
    
  } else {
    
    ID = .data[[.id]]
    
  }
  
  if(is.null(.code) | is.na(.code) | nchar(.code) == 0) {
    
     CODE = ""
    
  } else {
    
    CODE = .data[[.code]]
    
  }
  
  if(is.null(.category) | is.na(.category) | nchar(.category) == 0) {
    
     CATEGORY = ""
    
  } else {
    
     CATEGORY = .data[[.category]]
    
  }
  
  
  .out = data.table::data.table(name = .name_data,
                                type = .type,
                                payer = PAYER,
                                start = .data[[.start]],
                                end = .data[[.end]],
                                description = .data[[.description]],
                                id = as.character(ID),
                                code = as.character(CODE),
                                category = CATEGORY,
                                last = .data[[.other_col]])
  
  colnames(.out)[ncol(.out)] <- .other_name
  
  data.table::setkeyv(.out, .keys)
  
  return(.out)
  
}
