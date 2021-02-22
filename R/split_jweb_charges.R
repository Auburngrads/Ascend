#' Split Justice Web charges into separate rows 
#'
#' @param .data An R object containing the Justice Web data
#' 
#' @param .user_col Character string naming a column in \code{.data}
#'                  that contains the unique identifiers for each user 
#'
#' @param .charges_col Character string naming a column in \code{.data}
#'                     that contains the charges filed for an encounter
#'
#' @return A \code{data.frame}-class object containing one charge per row
#'
#' @export
split_jweb_charges <- function(.data, .user_col, .charges_col){
  
  .users = unique(.data[[.user_col]])
  
  .final_container = t(data.frame(rep(NA, ncol(.data))))
  colnames(.final_container) <- colnames(.data)
  rownames(.final_container) <- NULL
  
  for(i in seq_along(.users)){

      .user_container = t(data.frame(rep(NA, ncol(.data))))
      colnames(.user_container) <- colnames(.data)
      rownames(.user_container) <- NULL
    
      .user_sub = subset(.data, 
                         get(.user_col) == .users[i],
                         drop = T)
      
      for(j in 1:nrow(.user_sub)) {
        
          .other_cols = .user_sub[j,]
          
          .replace_breaks = gsub("<br/>",
                                 "sp-here",
                                 .other_cols[.charges_col])
          
          .split_charges = as.data.frame(col.names = "charges",
                                         strsplit(.replace_breaks,
                                                  split= "sp-here"))
          
          .other_cols[[.charges_col]] <- NULL
          
          .out = cbind(.other_cols,.split_charges)
          
          .user_container = rbind(.user_container, .out)
        
      }
      
      .user_container = .user_container[-1,]
      
      .final_container = rbind(.final_container, .user_container)
      
  }
  
  .final_container = .final_container[-1,]
  
  return(.final_container)
  
}