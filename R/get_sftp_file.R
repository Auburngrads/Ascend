#' Title
#'
#' @param protocol The protocol of the \code{server} - as a character string (typically "ftp" or "sftp")
#' @param server  The address of the \code{server} - as a character string
#' @param user The user name - as a character string
#' @param pwd The password - as a character string
#' @param sftp_path The path to the directory on the \code{server} (if exists) where the file will be posted
#' @param local_path The path to where \code{file} will be saved on the local machine
#' @param file The path to the file to be posted to the \code{server}
#'
#' @return The path to the where \code{file} was saved 
#' @export
get_sftp_file <- function(protocol = "sftp",
                          server = "sftp.ascend-innovations.com",
                          user = "hospice-shared",
                          pwd = Sys.getenv("OHI_SFTP_PWD"),
                          sftp_path = "/uploads/BrightView_data",
                          local_path =  rprojroot::find_root(rprojroot::is_git_root),
                          file = "active_clients_cls Centerville 1-21-2021.csv") {

# build the URL for the resource 
url <- paste0(protocol, "://", server, sftp_path,"/",file)

# GET the data using credentials
data <- RCurl::getURLContent(url = url, 
                             userpwd = paste0(user,":",pwd))

# Create a file connection 
fconn <- file(file.path(local_path,file))

# write content in data to file connection
writeLines(data, fconn)

# close file connection
close(fconn)

# return path to output file, invisibly
invisible(file.path(local_path,file))

}