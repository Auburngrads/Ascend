#' Post a file to a directory on an FTP/SFTP server
#'
#' @param protocol The protocol of the \code{server} - as a character string (typically "ftp" or "sftp")
#' @param server  The address of the \code{server} - as a character string
#' @param user The user name - as a character string
#' @param pwd The password - as a character string
#' @param sftp_path The path to the directory on the \code{server} (if exists) where the file will be posted
#' @param file The path to the file to be posted to the \code{server}
#'
#' @return NULL
#' @export
post_sftp_file <- function(protocol = "sftp",
                           server = "sftp.ascend-innovations.com",
                           user = "hospice-shared",
                           pwd = Sys.getenv("OHI_SFTP_PWD"),
                           sftp_path = "/uploads/BrightView_data",
                           file = "active_clients_cls Centerville 1-21-2021.csv"){
  
# build the URL for the resource 
url <- paste0(protocol, 
              "://", paste0(user,":",pwd),
              "@", server, 
              sftp_path,"/",basename(file))

# POST the data using credentials
RCurl::ftpUpload(file, url)
  
}
