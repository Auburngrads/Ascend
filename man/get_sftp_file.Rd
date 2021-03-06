% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get_sftp_file.R
\name{get_sftp_file}
\alias{get_sftp_file}
\title{Title}
\usage{
get_sftp_file(
  protocol = "sftp",
  server = "sftp.ascend-innovations.com",
  user = "hospice-shared",
  pwd = Sys.getenv("OHI_SFTP_PWD"),
  sftp_path = "/uploads/BrightView_data",
  local_path = rprojroot::find_root(rprojroot::is_git_root),
  file = "active_clients_cls Centerville 1-21-2021.csv"
)
}
\arguments{
\item{protocol}{The protocol of the \code{server} - as a character string (typically "ftp" or "sftp")}

\item{server}{The address of the \code{server} - as a character string}

\item{user}{The user name - as a character string}

\item{pwd}{The password - as a character string}

\item{sftp_path}{The path to the directory on the \code{server} (if exists) where the file will be posted}

\item{local_path}{The path to where \code{file} will be saved on the local machine}

\item{file}{The path to the file to be posted to the \code{server}}
}
\value{
The path to the where \code{file} was saved
}
\description{
Title
}
