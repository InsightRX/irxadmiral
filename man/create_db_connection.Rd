% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/create_db_connection.R
\name{create_db_connection}
\alias{create_db_connection}
\title{Create connection to a postgres database}
\usage{
create_db_connection(db)
}
\arguments{
\item{db}{list containing database connection details. Requires \code{host},
\code{dbname}, \code{user}, \code{password}, and \code{port}}
}
\value{
db connection object
}
\description{
Create connection to a postgres database
}
