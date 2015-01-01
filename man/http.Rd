\name{s3HTTP}
\alias{s3HTTP}
\title{S3 HTTP Requests}
\description{This is the workhorse function for executing API requests for S3.}
\usage{
s3HTTP(verb, url, region, key, secret, \dots)
}
\arguments{
    \item{verb}{A character string containing an HTTP verb, defaulting to \dQuote{GET}.}
    \item{url}{A character string containing the URL for the API endpoint.}
    \item{region}{A character string containing the AWS region. If missing, defaults to \dQuote{us-east-1}.}
    \item{key}{A character string containing an AWS Access Key ID. If missing, defaults to value stored in environment variable \dQuote{AWS_ACCESS_KEY_ID}.}
    \item{secret}{A character string containing an AWS Secret Access Key. If missing, defaults to value stored in environment variable \dQuote{AWS_SECRET_ACCESS_KEY}.}
    \item{\dots}{Additional arguments passed to an HTTP request function, such as \code{\link[httr]{GET}}.}
}
\details{This is mostly an internal function for executing API requests. In almost all cases, users do not need to access this directly.}
%\value{}
\author{Thomas J. Leeper <thosjleeper@gmail.com>}
%\references{}
%\seealso{}
%\examples{}
