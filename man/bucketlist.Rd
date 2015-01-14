\name{bucketlist}
\alias{bucketlist}
\title{List buckets for authenticated user}
\description{This function lists the S3 buckets available to the authenticated user. If no authentication credentials are supplied, the function will fail.}
\usage{
bucketlist(\dots)
}
\arguments{
    \item{\dots}{Additional arguments passed to \code{\link{s3HTTP}}.}
}
\details{If authentication is successful, this function provides a list of buckets available to the authenticated user. In this way, it can serve as a \dQuote{hello world!} function, to confirm that one's authentication credentials are working correctly.}
\value{}
\author{Thomas J. Leeper <thosjleeper@gmail.com>}
\references{\href{http://docs.aws.amazon.com/AmazonS3/latest/API/RESTServiceGET.html}{AWS Documentation}}
\keyword{service}
%\seealso{}
%\examples{}
