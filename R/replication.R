#' @rdname replication
#' @title Bucket replication
#' @description Get/Delete the replication configuration for a bucket.
#' @details \code{get_replication} gets the current replication policy. \code{delete_replication} deletes the replication policy for a bucket.
#'
#' @template bucket
#' @template dots
#'
#' @return For \code{get_replication}: A list containing the replication configuration, if one has been set. For \code{delete_replication}: \code{TRUE} if successful, \code{FALSE} otherwise.
#' @references
#' \href{http://docs.aws.amazon.com/AmazonS3/latest/API/RESTBucketGETreplication.html}{API Documentation}
#' \href{http://docs.aws.amazon.com/AmazonS3/latest/API/RESTBucketDELETEreplication.html}{API Documentation}
#' @export
get_replication <- function(bucket, ...){
    r <- s3HTTP(verb = "GET", 
                bucket = bucket,
                query = list(replication = ""),
                ...)
    return(r)
}

#' @rdname replication
#' @export
delete_replication <- function(bucket, ...){
    r <- s3HTTP(verb = "DELETE", 
                bucket = bucket,
                query = list(replication = ""),
                parse_response = FALSE,
                ...)
    return(r)
}
