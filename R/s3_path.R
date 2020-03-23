#' @rdname s3_path
#' @title Treatment of the s3 path.
#' @description Use S3 path to return object and bucket without needing further coding.
#' @param path For \code{s3_path}, a single path that would be decompose into object and bucket.
#' @return For \code{s3_path}, a vector with two variables, first one is bucket second one is object.


#' @examples
#' \dontrun{
#' s3read_using(read_delim, delim = ";", object = s3_path(path)[2],bucket = s3_path(path[1]))
#'}

s3_path <- function(s3_path) {
  
  bucket <- stringr::str_match(s3_path, "s3://([^/]*)/")[2]
  object <- stringr::str_match(s3_path, paste0(bucket,"/(.*)"))[2]
  
  return(c(bucket,object))
  
}
