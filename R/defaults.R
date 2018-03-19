
.aws.s3.globals <- new.env(parent=emptyenv())

#' @title Set the default base url
#'
#' @description set the default location for all aws.s3
#' requests. By default, the base_url is "s3.amazonaws.com".
#' @param new_url the new url to send requests.
#' @export
set_base_url <- function(new_url) {
  assign("base_url", new_url, pos=.aws.s3.globals, inherits=FALSE)
  invisible(TRUE)
}

#' @title Get the default base url
#' 
#' @description get the default location for all aw3.s3
#' request. By default, the base url is "s3.amazonaws.com"
#' @return the default base url.
#' @export
get_base_url <- function() {
  if (!exists("base_url", where=.aws.s3.globals, inherits=FALSE)) {
    set_base_url("s3.amazonaws.com")
  }
  .aws.s3.globals$base_url
}

