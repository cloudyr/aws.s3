
.aws.s3.globals <- new.env(parent=emptyenv())

#' @export
set_base_url <- function(new_url) {
  assign("base_url", new_url, pos=.aws.s3.globals, inherits=FALSE)
}

#' @export
get_base_url <- function() {
  if (!exists("base_url", where=.aws.s3.globals, inherits=FALSE)) {
    set_base_url("s3.amazonaws.com")
  }
  .aws.s3.globals$base_url
}

