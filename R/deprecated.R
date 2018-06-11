#' @rdname deprecated-aws.s3
#' @title Deprecated
#' @description These functions are deprecated.
#' @param \dots Arguments passed to updated versions of each function.
#' @export
getobject <- function(...) {
    .Deprecated("get_object", "aws.s3")
    get_object(...)
}
#' @rdname deprecated-aws.s3
#' @export
saveobject <- function(...) {
    .Deprecated("save_object", "aws.s3")
    save_object(...)
}
#' @rdname deprecated-aws.s3
#' @export
headobject <- function(...) {
    .Deprecated("head_object()", "aws.s3")
    head_object(...)
}
#' @rdname deprecated-aws.s3
#' @export
copyobject <- function(...) {
    .Deprecated("copy_object()", "aws.s3")
    copy_object(...)
}
#' @rdname deprecated-aws.s3
#' @export
copybucket <- function(...) {
    .Deprecated("copy_bucket()", "aws.s3")
    copy_bucket(...)
}
#' @rdname deprecated-aws.s3
#' @export
putbucket <- function(...) {
    .Deprecated("put_bucket()", "aws.s3")
    put_bucket(...)
}
#' @rdname deprecated-aws.s3
#' @export
putobject <- function(...) {
    .Deprecated("put_object()", "aws.s3")
    put_object(...)
}
#' @rdname deprecated-aws.s3
#' @export
deleteobject <- function(...) {
    .Deprecated("delete_object()", "aws.s3")
    delete_object(...)
}
#' @rdname deprecated-aws.s3
#' @export
getbucket <- function(...) {
    .Deprecated("get_bucket()", "aws.s3")
    get_bucket(...)
}
#' @rdname deprecated-aws.s3
#' @export
deletebucket <- function(...) {
    .Deprecated("delete_bucket()", "aws.s3")
    delete_bucket(...)
}
#' @rdname deprecated-aws.s3
#' @export
bucketexists <- function(...) {
    .Deprecated("bucket_exists()", "aws.s3")
    bucket_exists(...)
}
