#' @rdname deprecated
#' @title Deprecated
#' @description These functions are deprecated.
#' @param \dots Arguments passed to updated versions of each function.
#' @export
getobject <- function(...) {
    warning("'getobject()' is deprecated. Use 'get_object()' instead.")
    get_object(...)
}
#' @rdname deprecated
#' @export
saveobject <- function(...) {
    warning("'saveobject()' is deprecated. Use 'save_object()' instead.")
    save_object(...)
}
#' @rdname deprecated
#' @export
headobject <- function(...) {
    warning("'headobject()' is deprecated. Use 'head_object()' instead.")
    head_object(...)
}
#' @rdname deprecated
#' @export
copyobject <- function(...) {
    warning("'copyobject()' is deprecated. Use 'copy_object()' instead.")
    copy_object(...)
}
#' @rdname deprecated
#' @export
copybucket <- function(...) {
    warning("'copybucket()' is deprecated. Use 'copy_bucket()' instead.")
    copy_bucket(...)
}
#' @rdname deprecated
#' @export
putbucket <- function(...) {
    warning("'putbucket()' is deprecated. Use 'put_bucket()' instead.")
    put_bucket(...)
}
#' @rdname deprecated
#' @export
putobject <- function(...) {
    warning("'putobject()' is deprecated. Use 'put_object()' instead.")
    put_object(...)
}
#' @rdname deprecated
#' @export
deleteobject <- function(...) {
    warning("'deleteobject()' is deprecated. Use 'delete_object()' instead.")
    delete_object(...)
}
#' @rdname deprecated
#' @export
getbucket <- function(...) {
    warning("'getbucket()' is deprecated. Use 'get_bucket()' instead.")
    get_bucket(...)
}
#' @rdname deprecated
#' @export
deletebucket <- function(...) {
    warning("'deletebucket()' is deprecated. Use 'delete_bucket()' instead.")
    delete_bucket(...)
}
#' @rdname deprecated
#' @export
bucketexists <- function(...) {
    warning("'bucketexists()' is deprecated. Use 'bucket_exists()' instead.")
    bucket_exists(...)
}
