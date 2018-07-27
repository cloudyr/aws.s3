# @examples
# \dontrun{
# b <- put_bucket("myexamplebucket")
# s3write_using(mtcars, bucket = b, object = "mtcars.csv", FUN = utils::write.csv)
# con <- s3connection("mtcars.csv", bucket = b)
# 
# # line-by-line read
# while(length(x <- readLines(con, n = 1L))){
#   print(x)
# }
# 
# }
# @export
s3connection <-
function(object,
         bucket,
         headers = list(),
         ...) {
    if (missing(bucket)) {
        bucket <- get_bucketname(object)
    } 
    object <- get_objectkey(object)
    r <- s3HTTP(verb = "connection",
                bucket = bucket,
                path = paste0("/", object),
                headers = headers,
                ...)
    return(r)
}
