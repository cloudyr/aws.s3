parse_s3_error <- function(response) {
    x <- xmlToList(xmlParse(content(response, "text")))
    if(http_status(response)$category == "client error") {
        warn_for_status(response)
        h <- headers(response)
        return(structure(x, headers = h, class = "aws_error"))
    } else {
        return(x)
    }
}

s3HTTP <- function(verb, url, region, key, secret, ...) {
    if(missing(verb))
        verb <- "GET"
    if(missing(region))
        region <- "us-east-1"
    if(missing(key))
        key <- Sys.getenv("AWS_ACCESS_KEY_ID")
    if(missing(secret))
        secret <- Sys.getenv("AWS_SECRET_ACCESS_KEY")
    current <- Sys.time()
    #d_header <- format(current, "%a, %d %b %Y %T %Z", tz = "UTC")
    d_timestamp <- format(current, "%Y%m%dT%H%M%SZ", tz = "UTC")
    p <- parse_url(url)
    action <- if(p$path == "") "/" else paste0("/",p$path)
    if(key == "") {
        H <- add_headers(`x-amz-date` = d_timestamp)
    } else {
        S <- signature_v4_auth(
               datetime = d_timestamp,
               region = region,
               service = "s3",
               verb = verb,
               action = action,
               query_args = p$query,
               canonical_headers = list(host = p$hostname,
                                        `x-amz-date` = d_timestamp),
               request_body = "",
               key = key, secret = secret)
        H <- add_headers(`x-amz-date` = d_timestamp, 
                         `x-amz-content-sha256` = S$BodyHash,
                         Authorization = S$SignatureHeader)
    }
    if(verb == "GET") {
        r <- GET(url, H, ...)
    } else if (verb == "HEAD") {
        r <- HEAD(url, H, ...)
        return(headers(r))
    } else if (verb == "DELETE") {
        r <- DELETE(url, H, ...)
        return(headers(r))
    } else if (verb == "PUT") {
        r <- PUT(url, H, ...)
        return(headers(r))
    } else if (verb == "OPTIONS") {
        r <- VERB("OPTIONS", url, H, ...)
    }
    out <- parse_s3_error(r)
    if(inherits(out, "aws_error")) {
        attr(out, "request_canonical") <- S$CanonicalRequest
        attr(out, "request_string_to_sign") <- S$StringToSign
        attr(out, "request_signature") <- S$SignatureHeader
    }
    return(out)
}
