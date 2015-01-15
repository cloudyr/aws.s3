s3HTTP <- function(verb, url, headers, region, key, secret, parse_response = TRUE, ...) {
    if(missing(verb))
        verb <- "GET"
    if(missing(headers))
        headers <- list()
    if(missing(region))
        region <- "us-east-1"
    if(missing(key))
        key <- Sys.getenv("AWS_ACCESS_KEY_ID")
    if(missing(secret))
        secret <- Sys.getenv("AWS_SECRET_ACCESS_KEY")
    current <- Sys.time()
    d_timestamp <- format(current, "%Y%m%dT%H%M%SZ", tz = "UTC")
    p <- parse_url(url)
    action <- if(p$path == "") "/" else paste0("/",p$path)
    if(key == "") {
        headers$`x-amz-date` <- d_timestamp
        H <- do.call(add_headers, headers)
    } else {
        Sig <- signature_v4_auth(
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
        headers$`x-amz-date` <- d_timestamp
        headers$`x-amz-content-sha256` <- Sig$BodyHash
        headers$Authorization <- Sig$SignatureHeader
        H <- do.call(add_headers, headers)
    }
    if(verb == "GET") {
        r <- GET(url, H, ...)
        if(!parse_response) {
            h <- headers(r)
            switch(h[["content-type"]], )
            return(content(r)) # this needs to handle MIME-types
        } else {
            content <- content(r, "text")
            response <- xmlToList(xmlParse(content))
            if(http_status(r)$category == "client error") {
                warn_for_status(r)
                out <- list(response, headers = headers(r))
                class(out) <- "aws_error"
                if(exists("S")) {
                    attr(out, "request_canonical") <- S$CanonicalRequest
                    attr(out, "request_string_to_sign") <- S$StringToSign
                    attr(out, "request_signature") <- S$SignatureHeader
                }
                return(out)
            } else {
                return(response)
            }
        }
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
        return(headers(r))
    }

    if(parse_response) {
        x <- xmlToList(xmlParse(content(r, "text")))
        if(http_status(r)$category == "client error") {
            warn_for_status(r)
            h <- headers(r)
            out <- structure(x, headers = h, class = "aws_error")
        } else {
            out <- x
        }
        if(inherits(out, "aws_error")) {
            attr(out, "request_canonical") <- S$CanonicalRequest
            attr(out, "request_string_to_sign") <- S$StringToSign
            attr(out, "request_signature") <- S$SignatureHeader
        }
        return(out)
    } else {
        return(r)
    }
}
