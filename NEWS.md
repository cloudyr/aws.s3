# aws.s3 0.3.12

* `s3write_using()` now attaches the correct file extension to the temporary file being written to (just as `s3read_using()` already did). (#226, h/t @jon-mago)

# aws.s3 0.3.11

* `s3sync()` gains a `direction` argument allowing for unidirectional (upload-only or download-only) synchronization. The default remains bi-directional.
* New functions `put_encryption()`, `get_encryption()`, and `delete_encryption()` implement bucket-level encryption so that encryption does not need to be specified for each `put_object()` call. (#183, h/t Dan Tenenbaum)
* Fixed typos in `s3sync()`. (#211, h/t Nirmal Patel)
* `put_bucket()` only includes a LocationConstraint body when the region != "us-east-1". (#171, h/t David Griswold)

# aws.s3 0.3.10

* Fixed a typo in `setup_s3_url()`. (#223, h/t Peter Foley)
* Signatures are now calculated correctly when a port is specified. (#221, h/t @rvolykh)

# aws.s3 0.3.9

* Fixed a bug in `s3write_using()`. (#205, h/t Patrick Miller)
* Bumped **aws.signature** dependency to v0.3.7 to take advantage of automatic credential loading. (#184, h/t Dan Tenenbaum)
* `acl` argument was ignored by `put_bucket()`. This is now fixed. (#172)
* The `base_url` argument in `s3HTTP()` now defaults to an environment variable - `AWS_S3_ENDPOINT` - or the AWS S3 default in order to facilitate using the package with S3-compatible storage. (#189, #191, #194)

# aws.s3 0.3.8

* `save_object()` now uses `httr::write_disk()` to avoid having to load a file into memory. (#158, h/t Arturo Saco)

# aws.s3 0.3.7

* Remove usage of `endsWith()` in two places to reduce (implicit) base R dependency. (#147, h/t Huang Pan)

# aws.s3 0.3.6

* Bump **aws.signature** dependency to 0.3.4. (#142, #143, #144)

# aws.s3 0.3.5

* Attempt to fix bug introduced in 0.3.4. (#142)

# aws.s3 0.3.4

* Update code and documentation to use aws.signature (>=0.3.2) credentials handling.

# aws.s3 0.3.3

* `put_object()` and `put_bucket() now expose explicit `acl` arguments. (#137)
* `get_acl()` and `put_acl()` are now exported. (#137)
* Added a high-level `put_folder()` convenience function for creating an empty pseudo-folder.

# aws.s3 0.3.2

* `put_bucket()` now errors if the request is unsuccessful. (#132, h/t Sean Kross)
* Fixed a bug in the internal function `setup_s3_url()` when `region = ""`.

# aws.s3 0.3.1

* DESCRIPTION file fix for CRAN.

# aws.s3 0.3.0

* CRAN (beta) release. (#126)
* `bucketlist()` gains both an alias, `bucket_list_df()`, and an argument `add_region` to add a region column to the output data frame.

# aws.s3 0.2.8

* Exported the `s3sync()` function. (#20)
* `save_object()` now creates a local directory if needed before trying to save. This is useful for object keys contains `/`.

# aws.s3 0.2.7

* Some small bug fixes.
* Updated examples and links to API documentation.

# aws.s3 0.2.6

* Tweak region checking in `s3HTTP()`.

# aws.s3 0.2.5

* Fix reversed argument order in `s3readRDS()` and `s3saveRDS()`.
* Fixed the persistent bug related to `s3readRDS()`. (#59)
* Updated some documentation.

# aws.s3 0.2.4

* Mocked up multipart upload functionality within `put_object()` (#80)
* Use `tempfile()` instead of `rawConnection()` for high-level read/write functions. (#128)
* Allow multiple CommonPrefix values in `get_bucket()`. (#88)
* `get_object()` now returns a pure raw vector (without attributes). (#94)
* `s3sync()` relies on `get_bucket(max = Inf)`. (#20)
* `s3HTTP()` gains a `base_url` argument to (potentially) support S3-compatible storage on non-AWS servers. (#109)
* `s3HTTP()` gains a `dualstack` argument provide support for "dual stack" (IPv4 and IPv6) support. (#62)

# aws.s3 0.2.3

* Fixed a bug in `get_bucket()` when `max = Inf`. (#127, h/t Liz Macfie)

# aws.s3 0.2.2

* Two new functions - `s3read_using()` and `s3write_using()` provide a generic interface to reading and writing objects from S3 using a specified function. This provides a simple and extensible interface for the import and export of objects (such as data frames) in formats other than those provided by base R. (#125, #99)

# aws.s3 0.2.1

* `s3HTTP()` gains a `url_style` argument to control use of "path"-style (new default) versus "virtual"-style URL paths. (#23, #118)

# aws.s3 0.2.0

* All functions now produce errors when requests fail rather than returning an object of class "aws_error". (#86)

# aws.s3 0.1.39

* `s3save()` gains an `envir` argument. (#115)

# aws.s3 0.1.38

* `get_bucket()` now automatically handles pagination based upon the specified number of objects to return. (PR #104, h/t Thierry Onkelinx)
* `get_bucket_df()` now uses an available (but unexported) `as.data.frame.s3_bucket()` method. The resulting data frame always returns character rather than factor columns.

# aws.s3 0.1.37

* Further changes to region vertification in `s3HTTP()`. (#46, #106 h/t John Ramey)

# aws.s3 0.1.36

* `bucketlist()` now returns (in addition to past behavior of printing) a data frame of buckets.
* New function `get_bucket_df()` returns a data frame of bucket contents. `get_bucket()` continues to return a list. (#102, h/t Dean Attali)

# aws.s3 0.1.35

* `s3HTTP()` gains a `check_region` argument (default is `TRUE`). If `TRUE`, attempts are made to verify the bucket's region before performing the operation in order to avoid confusing out-of-region errors. (#46)
* Object keys can now be expressed using "S3URI" syntax, e.g., `object = "s3://bucket_name/object_key"`. In all cases, the bucketname and object key will be extracted from this string (meaning that a bucket does not need to be explicitly specified). (#100; h/t John Ramey)
* Fixed several places where query arguments were incorrectly being passed to the API as object key names, producing errors.

# aws.s3 0.1.34

* Update and rename policy-related functions.

# aws.s3 0.1.33

* Exported the `get_bucket()` S3 generic and methods.

# aws.s3 0.1.32

* Fixed a bug related to the handling of object keys that contained spaces. (#84, #85; h/t Bao Nguyen)

# aws.s3 0.1.29

* Fixed a bug related to the handling of object keys that contained atypical characters (e.g., `=`). (#64)
* Added a new function `s3save_image()` to save an entire workspace.
* Added a temporary fix for GitHub installation using the DESCRIPTION `Remotes` field.

# aws.s3 0.1.25

* Added function `s3source()` as a convenience function to source an R script directly from S3. (#54)

# aws.s3 0.1.23

* Added support for S3 "Acceleration" endpoints, enabling faster cross-region file transfers. (#52)
* `s3save()`, `s3load()`, `s3saveRDS()`, and `s3readRDS()` no longer write to disk, improving performance. (#51)

# aws.s3 0.1.22

* Added new functions `s3saveRDS()` and `s3readRDS()`. (h/t Steven Akins, #50)

# aws.s3 0.1.21

* Operations on non-default buckets (outside "us-east-1") now infer bucket region from bucket object. Some internals were simplified to better handle this. (h/t Tyler Hunt, #47)

# aws.s3 0.1.18

* All functions now use snake case (e.g., `get_object()`). Previously available functions that did not conform to this format have been deprecated. They continue to work, but issue a warning. (#28)
* Separated authenticated and unauthenticated testthat tests, conditional on presence of AWS keys.
* Numerous documentation fixes and consolidations.
* Dropped XML dependency in favor of xml2. (#40)

# aws.s3 0.1.17

* The structure of an object of class "s3_bucket" has changed. It now is simply a list of objects of class "s3_object" and bucket attributes are stored as attributes to the list.
* The order of `bucket` and `object` names was swapped in most object-related functions and the Bucket name has been added to the object lists returned by `getbucket()`. This means that `bucket` can be omitted when `object` is an object of class "s3_object".

# aws.s3 0.1.1

* Initial release.
