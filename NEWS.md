# CHANGES TO aws.s3 0.1.33

* Exported the `get_bucket()` S3 generic and methods.

# CHANGES TO aws.s3 0.1.32

* Fixed a bug related to the handling of object keys that contained spaces. (#84, #85; h/t Bao Nguyen)

# CHANGES TO aws.s3 0.1.29

* Fixed a bug related to the handling of object keys that contained atypical characters (e.g., `=`). (#64)
* Added a new function `s3save_image()` to save an entire workspace.
* Added a temporary fix for GitHub installation using the DESCRIPTION `Remotes` field.

# CHANGES TO aws.s3 0.1.25

* Added function `s3source()` as a convenience function to source an R script directly from S3. (#54)

# CHANGES TO aws.s3 0.1.23

* Added support for S3 "Acceleration" endpoints, enabling faster cross-region file transfers. (#52)
* `s3save()`, `s3load()`, `s3saveRDS()`, and `s3readRDS()` no longer write to disk, improving performance. (#51)

# CHANGES TO aws.s3 0.1.22

* Added new functions `s3saveRDS()` and `s3readRDS()`. (h/t Steven Akins, #50)

# CHANGES TO aws.s3 0.1.21

* Operations on non-default buckets (outside "us-east-1") now infer bucket region from bucket object. Some internals were simplified to better handle this. (h/t Tyler Hunt, #47)

# CHANGES TO aws.s3 0.1.18

* All functions now use snake case (e.g., `get_object()`). Previously available functions that did not conform to this format have been deprecated. They continue to work, but issue a warning. (#28)
* Separated authenticated and unauthenticated testthat tests, conditional on presence of AWS keys.
* Numerous documentation fixes and consolidations.
* Dropped XML dependency in favor of xml2. (#40)

# CHANGES TO aws.s3 0.1.17

* The structure of an object of class "s3_bucket" has changed. It now is simply a list of objects of class "s3_object" and bucket attributes are stored as attributes to the list.
* The order of `bucket` and `object` names was swapped in most object-related functions and the Bucket name has been added to the object lists returned by `getbucket()`. This means that `bucket` can be omitted when `object` is an object of class "s3_object".

# CHANGES TO aws.s3 0.1.1

* Initial release.
