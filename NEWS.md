# NEWS for anomalyDetection package

### Changes for version 0.2.2
* Added `NEWS` file.
* Better tolerance in `mahalanobis_distance` when inverting covariance matrices.
* `mahalanobis_distance` and `horns_curve` have been rewritten in C++ using the `RcppArmadillo` package. This greatly improved the speed (and accuracy) of these functions.
* `tabulate_state_vector` has been rewritten using the `dplyr` package, greatly improving the speed of this function. Greater traceability is now also present for missing values and numeric variables.
* Producing histogram matrix to visually display anomalous blocks made easier with addition of `hmat` function
* Properly registered native routines and disabled symbol search.
