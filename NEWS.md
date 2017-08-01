# NEWS for anomalyDetection package

### Changes for version 0.1.3
* Added `NEWS` file.
* Better tolerance in `mahalanobis_distance` when inverting covariance matrices.
* `mahalanobis_distance` and `horns_curve` have been rewritten in C++ using the `RcppArmadillo` package. This greatly improved the speed (and accuracy) of these functions.
* Properly registered native routines and disabled symbol search.
