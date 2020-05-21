## Test environments
* local OS X install, R 4.0.0
* OS X (on travis-ci), R release
* ubuntu 12.04 (on travis-ci), R oldrel, devel, release
* win-builder (devel and release)

## R CMD check results

0 errors | 0 warnings | 0 notes

* `anthro_prevalence` examples are wrapped in `\dontrun` as it takes too long to compute for CRAN checks. It was requested to lower the runtime, but that requires more research in how to speed up the code. So I wrapped it in `\dontrun` again for now. Will continue to work on making the code faster.

## Reverse dependencies

There are no reverse dependencies.