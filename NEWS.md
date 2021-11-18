# anthro 1.0.0

## General

* Improved prevalence computation code, it now consumes slightly less time and
  is better organized. In addition test-coverage and documentation is improved.
* Cluster/strata/sw values in `anthro_prevalence` now support an explicit `NULL`
  value to better control for (missing) cluster/strata/sw information.
* New API functions are exported, mainly useful to use complicated portions
  of the logic in other packages, not meant for end-users.
* `nest = TRUE` is now set for the survey designs in `anthro_prevalence`. See
  the documentation of `survey::svydesign` for more information.
* Removed `headc`, `armc`, `triskin` and `subskin` from parameters of
  `anthro_prevalence` as they were not used for the final output.
* `_pop/unwpop` values in prevalence calculation are now 0 instead of NA if no
  values are present for that group.

## Bugfix

* Previously, if one level in a group only has only one observation, all
  `stdev` columns in `anthro_prevalence` are `NA`. Now only the levels with
  one observations yield `NA`, for the others the `stdev` is still computed.

# anthro 0.9.4

## General

* Handling of age information is improved and loss of information from
  converting from age in days to age in months or vice verse is reduced.
* Some code improvements.

## Bugfix

* Fixed a bug where, observations where erroneously removed from prevalence
  computation. This happens if age in months was supplied, then values
  `> 59.992` months were considered `> 1826` days. Now anything above
  `60.009` months is considered `> 1826` days and thus excluded.

# anthro 0.9.3

## General

* Internal code improvements.

## Bugfix

* Z-scores are now only computed for `age < 60 months` instead of `age <= 60 months`.
* Z-scores for wfl were previously also computed when `age >= 60 months` and `<= 1856 days`.

# anthro 0.9.2

* Age in days is now rounded half to even (e.g. 730.5 days = 731) before joining
  the data with the reference values. This is in line with previous
  implementations and in particularly relevant for data points with
  age exactly 24 months. Previously the example above was converted to 730 days and with
  this release it is converted to 731 days (#17).
* The cleaned `measure` and `sex` variables are now part of the output
  data.frame of `anthro_zscores` (#20, #24).
* Removed `covr` from suggests dependencies.
* Fixed a typo in the docs (#15).

# anthro 0.9.1

* Initial release of the package.
