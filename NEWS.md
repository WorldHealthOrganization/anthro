# anthro 0.9.2

* Age in days is now rounded half to even (e.g. 730.5 days = 731) before joining
  the data with the reference values. This is in line with previous
  implementations and in particularly relevant for data points with
  age exactly 24 months. Previously the example above was converted to 730 days and with
  this release it is converted to 731 days (#17).
* The cleaned `measure` variable is now part of the output data.frame of `anthro_zscores` as `cmeasure` (#24).
* Removed `covr` from suggests dependencies.
* Fixed a typo in the docs (#15).

# anthro 0.9.1

* Initial release of the package.
