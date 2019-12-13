# 0.9.1.9000

* age in days is now rounded half to even (e.g. 730.5 days = 731) before joining
  the data with the reference values. This is in line with previous
  implementations and in particularly relevant to data points with
  age exactly 24 months. Previously this was converted to 730 days and with
  this release it is converted to 731 days.
* Removed `covr` from suggests depedencies
* Fixed a typo in the docs

# anthro 0.9.1

* Initial release of the package.
