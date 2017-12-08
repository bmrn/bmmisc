# bmmisc
Various useful functions for analyses and report preparation in R.

This package can be installed with the command `devtools::install_github("bmrn/bmmisc")`

### Functions
`age()` takes a "birthday" and returns age in years on today or another optionally given day.

`decade()` takes a date-time object and returns the decade as an integer (e.g. 1950, 1960 etc).

`iswholenumber()` takes a numeric object and returns TRUE if it is within a given tolerence of the nearest integer.

`lmprint()` neatly prints variable loading matrices, showing only loadings above a given cutoff. For use in factor analysis and principal component analysis.

`perday()` takes a set of date/times and returns TRUE/FALSE for whether they occur at a given frequency each day (or part thereof) over a given time period.
