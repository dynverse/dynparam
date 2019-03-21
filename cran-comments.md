This is a new package for being able to describe parameters in an abstract way.

## Test environments
* local Fedora install, R 3.5.2
* ubuntu 14.04 (on travis-ci), R 3.5.2
* win-builder (devel and release)

## R CMD check results

This function is not exported by ParamHelpers, but the usage thereof is crucial.

```
── R CMD check results ───────────────────────────────────── dynparam 1.0.0 ────
Duration: 38s

❯ checking dependencies in R code ... NOTE
  Unexported object imported by a ':::' call: ‘ParamHelpers:::makeParam’
    See the note in ?`:::` about the use of this operator.

0 errors ✔ | 0 warnings ✔ | 1 note ✖

R CMD check succeeded
```
