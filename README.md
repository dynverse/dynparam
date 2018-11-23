
<!-- README.md is generated from README.Rmd. Please edit that file -->

# dynparam

``` r
library(tidyverse)
devtools::load_all(".")
```

## Integer parameter

``` r
param <- integer_parameter(
  id = "num_iter", 
  default = 100,
  distribution = expuniform(lower = 1e0, upper = 1e4), 
  description = "Number of iterations"
)
param
```

    ## num_iter: integer, default=100, . ∈ e^U(0.00, 9.21)

``` r
as_paramhelper(param)
```

    ##      Type len Def Constr Req Tunable Trafo
    ## 1 numeric   - 0.5 0 to 1   -    TRUE     Y

## Numeric parameter

``` r
param <- numeric_parameter(
  id = "delta", 
  default = c(4.5, 2.4, 1.9), 
  distribution = normal(mean = 5, sd = 1),
  description = "Multiplying factors",
  length = 3
)

param
```

    ## delta: numeric, default={4.5, 2.4, 1.9}, . ∈ N(5, 1)

``` r
as_paramhelper(param)
```

    ##            Type len             Def Constr Req Tunable Trafo
    ## 1 numericvector   3 0.309,0.0046... 0 to 1   -    TRUE     Y

## Character parameter

``` r
param <- character_parameter(
  id = "method", 
  default = "kendall",
  values = set("kendall", "spearman", "pearson"), 
  description = "Correlation method"
)
param
```

    ## method: character, default=kendall, . ∈ {kendall, spearman, pearson}

``` r
as_paramhelper(param)
```

    ##       Type len     Def                   Constr Req Tunable Trafo
    ## 1 discrete   - kendall kendall,spearman,pearson   -    TRUE     -

## Logical parameter

``` r
param <- logical_parameter(
  id = "inverse",
  default = TRUE, 
  description = "Inversion parameter"
)
param
```

    ## inverse: logical, default=TRUE

``` r
as_paramhelper(param)
```

    ##      Type len  Def Constr Req Tunable Trafo
    ## 1 logical   - TRUE      -   -    TRUE     -

## Parsing

``` r
param <- list_to_parameter(list(
  type = "numeric",
  id = "gamma",
  default = c(1.1, 2.2),
  description = "Gamma factor",
  length = 2,
  distribution = "uniform",
  lower = 0,
  upper = 5
))
param
```

    ## gamma: numeric, default={1.1, 2.2}, . ∈ U(0, 5)
