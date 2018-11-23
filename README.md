
<!-- README.md is generated from README.Rmd. Please edit that file -->

# dynparam

``` r
library(tidyverse)
devtools::load_all(".")
```

``` r
param <- integer_parameter(id = "k", default = 3, distribution = uniform(2, 10), description = "Number of clusters")
param
```

    ## k: integer, default=3, . ∈ U(2, 10)

``` r
as_paramhelper(param)
```

    ##      Type len   Def Constr Req Tunable Trafo
    ## 1 numeric   - 0.125 0 to 1   -    TRUE     Y

``` r
param <- numeric_parameter(id = "delta", default = c(4.5, 2.4, 1.9), distribution = normal(5, 1), description = "Multiplying factors", length = 3)
param
```

    ## delta: numeric, default={4.5, 2.4, 1.9}, . ∈ N(5, 1)

``` r
as_paramhelper(param)
```

    ##            Type len             Def Constr Req Tunable Trafo
    ## 1 numericvector   3 0.309,0.0046... 0 to 1   -    TRUE     Y

``` r
param <- character_parameter(id = "method", default = "kendall", values = set("kendall", "spearman", "pearson"), description = "Correlation method")
param
```

    ## method: character, default=kendall, . ∈ {kendall, spearman, pearson}

``` r
as_paramhelper(param)
```

    ##       Type len     Def                   Constr Req Tunable Trafo
    ## 1 discrete   - kendall kendall,spearman,pearson   -    TRUE     -

``` r
param <- logical_parameter(id = "inverse", default = TRUE, description = "Inversion parameter")
param
```

    ## inverse: logical, default=TRUE

``` r
as_paramhelper(param)
```

    ##      Type len  Def Constr Req Tunable Trafo
    ## 1 logical   - TRUE      -   -    TRUE     -
