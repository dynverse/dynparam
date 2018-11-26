
<!-- README.md is generated from README.Rmd. Please edit that file -->

# dynparam

``` r
library(tidyverse)
library(dynparam)
```

## Integer parameter

``` r
param <- integer_parameter(
  id = "num_iter", 
  default = 100,
  distribution = expuniform_distribution(lower = 1e0, upper = 1e4),
  description = "Number of iterations"
)
param
```

    ## num_iter ∈ e^U(0.00, 9.21), type=integer, default=100

## Numeric parameter

``` r
param <- numeric_parameter(
  id = "delta", 
  default = c(4.5, 2.4, 1.9), 
  distribution = normal_distribution(mean = 5, sd = 1),
  description = "Multiplying factors",
  length = 3
)

param
```

    ## delta ⊆ N(5, 1), type=numeric, default={4.5, 2.4, 1.9}

## Character parameter

``` r
param <- character_parameter(
  id = "method", 
  default = "kendall",
  values = c("kendall", "spearman", "pearson"), 
  description = "Correlation method"
)
param
```

    ## method ∈ {kendall, spearman, pearson}, type=character, default=kendall

## Logical parameter

``` r
param <- logical_parameter(
  id = "inverse",
  default = TRUE, 
  description = "Inversion parameter"
)
param
```

    ## inverse, type=logical, default=TRUE

## Subset parameter

``` r
param <- subset_parameter(
 id = "dimreds",
 default = c("pca", "mds"),
 values = c("pca", "mds", "tsne", "umap", "ica"),
 description = "Which dimensionality reduction methods to apply (can be multiple)"
)
param
```

    ## dimreds = {x | x ⊆ {pca, mds, tsne, umap, ica}}, type=subset, default={pca, mds}

## Parsing

``` r
param <- list_as_parameter(list(
  type = "numeric",
  id = "gamma",
  default = c(1.1, 2.2),
  description = "Gamma factor",
  length = 2,
  distribution = list(
    distribution = "uniform",
    lower = 0,
    upper = 5
  ) 
))
param
```

    ## gamma ⊆ U(0, 5), type=numeric, default={1.1, 2.2}
