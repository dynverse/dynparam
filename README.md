
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

    ## num_iter ∈ 1, type=, default={} num_iter ∈ 10000, type=, default={}

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

    ## delta ⊆ 5, type=, default={} delta ⊆ 1, type=, default={} delta ⊆ -Inf, type=, default={} delta ⊆ Inf, type=, default={}

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

    ## method ∈ {kendall, spearman, pearson}, type=, default={}

## Logical parameter

``` r
param <- logical_parameter(
  id = "inverse",
  default = TRUE, 
  description = "Inversion parameter"
)
param
```

    ## inverse, type=, default={}

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

    ## dimreds = {x | x ⊆ {pca, mds, tsne, umap, ica}}, type=, default={}

## Range parameter

``` r
range_parameter(
  id = "ks",
  lower_default = 3,
  upper_default = 15,
  lower_distribution = uniform_distribution(1, 5),
  upper_distribution = uniform_distribution(10, 20),
  description = "The numbers of clusters to be evaluated."
)
```

    ## ks ∈ ( 1, 10 ), type=, default=(3,15) ks ∈ ( 5, 20 ), type=, default=(3,15)

## Parsing

``` r
param <- list_as_parameter(list(
  class = "numeric_parameter",
  id = "gamma",
  default = c(1.1, 2.2),
  description = "Gamma factor",
  length = 2,
  distribution = list(
    class = "uniform_distribution",
    lower = 0,
    upper = 5
  ) 
))
param
```

    ## gamma ⊆ 0, type=, default={} gamma ⊆ 5, type=, default={}
