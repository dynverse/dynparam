
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

    ## num_iter ∈ e^U(0.00, 9.21), class=, default=100

As yaml:

``` r
cat(yaml::as.yaml(as_list(param)))
```

    ## class: integer_parameter
    ## id: num_iter
    ## default: 100.0
    ## distribution:
    ##   class: expuniform_distribution
    ##   lower: 1.0
    ##   upper: 10000.0
    ## description: Number of iterations
    ## length: 1.0

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

    ## delta ⊆ N(5, 1), class=, default={4.5, 2.4, 1.9}

As yaml:

``` r
cat(yaml::as.yaml(as_list(param)))
```

    ## class: numeric_parameter
    ## id: delta
    ## default:
    ## - 4.5
    ## - 2.4
    ## - 1.9
    ## description: Multiplying factors
    ## distribution:
    ##   class: normal_distribution
    ##   mean: 5.0
    ##   sd: 1.0
    ##   lower: -.inf
    ##   upper: .inf
    ## length: 3.0

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

    ## method ∈ {kendall, spearman, pearson}, class=, default=kendall

As yaml:

``` r
cat(yaml::as.yaml(as_list(param)))
```

    ## class: character_parameter
    ## id: method
    ## default: kendall
    ## values:
    ## - kendall
    ## - spearman
    ## - pearson
    ## description: Correlation method
    ## length: 1.0

## Logical parameter

``` r
param <- logical_parameter(
  id = "inverse",
  default = TRUE, 
  description = "Inversion parameter"
)
param
```

    ## inverse, class=, default=TRUE

As yaml:

``` r
cat(yaml::as.yaml(as_list(param)))
```

    ## class: logical_parameter
    ## id: inverse
    ## default: yes
    ## description: Inversion parameter
    ## length: 1.0

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

    ## dimreds = {x | x ⊆ {pca, mds, tsne, umap, ica}}, class=, default={pca, mds}

As yaml:

``` r
cat(yaml::as.yaml(as_list(param)))
```

    ## class: subset_parameter
    ## id: dimreds
    ## default:
    ## - pca
    ## - mds
    ## values:
    ## - pca
    ## - mds
    ## - tsne
    ## - umap
    ## - ica
    ## description: Which dimensionality reduction methods to apply (can be multiple)
    ## length: ~

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

    ## ks ∈ ( U(1, 5), U(10, 20) ), class=, default=(3,15)

As yaml:

``` r
cat(yaml::as.yaml(as_list(param)))
```

    ## class: subset_parameter
    ## id: dimreds
    ## default:
    ## - pca
    ## - mds
    ## values:
    ## - pca
    ## - mds
    ## - tsne
    ## - umap
    ## - ica
    ## description: Which dimensionality reduction methods to apply (can be multiple)
    ## length: ~
