
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

    ## [integer] num_iter ∈ e^U(0.00, 9.21), default=100

It can be transformed to a list, then to a yaml:

``` r
ya <- yaml::as.yaml(as.list(param))
cat(ya)
```

    ## id: num_iter
    ## default: 100.0
    ## description: Number of iterations
    ## distribution:
    ##   lower: 1.0
    ##   upper: 10000.0
    ##   class: expuniform_distribution
    ## class: integer_parameter

And back:

``` r
as_parameter(yaml::yaml.load(ya))
```

    ## [integer] num_iter ∈ e^U(0.00, 9.21), default=100

## Numeric parameter

``` r
param <- numeric_parameter(
  id = "delta", 
  default = c(4.5, 2.4, 1.9), 
  distribution = normal_distribution(mean = 5, sd = 1),
  description = "Multiplying factors"
)

param
```

    ## [numeric] delta ⊆ N(5, 1), default={4.5, 2.4, 1.9}

As yaml:

``` r
cat(yaml::as.yaml(as.list(param)))
```

    ## id: delta
    ## default:
    ## - 4.5
    ## - 2.4
    ## - 1.9
    ## description: Multiplying factors
    ## distribution:
    ##   mean: 5.0
    ##   sd: 1.0
    ##   lower: -.inf
    ##   upper: .inf
    ##   class: normal_distribution
    ## class: numeric_parameter

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

    ## [character] method ∈ {kendall, spearman, pearson}, default=kendall

As yaml:

``` r
cat(yaml::as.yaml(as.list(param)))
```

    ## id: method
    ## default: kendall
    ## description: Correlation method
    ## values:
    ## - kendall
    ## - spearman
    ## - pearson
    ## class: character_parameter

## Logical parameter

``` r
param <- logical_parameter(
  id = "inverse",
  default = TRUE, 
  description = "Inversion parameter"
)
param
```

    ## [logical] inverse, default=TRUE

As yaml:

``` r
cat(yaml::as.yaml(as.list(param)))
```

    ## id: inverse
    ## default: yes
    ## description: Inversion parameter
    ## class: logical_parameter

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

    ## [subset] dimreds = {x | x ⊆ {pca, mds, tsne, umap, ica}}, default={pca, mds}

As yaml:

``` r
cat(yaml::as.yaml(as.list(param)))
```

    ## id: dimreds
    ## default:
    ## - pca
    ## - mds
    ## description: Which dimensionality reduction methods to apply (can be multiple)
    ## values:
    ## - pca
    ## - mds
    ## - tsne
    ## - umap
    ## - ica
    ## class: subset_parameter

## Range parameter

``` r
param <- range_parameter(
  id = "ks",
  default = c(3, 15),
  lower_distribution = uniform_distribution(1, 5),
  upper_distribution = uniform_distribution(10, 20),
  description = "The numbers of clusters to be evaluated."
)
param
```

    ## [range] ks ∈ ( U(1, 5), U(10, 20) ), default=(3,15)

As yaml:

``` r
cat(yaml::as.yaml(as.list(param)))
```

    ## id: ks
    ## default:
    ## - 3.0
    ## - 15.0
    ## description: The numbers of clusters to be evaluated.
    ## lower_distribution:
    ##   lower: 1.0
    ##   upper: 5.0
    ##   class: uniform_distribution
    ## upper_distribution:
    ##   lower: 10.0
    ##   upper: 20.0
    ##   class: uniform_distribution
    ## class: range_parameter
