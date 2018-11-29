
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
  default = 100L,
  distribution = expuniform_distribution(lower = 1L, upper = 10000L),
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

``` yaml
id: num_iter
default: 100
description: Number of iterations
distribution:
  lower: 1
  upper: 10000
  type: expuniform
type: integer
```

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

``` yaml
id: delta
default:
- 4.5
- 2.4
- 1.9
description: Multiplying factors
distribution:
  lower: -.inf
  upper: .inf
  mean: 5.0
  sd: 1.0
  type: normal
type: numeric
```

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

``` yaml
id: method
default: kendall
description: Correlation method
values:
- kendall
- spearman
- pearson
type: character
```

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

``` yaml
id: inverse
default: yes
description: Inversion parameter
type: logical
```

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

``` yaml
id: dimreds
default:
- pca
- mds
description: Which dimensionality reduction methods to apply (can be multiple)
values:
- pca
- mds
- tsne
- umap
- ica
type: subset
```

## Range parameter

``` r
param <- range_parameter(
  id = "ks",
  default = c(3L, 15L),
  lower_distribution = uniform_distribution(1L, 5L),
  upper_distribution = uniform_distribution(10L, 20L),
  description = "The numbers of clusters to be evaluated."
)
param
```

    ## [range] ks ∈ ( U(1, 5), U(10, 20) ), default=(3, 15)

As yaml:

``` r
cat(yaml::as.yaml(as.list(param)))
```

``` yaml
id: ks
default:
- 3
- 15
description: The numbers of clusters to be evaluated.
as_integer: yes
lower_distribution:
  lower: 1
  upper: 5
  type: uniform
upper_distribution:
  lower: 10
  upper: 20
  type: uniform
type: range
```
