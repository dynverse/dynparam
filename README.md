
<!-- README.md is generated from README.Rmd. Please edit that file -->

# dynparam

``` r
library(tidyverse)
library(dynparam)
set.seed(1)
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

    ## num_iter | type=integer | domain=e^U(0.00, 9.21) | default=100

It can be transformed to a list, then to a yaml:

``` r
ya <- yaml::as.yaml(as.list(param))
cat(ya)
```

``` yaml
id: num_iter
default: 100
description: Number of iterations
tuneable: yes
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

    ## num_iter | type=integer | domain=e^U(0.00, 9.21) | default=100

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

    ## delta | type=numeric | domain=N(5, 1) | default={4.5, 2.4, 1.9}

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
tuneable: yes
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

    ## method | type=character | domain={kendall, spearman, pearson} | default=kendall

As yaml:

``` r
cat(yaml::as.yaml(as.list(param)))
```

``` yaml
id: method
default: kendall
description: Correlation method
tuneable: yes
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

    ## inverse | type=logical | default=TRUE

As yaml:

``` r
cat(yaml::as.yaml(as.list(param)))
```

``` yaml
id: inverse
default: yes
description: Inversion parameter
tuneable: yes
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

    ## dimreds | type=subset | domain=all subsets of {pca, mds, tsne, umap, ica} | default={pca, mds}

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
tuneable: yes
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
param <- integer_range_parameter(
  id = "ks",
  default = c(3L, 15L),
  lower_distribution = uniform_distribution(1L, 5L),
  upper_distribution = uniform_distribution(10L, 20L),
  description = "The numbers of clusters to be evaluated."
)
param
```

    ## ks | type=integer_range | domain=( U(1, 5), U(10, 20) ) | default=(3, 15)

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
tuneable: yes
lower_distribution:
  lower: 1
  upper: 5
  type: uniform
upper_distribution:
  lower: 10
  upper: 20
  type: uniform
type: integer_range
```

## Example of parameter set

``` r
parameters <- parameter_set(
  integer_parameter(
    id = "num_iter",
    default = 100L,
    distribution = expuniform_distribution(lower = 1L, upper = 10000L),
    description = "Number of iterations"
  ),

  numeric_parameter(
    id = "delta",
    default = c(4.5, 2.4, 1.9),
    distribution = normal_distribution(mean = 5, sd = 1),
    description = "Multiplying factors"
  ),

  character_parameter(
    id = "method",
    default = "kendall",
    values = c("kendall", "spearman", "pearson"),
    description = "Correlation method"
  ),

  logical_parameter(
    id = "inverse",
    default = TRUE,
    description = "Inversion parameter"
  ),

  subset_parameter(
    id = "dimreds",
    default = c("pca", "mds"),
    values = c("pca", "mds", "tsne", "umap", "ica"),
    description = "Which dimensionality reduction methods to apply (can be multiple)"
  ),
  
  integer_range_parameter(
    id = "ks",
    default = c(3L, 15L),
    lower_distribution = uniform_distribution(1L, 5L),
    upper_distribution = uniform_distribution(10L, 20L),
    description = "The numbers of clusters to be evaluated"
  ),

  numeric_range_parameter(
    id = "quantiles",
    default = c(0.15, 0.90),
    lower_distribution = uniform_distribution(0, .4),
    upper_distribution = uniform_distribution(.6, 1),
    description = "Quantile cutoff range"
  ),
  
  forbidden = "inverse == (method == 'kendall')"
)
```

As paramhelper object:

``` r
paramset <- as_paramhelper(parameters)

paramset %>%
  ParamHelpers::generateDesign(n = 1) %>% 
  ParamHelpers::dfRowToList(paramset, 1) %>% 
  ParamHelpers::trafoValue(paramset, .)
```

    ## $num_iter
    ## [1] 247
    ## 
    ## $delta
    ## [1] 5.153253 5.065288 7.172612
    ## 
    ## $method
    ## [1] "spearman"
    ## 
    ## $inverse
    ## [1] TRUE
    ## 
    ## $dimreds
    ## [1] "mds"  "tsne" "ica" 
    ## 
    ## $ks
    ## [1]  2 17
    ## 
    ## $quantiles
    ## [1] 0.04199506 0.94581798

As yaml:

``` r
cat(yaml::as.yaml(as.list(parameters)))
```

``` yaml
- id: num_iter
  default: 100
  description: Number of iterations
  tuneable: yes
  distribution:
    lower: 1
    upper: 10000
    type: expuniform
  type: integer
- id: delta
  default:
  - 4.5
  - 2.4
  - 1.9
  description: Multiplying factors
  tuneable: yes
  distribution:
    lower: -.inf
    upper: .inf
    mean: 5.0
    sd: 1.0
    type: normal
  type: numeric
- id: method
  default: kendall
  description: Correlation method
  tuneable: yes
  values:
  - kendall
  - spearman
  - pearson
  type: character
- id: inverse
  default: yes
  description: Inversion parameter
  tuneable: yes
  type: logical
- id: dimreds
  default:
  - pca
  - mds
  description: Which dimensionality reduction methods to apply (can be multiple)
  tuneable: yes
  values:
  - pca
  - mds
  - tsne
  - umap
  - ica
  type: subset
- id: ks
  default:
  - 3
  - 15
  description: The numbers of clusters to be evaluated
  tuneable: yes
  lower_distribution:
    lower: 1
    upper: 5
    type: uniform
  upper_distribution:
    lower: 10
    upper: 20
    type: uniform
  type: integer_range
- id: quantiles
  default:
  - 0.15
  - 0.9
  description: Quantile cutoff range
  tuneable: yes
  lower_distribution:
    lower: 0.0
    upper: 0.4
    type: uniform
  upper_distribution:
    lower: 0.6
    upper: 1.0
    type: uniform
  type: numeric_range
- forbidden: inverse == (method == 'kendall')
```

## Latest changes

Check out `news(package = "dynwrap")` or [NEWS.md](inst/NEWS.md) for a
full list of
changes.

<!-- This section gets automatically generated from inst/NEWS.md, and also generates inst/NEWS -->

### Recent changes in dynparam 1.0.0 (unreleased)

  - INITIAL RELEASE: dynparam helps describe method
parameters.

## Dynverse dependencies

<!-- Generated by "update_dependency_graphs.R" in the main dynverse repo -->

![](man/figures/dependencies.png)
