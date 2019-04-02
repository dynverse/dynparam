
<!-- README.md is generated from README.Rmd. Please edit that file -->

# dynparam

Provides tools for describing parameters of algorithms in an abstract
way. Description can include an id, a description, a domain (range or
list of values), and a default value. ‘dynparam’ can also convert
parameter sets to a ‘ParamHelpers’ format, in order to be able to use
‘dynparam’ in conjunction with ‘mlrMBO’. Check `?dynparam` for an
overview of all functionality provided by dynparam.

``` r
library(tidyverse)
library(dynparam)
set.seed(1)
```

## Integer parameter

``` r
num_iter <- integer_parameter(
  id = "num_iter", 
  default = 100L,
  distribution = expuniform_distribution(lower = 1L, upper = 10000L),
  description = "Number of iterations"
)
num_iter
```

    ## num_iter | type=integer | domain=e^U(0.00, 9.21) | default=100

It can be transformed to a list, then to a yaml:

``` r
ya <- yaml::as.yaml(as.list(num_iter))
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
delta <- numeric_parameter(
  id = "delta", 
  default = c(4.5, 2.4, 1.9), 
  distribution = normal_distribution(mean = 5, sd = 1),
  description = "Multiplying factors"
)
delta
```

    ## delta | type=numeric | domain=N(5, 1) | default={4.5, 2.4, 1.9}

As yaml:

``` r
cat(yaml::as.yaml(as.list(delta)))
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
method <- character_parameter(
  id = "method", 
  default = "kendall",
  values = c("kendall", "spearman", "pearson"), 
  description = "Correlation method"
)
method
```

    ## method | type=character | domain={kendall, spearman, pearson} | default=kendall

As yaml:

``` r
cat(yaml::as.yaml(as.list(method)))
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
inverse <- logical_parameter(
  id = "inverse",
  default = TRUE, 
  description = "Inversion parameter"
)
inverse
```

    ## inverse | type=logical | default=TRUE

As yaml:

``` r
cat(yaml::as.yaml(as.list(inverse)))
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
dimreds <- subset_parameter(
 id = "dimreds",
 default = c("pca", "mds"),
 values = c("pca", "mds", "tsne", "umap", "ica"),
 description = "Which dimensionality reduction methods to apply (can be multiple)"
)
dimreds
```

    ## dimreds | type=subset | domain=all subsets of {pca, mds, tsne, umap, ica} | default={pca, mds}

As yaml:

``` r
cat(yaml::as.yaml(as.list(dimreds)))
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

## Integer range parameter

``` r
ks <- integer_range_parameter(
  id = "ks",
  default = c(3L, 15L),
  lower_distribution = uniform_distribution(1L, 5L),
  upper_distribution = uniform_distribution(10L, 20L),
  description = "The numbers of clusters to be evaluated."
)
ks
```

    ## ks | type=integer_range | domain=( U(1, 5), U(10, 20) ) | default=(3, 15)

As yaml:

``` r
cat(yaml::as.yaml(as.list(ks)))
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

## Numeric range parameter

``` r
quantiles <- numeric_range_parameter(
  id = "quantiles",
  default = c(0.15, 0.90),
  lower_distribution = uniform_distribution(0, .4),
  upper_distribution = uniform_distribution(.6, 1),
  description = "Quantile cutoff range"
)
quantiles
```

    ## quantiles | type=numeric_range | domain=( U(0, 0.4), U(0.6, 1) ) | default=(0.15, 0.9)

As yaml:

``` r
cat(yaml::as.yaml(as.list(quantiles)))
```

``` yaml
id: quantiles
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
```

## Example of parameter set

``` r
parameters <- parameter_set(
  num_iter,
  delta,
  method,
  inverse,
  dimreds,
  ks,
  quantiles,
  forbidden = "inverse == (method == 'kendall')"
)
```

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

Generate a random parameter set:

``` r
sip(parameters, n = 1)
```

    ## Loading required namespace: ParamHelpers

    ## Loading required namespace: lhs

    ## # A tibble: 1 x 8
    ##   num_iter delta   method  inverse dimreds  ks      quantiles .object_class
    ##      <int> <list>  <chr>   <lgl>   <list>   <list>  <list>    <list>       
    ## 1      247 <dbl [… spearm… TRUE    <chr [2… <dbl [… <dbl [2]> <chr [1]>

Convert paramhelper
    object:

``` r
as_paramhelper(parameters)
```

    ##                    Type len             Def                   Constr Req
    ## num_iter        numeric   -             0.5                   0 to 1   -
    ## delta     numericvector   3 0.309,0.0046...                   0 to 1   -
    ## method         discrete   -         kendall kendall,spearman,pearson   -
    ## inverse         logical   -            TRUE                        -   -
    ## dimreds   integervector   5       1,1,0,0,0                   0 to 1   -
    ## ks        numericvector   2         0.5,0.5                   0 to 1   -
    ## quantiles numericvector   2      0.375,0.75                   0 to 1   -
    ##           Tunable Trafo
    ## num_iter     TRUE     Y
    ## delta        TRUE     Y
    ## method       TRUE     -
    ## inverse      TRUE     -
    ## dimreds      TRUE     Y
    ## ks           TRUE     Y
    ## quantiles    TRUE     Y
    ## Forbidden region specified.

## Latest changes

Check out `news(package = "dynparam")` or [NEWS.md](inst/NEWS.md) for a
full list of
changes.

<!-- This section gets automatically generated from inst/NEWS.md, and also generates inst/NEWS -->

### Recent changes in dynparam 1.0.0 (02-04-2019)

  - INITIAL RELEASE: dynparam helps describe method
parameters.

## Dynverse dependencies

<!-- Generated by "update_dependency_graphs.R" in the main dynverse repo -->

![](man/figures/dependencies.png)
