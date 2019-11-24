# `postpack`

An R package for streamlining the workflow with R objects of class `mcmc.list`. 

## Installation
To install the current working version of this package to your computer:

```R
devtools::install_github("bstaton1/postpack")
```
Current versions (>=v0.1.9) of the package now include a vignette describing how to use the package by working through some example posterior samples included with the package. To build the vignette upon installing the package, instead run:

```R
devtools::install_github("bstaton1/postpack", build_vignettes = TRUE)
```
Because the R packages `rmarkdown` and `knitr` are used to build this vignette to HTML output, you are required to have these installed before running this code. The vignette can then be accessed using:

```R
vignette("main-tutorial", package = "postpack")
```

## Motivations

This package is an attempt at creating a simple-to-use and standardized workflow for processing output from models fitted using MCMC methods in which the output is stored in an R object of class `mcmc.list`.  Without `postpack`, I find working with `mcmc.list` objects cumbersome - particularly because it is difficult to access only the output you desire. In essence, it attempts make working with `mcmc.list` objects neater, more consistent, and more flexible. Whether it be diagnosing sampling convergence, summarizing posterior estimates, paring output down for development of long-running calculations, combining `mcmc.lists`  from the same or different models, or whatever else it takes to get the job done, it is the intent that `postpack` has got you covered. In an additional attempt to make the subsetting of particular nodes more flexible, regular expressions are now accepted!

The next section provides some examples, but you are encouraged to explore the package vignette ("main-tutorial") for a more exhaustive description of the main functions.

## Example functionality

Suppose you have standard simple linear regression model that looks something like this:

```
model {
  b0 ~ dnorm(0, 0.001)
  b1 ~ dnorm(0, 0.001)
  sig ~ dunif(0, 1)
  tau <- 1/sig^2
  
  for (i in 1:n) {
    y[i] ~ dnorm(pred_y[i], tau)
    pred_y[i] <- b0 + b1 * x[i]
  }
}
```

Suppose you have fitted this model in R using the packages `R2OpenBUGS`, `R2jags` `rjags`, or `jagsUI` (the last of these is my personal favorite), and they have returned an R object named `mypost`, and `class(mypost) == "mcmc.list"` is `TRUE`. It is time to do some post-processing.

#### Learn about your samples

To determine which nodes were tracked:

```R
get_p(mypost)
```

You can obtain the MCMC dimensions of the sampling:

```R
post_dim(mypost)
```

to return the number of burn-in samples, post-burn-in samples, chains, and the like.

#### Manipulate your samples

If you are writing code that takes a long time to run if provided with the full posterior output, then:

```R
post_thin(mypost, 0.8)
```

will remove 80% of the samples at regularly spaced intervals from each chain. 

You can subset `mypost` to retrieve only samples for the `b0` parameter:

```
post_subset(mypost, "b0")
```

This will return the output as an `mcmc.list` object by default, but if you set the `matrix = T`, argument, then it will be coerced to a matrix where the further arguments `chains = T` and `iters = T` come into play. See `?post_subset` for more details. 

Because of node matching via regular expressions, running:

```R
post_subset(mypost, "b")
```

will return samples of both the `b0` and `b1` nodes. This magic stems from the usage of `stringr::str_detect()` within the `match_p()` function of `postpack`. This function performs the searching to extract particular node(s), so if you are having trouble with the regular expressions, try running:

```R
match_p(mypost, "b")
```

to see what is being matched. See `?str_detect` and `?match_p` for more information and details.

If you wish to extract all elements of `pred_p`, you would run `match_p(mypost, "pred_y")`. To extract only one element of this node, you can run `match_p(mypost, "pred_y[5]")` without needing to escape the square brackets (though you may escape them with `"pred_y\\[5\\]"` if you like). However, this means you won't be able to use `"["` or `"]"` in their standard regular expression usage until I add an `auto_escape = F` feature (which I plan to do).

#### Summary and covergence

You can summarize a node or set of nodes:

```
post_summ(mypost, "b")
```

See `?post_summ` for more arguments, including options for rounding and returning convergence and sampling diagnostics for each node.

You can view diagnostic plots (trace and density) for desired nodes:

```R
diag_plots(post, c("b", "sig"))
```

See `?diag_plots` for more settings, including the layout of the output, and where it is dumped (i.e., the current device, a new device, or even saved in a PDF file for viewing later).

### Questions?

Feel free to post an issue to this repository for bug fixes, questions, requested features, etc.