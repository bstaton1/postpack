# postpack

An R package for streamlining the workflow with R objects of class `mcmc.list`. 

To install the current working version of this package to your computer:

```R
devtools::install_github("bstaton1/postpack")
```

To install a specific branch (i.e., development versions), use:

```R
devtools::install_github("bstaton1/postpack", ref = "branchname")
```

## Motivations

The author has found the default and built-in workflow for post-processing output stored in R objects of class `mcmc.list` (possibly obtained via BUGS, JAGS, or Stan models) rather clunky. For example, suppose you have such an object called `mypost`. If you run `summary(mypost)` you get all of the information you need, but it is for all nodes and the measures of central tendency and spread (e.g., posterior mean and standard deviation) are stored in a separate list element than their quantiles. Further, if you run `plot(mypost)`, density plots and trace plots are presented for ALL monitored nodes (without any way to specify a subset), and only trace plots are displayed separated by chain. This organization makes subsetting, plotting, and table generation incredibly cumbersome. The user needs to either (1) sift through all output (which could take a while to calculate and do if many nodes were monitored) to find the specific information required or (2) write highly specific subsetting code for each node or set of nodes you wish to extract. Add multiple models into the mix, and one can see where even basic tasks can take dozens of lines of code, let alone many intermediate objects of little value.

Since B. Staton first discovered these issues in 2014, he has written various functions to attempt to circumvent them, or at least hide the nasty subsetting behind wrapper functions. Until recently (Fall 2018), these functions (primarily `get.post` and `sort.post`, as they were called back then) had been floating around in various directories (and in various states of completeness) and were generally `source`-ed at the start of any script that used `mcmc.list` objects. This made it very difficult to share code with collaborators or workshop participants, as the functions needed to be shared (and stored in the right place) for the rest of the code to run.

### Enter `postpack`...

This package synthesizes these functions into a standardized workflow that ensures they all play nice together (or at least tries to). In essence, it attempts make working with `mcmc.list` objects neater. Whether it be diagnosing sampling convergence, summarizing posterior estimates, paring down for faster calculations, combining with `mcmc.lists`  from the same or other models, or whatever else it takes the job done, it is the intent that `postpack` has got you covered. In an additional attempt to make the subsetting of particular nodes more flexible, regular expressions are now accepted!

The next section provides some examples.

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

will remove 80% of the samples at regularly spaced intervals from each chain. If you have two `mcmc.list` objects, you can combine them into one:

```R
post_bind(mypost1, mypost2)
```

Duplicate names will be appended with `"_p2"` by default, which allows combining output from more than one model into a single `mcmc.list`. The dimensions of both `mypost1` and `mypost2` must be identical.

You can subset `mypost` to retrieve only samples for the `b0` parameter:

```
post_subset(mypost, "b0")
```

This will return the output as an `mcmc.list` object by default, but if you set the `matrix = T`, argument, then it will be coerced to a matrix where the further arguments `chains = T` and `iters = T` come into play. See `?post_subset` for more details. 

Because of regular expression matching, running:

```R
post_subset(mypost, "b")
```

will return samples of both the `b0` and `b1` nodes. This magic stems from the usage of `stringr::str_detect()` within the `match_p` function here. This function performs the searching to extract a particular node, so if you are having trouble with the regular expressions, try running:

```R
match_p(mypost, "b", warn = T)
```

to see what is being matched. Matching of multiple elements is allowed. See `?str_detect` and `?match_p` for more information and details.

If you wish to extract all elements of `pred_p`, you would run `match_p(mypost, "pred_y")`. To extract only one element of this node, you can run `match_p(mypost, "pred_y[5]")` without needing to escape the square brackets (though you may escape them with `"pred_y\\[5\\]"` if you like).

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

See `?diag_plots` for more settings, including opening device selection and saving to a PDF file.

### Questions?

Feel free to post an issue to this repository for bug fixes, questions, requested features, etc.