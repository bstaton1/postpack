# codaTools

An R package for streamlining the workflow with R objects of class `mcmc.list`. 

To install the development version of this package to your computer:

```
devtools::install_github("bstaton1/codaTools")
```

To install a specific version (i.e., release), use:

```
devtools::install_github("bstaton1/codaTools@v0.1.1")
```

## Example usage

Suppose you have an R object named `mypost`, and `class(mypost) == "mcmc.list"` is `TRUE`. Say this object stores the output from a simple linear regression model, and you tracked the nodes `b0`, `b1`, and `sigma`. You can query which nodes were tracked:

```
get_nodes(mypost)
```

You can extract all posterior samples for requested nodes:

```
filter_post(mypost, "b0")
```

which will return the output as an `mcmc.list` object by default, but if you set the argument `format = "matrix"`, then it will be coerced to a matrix where the further arguments `chains = T` and `iters = T` come into play. See `?filter_post` for more details.

>**Note:** the matching of a requested node to the `p` argument uses _regular expressions_ (implemented using `stringr::str_detect()`). Thus, if you pass `p = "b"`, samples for both `b0` and `b1` will be extracted. All functions that take a `p` argument in this package use regular expressions.

You can summarize a node or set of nodes:

```
summ_post(post, "b")
```

See `?summ_post` for more details.

You can view diagnostic plots (trace and density plots) for desired nodes:

```
diag_plots(post, "b")
```

Sometimes it is nice to have a posterior summary statistic in the same format used in the model. For example, say you have a matrix object in your model `X`. If you run `summ_post(post, "X")`, the summaries will become vectorized. You can coerce a vector like this back to its native format in the model using:

```
native_format(summ_post(mypost, "X")["mean",])
```

Currently, this works for up to 3 dimensions.

Sometimes it is nice to have a smaller subset of your complete model output to test post-processing code with prior to running it with all retained posterior samples:

`thin_post(mypost, 0.5)`

will return an object of class `mcmc.list` with every other posterior sample from each chain removed. 
