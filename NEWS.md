# *NEWS*

# postpack 0.5.3 (2021-06-02)

* No user-facing changes made.
* Removed 'matrixcalc' as a dependency (#45)

# postpack 0.5.2 (2020-09-09)

* Addressed CRAN feedback and resubmitted
* See feedback received and to-do items -- none are major or user-facing (#36)

# postpack 0.5.1 (2020-09-01)

* Addressed CRAN feedback and resubmitted
* See feedback received and to-do items -- none are major or user-facing (#33)

# postpack 0.5.0 (2020-08-31)

* First submission to CRAN.

# postpack 0.4.4 (2020-08-28)

* Include `pkgdown` stuff - not much more needs to be said here.
* Site is hosted [here](https://bstaton1.github.io/postpack/)

# postpack 0.4.3 (2020-08-28)

* Split the large vignette into four (#29).
* This change is in preparation for creating a `pkgdown` website.

# postpack 0.4.2 (2020-08-25)

* Primarily standardization of function argument descriptions (#19, #32)
* Switched to using markdown for writing Roxygen comments -- big improvement
* Included more links to other relevant help files

# postpack 0.4.1 (2020-08-24)

* Improvements to function examples (#18)
* Some previous examples were entirely hypothetical (non-executable), and some functions didn't even have them.
* Nearly all examples now rely heavily on the `data(cjs)` object.

# postpack 0.4.0 (2020-08-24)

## Renamed Functions

* `get_p()` became `get_params()`
* `match_p()` became `match_params()`
* `base_p()` became `drop_index()`

## Renamed Function Arguments

* All functions that previously accepted a `p` argument now accept a `params` argument (`match_params()`, `diag_plots()` `post_subset()`, `post_summ()`, `post_remove()`, `vcov_decomp()`, etc.) (#14)
* `p_summ` has been replaced with `probs` in `post_summ()` (#25)
* `match_params` and `get_params` now both take a `type` argument, and both accept only values of `"base_only"` or `"base_index"` (#20 - some other function and arguments changed here as well).
* `density_plot()` and `trace_plot()` previously accepted a `p_one` argument -- this is now `param` (distinction between multiple parameters and one parameter); but most users won't notice this as these are non-exported functions.
* The non-exported functions `ins_regex_bracket()`, `rm_regex_bracket()`, `ins_regex_lock()`, and `rm_regex_lock()` now take the `params` argument rather than the `string` argument, but most users won't notice this.

## Deleted Functions

* `matrix2mcmclist()` was deleted and now `post_convert()` does its job (#23).

# postpack 0.3.1 (2020-08-23)

* Reset `par()` after completing `diag_plots()` (#17)

# postpack 0.3.0 (2020-08-23)

* Remove `StatonMisc` dependencies (#16)
* **This change won't affect users** -- just what goes on under the hood

# postpack 0.2.7 (2020-08-20)

* Updates to `post_thin()`
    * Remove errors that arise when `thin_percent` is not a perfect multiple of the number of iterations per chain - this causes the thinning to be "quasi-evenly" spaced. As this function is only ever used in code development and not final products, this should not be an issue (#13).
    * Reversed `thin_percent` to be `keep_percent`, and implemented throughout.
    * Added a `keep_iters` argument -- now users can choose to keep either a fixed number of samples (again, may be quasi-equal to `keep_iters`, due to rounding errors) or a fixed percent of samples.

# postpack 0.2.6 (2020-08-20)

* Add a `by_chain` argument to `post_summ()` (#10)
    * Sometimes a user may wish to summarize each chain separately rather than the aggregate across chains. They can now do this by setting `post_summ(..., by_chain = TRUE)`. For now, Rhat, ess, and mcse will be ignored if `by_chain = TRUE`.

# postpack 0.2.5 (2020-08-20)

## Add a `post_convert()` function

* `postpack` requires (for the most part) that MCMC samples be stored in `mcmc.list` objects. But not all R packages that perform MCMC return samples immediately in this format. This new function may help users get their samples into `mcmc.list` format, potentially broadening the accessibility of the functions in this package.

# postpack 0.2.4 (2020-07-14)

* Fix warnings introduced by R 4.0.0 (#12)
* User-visible behavior is identical to previous versions, but the warnings are no longer returned.

# postpack 0.2.3 (2020-04-13)

* Remove `density_plot()` and `trace_plot()` from exported list

# postpack 0.2.2 (2020-04-13)

## Include a `post_remove` function

*  In some circumstances, it might be better to exclude nodes rather than explicitly say which you want to keep. This new function behaves just like `post_subset`, except that it prompts the user to verify that they wish to exclude the match nodes and returns everything that does not match the `p` argument.

# postpack 0.2.1 (2020-04-10)

* Include ability to return Monte Carlo standard errors in `post_summ` (#11)

# postpack 0.2.0 (2020-04-10)

* Include an `auto_escape` option (#5)

# postpack 0.1.18 (2020-04-09)

* Require that R version 3.5.0 or later is available (#8)

# postpack 0.1.17 (2020-04-09)

* Bug fixes when working with one-node `mcmc.lists` (#7, #9)
    * `post_subset()` and `post_thin()` previously used `coda::as.mcmc.list()` to convert matrices to mcmc.list objects, however, if the matrix contained only one node, this would result in the mcmc.list no longer having a name for that node. Because most of the postpack functions rely on node names, this caused issues (e.g., `diag_plots()`, `post_bind()`).
    *  These functions now rely on `matrix2mcmclist` to perform this task, which should fix these issues.

# postpack 0.1.16 (2019-12-04)

* Bug fix when displaying NaN Rhat values (#7)

# postpack 0.1.15 (2019-11-30)

* Bug fix when calculating Rhat
* When calculating Rhat using `post_summ(..., Rhat = T)` (which wraps `coda::gelman.diag`) on an object containing only one node, an error would be thrown:

  ```
  Error in data[1:nobs, , drop = FALSE] : incorrect number of dimensions
  ```
  Which I tracked down to be a result of the `autoburnin` argument being true by default. Being able to perform the calculations for only one node was required for the `diag_plots(..., show_diags = T)` update. So I have forced this argument to be `FALSE` in its usage within `post_summ()`. 

# postpack 0.1.14 (2019-11-27)

* Functions that were previously exported but could confuse users include:
    * `ins_regex_bracket()`
    * `rm_regex_bracket()`
    * `id_mat()`
* These functions were removed from the exported NAMESPACE
* Consistency was improved for `vcov_decomp()`: it now internally uses `id_mat()`
* `post_bind()` now allows one of the objects to be a matrix of derived quantities, but still requires that it has the same number of samples as the object it is being binded to.
* The vignette was updated to reflect these and other changes to the package since its initial creation.

# postpack 0.1.13 (2019-11-26)

* Include NEWS.md by going through git commit history and documenting changes at each version shift

# postpack 0.1.12 (2019-11-26)

* Users now have the option to set a `diags_plots(..., show_diags)` argument to display Rhat and ESS on the density plot

# postpack 0.1.11 (2019-11-24)

* Improved error messages in `match_p` now uses `StatonMisc::list_out` to print the unique node names, which makes its printing much cleaner (#2)

# postpack 0.1.10 (2019-11-24)

* Removed the `warn` argument from all functions that call `match_p`
* Removed the `prettify` argument to `post_summ()` was removed

# postpack 0.1.9 (2019-11-24)

## Improvements to internal consistency

* The updated usage of `post_dim(..., kind)` was included in other functions including:
  * `post_bind`
  * `post_thin`
  * `vcov_decomp`
* `vcov_decomp` has improved error messages and relies on `StatonMisc::progress_updater` to print the progress of the calculation
* error message in `post_dim` improved to rely on `StatonMisc::list_out`
* included an `id_mat` function

## Improvements to documentation

* included a vignette and example mcmc.list objects
* readme updated to instruct users to set `build_vignettes = T` when installing from GitHub and several other simplifications to the readme since there is now a vignette

# postpack 0.1.8 (2019-10-03)

* `post_dim()` now accepts a `kind` argument, which represents certain element(s) from the returned vector. 

# postpack 0.1.7 (2019-09-19)

* Added `vcov_decomp()` function 
    * Will decompose the posterior of a variance-covariance node into its correlation matrix and standard deviation vector, and return an `mcmc.list` object with these nodes

# postpack 0.1.6 (2019-09-13)

* Added more layouts to `diag_plots` (#6)
    * Options for `diag_plots(..., layout = "2x1")` and `diag_plots(..., layout = "4x2")` added, and the `"auto"` option will now consider these when selecting the best option.

# postpack 0.1.5 (2019-09-03)

* Added `write_model()`: I essentially duplicated `R2OpenBUGS::write.model` because it was the only function I ever used from that package now all my BUGS models are fitted with JAGS. 
    * I got sick of having to load it and telling others to install it just for this one function
* Bug fix: `post_dim` was counting the chain and iters columns as nodes (#1)

# postpack 0.1.4 (2019-06-30)

* **Renamed codaTools to postpack**.
* The name of the package was changed from `{codaTools}` to `{postpack}`. This is a better name, because it is not just used for convergence diagnostics.
* Package helpfile created

# codaTools 0.1.3 (2019-06-30)

* Improvements to `get_nodes` and `filter_post`:  better error messages
* Switched `format = "matrix"` to `matrix = T/F` argument
* Included `match_p` function - now **all** subsetting is handled by this one function
* `filter_post` became `post_subset`
* inclusion of `ins_regex_lock`, `rm_regex_lock`, and `base_p` - all intended to improve the behavior of `diag_plots`
* better usage of  `StatonMisc` functions, they are now imported rather than requiring the whole package at the start of a function that needs them
* `summ_post` became `post_summ`
* `native_format` became `array_format`
* `get_nodes` became `get_p`
* `bind_post` became `post_bind`
* `thin_post` became `post_thin`
* addition of `post_dim`
* better titles in `diag_plots` through the `my_title` function

# codaTools 0.1.2 (2019-04-06)

* Bug fixes: `thin_post`, `native_format`
* Creation of readme: basic installation instructions and example functionality

# codaTools 0.1.1 (2018-11-16)

* Added new functions
  * `bind_post`: combines two `mcmc.list` objects
  * `thin_post`: thins an `mcmc.list` object at regularly-spaced intervals from each chain

# codaTools 0.1.0 (2018-11-14)

* First substantially usable version of package
* Package now contains functional capabilities to do everything I was doing before (extracting, diagnostic, summarizing particular nodes), now with cleaner functions that do specific tasks, and with the ability to extract particular nodes using regular expressions

# codaTools 0.0.9000 (2018-11-05)

## Initial Repository Creation

* First attempt at wrapping my commonly used MCMC post-processing functions into an R package, allowing them to be easily accessed during my work

