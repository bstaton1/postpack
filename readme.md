## Why 'postpack'?

'postpack' exists to facilitate processing the output of models fitted using Markov Chain Monte Carlo (MCMC) methods, as is commonly done in Bayesian inference. Although substantial capabilities exist to interface from R with programs that perform MCMC like JAGS, WinBUGS, OpenBUGS, NIMBLE, and Stan, the functionality to easily process the output is sometimes lacking. 

>In particular, it is often cumbersome to perform post-processing tasks for **specific nodes** from the model.
>
>'postpack' makes extracting specific nodes more transparent by accepting **regular expressions** and returning output in predictable and manipulable formats. 

Common tasks encompassed by "post-processing" include:

* Extracting raw samples
* Summarizing measures of posterior central tendency, spread, quantiles, and the MCMC precision of these quantities
* Calculating and organizing derived quantities based on each posterior sample
* Diagnosing the behavior of MCMC sampling, both numerically and visually
* Producing plots of model fit, relationships, and parameter comparisons while showing posterior uncertainty
* Comparing posteriors of the same quantity across many models that make different assumptions

'postpack' seeks to enforce consistent rules in how many these actions are performed by the user, all based around `mcmc.list` objects (one per model).

## Documentation

> Want to learn more?

* Have a look at the [feature overview](https://bstaton1.github.io/postpack/articles/feature-overview.html) vignette to see whether 'postpack' can help clean up your post-MCMC workflow. 
* An overview of how to make the most of regular expressions in this context can be found in the [pattern matching](https://bstaton1.github.io/postpack/articles/pattern-matching.html) vignette.  
* As a more advanced topic, the vignette about working with [multiple models](https://bstaton1.github.io/postpack/articles/multiple-models.html) illustrates how the transparent and short syntax of 'postpack' can be scaled to compare inferences between similar models.
* For more details on the example `mcmc.list` objects used in the examples and vignettes, take a look at [that vignette](https://bstaton1.github.io/postpack/articles/example-mcmclists.html).

## Installation

> Ready to give 'postpack' a try in your workflow? 

A stable version can be found on [CRAN](https://CRAN.R-project.org/package=postpack):

```R
install.packages("postpack")
```

Or the development version can be installed via:

```R
remotes::install_github("bstaton1/postpack")
```
