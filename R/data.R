#' @title Example mcmc.list 1
#' @description An example of samples from a joint posterior distribution from a Cormack-Jolly-Seber model.
#'   **Users should note that the specific context does not matter, this object is provided solely
#'   to show examples of 'postpack' functionality**. All further details here are provided
#'   for interested users and completeness.
#' @format A [`mcmc.list`][coda::mcmc.list] object. The nodes stored in it include:
#'   * `B0`: intercept of logit-linear survival v. length relationship: global mean across years
#'   * `B1`: slope of logit-linear survival v. length relationship: global mean across years
#'   * `sig_B0`: standard deviation of between-year fluctuations in the survival intercept term
#'   * `sig_B1`: standard deviation of between-year fluctuations in the survival slope term
#'   * `b0[1:5]`: year-specific survival random intercepts
#'   * `b1[1:5]`: year-specific survival random slopes
#'   * `SIG[1:2,1:2]`: variance-covariance matrix modeling the covariance between random slopes and intercepts
#'   * `p[2:4]`: detection probability for each receiver array, assumed constant across years (note the first detection occasion is considered to be tagging, and so is omitted in the model)
#' @source Posterior samples generated from a model fitted to an entirely hypothetical data set.
#'   Suppose for 5 years, biologists individually tagged juvenile salmon in the headwaters of a river system
#'   before the fish made their migration out to sea. The length of each fish was measured as well,
#'   and it is safe to assume they are a random sample from the population and that they were all released
#'   at the same time within a given year.
#'   Three receiver arrays exist in the river that can detect fish as they move past them,
#'   but they are imperfect (sometimes a receiver will not detect a tagged fish passing it).
#'   The principle research goal is to quantify whether survival during migration
#'   is associated with fish size.
#'
#'   The object `cjs` (accessed via `data(cjs, package = "postpack"`)
#'   stores heavily thinned posterior samples (to reduce file size) from the model below, fitted with JAGS.
#'
#'   ```
#'   model{
#'     # HYPERPARAMETERS: SURVIVAL COEFFICIENTS AND VARIABILITY
#'     B0 ~ dnorm(0, 0.001)
#'     B1 ~ dnorm(0, 0.001)
#'     sig_B0 ~ dunif(0,10)
#'     sig_B1 ~ dunif(0,10)
#'     rho ~ dunif(-1,1)
#'
#'     # COVARIANCE MATRIX FOR YEAR-SPECIFIC SURVIVAL COEFFICIENTS
#'     SIG[1,1] <- sig_B0^2
#'     SIG[2,2] <- sig_B1^2
#'     SIG[2,1] <- sig_B0 * sig_B1 * rho
#'     SIG[1,2] <- sig_B0 * sig_B1 * rho
#'
#'     # YEAR-SPECIFIC SURVIVAL COEFFICIENTS
#'     Bmean[1] <- B0
#'     Bmean[2] <- B1
#'     for (t in 1:nt) {
#'       b[t,1:2] ~ dmnorm.vcov(Bmean[1:2], SIG[1:2,1:2])
#'       b0[t] <- b[t,1]
#'       b1[t] <- b[t,2]
#'     }
#'
#'     # DETECTION PROBABILITY: OCCASION-SPECIFIC, BUT YEAR-CONSTANT
#'     for (j in 2:J) {
#'       p[j] ~ dunif(0,1)
#'     }
#'
#'     # LIKELIHOOD
#'     for (i in 1:N) {
#'       logit(phi[i]) <- b0[year[i]] + b1[year[i]] * FL[i]
#'       for (j in 2:J) {
#'         z[i,j] ~ dbern(z[i,j-1] * phi[i])
#'         y[i,j] ~ dbern(z[i,j] * p[j])
#'       }
#'     }
#'   }
#'   ```
#'
#'   Users interested in this kind of model are encouraged to read
#'   _Bayesian Population Analysis using WinBUGS: A Hierarchical Perspective_
#'   by Marc Kéry and Michael Schuab.
"cjs"

#' @title Example mcmc.list 2
#' @description An example of samples from a joint posterior distribution from a Cormack-Jolly-Seber model.
#'   **Users should note that the specific context does not matter, this object is provided solely
#'   to show examples of 'postpack' functionality**. This second object is provided
#'   to illustrate how to scale 'postpack' functionality up to multiple models.
#'   All further details here are provided for interested users and completeness.
#' @source This object stores samples from the same hypothetical example as for the [`cjs`] example object,
#'   with one small change to the model. The `rho` term that models correlation between slopes and intercepts
#'   is forced to be zero, rather than estimating it. Consult [`cjs`] for more details.
#' @format A [`mcmc.list`][coda::mcmc.list] object.
"cjs_no_rho"
