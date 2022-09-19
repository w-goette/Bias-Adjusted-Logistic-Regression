#
# SET-UP SCRIPT FOR BAYESIAN LATENT CLASS ANALYSIS SIMULATION
#



# Prepare Environment -----------------------------------------------------

#load needed packages
library(cmdstanr)   #used for Bayesian models
library(posterior)  #used for extracting results
library(pROC)       #used for extracting AUC values
library(foreach)    #used for parallel computing of simulations
library(doParallel) #used for parallel computing of simulations

#set seed for reproducibility
SEED <- 124323
set.seed(seed = SEED)

#prepare for parallel processes
nworkers <- parallel::detectCores()
cl <- makePSOCKcluster(nworkers)
clusterSetRNGStream(cl)
registerDoParallel(cl)



# Prepare Bayesian Models -------------------------------------------------

#
# BASE BAYESIAN LOGISTIC REGRESSION FOR COMPARISON
#

log_mod <- '
data {

  int<lower=0> N;                          // number of observations
  int<lower=0> P;                          // number of predictors
  array[N] int<lower=0, upper=1> y;        // class observations
  matrix[N, P] x;                          // predictor matrix

}

transformed data {

  vector[P] x_m;                           // predictor means
  matrix[N, P] x_c;                        // centered predictors

  for ( p in 1 : P ) {
    x_m[p] = mean(x[, p]);
    x_c[, p] = x[, p] - x_m[p];
  }

  // perform QR decomposition on centered predictors
  matrix[N, P] b_x_r = qr_thin_Q(x_c) * sqrt(N - 1);
  matrix[P, P] b_x_a = inverse(qr_thin_R(x_c) / sqrt(N - 1));

}

parameters {

  real b_0_c;                              // intercept parameter
  vector[P] t_x;                           // QR decomposed coefficient parameter(s)

}

model {

  // simple priors for intercept and coefficient(s)
  b_0_c ~ normal(0, 3);
  t_x   ~ normal(0, 3);

  // logistic likelihood
  y ~ bernoulli_logit_glm(x_c, b_0_c, t_x);

}

generated quantities {

  // posterior predictions
  array[N] int pop;
  pop  = bernoulli_logit_glm_rng(x_c, rep_vector(b_0_c, N), t_x);

  // compute rescaled coefficients
  vector[P] b_x = b_x_a * t_x;
  real b_0 = b_0_c - dot_product(x_m, b_x);

}
'

log_mod_fun <- write_stan_file(log_mod)
log_mod_fun <- cmdstan_model(log_mod_fun, stanc_options = list("O1"))

#
# BIAS-ADJUSTED LOGISTIC REGRESSION MODEL
#

dsm_mod <- "
data {

  int<lower=0> N;                          // number of observations
  int<lower=0> P;                          // number of predictors
  array[N] int<lower=0, upper=1> y;        // class observations
  matrix[N, P] x;                          // predictor matrix

}

transformed data {

  vector[P] x_m;                           // predictor means
  matrix[N, P] x_c;                        // centered predictors

  // mean center predictors
  for ( p in 1 : P ) {
    x_m[p] = mean(x[, p]);
    x_c[, p] = x[, p] - x_m[p];
  }

  // perform QR decomposition on centered predictors
  matrix[N, P] b_x_r = qr_thin_Q(x_c) * sqrt(N - 1);
  matrix[P, P] b_x_a = inverse(qr_thin_R(x_c) / sqrt(N - 1));

}

parameters {

  vector[P] t_x;                           // QR decomposed coefficient parameter(s)
  real b_0_c;                              // intercept parameter
  vector<lower=1>[3] theta;                // hyperprior for accuracy
  simplex[3] accuracy;                     // constraint for misclassification error

}

transformed parameters {

  // convert to sensitivity and specificity (assuming model is better than chance)
  real<lower=0, upper=1> sens = 1 - accuracy[1];
  real<lower=0, upper=1> spec = 1 - accuracy[2];

  // linear component of logistic regression
  vector[N] logit_z_hat = b_0_c + x_c * t_x;

}

model {

  // (hyper)priors
  theta ~ chi_square(5);
  accuracy ~ dirichlet(theta);
  b_0_c ~ std_normal();
  t_x   ~ std_normal();

  // likelihood
  for ( n in 1 : N ) {
    target += log_sum_exp(bernoulli_lpmf(    y[n] | sens) + bernoulli_logit_lpmf(1 | logit_z_hat[n]),
                          bernoulli_lpmf(1 - y[n] | spec) + bernoulli_logit_lpmf(0 | logit_z_hat[n]));
  }
}

generated quantities {

  // posterior predictions
  vector<lower=0, upper=1>[N] prob;        // probability of 1
  array[N] int<lower=0, upper=1> pop;      // realization of diagnostic class

  for ( n in 1 : N ) {
    prob[n] = softmax([bernoulli_lpmf(    y[n] | sens) + bernoulli_logit_lpmf(1 | logit_z_hat[n]),
                       bernoulli_lpmf(1 - y[n] | spec) + bernoulli_logit_lpmf(0 | logit_z_hat[n])]')[1];
  }

  // compute class predictions
  pop = bernoulli_rng(prob);

  // compute rescaled coefficients
  vector[P] b_x = b_x_a * t_x;
  real b_0 = b_0_c - dot_product(x_m, b_x);

}
"

dsm_mod_fun <- write_stan_file(dsm_mod)
dsm_mod_fun <- cmdstan_model(dsm_mod_fun, stanc_options = list("O1"))



# Create Helper Functions -------------------------------------------------

#close off all CPU commitments
unregister <- function() {
  env <- foreach:::.foreachGlobals
  rm(list = ls(name = env), pos = env)
}

#truncated beta distribution
rtbeta <- function(n = 1, m = 0.65) {

  # convert mean to alpha and beta shape terms
  alpha <- ((1 - m) / 0.025^2 - 1 / m) * m^2
  beta  <- alpha * (1 / m - 1)

  # generate random variate of size n
  x <- runif(n, min = 0, max = 1)

  # limit random values to be no smaller than 0.60
  lb <- pbeta(0.35, alpha, beta)

  # limit random values to be no larger than 0.90
  ub <- pbeta(0.90, alpha, beta)

  # rescale random variate to beta given bounds
  y <- (1 - x) * lb + x * ub

  # return value
  return(qbeta(y, alpha, beta))
}

#define function to simulate data
simDat <- function(N, P, Sn_m, Sp_m) {

  # generate matrix of predictor data: N (sample size) x P (predictors)
  x <- matrix(rnorm(N * P, mean = 0, sd = 1), nrow = N, ncol = P)

  # generate logistic regression parameters
  b_0 <- rnorm(1, mean = 0, sd = 0.25)       # intercept
  b_x <- rnorm(P, mean = 1, sd = 0.25)       # coefficients

  # since coefficients favor large effects, make ~1/4 negative
  if ( P > 1 )
    b_x[seq.int(2, P, 4)] <- -1 * b_x[seq.int(2, P, 4)]

  # use logistic regression to create true observations and probabilities
  y <- rep(0, N)
  pr <- rep(0, N)

  for ( n in 1 : N ) {
    pr[n] <- 1 / (1 + exp(-1 * (b_0 + (x[n, ] %*% b_x))))
    y[n] <- rbinom(1, 1, pr[n])
  }

  # compute standard error for S- and M-errors
  se <- sqrt(diag((t(cbind(rep(1, N), x)) %*% diag(sapply(pr, function(p) p * (1 - p))) %*% cbind(rep(1, N), x))^-1))

  # draw sensitivity and specificity from truncated beta distribution
  Sn <- rtbeta(n = 1, m = Sn_m)
  Sp <- rtbeta(n = 1, m = Sp_m)

  # respect magnitude ranking of sensitivity and specificity in the condition
  while ( Sn_m > Sp_m & Sn < Sp ) {
    Sn <- rtbeta(n = 1, m = Sn_m)
    Sp <- rtbeta(n = 1, m = Sp_m)
  }

  while ( Sn_m < Sp_m & Sn > Sp ) {
    Sn <- rtbeta(n = 1, m = Sn_m)
    Sp <- rtbeta(n = 1, m = Sp_m)
  }

  # use sensitivity and specificity to create misdiagnoses
  obs <- rep(0, N)

  for ( n in 1 : N )
    obs[n] <- rbinom(1, 1, ifelse(y[n], Sn, 1 - Sp))

  # create dataframe of simulated data for output
  data <- as.data.frame(x)
  data$obs <- obs
  data$y <- y

  # tidy the output
  colnames(data) <- c(paste("var", 1:P, sep = ""), "y_err", "y_tru")

  # combine simulated data with parameter values
  out <- list(data = data,
              pars = c(b_0, b_x, se, Sn, Sp),
              prob = pr)

  # return the data
  return(out)

}

#define function to run models
simFit <- function(N, P, data) {

  # extract needed information from inputs
  data <- data$data

  # create dataframe for the models
  if ( P == 1 ) {
    stan_dat <- list(N = N, P = P, y = data$y_err, x = array(data[, 1], c(N, 1)))
  } else {
    stan_dat <- list(N = N, P = P, y = data$y_err, x = data[, 1:P])
  }

  # run Bayesian models

  log_mod_fit <-
    log_mod_fun$variational(data = stan_dat,
                            refresh = 0, seed = SEED)

  dsm_mod_fit <-
    dsm_mod_fun$variational(data = stan_dat,
                            refresh = 0, seed = SEED)

  # prepare models to be passed to results summary
  out <- list(log_mod = log_mod_fit,
              dsm_mod = dsm_mod_fit)

  return(out)

}

#define function to assemble results
simRes <- function(fits, data, N, P) {

  # extract fits from list
  log_mod_fit <- fits$log_mod
  dsm_mod_fit <- fits$dsm_mod

  # extract relevant data
  pars <- data$pars
  prob <- data$prob
  data <- data$data

  # extract parameters of interest from models
  log_fit_par <- log_mod_fit$summary(variables = c("b_0", "b_x"))
  dsm_fit_par <- dsm_mod_fit$summary(variables = c("b_0", "b_x", "sens", "spec"))

  # extract model diagnostic predictions
  dxp <- data[, c("y_err", "y_tru")]
  dxp$log_pred <- log_mod_fit$summary(variables = "pop", mean)$mean
  dxp$dsm_pred <- dsm_mod_fit$summary(variables = "pop", mean)$mean

  # compute relevant results
  out_all_but_coef <-
    c(
      acc_tru_con = pars[3 + P * 2] * mean(dxp$y_tru) + pars[4 + P * 2] * (1 - mean(dxp$y_tru)),
      acc_log_tru = sum(diag(table(round(dxp$log_pred), dxp$y_tru))) / N,
      acc_dsm_tru = sum(diag(table(round(dxp$dsm_pred), dxp$y_tru))) / N,
      acc_log_err = sum(diag(table(round(dxp$log_pred), dxp$y_err))) / N,
      acc_dsm_err = sum(diag(table(round(dxp$dsm_pred), dxp$y_err))) / N,
      auc_tru_tru = auc(dxp$y_tru ~ prob, direction = "<", quiet = TRUE)[1],
      auc_log_tru = auc(y_tru ~ log_pred, data = dxp, direction = "<", quiet = TRUE)[1],
      auc_dsm_tru = auc(y_tru ~ dsm_pred, data = dxp, direction = "<", quiet = TRUE)[1],
      auc_tru_err = auc(dxp$y_err ~ prob, direction = "<", quiet = TRUE)[1],
      auc_log_err = auc(y_err ~ log_pred, data = dxp, direction = "<", quiet = TRUE)[1],
      auc_dsm_err = auc(y_err ~ dsm_pred, data = dxp, direction = "<", quiet = TRUE)[1],
      sens_cond   = pars[3 + P * 2],
      sens_dsm    = dsm_fit_par[[2 + P, 2]],
      sens_cil    = dsm_fit_par[[2 + P, 6]],
      sens_ciu    = dsm_fit_par[[2 + P, 7]],
      spec_cond   = pars[4 + P * 2],
      spec_dsm    = dsm_fit_par[[3 + P, 2]],
      spec_cil    = dsm_fit_par[[3 + P, 6]],
      spec_ciu    = dsm_fit_par[[3 + P, 7]],
      int_tru_raw = pars[1],
      int_tru_ste = pars[2 + P],
      int_log_raw = log_fit_par[[1, 2]],
      int_log_ste = log_fit_par[[1, 4]],
      int_log_cil = log_fit_par[[1, 6]],
      int_log_ciu = log_fit_par[[1, 7]],
      int_dsm_raw = dsm_fit_par[[1, 2]],
      int_dsm_ste = dsm_fit_par[[1, 4]],
      int_dsm_cil = dsm_fit_par[[1, 6]],
      int_dsm_ciu = dsm_fit_par[[1, 7]]
    )

  out_just_coef <-
    c(
      pars[2:(P + 1)],
      pars[(P + 3):(2 + P * 2)],
      log_fit_par[2:(P + 1), 2][[1]],
      log_fit_par[2:(P + 1), 4][[1]],
      log_fit_par[2:(P + 1), 6][[1]],
      log_fit_par[2:(P + 1), 7][[1]],
      dsm_fit_par[2:(P + 1), 2][[1]],
      dsm_fit_par[2:(P + 1), 4][[1]],
      dsm_fit_par[2:(P + 1), 6][[1]],
      dsm_fit_par[2:(P + 1), 7][[1]]
    )

  names(out_just_coef) <- c(paste("coef", 1:P, "_tru_raw", sep = ""),
                            paste("coef", 1:P, "_tru_ste", sep = ""),
                            paste("coef", 1:P, "_log_raw", sep = ""),
                            paste("coef", 1:P, "_log_ste", sep = ""),
                            paste("coef", 1:P, "_log_cil", sep = ""),
                            paste("coef", 1:P, "_log_ciu", sep = ""),
                            paste("coef", 1:P, "_dsm_raw", sep = ""),
                            paste("coef", 1:P, "_dsm_ste", sep = ""),
                            paste("coef", 1:P, "_dsm_cil", sep = ""),
                            paste("coef", 1:P, "_dsm_ciu", sep = ""))

  # combine results into a final dataframe
  out <- c(
    out_all_but_coef,
    out_just_coef
  )

  # return the dataframe
  return(out)

}

#define function to knit all results together
simRun <- function(N, P, Sn_m, Sp_m) {

  # simulate data
  data <- simDat(N = N, P = P, Sn_m = Sn_m, Sp_m = Sp_m)

  # fit models
  fits <- simFit(N = N, P = P, data = data)

  # extract results
  res <- simRes(fits = fits, data = data, N = N, P = P)

  # add results to simulation conditions
  cbind.data.frame(t(res))

}
