#
# APPLY MODEL ON NACC DATA FOR CASE STUDY
#


#* This script file assumes that the set-up script has already been run.
#* The various model syntax and functions needed for this particular script
#* script are available in the corresponding set-up script file.



# Prepare environment -----------------------------------------------------

#load needed libraries
library(haven)     #used for reading in .sav file from NACC
library(broom)     #helper functions for plots


#read in data
df <- read_sav(file = "investigator_naccIIV.sav")



# Update Models for Fit Statistics and Post-Hocs --------------------------

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

  // Bayesian R^2 statistic
  vector[N] prob = inv_logit(b_0_c + x_c * t_x);
  real<lower=0, upper=1> BR2 = variance(prob) / (variance(prob) + mean(prob .* (1 - prob)));

  // log-likelihood
  vector[N] log_lik;
  for ( n in 1 : N )
    log_lik[n] = bernoulli_logit_lpmf(y[n] | b_0_c + x_c[n, ] * t_x);

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
  theta ~ chi_square(3);
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

  // Bayesian R^2 statistic
  real<lower=0, upper=1> BR2 = variance(prob) / (variance(prob) + mean(prob .* (1 - prob)));

  // log-likelihood
  vector[N] log_lik;
  for ( n in 1 : N )
    log_lik[n] = log_sum_exp(bernoulli_lpmf(    y[n] | sens) + bernoulli_logit_lpmf(1 | logit_z_hat[n]),
                             bernoulli_lpmf(1 - y[n] | spec) + bernoulli_logit_lpmf(0 | logit_z_hat[n]));

}
"

dsm_mod_fun <- write_stan_file(dsm_mod)
dsm_mod_fun <- cmdstan_model(dsm_mod_fun, stanc_options = list("O1"))


# Scenario 1: Alzheimer's Disease Diagnoses -----------------------

#narrow dataset down to relevant cases
df_AD <- df %>%
  filter(
    NPADNC == 0 | NPADNC == 3
    ) %>%
  filter(
    NORMCOG.1 == 1 | NACCALZP.1 == 1
  ) %>%
  select(NACCAGE.1, EDUC, NACCNIHR, SEX, HISPANIC, NACCAPOE, NORMCOG.1, NPADNC, DIGIF.1, DIGIB.1, WAIS.1, TRAILA.1, TRAILB.1, ANIMALS.1, VEG.1, BOSTON.1, LOGIMEM.1, MEMUNITS.1, MEMTIME.1) %>%
  drop_na() %>%
  rename_at(
    1:19,
    ~ c("Age", "Edu", "Race", "Sex", "Ethnicity", "APOE", "Dx", "Pathology", "DigiF", "DigiB", "WAIS", "TMTA", "TMTB", "Anim", "Vege", "BNT", "LMI", "LMII", "Delay")
    ) %>%
  mutate(
    Race = factor(ifelse(Race == 1, 0, 1), levels = c(0, 1), labels = c("White", "Non-White")),
    Ethnicity = factor(ifelse(Ethnicity == 1, 1, ifelse(Ethnicity == 0, 0, NA)), levels = c(0, 1), labels = c("Non-Hispanic", "Hispanic")),
    APOE = ifelse(APOE %in% c(2, 5), 1, ifelse(APOE == 4, 2, 0)),
    Impaired = factor(ifelse(Dx == 1, "Normal", "Abnormal"), levels = c("Normal", "Abnormal"), labels = c("Normal", "Abnormal")),
    DigiF.z = (DigiF - (7.49757817 + (0.09211753*Sex)  + (-0.02061972*Age) + (0.16761972*Edu))) / 1.991555,
    DigiB.z = (DigiB - (5.42325112 + (-0.21758741*Sex) + (-0.02213182*Age) + (0.20484328*Edu))) / 2.081026,
    WAIS.z  = (WAIS  - (71.2260987 + (-3.1523639*Sex) + (-0.5580261*Age) + (1.123672*Edu))) / 10.47032,
    TMTA.z  = (TMTA  - (9.7995236  + (-0.327358*Sex)  + (0.5602899*Age)  + (-1.0250806*Edu))) / 13.91769 * -1,
    TMTB.z  = (TMTB  - (43.216988  + (-2.948423*Sex)  + (1.700573*Age)   + (-4.884626*Edu))) / 44.42661 * -1,
    Anim.z  = (Anim  - (20.65774665 + (0.00454905*Sex) + (-0.12844788*Age) + (0.56508779*Edu))) / 5.210138,
    Vege.z  = (Vege  - (17.88141662 + (-2.946307*Sex)  + (-0.08972787*Age) + (0.28194984*Edu))) / 3.993738,
    BNT.z   = (BNT   - (25.49287807 + (0.61686473*Sex) + (-0.05142736*Age) + (0.33669818*Edu))) / 2.992948,
    LMI.z   = (LMI   - (10.2246123  + (-1.2134844*Sex) + (-0.0229675*Age)  + (0.3713205*Edu))) / 3.727395,
    LMII.z  = (LMII  - (9.1318338 + (-1.38710825*Sex) + (-0.02623125*Age) + (0.40779118*Delay) + (-0.02285031*Edu))) / 4.058524
  ) %>%
  mutate(
    MahalDist = mahalanobis(.[, 21:30], rep(0, 10), cov(.[.$Impaired == "Normal", 21:30])),
    TotalIIV  = apply(.[, 21:30], 1, function(x) sqrt(0.9) * sd(x)),
    EFAS_IIV  = apply(.[, 21:25], 1, function(x) sqrt(4/5) * sd(x)),
    Lang_IIV  = apply(.[, 26:28], 1, function(x) sqrt(2/3) * sd(x)),
    Mems_IIV  = apply(.[, 29:30], 1, function(x) sqrt(1/2) * sd(x)),
    Standard  = factor(ifelse(Pathology == 0, "Normal", "AD"), levels = c("Normal", "AD"), labels = c("Normal", "AD"))
  )

#create data for Stan models
AD_stan_dat_std <- list(N = nrow(df_AD), P = 16, y = as.numeric(df_AD$Impaired) - 1, x = df_AD[, c(1:6, 21:30)])
AD_stan_dat_TIV <- list(N = nrow(df_AD), P = 8,  y = as.numeric(df_AD$Impaired) - 1, x = df_AD[, c(1:6, 31:32)])
AD_stan_dat_DIV <- list(N = nrow(df_AD), P = 10, y = as.numeric(df_AD$Impaired) - 1, x = df_AD[, c(1:6, 31, 33:35)])

#run Stan models
log_mod_AD_std <-
  log_mod_fun$sample(data = AD_stan_dat_std,
                     chains = 4, parallel_chains = 4,
                     refresh = 0, seed = SEED)

dsm_mod_AD_std <-
  dsm_mod_fun$sample(data = AD_stan_dat_std,
                     chains = 4, parallel_chains = 4,
                     refresh = 0, seed = SEED)

log_mod_AD_TIV <-
  log_mod_fun$sample(data = AD_stan_dat_TIV,
                     chains = 4, parallel_chains = 4,
                     refresh = 0, seed = SEED)

dsm_mod_AD_TIV <-
  dsm_mod_fun$sample(data = AD_stan_dat_TIV,
                     chains = 4, parallel_chains = 4,
                     refresh = 0, seed = SEED)

log_mod_AD_DIV <-
  log_mod_fun$sample(data = AD_stan_dat_DIV,
                     chains = 4, parallel_chains = 4,
                     refresh = 0, seed = SEED)

dsm_mod_AD_DIV <-
  dsm_mod_fun$sample(data = AD_stan_dat_DIV,
                     chains = 4, parallel_chains = 4,
                     refresh = 0, seed = SEED)

#extract accuracy results for diagnoses with error
sum(diag(table(round(log_mod_AD_std$summary(variables = "pop", mean)$mean),
               df_AD$Impaired))) / nrow(df_AD)
sum(diag(table(round(dsm_mod_AD_std$summary(variables = "pop", mean)$mean),
               df_AD$Impaired))) / nrow(df_AD)

sum(diag(table(round(log_mod_AD_TIV$summary(variables = "pop", mean)$mean),
               df_AD$Impaired))) / nrow(df_AD)
sum(diag(table(round(dsm_mod_AD_TIV$summary(variables = "pop", mean)$mean),
               df_AD$Impaired))) / nrow(df_AD)

sum(diag(table(round(log_mod_AD_DIV$summary(variables = "pop", mean)$mean),
               df_AD$Impaired))) / nrow(df_AD)
sum(diag(table(round(dsm_mod_AD_DIV$summary(variables = "pop", mean)$mean),
               df_AD$Impaired))) / nrow(df_AD)

#extract accuracy results for gold-standard diagnoses
sum(diag(table(round(log_mod_AD_std$summary(variables = "pop", mean)$mean),
               df_AD$Standard))) / nrow(df_AD)
sum(diag(table(round(dsm_mod_AD_std$summary(variables = "pop", mean)$mean),
               df_AD$Standard))) / nrow(df_AD)

sum(diag(table(round(log_mod_AD_TIV$summary(variables = "pop", mean)$mean),
               df_AD$Standard))) / nrow(df_AD)
sum(diag(table(round(dsm_mod_AD_TIV$summary(variables = "pop", mean)$mean),
               df_AD$Standard))) / nrow(df_AD)

sum(diag(table(round(log_mod_AD_DIV$summary(variables = "pop", mean)$mean),
               df_AD$Standard))) / nrow(df_AD)
sum(diag(table(round(dsm_mod_AD_DIV$summary(variables = "pop", mean)$mean),
               df_AD$Standard))) / nrow(df_AD)

#ensure all of cmdstanr files are saved for supplemental information document
log_mod_AD_std$draws(variables = c("b_x", "b_0", "t_x", "b_0_c", "pop", "BR2"))
log_mod_AD_TIV$draws(variables = c("b_x", "b_0", "t_x", "b_0_c", "pop", "BR2"))
log_mod_AD_DIV$draws(variables = c("b_x", "b_0", "t_x", "b_0_c", "pop", "BR2"))
dsm_mod_AD_std$draws(variables = c("b_x", "b_0", "t_x", "b_0_c", "pop", "BR2", "sens", "spec"))
dsm_mod_AD_TIV$draws(variables = c("b_x", "b_0", "t_x", "b_0_c", "pop", "BR2", "sens", "spec"))
dsm_mod_AD_DIV$draws(variables = c("b_x", "b_0", "t_x", "b_0_c", "pop", "BR2", "sens", "spec"))



# Scenario 2: MCI Diagnoses -----------------------

#narrow dataset down to relevant cases
df_MCI <- df %>%
  filter(
    NPADNC == 0 | (NACCIDEM == 1 & NPADNC == 3)
  ) %>%
  filter(
    NORMCOG.1 == 1 | NACCUDSD.1 == 3
  ) %>%
  select(NACCAGE.1, EDUC, NACCNIHR, SEX, HISPANIC, NACCAPOE, NORMCOG.1, NPADNC, DIGIF.1, DIGIB.1, WAIS.1, TRAILA.1, TRAILB.1, ANIMALS.1, VEG.1, BOSTON.1, LOGIMEM.1, MEMUNITS.1, MEMTIME.1) %>%
  drop_na() %>%
  rename_at(
    1:19,
    ~ c("Age", "Edu", "Race", "Sex", "Ethnicity", "APOE", "Dx", "Pathology", "DigiF", "DigiB", "WAIS", "TMTA", "TMTB", "Anim", "Vege", "BNT", "LMI", "LMII", "Delay")
  ) %>%
  mutate(
    Race = factor(ifelse(Race == 1, 0, 1), levels = c(0, 1), labels = c("White", "Non-White")),
    Ethnicity = factor(ifelse(Ethnicity == 1, 1, ifelse(Ethnicity == 0, 0, NA)), levels = c(0, 1), labels = c("Non-Hispanic", "Hispanic")),
    APOE = ifelse(APOE %in% c(2, 5), 1, ifelse(APOE == 4, 2, 0)),
    Impaired = factor(ifelse(Dx == 1, "Normal", "Abnormal"), levels = c("Normal", "Abnormal"), labels = c("Normal", "Abnormal")),
    DigiF.z = (DigiF - (7.49757817 + (0.09211753*Sex)  + (-0.02061972*Age) + (0.16761972*Edu))) / 1.991555,
    DigiB.z = (DigiB - (5.42325112 + (-0.21758741*Sex) + (-0.02213182*Age) + (0.20484328*Edu))) / 2.081026,
    WAIS.z  = (WAIS  - (71.2260987 + (-3.1523639*Sex) + (-0.5580261*Age) + (1.123672*Edu))) / 10.47032,
    TMTA.z  = (TMTA  - (9.7995236  + (-0.327358*Sex)  + (0.5602899*Age)  + (-1.0250806*Edu))) / 13.91769 * -1,
    TMTB.z  = (TMTB  - (43.216988  + (-2.948423*Sex)  + (1.700573*Age)   + (-4.884626*Edu))) / 44.42661 * -1,
    Anim.z  = (Anim  - (20.65774665 + (0.00454905*Sex) + (-0.12844788*Age) + (0.56508779*Edu))) / 5.210138,
    Vege.z  = (Vege  - (17.88141662 + (-2.946307*Sex)  + (-0.08972787*Age) + (0.28194984*Edu))) / 3.993738,
    BNT.z   = (BNT   - (25.49287807 + (0.61686473*Sex) + (-0.05142736*Age) + (0.33669818*Edu))) / 2.992948,
    LMI.z   = (LMI   - (10.2246123  + (-1.2134844*Sex) + (-0.0229675*Age)  + (0.3713205*Edu))) / 3.727395,
    LMII.z  = (LMII  - (9.1318338 + (-1.38710825*Sex) + (-0.02623125*Age) + (0.40779118*Delay) + (-0.02285031*Edu))) / 4.058524
  ) %>%
  mutate(
    MahalDist = mahalanobis(.[, 21:30], rep(0, 10), cov(.[.$Impaired == "Normal", 21:30])),
    TotalIIV  = apply(.[, 21:30], 1, function(x) sqrt(0.9) * sd(x)),
    EFAS_IIV  = apply(.[, 21:25], 1, function(x) sqrt(4/5) * sd(x)),
    Lang_IIV  = apply(.[, 26:28], 1, function(x) sqrt(2/3) * sd(x)),
    Mems_IIV  = apply(.[, 29:30], 1, function(x) sqrt(1/2) * sd(x))
  )

#create data for Stan models
MCI_stan_dat_std <- list(N = nrow(df_MCI), P = 16, y = as.numeric(df_MCI$Impaired) - 1, x = df_MCI[, c(1:6, 21:30)])
MCI_stan_dat_TIV <- list(N = nrow(df_MCI), P = 8, y = as.numeric(df_MCI$Impaired) - 1, x = df_MCI[, c(1:6, 31:32)])
MCI_stan_dat_DIV <- list(N = nrow(df_MCI), P = 10, y = as.numeric(df_MCI$Impaired) - 1, x = df_MCI[, c(1:6, 31, 33:35)])

#run Stan models
log_mod_MCI_std <-
  log_mod_fun$sample(data = MCI_stan_dat_std,
                     chains = 4, parallel_chains = 4,
                     refresh = 0, seed = SEED)

dsm_mod_MCI_std <-
  dsm_mod_fun$sample(data = MCI_stan_dat_std,
                     chains = 4, parallel_chains = 4,
                     refresh = 0, seed = SEED)

log_mod_MCI_TIV <-
  log_mod_fun$sample(data = MCI_stan_dat_TIV,
                     chains = 4, parallel_chains = 4,
                     refresh = 0, seed = SEED)

dsm_mod_MCI_TIV <-
  dsm_mod_fun$sample(data = MCI_stan_dat_TIV,
                     chains = 4, parallel_chains = 4,
                     refresh = 0, seed = SEED)

log_mod_MCI_DIV <-
  log_mod_fun$sample(data = MCI_stan_dat_DIV,
                     chains = 4, parallel_chains = 4,
                     refresh = 0, seed = SEED)

dsm_mod_MCI_DIV <-
  dsm_mod_fun$sample(data = MCI_stan_dat_DIV,
                     chains = 4, parallel_chains = 4,
                     refresh = 0, seed = SEED)

#extract accuracy results for diagnoses with error
sum(diag(table(round(log_mod_MCI_std$summary(variables = "pop", mean)$mean),
               df_MCI$Impaired))) / nrow(df_MCI)
sum(diag(table(round(dsm_mod_MCI_std$summary(variables = "pop", mean)$mean),
               df_MCI$Impaired))) / nrow(df_MCI)

sum(diag(table(round(log_mod_MCI_TIV$summary(variables = "pop", mean)$mean),
               df_MCI$Impaired))) / nrow(df_MCI)
sum(diag(table(round(dsm_mod_MCI_TIV$summary(variables = "pop", mean)$mean),
               df_MCI$Impaired))) / nrow(df_MCI)

sum(diag(table(round(log_mod_MCI_DIV$summary(variables = "pop", mean)$mean),
               df_MCI$Impaired))) / nrow(df_MCI)
sum(diag(table(round(dsm_mod_MCI_DIV$summary(variables = "pop", mean)$mean),
               df_MCI$Impaired))) / nrow(df_MCI)

#extract accuracy results for gold-standard diagnoses
sum(diag(table(round(log_mod_MCI_std$summary(variables = "pop", mean)$mean),
               df_MCI$Pathology))) / nrow(df_MCI)
sum(diag(table(round(dsm_mod_MCI_std$summary(variables = "pop", mean)$mean),
               df_MCI$Pathology))) / nrow(df_MCI)

sum(diag(table(round(log_mod_MCI_TIV$summary(variables = "pop", mean)$mean),
               df_MCI$Pathology))) / nrow(df_MCI)
sum(diag(table(round(dsm_mod_MCI_TIV$summary(variables = "pop", mean)$mean),
               df_MCI$Pathology))) / nrow(df_MCI)

sum(diag(table(round(log_mod_MCI_DIV$summary(variables = "pop", mean)$mean),
               df_MCI$Pathology))) / nrow(df_MCI)
sum(diag(table(round(dsm_mod_MCI_DIV$summary(variables = "pop", mean)$mean),
               df_MCI$Pathology))) / nrow(df_MCI)

#ensure all of cmdstanr files are saved for supplemental information document
log_mod_MCI_std$draws(variables = c("b_x", "b_0", "t_x", "b_0_c", "pop", "BR2"))
log_mod_MCI_TIV$draws(variables = c("b_x", "b_0", "t_x", "b_0_c", "pop", "BR2"))
log_mod_MCI_DIV$draws(variables = c("b_x", "b_0", "t_x", "b_0_c", "pop", "BR2"))
dsm_mod_MCI_std$draws(variables = c("b_x", "b_0", "t_x", "b_0_c", "pop", "BR2", "sens", "spec"))
dsm_mod_MCI_TIV$draws(variables = c("b_x", "b_0", "t_x", "b_0_c", "pop", "BR2", "sens", "spec"))
dsm_mod_MCI_DIV$draws(variables = c("b_x", "b_0", "t_x", "b_0_c", "pop", "BR2", "sens", "spec"))

#compute marginal plots of "significant" variables
nd_MCIa <- data.frame(Age  = seq(35, 94, length.out = 1000),
                      Edu  = rep(mean(df_MCI$Edu), 1000),
                      Race = rep(mean(as.numeric(df_MCI$Race)), 1000),
                      Sex  = rep(mean(df_MCI$Sex), 1000),
                      Ethnicity = rep(mean(as.numeric(df_MCI$Ethnicity)), 1000),
                      APOE = rep(mean(df_MCI$APOE), 1000),
                      DigiF.z = rep(mean(df_MCI$DigiF.z), 1000),
                      DigiB.z = rep(mean(df_MCI$DigiB.z), 1000),
                      WAIS.z = rep(mean(df_MCI$WAIS.z), 1000),
                      TMTA.z = rep(mean(df_MCI$TMTA.z), 1000),
                      TMTB.z = rep(mean(df_MCI$TMTB.z), 1000),
                      Anim.z = rep(mean(df_MCI$Anim.z), 1000),
                      Vege.z = rep(mean(df_MCI$Vege.z), 1000),
                      BNT.z  = rep(mean(df_MCI$BNT.z), 1000),
                      LMI.z  = rep(mean(df_MCI$LMI.z), 1000),
                      LMII.z = rep(mean(df_MCI$LMII.z), 1000))

nd_MCIb <- data.frame(Age  = rep(mean(df_MCI$Age), 1000),
                      Edu  = rep(mean(df_MCI$Edu), 1000),
                      Race = rep(mean(as.numeric(df_MCI$Race)), 1000),
                      Sex  = rep(mean(df_MCI$Sex), 1000),
                      Ethnicity = rep(mean(as.numeric(df_MCI$Ethnicity)), 1000),
                      APOE = rep(mean(df_MCI$APOE), 1000),
                      DigiF.z = rep(mean(df_MCI$DigiF.z), 1000),
                      DigiB.z = rep(mean(df_MCI$DigiB.z), 1000),
                      WAIS.z = rep(mean(df_MCI$WAIS.z), 1000),
                      TMTA.z = rep(mean(df_MCI$TMTA.z), 1000),
                      TMTB.z = rep(mean(df_MCI$TMTB.z), 1000),
                      Anim.z = rep(mean(df_MCI$Anim.z), 1000),
                      Vege.z = rep(mean(df_MCI$Vege.z), 1000),
                      BNT.z  = seq(-5.5, 1.3, length.out = 1000),
                      LMI.z  = rep(mean(df_MCI$LMI.z), 1000),
                      LMII.z = rep(mean(df_MCI$LMII.z), 1000))

nd_MCIm <- data.frame(Age  = rep(mean(df_MCI$Age), 1000),
                      Edu  = rep(mean(df_MCI$Edu), 1000),
                      Race = rep(mean(as.numeric(df_MCI$Race)), 1000),
                      Sex  = rep(mean(df_MCI$Sex), 1000),
                      Ethnicity = rep(mean(as.numeric(df_MCI$Ethnicity)), 1000),
                      APOE = rep(mean(df_MCI$APOE), 1000),
                      DigiF.z = rep(mean(df_MCI$DigiF.z), 1000),
                      DigiB.z = rep(mean(df_MCI$DigiB.z), 1000),
                      WAIS.z = rep(mean(df_MCI$WAIS.z), 1000),
                      TMTA.z = rep(mean(df_MCI$TMTA.z), 1000),
                      TMTB.z = rep(mean(df_MCI$TMTB.z), 1000),
                      Anim.z = rep(mean(df_MCI$Anim.z), 1000),
                      Vege.z = rep(mean(df_MCI$Vege.z), 1000),
                      BNT.z  = rep(mean(df_MCI$BNT.z), 1000),
                      LMI.z  = rep(mean(df_MCI$LMI.z), 1000),
                      LMII.z = seq(-5, 2.25, length.out = 1000))

#* plot marginal effect for age ----
dsm_mod_MCI_std$draws(c("b_x"), format = "matrix") %>%
  .[, 1:16] %*% t(as.matrix(nd_MCIa)) %>%
  sweep(., 1, dsm_mod_MCI_std$draws(c("b_0"), format = "matrix"), FUN = "+") %>%
  summarise_draws() %>%
  select(c("mean", "q5", "q95")) %>%
  data.frame() %>%
  mutate_all(function(x) exp(x) / (1 + exp(x))) %>%
  bind_cols(nd_MCIa) %>%
  ggplot(aes(x = Age)) +
  geom_ribbon(aes(ymin = q5, ymax = q95),
              alpha = 1/2) +
  geom_line(
    aes(y = mean)
  ) +
  stat_slab(
    data = df_MCI %>% mutate(Impaired.n = as.numeric(Impaired) - 1),
    aes(
      y = Impaired.n,
      side = ifelse(Impaired.n == 1, "bottom", "top"),
      fill = Impaired, color = Impaired
    ),
    slab_type = "histogram",
    scale = 0.30,
    breaks = 25,
    size = 0.75
  ) +
  scale_fill_manual(
    "Diagnosis",
    values = c(alpha("#009E73", .7),
               alpha("#D55E00", .7))
  ) +
  scale_color_manual(
    "Diagnosis",
    values = c("#009E73", "#D55E00"),
    guide = "none"
  ) +
  scale_x_continuous(
    "Age (years)",
    breaks = seq(35, 94, 10),
    expand = c(0, 0),
    limits = c(35, 94)
  ) +
  scale_y_continuous(
    "Probability of Impairment",
    expand = c(0, 0),
    limits = 0:1
  ) +
  labs(
    title = "Marginal Effect of Age"
  ) +
  theme(
    panel.background   = element_rect(fill = NA, colour = "black", linetype = "solid", size = 1.5),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank(),
    plot.title = element_text(family = "serif", face = "bold", size = 16, hjust = 0.5),
    axis.text = element_text(family = "serif", face = "bold", size = 12),
    axis.title = element_text(family = "serif", face = "bold", size = 14),
    legend.position = "bottom",
    legend.box = "horizontal",
    legend.text = element_text(family = "serif", size = 14)
  ) +
  guides(
    fill = guide_legend(title = "Clinical Diagnosis", title.position = "top", title.hjust = 0.5,
                        title.theme = element_text(family = "serif", size = 14),
                        override.aes = list(color = c("#009E73", "#D55E00")))
  )

#* plot marginal effect for bnt ----
dsm_mod_MCI_std$draws(c("b_x"), format = "matrix") %>%
  .[, 1:16] %*% t(as.matrix(nd_MCIb)) %>%
  sweep(., 1, dsm_mod_MCI_std$draws(c("b_0"), format = "matrix"), FUN = "+") %>%
  summarise_draws() %>%
  select(c("mean", "q5", "q95")) %>%
  data.frame() %>%
  mutate_all(function(x) exp(x) / (1 + exp(x))) %>%
  bind_cols(nd_MCIb) %>%
  ggplot(aes(x = BNT.z)) +
  geom_ribbon(aes(ymin = q5, ymax = q95),
              alpha = 1/2) +
  geom_line(
    aes(y = mean)
  ) +
  stat_slab(
    data = df_MCI %>% mutate(Impaired.n = as.numeric(Impaired) - 1),
    aes(
      y = Impaired.n,
      side = ifelse(Impaired.n == 1, "bottom", "top"),
      fill = Impaired, color = Impaired
    ),
    slab_type = "histogram",
    scale = 0.30,
    breaks = 50,
    size = 0.75
  ) +
  scale_fill_manual(
    "Diagnosis",
    values = c(alpha("#009E73", .7),
               alpha("#D55E00", .7))
  ) +
  scale_color_manual(
    "Diagnosis",
    values = c("#009E73", "#D55E00"),
    guide = "none"
  ) +
  scale_x_continuous(
    "Boston Naming Test 30 Odd Item Standardized Score",
    breaks = seq(-5.5, 1.3, 1.5),
    expand = c(0, 0),
    limits = c(-5.5, 1.3)
  ) +
  scale_y_continuous(
    "Probability of Impairment",
    expand = c(0, 0),
    limits = 0:1
  ) +
  labs(
    title = "Marginal Effect of the Boston Naming Test"
  ) +
  theme(
    panel.background   = element_rect(fill = NA, colour = "black", linetype = "solid", size = 1.5),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank(),
    plot.title = element_text(family = "serif", face = "bold", size = 16, hjust = 0.5),
    axis.text = element_text(family = "serif", face = "bold", size = 12),
    axis.title = element_text(family = "serif", face = "bold", size = 14),
    legend.position = "bottom",
    legend.box = "horizontal",
    legend.text = element_text(family = "serif", size = 14)
  ) +
  guides(
    fill = guide_legend(title = "Clinical Diagnosis", title.position = "top", title.hjust = 0.5,
                        title.theme = element_text(family = "serif", size = 14),
                        override.aes = list(color = c("#009E73", "#D55E00")))
  )

#* plot marginal effect for lmii ----
dsm_mod_MCI_std$draws(c("b_x"), format = "matrix") %>%
  .[, 1:16] %*% t(as.matrix(nd_MCIm)) %>%
  sweep(., 1, dsm_mod_MCI_std$draws(c("b_0"), format = "matrix"), FUN = "+") %>%
  summarise_draws() %>%
  select(c("mean", "q5", "q95")) %>%
  data.frame() %>%
  mutate_all(function(x) exp(x) / (1 + exp(x))) %>%
  bind_cols(nd_MCIm) %>%
  ggplot(aes(x = LMII.z)) +
  geom_ribbon(aes(ymin = q5, ymax = q95),
              alpha = 1/2) +
  geom_line(
    aes(y = mean)
  ) +
  stat_slab(
    data = df_MCI %>% mutate(Impaired.n = as.numeric(Impaired) - 1),
    aes(
      y = Impaired.n,
      side = ifelse(Impaired.n == 1, "bottom", "top"),
      fill = Impaired, color = Impaired
    ),
    slab_type = "histogram",
    scale = 0.30,
    breaks = 50,
    size = 0.75
  ) +
  scale_fill_manual(
    "Diagnosis",
    values = c(alpha("#009E73", .7),
               alpha("#D55E00", .7))
  ) +
  scale_color_manual(
    "Diagnosis",
    values = c("#009E73", "#D55E00"),
    guide = "none"
  ) +
  scale_x_continuous(
    "Logical Memory II Standardized Score",
    breaks = seq(-5.0, 2.25, 1.25),
    expand = c(0, 0),
    limits = c(-5.0, 2.25)
  ) +
  scale_y_continuous(
    "Probability of Impairment",
    expand = c(0, 0),
    limits = 0:1
  ) +
  labs(
    title = "Marginal Effect of the Logical Memory II"
  ) +
  theme(
    panel.background   = element_rect(fill = NA, colour = "black", linetype = "solid", size = 1.5),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank(),
    plot.title = element_text(family = "serif", face = "bold", size = 16, hjust = 0.5),
    axis.text = element_text(family = "serif", face = "bold", size = 12),
    axis.title = element_text(family = "serif", face = "bold", size = 14),
    legend.position = "bottom",
    legend.box = "horizontal",
    legend.text = element_text(family = "serif", size = 14)
  ) +
  guides(
    fill = guide_legend(title = "Clinical Diagnosis", title.position = "top", title.hjust = 0.5,
                        title.theme = element_text(family = "serif", size = 14),
                        override.aes = list(color = c("#009E73", "#D55E00")))
  )
#* break code folding ----

#tidy environment
rm(df)
gc()
