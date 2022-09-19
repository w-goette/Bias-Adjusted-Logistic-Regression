#
# PREPARE AND VISUALIZE SIMULATION RESULTS
#


#* This script file assumes that the set-up and simulation scripts have
#* already been run. The various model syntax and functions needed for
#* this particular script script are available in the corresponding
#* set-up script file and then the simulation run script file.



# Plots of Overall Results ------------------------------------------------

#load packages
library(tidyverse)  #used for summarizing results
library(ggdist)     #used for nicer plots
library(SimDesign)  #convenience functions for simulation summaries

#plot simulations of sensitivity and specificity by condition
sns_spc_comb <- do.call("rbind", list(C_1P[ , c("sens_cond", "sens_dsm", "spec_cond", "spec_dsm", "Sensitivity", "Specificity", "SampleSize")],
                                      C_3P[ , c("sens_cond", "sens_dsm", "spec_cond", "spec_dsm", "Sensitivity", "Specificity", "SampleSize")],
                                      C_5P[ , c("sens_cond", "sens_dsm", "spec_cond", "spec_dsm", "Sensitivity", "Specificity", "SampleSize")],
                                      C_10P[, c("sens_cond", "sens_dsm", "spec_cond", "spec_dsm", "Sensitivity", "Specificity", "SampleSize")]))

sns_spc_comb$Parameters <- rep(c(1, 3, 5, 10), each = 12000)

sns_spc_comb$Sensitivity <- factor(sns_spc_comb$Sensitivity, levels = c(0.70, 0.75, 0.85), labels = c(0.70, 0.75, 0.85))
sns_spc_comb$Specificity <- factor(sns_spc_comb$Specificity, levels = c(0.45, 0.60, 0.70), labels = c(0.45, 0.60, 0.70))
sns_spc_comb$SampleSize  <- factor(sns_spc_comb$SampleSize , levels = c(100, 250, 500, 1000), labels = c(100, 250, 500, 1000))
sns_spc_comb$Parameters  <- factor(sns_spc_comb$Parameters , levels = c(1, 3, 5, 10), labels = c(1, 3, 5, 10))

labs_Pr <- c("Predictors: 1", "Predictors: 3", "Predictors: 5", "Predictors: 10")
labs_Ss <- c("N = 100", "N = 250", "N = 500", "N = 1000")
labs_Sn <- c("Sensitivity = 0.70", "Sensitivity = 0.75", "Sensitivity = 0.85")
labs_Sp <- c("Specificity = 0.45", "Specificity = 0.60", "Specificity = 0.70")
names(labs_Pr) <- c(1, 3, 5, 10)
names(labs_Ss) <- c(100, 250, 500, 1000)
names(labs_Sn) <- c(0.70, 0.75, 0.85)
names(labs_Sp) <- c(0.45, 0.60, 0.70)

#* sensitivity plot ----
sns_spc_comb %>%
  mutate(SampleSizeN = as.numeric(SampleSize)*2,
         SensitivityN = as.numeric(levels(Sensitivity))[Sensitivity]) %>%
  ggplot() +
  stat_eye(
    aes(x = SampleSizeN, y = sens_dsm, group = SampleSize, fill = 'Model-based Estimate'),
    side = "right",
    width = 1,
    point_interval = NULL,
    justification = -0.15,
    point_color = NA
  ) +
  geom_boxplot(
    aes(x = SampleSizeN, y = sens_dsm, group = SampleSize),
    width = 0.20,
    outlier.shape = NA,
    position = position_nudge(0.15),
    col = RColorBrewer::brewer.pal(3, "Dark2")[2],
    size = 1
  ) +
  stat_eye(
    aes(x = SampleSizeN, y = sens_cond, group = SampleSize, fill = 'Simulation Condition'),
    side = "left",
    width = 1,
    point_interval = NULL,
    justification = 1.15,
    point_color = NA
  ) +
  geom_boxplot(
    aes(x = SampleSizeN, y = sens_cond, group = SampleSize),
    width = 0.20,
    outlier.shape = NA,
    position = position_nudge(-0.15),
    col = RColorBrewer::brewer.pal(3, "Dark2")[1],
    size = 1
  ) +
  geom_hline(
    aes(yintercept = SensitivityN),
    linetype = 2,
    size = 0.5
  ) +
  scale_x_continuous(
    breaks = c(2, 4, 6, 8), labels = levels(sns_spc_comb$SampleSize),
    name = "Sample Size",
    minor_breaks = NULL
  ) +
  scale_y_continuous(
    n.breaks = 5
  ) +
  scale_fill_manual(
    name='Source of Sensitivity Values',
    breaks=c('Simulation Condition', 'Model-based Estimate'),
    values=c('Simulation Condition' = RColorBrewer::brewer.pal(3, "Set2")[1],
             'Model-based Estimate' = RColorBrewer::brewer.pal(3, "Set2")[2])
  ) +
  labs(
    x = "Sample Size",
    y = "Sensitivity",
    title = "Estimate Accuracy for Sensitivity"
  ) +
  theme(
    panel.background   = element_rect(fill = NA, colour = "black", linetype = "solid", size = 1.5),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank(),
    strip.background = element_rect(fill = "darkgrey", linetype = "solid", size = 1.5),
    plot.title = element_text(family = "serif", face = "bold", size = 16, hjust = 0.5),
    axis.text = element_text(family = "serif", face = "bold", size = 12),
    axis.title = element_text(family = "serif", face = "bold", size = 14),
    strip.text = element_text(family = "serif", face = "bold", size = 14, color = "white"),
    legend.position = "bottom",
    legend.box = "horizontal",
    legend.text = element_text(family = "serif", size = 14)
  ) +
  guides(
    fill = guide_legend(title = "Source of Sensitivity Values", title.position = "top", title.hjust = 0.5,
                        title.theme = element_text(family = "serif", size = 14))
  ) +
  facet_grid(
    Parameters ~ Sensitivity,
    labeller = labeller(
      Parameters = labs_Pr,
      Sensitivity = labs_Sn
    ))

#* specificity plot----
sns_spc_comb %>%
  mutate(SampleSizeN = as.numeric(SampleSize)*2,
         SpecificityN = as.numeric(levels(Specificity))[Specificity]) %>%
  ggplot() +
  stat_eye(
    aes(x = SampleSizeN, y = spec_dsm, group = SampleSize, fill = 'Model-based Estimate'),
    side = "right",
    width = 1,
    point_interval = NULL,
    justification = -0.15,
    point_color = NA
  ) +
  geom_boxplot(
    aes(x = SampleSizeN, y = spec_dsm, group = SampleSize),
    width = 0.20,
    outlier.shape = NA,
    position = position_nudge(0.15),
    col = RColorBrewer::brewer.pal(3, "Dark2")[2],
    size = 1
  ) +
  stat_eye(
    aes(x = SampleSizeN, y = spec_cond, group = SampleSize, fill = 'Simulation Condition'),
    side = "left",
    width = 1,
    point_interval = NULL,
    justification = 1.15,
    point_colour = NA,
    fill = RColorBrewer::brewer.pal(3, "Dark2")[1]
  ) +
  geom_boxplot(
    aes(x = SampleSizeN, y = spec_cond, group = SampleSize),
    width = 0.20,
    outlier.shape = NA,
    position = position_nudge(-0.15),
    col = RColorBrewer::brewer.pal(3, "Dark2")[1],
    size = 1
  ) +
  geom_hline(
    aes(yintercept = SpecificityN),
    linetype = 2,
    size = 0.5
  ) +
  scale_x_continuous(
    breaks = c(2, 4, 6, 8), labels = levels(sns_spc_comb$SampleSize),
    name = "Sample Size",
    minor_breaks = NULL
  ) +
  scale_y_continuous(
    n.breaks = 5
  ) +
  scale_fill_manual(
    name='Source of Specificity Values',
    breaks=c('Simulation Condition', 'Model-based Estimate'),
    values=c('Simulation Condition' = RColorBrewer::brewer.pal(3, "Set2")[1],
             'Model-based Estimate' = RColorBrewer::brewer.pal(3, "Set2")[2])
  ) +
  labs(
    x = "Sample Size",
    y = "Specificity",
    title = "Estimate Accuracy for Specificity"
  ) +
  theme(
    panel.background   = element_rect(fill = NA, colour = "black", linetype = "solid", size = 1.5),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank(),
    strip.background = element_rect(fill = "darkgrey", linetype = "solid", size = 1.5),
    plot.title = element_text(family = "serif", face = "bold", size = 16, hjust = 0.5),
    axis.text = element_text(family = "serif", face = "bold", size = 12),
    axis.title = element_text(family = "serif", face = "bold", size = 14),
    strip.text = element_text(family = "serif", face = "bold", size = 14, color = "white"),
    legend.position = "bottom",
    legend.box = "horizontal",
    legend.text = element_text(family = "serif", size = 14)
  ) +
  guides(
    fill = guide_legend(title = "Source of Specificity Values", title.position = "top", title.hjust = 0.5,
                        title.theme = element_text(family = "serif", size = 14))
  ) +
  facet_grid(
    Parameters ~ Specificity,
    labeller = labeller(
      Parameters = labs_Pr,
      Specificity = labs_Sp
    ))

#plot accuracy by simulation condition ----
acc_auc_comb <- do.call("rbind", list(C_1P[ , c("acc_tru_con", "acc_log_tru", "acc_dsm_tru", "auc_tru_tru", "auc_log_tru", "auc_dsm_tru", "SampleSize")],
                                      C_3P[ , c("acc_tru_con", "acc_log_tru", "acc_dsm_tru", "auc_tru_tru", "auc_log_tru", "auc_dsm_tru", "SampleSize")],
                                      C_5P[ , c("acc_tru_con", "acc_log_tru", "acc_dsm_tru", "auc_tru_tru", "auc_log_tru", "auc_dsm_tru", "SampleSize")],
                                      C_10P[, c("acc_tru_con", "acc_log_tru", "acc_dsm_tru", "auc_tru_tru", "auc_log_tru", "auc_dsm_tru", "SampleSize")]))

acc_auc_comb$Parameters <- rep(c(1, 3, 5, 10), each = 12000)

acc_auc_comb$SampleSize  <- factor(acc_auc_comb$SampleSize , levels = c(100, 250, 500, 1000), labels = c(100, 250, 500, 1000))
acc_auc_comb$Parameters  <- factor(acc_auc_comb$Parameters , levels = c(1, 3, 5, 10), labels = c(1, 3, 5, 10))

#* accuracy plot----
acc_auc_comb %>%
  mutate(SampleSizeN = as.numeric(SampleSize)*2) %>%
  group_by(SampleSize, Parameters) %>%
  mutate(mEA = mean(acc_tru_con)) %>%
  ungroup() %>%
  pivot_longer(cols = acc_tru_con:acc_dsm_tru) %>%
  mutate(name = factor(name, levels = c("acc_dsm_tru", "acc_log_tru", "acc_tru_con"),
                       labels = c("Adjusted Logistic", "Unadjusted Logistic", "Expected Accuracy"))) %>%
  ggplot() +
  stat_eye(
    aes(x = value, y = name, group = SampleSize, fill = name),
    side = "right",
    width = 0.6,
    point_interval = "mean_qi",
    alpha = 0.75
  ) +
  geom_vline(
    aes(xintercept = mEA),
    linetype = 2,
    size = 0.5
  ) +
  scale_x_continuous(
    n.breaks = 5
  ) +
  scale_fill_brewer(palette = "Set2") +
  scale_fill_discrete(breaks = c("Expected Accuracy", "Unadjusted Logistic", "Adjusted Logistic")) +
  labs(
    x = "Accuracy",
    y = NULL,
    title = "Obtained Diagnostic Accuracy from Simulations"
  ) +
  theme(
    panel.background   = element_rect(fill = NA, colour = "black", linetype = "solid", size = 1.5),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank(),
    strip.background = element_rect(fill = "darkgrey", linetype = "solid", size = 1.5),
    plot.title = element_text(family = "serif", face = "bold", size = 16, hjust = 0.5),
    axis.text.x = element_text(family = "serif", face = "bold", size = 12),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    axis.title = element_text(family = "serif", face = "bold", size = 14),
    strip.text = element_text(family = "serif", face = "bold", size = 14, color = "white"),
    legend.position = "bottom",
    legend.box = "horizontal",
    legend.text = element_text(family = "serif", size = 14)
  ) +
  guides(
    fill = guide_legend(title = "Model Used for Classification", title.position = "top", title.hjust = 0.5,
                        title.theme = element_text(family = "serif", size = 14),
                        override.aes = list(linetype = 0, shape = NA))
  ) +
  facet_grid(
    SampleSize~ Parameters,
    labeller = labeller(
      SampleSize = labs_Ss,
      Parameters = labs_Pr
    ))

#* auc plot ----
acc_auc_comb %>%
  mutate(SampleSizeN = as.numeric(SampleSize)*2) %>%
  group_by(SampleSize, Parameters) %>%
  mutate(mEA = mean(auc_tru_tru)) %>%
  ungroup() %>%
  pivot_longer(cols = auc_tru_tru:auc_dsm_tru) %>%
  mutate(name = factor(name, levels = c("auc_dsm_tru", "auc_log_tru", "auc_tru_tru"),
                       labels = c("Adjusted Logistic", "Unadjusted Logistic", "Expected AUROC"))) %>%
  ggplot() + aes(mEA) +
  geom_vline(
    aes(xintercept = mEA),
    linetype = 2,
    size = 0.5
  ) +
  stat_eye(
    aes(x = value, y = name, group = SampleSize, fill = name),
    side = "right",
    width = 0.6,
    point_interval = "mean_qi",
    alpha = 0.75
  ) +
  scale_x_continuous(
    n.breaks = 5
  ) +
  scale_fill_brewer(palette = "Set2") +
  scale_fill_discrete(breaks = c("Expected AUROC", "Unadjusted Logistic", "Adjusted Logistic")) +
  labs(
    x = "Area Under Receiver Operator Characteristic Curve",
    y = NULL,
    title = "Obtained Areas Under the Curve from Simulations"
  ) +
  theme(
    panel.background   = element_rect(fill = NA, colour = "black", linetype = "solid", size = 1.5),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank(),
    strip.background = element_rect(fill = "darkgrey", linetype = "solid", size = 1.5),
    plot.title = element_text(family = "serif", face = "bold", size = 16, hjust = 0.5),
    axis.text.x = element_text(family = "serif", face = "bold", size = 12),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    axis.title = element_text(family = "serif", face = "bold", size = 14),
    strip.text = element_text(family = "serif", face = "bold", size = 14, color = "white"),
    legend.position = "bottom",
    legend.box = "horizontal",
    legend.text = element_text(family = "serif", size = 14)
  ) +
  guides(
    fill = guide_legend(title = "Model Used for Classification", title.position = "top", title.hjust = 0.5,
                        title.theme = element_text(family = "serif", size = 14),
                        override.aes = list(linetype = 0, shape = NA))
  ) +
  facet_grid(
    SampleSize~ Parameters,
    labeller = labeller(
      SampleSize = labs_Ss,
      Parameters = labs_Pr
    ))

#* break code folding ----

#tidy environment
rm(acc_auc_comb, sns_spc_comb)



# Generate Simulation Outcome Statistics ----------------------------------

#extract summary statistics from simulations

#* one parameter conditions ----
C_1P_rec <-
  C_1P %>%
  group_by(SampleSize) %>%
  summarize(log_int_rms = RMSE(estimate = int_log_raw,
                               parameter = int_tru_raw),
            log_int_mae = MAE(estimate = int_log_raw,
                              parameter = int_tru_raw),
            log_int_bia = bias(estimate = int_log_raw,
                               parameter = int_tru_raw),
            log_int_ecr = ECR(CIs = cbind(int_log_cil, int_log_ciu),
                              parameter = int_tru_raw),
            log_int_wdt = ECR(CIs = cbind(int_log_cil, int_log_ciu),
                              parameter = int_tru_raw, CI_width = TRUE),

            dsm_int_rms = RMSE(estimate = int_dsm_raw,
                               parameter = int_tru_raw),
            dsm_int_mae = MAE(estimate = int_dsm_raw,
                              parameter = int_tru_raw),
            dsm_int_bia = bias(estimate = int_dsm_raw,
                               parameter = int_tru_raw),
            dsm_int_ecr = ECR(CIs = cbind(int_dsm_cil, int_dsm_ciu),
                              parameter = int_tru_raw),
            dsm_int_wdt = ECR(CIs = cbind(int_dsm_cil, int_dsm_ciu),
                              parameter = int_tru_raw, CI_width = TRUE),

            log_co1_rms = RMSE(estimate = coef1_log_raw,
                               parameter = coef1_tru_raw),
            log_co1_mae = MAE(estimate = coef1_log_raw,
                              parameter = coef1_tru_raw),
            log_co1_bia = bias(estimate = coef1_log_raw,
                               parameter = coef1_tru_raw),
            log_co1_ecr = ECR(CIs = cbind(coef1_log_cil, coef1_log_ciu),
                              parameter = coef1_tru_raw),
            log_co1_wdt = ECR(CIs = cbind(coef1_log_cil, coef1_log_ciu),
                              parameter = coef1_tru_raw, CI_width = TRUE),

            dsm_co1_rms = RMSE(estimate = coef1_dsm_raw,
                               parameter = coef1_tru_raw),
            dsm_co1_mae = MAE(estimate = coef1_dsm_raw,
                              parameter = coef1_tru_raw),
            dsm_co1_bia = bias(estimate = coef1_dsm_raw,
                               parameter = coef1_tru_raw),
            dsm_co1_ecr = ECR(CIs = cbind(coef1_dsm_cil, coef1_dsm_ciu),
                              parameter = coef1_tru_raw),
            dsm_co1_wdt = ECR(CIs = cbind(coef1_dsm_cil, coef1_dsm_ciu),
                              parameter = coef1_tru_raw, CI_width = TRUE),

            dsm_sen_rms = RMSE(estimate = sens_dsm,
                               parameter = sens_cond),
            dsm_sen_mae = MAE(estimate = sens_dsm,
                              parameter = sens_cond),
            dsm_sen_bia = bias(estimate = sens_dsm,
                               parameter = sens_cond),
            dsm_spc_rms = RMSE(estimate = spec_dsm,
                               parameter = spec_cond),
            dsm_spc_mae = MAE(estimate = spec_dsm,
                              parameter = spec_cond),
            dsm_spc_bia = bias(estimate = spec_dsm,
                               parameter = spec_cond))

#* three parameter conditions ----
C_3P_rec <-
  C_3P %>%
  group_by(SampleSize) %>%
  summarize(log_int_rms = RMSE(estimate = int_log_raw,
                               parameter = int_tru_raw),
            log_int_mae = MAE(estimate = int_log_raw,
                              parameter = int_tru_raw),
            log_int_bia = bias(estimate = int_log_raw,
                               parameter = int_tru_raw),
            log_int_ecr = ECR(CIs = cbind(int_log_cil, int_log_ciu),
                              parameter = int_tru_raw),
            log_int_wdt = ECR(CIs = cbind(int_log_cil, int_log_ciu),
                              parameter = int_tru_raw, CI_width = TRUE),

            dsm_int_rms = RMSE(estimate = int_dsm_raw,
                               parameter = int_tru_raw),
            dsm_int_mae = MAE(estimate = int_dsm_raw,
                              parameter = int_tru_raw),
            dsm_int_bia = bias(estimate = int_dsm_raw,
                               parameter = int_tru_raw),
            dsm_int_ecr = ECR(CIs = cbind(int_dsm_cil, int_dsm_ciu),
                              parameter = int_tru_raw),
            dsm_int_wdt = ECR(CIs = cbind(int_dsm_cil, int_dsm_ciu),
                              parameter = int_tru_raw, CI_width = TRUE),

            log_co1_rms = RMSE(estimate = coef1_log_raw,
                               parameter = coef1_tru_raw),
            log_co1_mae = MAE(estimate = coef1_log_raw,
                              parameter = coef1_tru_raw),
            log_co1_bia = bias(estimate = coef1_log_raw,
                               parameter = coef1_tru_raw),
            log_co1_ecr = ECR(CIs = cbind(coef1_log_cil, coef1_log_ciu),
                              parameter = coef1_tru_raw),
            log_co1_wdt = ECR(CIs = cbind(coef1_log_cil, coef1_log_ciu),
                              parameter = coef1_tru_raw, CI_width = TRUE),

            dsm_co1_rms = RMSE(estimate = coef1_dsm_raw,
                               parameter = coef1_tru_raw),
            dsm_co1_mae = MAE(estimate = coef1_dsm_raw,
                              parameter = coef1_tru_raw),
            dsm_co1_bia = bias(estimate = coef1_dsm_raw,
                               parameter = coef1_tru_raw),
            dsm_co1_ecr = ECR(CIs = cbind(coef1_dsm_cil, coef1_dsm_ciu),
                              parameter = coef1_tru_raw),
            dsm_co1_wdt = ECR(CIs = cbind(coef1_dsm_cil, coef1_dsm_ciu),
                              parameter = coef1_tru_raw, CI_width = TRUE),

            log_co2_rms = RMSE(estimate = coef2_log_raw,
                               parameter = coef2_tru_raw),
            log_co2_mae = MAE(estimate = coef2_log_raw,
                              parameter = coef2_tru_raw),
            log_co2_bia = bias(estimate = coef2_log_raw,
                               parameter = coef2_tru_raw),
            log_co2_ecr = ECR(CIs = cbind(coef2_log_cil, coef2_log_ciu),
                              parameter = coef2_tru_raw),
            log_co2_wdt = ECR(CIs = cbind(coef2_log_cil, coef2_log_ciu),
                              parameter = coef2_tru_raw, CI_width = TRUE),

            dsm_co2_rms = RMSE(estimate = coef2_dsm_raw,
                               parameter = coef2_tru_raw),
            dsm_co2_mae = MAE(estimate = coef2_dsm_raw,
                              parameter = coef2_tru_raw),
            dsm_co2_bia = bias(estimate = coef2_dsm_raw,
                               parameter = coef2_tru_raw),
            dsm_co2_ecr = ECR(CIs = cbind(coef2_dsm_cil, coef2_dsm_ciu),
                              parameter = coef2_tru_raw),
            dsm_co2_wdt = ECR(CIs = cbind(coef2_dsm_cil, coef2_dsm_ciu),
                              parameter = coef2_tru_raw, CI_width = TRUE),

            log_co3_rms = RMSE(estimate = coef3_log_raw,
                               parameter = coef3_tru_raw),
            log_co3_mae = MAE(estimate = coef3_log_raw,
                              parameter = coef3_tru_raw),
            log_co3_bia = bias(estimate = coef3_log_raw,
                               parameter = coef3_tru_raw),
            log_co3_ecr = ECR(CIs = cbind(coef3_log_cil, coef3_log_ciu),
                              parameter = coef3_tru_raw),
            log_co3_wdt = ECR(CIs = cbind(coef3_log_cil, coef3_log_ciu),
                              parameter = coef3_tru_raw, CI_width = TRUE),

            dsm_co3_rms = RMSE(estimate = coef3_dsm_raw,
                               parameter = coef3_tru_raw),
            dsm_co3_mae = MAE(estimate = coef3_dsm_raw,
                              parameter = coef3_tru_raw),
            dsm_co3_bia = bias(estimate = coef3_dsm_raw,
                               parameter = coef3_tru_raw),
            dsm_co3_ecr = ECR(CIs = cbind(coef3_dsm_cil, coef3_dsm_ciu),
                              parameter = coef3_tru_raw),
            dsm_co3_wdt = ECR(CIs = cbind(coef3_dsm_cil, coef3_dsm_ciu),
                              parameter = coef3_tru_raw, CI_width = TRUE),

            dsm_sen_rms = RMSE(estimate = sens_dsm,
                               parameter = sens_cond),
            dsm_sen_mae = MAE(estimate = sens_dsm,
                              parameter = sens_cond),
            dsm_sen_bia = bias(estimate = sens_dsm,
                               parameter = sens_cond),
            dsm_spc_rms = RMSE(estimate = spec_dsm,
                               parameter = spec_cond),
            dsm_spc_mae = MAE(estimate = spec_dsm,
                              parameter = spec_cond),
            dsm_spc_bia = bias(estimate = spec_dsm,
                               parameter = spec_cond))

#* five parameter conditions ----
C_5P_rec <-
  C_5P %>%
  group_by(SampleSize) %>%
  summarize(log_int_rms = RMSE(estimate = int_log_raw,
                               parameter = int_tru_raw),
            log_int_mae = MAE(estimate = int_log_raw,
                              parameter = int_tru_raw),
            log_int_bia = bias(estimate = int_log_raw,
                               parameter = int_tru_raw),
            log_int_ecr = ECR(CIs = cbind(int_log_cil, int_log_ciu),
                              parameter = int_tru_raw),
            log_int_wdt = ECR(CIs = cbind(int_log_cil, int_log_ciu),
                              parameter = int_tru_raw, CI_width = TRUE),

            dsm_int_rms = RMSE(estimate = int_dsm_raw,
                               parameter = int_tru_raw),
            dsm_int_mae = MAE(estimate = int_dsm_raw,
                              parameter = int_tru_raw),
            dsm_int_bia = bias(estimate = int_dsm_raw,
                               parameter = int_tru_raw),
            dsm_int_ecr = ECR(CIs = cbind(int_dsm_cil, int_dsm_ciu),
                              parameter = int_tru_raw),
            dsm_int_wdt = ECR(CIs = cbind(int_dsm_cil, int_dsm_ciu),
                              parameter = int_tru_raw, CI_width = TRUE),

            log_co1_rms = RMSE(estimate = coef1_log_raw,
                               parameter = coef1_tru_raw),
            log_co1_mae = MAE(estimate = coef1_log_raw,
                              parameter = coef1_tru_raw),
            log_co1_bia = bias(estimate = coef1_log_raw,
                               parameter = coef1_tru_raw),
            log_co1_ecr = ECR(CIs = cbind(coef1_log_cil, coef1_log_ciu),
                              parameter = coef1_tru_raw),
            log_co1_wdt = ECR(CIs = cbind(coef1_log_cil, coef1_log_ciu),
                              parameter = coef1_tru_raw, CI_width = TRUE),

            dsm_co1_rms = RMSE(estimate = coef1_dsm_raw,
                               parameter = coef1_tru_raw),
            dsm_co1_mae = MAE(estimate = coef1_dsm_raw,
                              parameter = coef1_tru_raw),
            dsm_co1_bia = bias(estimate = coef1_dsm_raw,
                               parameter = coef1_tru_raw),
            dsm_co1_ecr = ECR(CIs = cbind(coef1_dsm_cil, coef1_dsm_ciu),
                              parameter = coef1_tru_raw),
            dsm_co1_wdt = ECR(CIs = cbind(coef1_dsm_cil, coef1_dsm_ciu),
                              parameter = coef1_tru_raw, CI_width = TRUE),

            log_co2_rms = RMSE(estimate = coef2_log_raw,
                               parameter = coef2_tru_raw),
            log_co2_mae = MAE(estimate = coef2_log_raw,
                              parameter = coef2_tru_raw),
            log_co2_bia = bias(estimate = coef2_log_raw,
                               parameter = coef2_tru_raw),
            log_co2_ecr = ECR(CIs = cbind(coef2_log_cil, coef2_log_ciu),
                              parameter = coef2_tru_raw),
            log_co2_wdt = ECR(CIs = cbind(coef2_log_cil, coef2_log_ciu),
                              parameter = coef2_tru_raw, CI_width = TRUE),

            dsm_co2_rms = RMSE(estimate = coef2_dsm_raw,
                               parameter = coef2_tru_raw),
            dsm_co2_mae = MAE(estimate = coef2_dsm_raw,
                              parameter = coef2_tru_raw),
            dsm_co2_bia = bias(estimate = coef2_dsm_raw,
                               parameter = coef2_tru_raw),
            dsm_co2_ecr = ECR(CIs = cbind(coef2_dsm_cil, coef2_dsm_ciu),
                              parameter = coef2_tru_raw),
            dsm_co2_wdt = ECR(CIs = cbind(coef2_dsm_cil, coef2_dsm_ciu),
                              parameter = coef2_tru_raw, CI_width = TRUE),

            log_co3_rms = RMSE(estimate = coef3_log_raw,
                               parameter = coef3_tru_raw),
            log_co3_mae = MAE(estimate = coef3_log_raw,
                              parameter = coef3_tru_raw),
            log_co3_bia = bias(estimate = coef3_log_raw,
                               parameter = coef3_tru_raw),
            log_co3_ecr = ECR(CIs = cbind(coef3_log_cil, coef3_log_ciu),
                              parameter = coef3_tru_raw),
            log_co3_wdt = ECR(CIs = cbind(coef3_log_cil, coef3_log_ciu),
                              parameter = coef3_tru_raw, CI_width = TRUE),

            dsm_co3_rms = RMSE(estimate = coef3_dsm_raw,
                               parameter = coef3_tru_raw),
            dsm_co3_mae = MAE(estimate = coef3_dsm_raw,
                              parameter = coef3_tru_raw),
            dsm_co3_bia = bias(estimate = coef3_dsm_raw,
                               parameter = coef3_tru_raw),
            dsm_co3_ecr = ECR(CIs = cbind(coef3_dsm_cil, coef3_dsm_ciu),
                              parameter = coef3_tru_raw),
            dsm_co3_wdt = ECR(CIs = cbind(coef3_dsm_cil, coef3_dsm_ciu),
                              parameter = coef3_tru_raw, CI_width = TRUE),

            log_co4_rms = RMSE(estimate = coef4_log_raw,
                               parameter = coef4_tru_raw),
            log_co4_mae = MAE(estimate = coef4_log_raw,
                              parameter = coef4_tru_raw),
            log_co4_bia = bias(estimate = coef4_log_raw,
                               parameter = coef4_tru_raw),
            log_co4_ecr = ECR(CIs = cbind(coef4_log_cil, coef4_log_ciu),
                              parameter = coef4_tru_raw),
            log_co4_wdt = ECR(CIs = cbind(coef4_log_cil, coef4_log_ciu),
                              parameter = coef4_tru_raw, CI_width = TRUE),

            dsm_co4_rms = RMSE(estimate = coef4_dsm_raw,
                               parameter = coef4_tru_raw),
            dsm_co4_mae = MAE(estimate = coef4_dsm_raw,
                              parameter = coef4_tru_raw),
            dsm_co4_bia = bias(estimate = coef4_dsm_raw,
                               parameter = coef4_tru_raw),
            dsm_co4_ecr = ECR(CIs = cbind(coef4_dsm_cil, coef4_dsm_ciu),
                              parameter = coef4_tru_raw),
            dsm_co4_wdt = ECR(CIs = cbind(coef4_dsm_cil, coef4_dsm_ciu),
                              parameter = coef4_tru_raw, CI_width = TRUE),

            log_co5_rms = RMSE(estimate = coef5_log_raw,
                               parameter = coef5_tru_raw),
            log_co5_mae = MAE(estimate = coef5_log_raw,
                              parameter = coef5_tru_raw),
            log_co5_bia = bias(estimate = coef5_log_raw,
                               parameter = coef5_tru_raw),
            log_co5_ecr = ECR(CIs = cbind(coef5_log_cil, coef5_log_ciu),
                              parameter = coef5_tru_raw),
            log_co5_wdt = ECR(CIs = cbind(coef5_log_cil, coef5_log_ciu),
                              parameter = coef5_tru_raw, CI_width = TRUE),

            dsm_co5_rms = RMSE(estimate = coef5_dsm_raw,
                               parameter = coef5_tru_raw),
            dsm_co5_mae = MAE(estimate = coef5_dsm_raw,
                              parameter = coef5_tru_raw),
            dsm_co5_bia = bias(estimate = coef5_dsm_raw,
                               parameter = coef5_tru_raw),
            dsm_co5_ecr = ECR(CIs = cbind(coef5_dsm_cil, coef5_dsm_ciu),
                              parameter = coef5_tru_raw),
            dsm_co5_wdt = ECR(CIs = cbind(coef5_dsm_cil, coef5_dsm_ciu),
                              parameter = coef5_tru_raw, CI_width = TRUE),

            dsm_sen_rms = RMSE(estimate = sens_dsm,
                               parameter = sens_cond),
            dsm_sen_mae = MAE(estimate = sens_dsm,
                              parameter = sens_cond),
            dsm_sen_bia = bias(estimate = sens_dsm,
                               parameter = sens_cond),
            dsm_spc_rms = RMSE(estimate = spec_dsm,
                               parameter = spec_cond),
            dsm_spc_mae = MAE(estimate = spec_dsm,
                              parameter = spec_cond),
            dsm_spc_bia = bias(estimate = spec_dsm,
                               parameter = spec_cond))

#* ten parameter conditions ----
C_10P_rec <-
  C_10P %>%
  group_by(SampleSize) %>%
  summarize(log_int_rms = RMSE(estimate = int_log_raw,
                               parameter = int_tru_raw),
            log_int_mae = MAE(estimate = int_log_raw,
                              parameter = int_tru_raw),
            log_int_bia = bias(estimate = int_log_raw,
                               parameter = int_tru_raw),
            log_int_ecr = ECR(CIs = cbind(int_log_cil, int_log_ciu),
                              parameter = int_tru_raw),
            log_int_wdt = ECR(CIs = cbind(int_log_cil, int_log_ciu),
                              parameter = int_tru_raw, CI_width = TRUE),

            dsm_int_rms = RMSE(estimate = int_dsm_raw,
                               parameter = int_tru_raw),
            dsm_int_mae = MAE(estimate = int_dsm_raw,
                              parameter = int_tru_raw),
            dsm_int_bia = bias(estimate = int_dsm_raw,
                               parameter = int_tru_raw),
            dsm_int_ecr = ECR(CIs = cbind(int_dsm_cil, int_dsm_ciu),
                              parameter = int_tru_raw),
            dsm_int_wdt = ECR(CIs = cbind(int_dsm_cil, int_dsm_ciu),
                              parameter = int_tru_raw, CI_width = TRUE),

            log_co1_rms = RMSE(estimate = coef1_log_raw,
                               parameter = coef1_tru_raw),
            log_co1_mae = MAE(estimate = coef1_log_raw,
                              parameter = coef1_tru_raw),
            log_co1_bia = bias(estimate = coef1_log_raw,
                               parameter = coef1_tru_raw),
            log_co1_ecr = ECR(CIs = cbind(coef1_log_cil, coef1_log_ciu),
                              parameter = coef1_tru_raw),
            log_co1_wdt = ECR(CIs = cbind(coef1_log_cil, coef1_log_ciu),
                              parameter = coef1_tru_raw, CI_width = TRUE),

            dsm_co1_rms = RMSE(estimate = coef1_dsm_raw,
                               parameter = coef1_tru_raw),
            dsm_co1_mae = MAE(estimate = coef1_dsm_raw,
                              parameter = coef1_tru_raw),
            dsm_co1_bia = bias(estimate = coef1_dsm_raw,
                               parameter = coef1_tru_raw),
            dsm_co1_ecr = ECR(CIs = cbind(coef1_dsm_cil, coef1_dsm_ciu),
                              parameter = coef1_tru_raw),
            dsm_co1_wdt = ECR(CIs = cbind(coef1_dsm_cil, coef1_dsm_ciu),
                              parameter = coef1_tru_raw, CI_width = TRUE),

            log_co2_rms = RMSE(estimate = coef2_log_raw,
                               parameter = coef2_tru_raw),
            log_co2_mae = MAE(estimate = coef2_log_raw,
                              parameter = coef2_tru_raw),
            log_co2_bia = bias(estimate = coef2_log_raw,
                               parameter = coef2_tru_raw),
            log_co2_ecr = ECR(CIs = cbind(coef2_log_cil, coef2_log_ciu),
                              parameter = coef2_tru_raw),
            log_co2_wdt = ECR(CIs = cbind(coef2_log_cil, coef2_log_ciu),
                              parameter = coef2_tru_raw, CI_width = TRUE),

            dsm_co2_rms = RMSE(estimate = coef2_dsm_raw,
                               parameter = coef2_tru_raw),
            dsm_co2_mae = MAE(estimate = coef2_dsm_raw,
                              parameter = coef2_tru_raw),
            dsm_co2_bia = bias(estimate = coef2_dsm_raw,
                               parameter = coef2_tru_raw),
            dsm_co2_ecr = ECR(CIs = cbind(coef2_dsm_cil, coef2_dsm_ciu),
                              parameter = coef2_tru_raw),
            dsm_co2_wdt = ECR(CIs = cbind(coef2_dsm_cil, coef2_dsm_ciu),
                              parameter = coef2_tru_raw, CI_width = TRUE),

            log_co3_rms = RMSE(estimate = coef3_log_raw,
                               parameter = coef3_tru_raw),
            log_co3_mae = MAE(estimate = coef3_log_raw,
                              parameter = coef3_tru_raw),
            log_co3_bia = bias(estimate = coef3_log_raw,
                               parameter = coef3_tru_raw),
            log_co3_ecr = ECR(CIs = cbind(coef3_log_cil, coef3_log_ciu),
                              parameter = coef3_tru_raw),
            log_co3_wdt = ECR(CIs = cbind(coef3_log_cil, coef3_log_ciu),
                              parameter = coef3_tru_raw, CI_width = TRUE),

            dsm_co3_rms = RMSE(estimate = coef3_dsm_raw,
                               parameter = coef3_tru_raw),
            dsm_co3_mae = MAE(estimate = coef3_dsm_raw,
                              parameter = coef3_tru_raw),
            dsm_co3_bia = bias(estimate = coef3_dsm_raw,
                               parameter = coef3_tru_raw),
            dsm_co3_ecr = ECR(CIs = cbind(coef3_dsm_cil, coef3_dsm_ciu),
                              parameter = coef3_tru_raw),
            dsm_co3_wdt = ECR(CIs = cbind(coef3_dsm_cil, coef3_dsm_ciu),
                              parameter = coef3_tru_raw, CI_width = TRUE),

            log_co4_rms = RMSE(estimate = coef4_log_raw,
                               parameter = coef4_tru_raw),
            log_co4_mae = MAE(estimate = coef4_log_raw,
                              parameter = coef4_tru_raw),
            log_co4_bia = bias(estimate = coef4_log_raw,
                               parameter = coef4_tru_raw),
            log_co4_ecr = ECR(CIs = cbind(coef4_log_cil, coef4_log_ciu),
                              parameter = coef4_tru_raw),
            log_co4_wdt = ECR(CIs = cbind(coef4_log_cil, coef4_log_ciu),
                              parameter = coef4_tru_raw, CI_width = TRUE),

            dsm_co4_rms = RMSE(estimate = coef4_dsm_raw,
                               parameter = coef4_tru_raw),
            dsm_co4_mae = MAE(estimate = coef4_dsm_raw,
                              parameter = coef4_tru_raw),
            dsm_co4_bia = bias(estimate = coef4_dsm_raw,
                               parameter = coef4_tru_raw),
            dsm_co4_ecr = ECR(CIs = cbind(coef4_dsm_cil, coef4_dsm_ciu),
                              parameter = coef4_tru_raw),
            dsm_co4_wdt = ECR(CIs = cbind(coef4_dsm_cil, coef4_dsm_ciu),
                              parameter = coef4_tru_raw, CI_width = TRUE),

            log_co5_rms = RMSE(estimate = coef5_log_raw,
                               parameter = coef5_tru_raw),
            log_co5_mae = MAE(estimate = coef5_log_raw,
                              parameter = coef5_tru_raw),
            log_co5_bia = bias(estimate = coef5_log_raw,
                               parameter = coef5_tru_raw),
            log_co5_ecr = ECR(CIs = cbind(coef5_log_cil, coef5_log_ciu),
                              parameter = coef5_tru_raw),
            log_co5_wdt = ECR(CIs = cbind(coef5_log_cil, coef5_log_ciu),
                              parameter = coef5_tru_raw, CI_width = TRUE),

            dsm_co5_rms = RMSE(estimate = coef5_dsm_raw,
                               parameter = coef5_tru_raw),
            dsm_co5_mae = MAE(estimate = coef5_dsm_raw,
                              parameter = coef5_tru_raw),
            dsm_co5_bia = bias(estimate = coef5_dsm_raw,
                               parameter = coef5_tru_raw),
            dsm_co5_ecr = ECR(CIs = cbind(coef5_dsm_cil, coef5_dsm_ciu),
                              parameter = coef5_tru_raw),
            dsm_co5_wdt = ECR(CIs = cbind(coef5_dsm_cil, coef5_dsm_ciu),
                              parameter = coef5_tru_raw, CI_width = TRUE),

            log_co6_rms = RMSE(estimate = coef6_log_raw,
                               parameter = coef6_tru_raw),
            log_co6_mae = MAE(estimate = coef6_log_raw,
                              parameter = coef6_tru_raw),
            log_co6_bia = bias(estimate = coef6_log_raw,
                               parameter = coef6_tru_raw),
            log_co6_ecr = ECR(CIs = cbind(coef6_log_cil, coef6_log_ciu),
                              parameter = coef6_tru_raw),
            log_co6_wdt = ECR(CIs = cbind(coef6_log_cil, coef6_log_ciu),
                              parameter = coef6_tru_raw, CI_width = TRUE),

            dsm_co6_rms = RMSE(estimate = coef6_dsm_raw,
                               parameter = coef6_tru_raw),
            dsm_co6_mae = MAE(estimate = coef6_dsm_raw,
                              parameter = coef6_tru_raw),
            dsm_co6_bia = bias(estimate = coef6_dsm_raw,
                               parameter = coef6_tru_raw),
            dsm_co6_ecr = ECR(CIs = cbind(coef6_dsm_cil, coef6_dsm_ciu),
                              parameter = coef6_tru_raw),
            dsm_co6_wdt = ECR(CIs = cbind(coef6_dsm_cil, coef6_dsm_ciu),
                              parameter = coef6_tru_raw, CI_width = TRUE),

            log_co7_rms = RMSE(estimate = coef7_log_raw,
                               parameter = coef7_tru_raw),
            log_co7_mae = MAE(estimate = coef7_log_raw,
                              parameter = coef7_tru_raw),
            log_co7_bia = bias(estimate = coef7_log_raw,
                               parameter = coef7_tru_raw),
            log_co7_ecr = ECR(CIs = cbind(coef7_log_cil, coef7_log_ciu),
                              parameter = coef7_tru_raw),
            log_co7_wdt = ECR(CIs = cbind(coef7_log_cil, coef7_log_ciu),
                              parameter = coef7_tru_raw, CI_width = TRUE),

            dsm_co7_rms = RMSE(estimate = coef7_dsm_raw,
                               parameter = coef7_tru_raw),
            dsm_co7_mae = MAE(estimate = coef7_dsm_raw,
                              parameter = coef7_tru_raw),
            dsm_co7_bia = bias(estimate = coef7_dsm_raw,
                               parameter = coef7_tru_raw),
            dsm_co7_ecr = ECR(CIs = cbind(coef7_dsm_cil, coef7_dsm_ciu),
                              parameter = coef7_tru_raw),
            dsm_co7_wdt = ECR(CIs = cbind(coef7_dsm_cil, coef7_dsm_ciu),
                              parameter = coef7_tru_raw, CI_width = TRUE),

            log_co8_rms = RMSE(estimate = coef8_log_raw,
                               parameter = coef8_tru_raw),
            log_co8_mae = MAE(estimate = coef8_log_raw,
                              parameter = coef8_tru_raw),
            log_co8_bia = bias(estimate = coef8_log_raw,
                               parameter = coef8_tru_raw),
            log_co8_ecr = ECR(CIs = cbind(coef8_log_cil, coef8_log_ciu),
                              parameter = coef8_tru_raw),
            log_co8_wdt = ECR(CIs = cbind(coef8_log_cil, coef8_log_ciu),
                              parameter = coef8_tru_raw, CI_width = TRUE),

            dsm_co8_rms = RMSE(estimate = coef8_dsm_raw,
                               parameter = coef8_tru_raw),
            dsm_co8_mae = MAE(estimate = coef8_dsm_raw,
                              parameter = coef8_tru_raw),
            dsm_co8_bia = bias(estimate = coef8_dsm_raw,
                               parameter = coef8_tru_raw),
            dsm_co8_ecr = ECR(CIs = cbind(coef8_dsm_cil, coef8_dsm_ciu),
                              parameter = coef8_tru_raw),
            dsm_co8_wdt = ECR(CIs = cbind(coef8_dsm_cil, coef8_dsm_ciu),
                              parameter = coef8_tru_raw, CI_width = TRUE),

            log_co9_rms = RMSE(estimate = coef9_log_raw,
                               parameter = coef9_tru_raw),
            log_co9_mae = MAE(estimate = coef9_log_raw,
                              parameter = coef9_tru_raw),
            log_co9_bia = bias(estimate = coef9_log_raw,
                               parameter = coef9_tru_raw),
            log_co9_ecr = ECR(CIs = cbind(coef9_log_cil, coef9_log_ciu),
                              parameter = coef9_tru_raw),
            log_co9_wdt = ECR(CIs = cbind(coef9_log_cil, coef9_log_ciu),
                              parameter = coef9_tru_raw, CI_width = TRUE),

            dsm_co9_rms = RMSE(estimate = coef9_dsm_raw,
                               parameter = coef9_tru_raw),
            dsm_co9_mae = MAE(estimate = coef9_dsm_raw,
                              parameter = coef9_tru_raw),
            dsm_co9_bia = bias(estimate = coef9_dsm_raw,
                               parameter = coef9_tru_raw),
            dsm_co9_ecr = ECR(CIs = cbind(coef9_dsm_cil, coef9_dsm_ciu),
                              parameter = coef9_tru_raw),
            dsm_co9_wdt = ECR(CIs = cbind(coef9_dsm_cil, coef9_dsm_ciu),
                              parameter = coef9_tru_raw, CI_width = TRUE),

            log_co10_rms = RMSE(estimate = coef10_log_raw,
                               parameter = coef10_tru_raw),
            log_co10_mae = MAE(estimate = coef10_log_raw,
                              parameter = coef10_tru_raw),
            log_co10_bia = bias(estimate = coef10_log_raw,
                               parameter = coef10_tru_raw),
            log_co10_ecr = ECR(CIs = cbind(coef10_log_cil, coef10_log_ciu),
                              parameter = coef10_tru_raw),
            log_co10_wdt = ECR(CIs = cbind(coef10_log_cil, coef10_log_ciu),
                              parameter = coef10_tru_raw, CI_width = TRUE),

            dsm_co10_rms = RMSE(estimate = coef10_dsm_raw,
                               parameter = coef10_tru_raw),
            dsm_co10_mae = MAE(estimate = coef10_dsm_raw,
                              parameter = coef10_tru_raw),
            dsm_co10_bia = bias(estimate = coef10_dsm_raw,
                               parameter = coef10_tru_raw),
            dsm_co10_ecr = ECR(CIs = cbind(coef10_dsm_cil, coef10_dsm_ciu),
                              parameter = coef10_tru_raw),
            dsm_co10_wdt = ECR(CIs = cbind(coef10_dsm_cil, coef10_dsm_ciu),
                              parameter = coef10_tru_raw, CI_width = TRUE),

            dsm_sen_rms = RMSE(estimate = sens_dsm,
                               parameter = sens_cond),
            dsm_sen_mae = MAE(estimate = sens_dsm,
                              parameter = sens_cond),
            dsm_sen_bia = bias(estimate = sens_dsm,
                               parameter = sens_cond),
            dsm_spc_rms = RMSE(estimate = spec_dsm,
                               parameter = spec_cond),
            dsm_spc_mae = MAE(estimate = spec_dsm,
                              parameter = spec_cond),
            dsm_spc_bia = bias(estimate = spec_dsm,
                               parameter = spec_cond))
#* break code folding ----

#combine recovery results for convenience in plotting
C_3P_rec_simplified <- C_3P_rec
C_3P_rec_simplified <- C_3P_rec_simplified[, c(1:21, 42:47)]
for ( i in 12 : 16 )
  C_3P_rec_simplified[, i] <- rowMeans(C_3P_rec[, c(i, i+10, i+20)])
for ( i in 17 : 21 )
  C_3P_rec_simplified[, i] <- rowMeans(C_3P_rec[, c(i, i+10, i+20)])

C_5P_rec_simplified <- C_5P_rec
C_5P_rec_simplified <- C_5P_rec_simplified[, c(1:21, 62:67)]
for ( i in 12 : 16 )
  C_5P_rec_simplified[, i] <- rowMeans(C_5P_rec[, c(i, i+10, i+20, i+30, i+40)])
for ( i in 17 : 21 )
  C_5P_rec_simplified[, i] <- rowMeans(C_5P_rec[, c(i, i+10, i+20, i+30, i+40)])

C_10P_rec_simplified <- C_10P_rec
C_10P_rec_simplified <- C_10P_rec_simplified[, c(1:21, 112:117)]
for ( i in 12 : 16 )
  C_10P_rec_simplified[, i] <- rowMeans(C_10P_rec[, c(i, i+10, i+20, i+30, i+40, i+50, i+60, i+70, i+80, i+90)])
for ( i in 17 : 21 )
  C_10P_rec_simplified[, i] <- rowMeans(C_10P_rec[, c(i, i+10, i+20, i+30, i+40, i+50, i+60, i+70, i+80, i+90)])


Rec_res <- rbind(C_1P_rec,
                 C_3P_rec_simplified,
                 C_5P_rec_simplified,
                 C_10P_rec_simplified)

rm(C_3P_rec_simplified, C_5P_rec_simplified, C_10P_rec_simplified, i)

Rec_res$Parameters <- factor(rep(c(1, 3, 5, 10), each = 4), levels = c(1, 3, 5, 10),
                             labels = c("Predictors: 1", "Predictors: 3", "Predictors: 5", "Predictors: 10"))
Rec_res$SampleSize <- factor(Rec_res$SampleSize, levels = c(100, 250, 500, 1000),
                             labels = c("100", "250", "500", "1000"))
Rec_res <- Rec_res[, c(1, 28, 2:27)]

#* rmse and mae of intercepts plot ----
Rec_res %>%
  mutate(SampleSizeN = as.numeric(SampleSize)*2) %>%
  ggplot() +
  geom_point(
    aes(x = SampleSizeN, y = log_int_rms, color = 'Unadjusted Logistic', shape = 'Root Mean Square Error'),
    size = 3
  ) +
  geom_line(
    aes(x = SampleSizeN, y = log_int_rms, color = 'Unadjusted Logistic', linetype = 'Root Mean Square Error'),
    size = 1.15
  ) +
  geom_point(
    aes(x = SampleSizeN, y = log_int_mae, color = 'Unadjusted Logistic', shape = 'Mean Absolute Error'),
    size = 3
  ) +
  geom_line(
    aes(x = SampleSizeN, y = log_int_mae, color = 'Unadjusted Logistic', linetype = 'Mean Absolute Error'),
    size = 1.15
  ) +
  geom_point(
    aes(x = SampleSizeN, y = dsm_int_rms, color = 'Adjusted Logistic', shape = 'Root Mean Square Error'),
    size = 3
  ) +
  geom_line(
    aes(x = SampleSizeN, y = dsm_int_rms, color = 'Adjusted Logistic', linetype = 'Root Mean Square Error'),
    size = 1.15
  ) +
  geom_point(
    aes(x = SampleSizeN, y = dsm_int_mae, color = 'Adjusted Logistic', shape = 'Mean Absolute Error'),
    size = 3
  ) +
  geom_line(
    aes(x = SampleSizeN, y = dsm_int_mae, color = 'Adjusted Logistic', linetype = 'Mean Absolute Error'),
    size = 1.15
  ) +
  scale_x_continuous(
    breaks = c(2, 4, 6, 8), labels = levels(Rec_res$SampleSize),
    name = "Sample Size",
    minor_breaks = NULL
  ) +
  scale_y_continuous(
    n.breaks = 10
  ) +
  scale_color_manual(
    name='Model',
    breaks=c('Unadjusted Logistic', 'Adjusted Logistic'),
    values=c('Unadjusted Logistic' = RColorBrewer::brewer.pal(3, "Set2")[1],
             'Adjusted Logistic' = RColorBrewer::brewer.pal(3, "Set2")[2])
  ) +
  scale_linetype_manual(
    name = 'Error Type',
    breaks = c('Root Mean Square Error', 'Mean Absolute Error'),
    values = c('Root Mean Square Error' = 1,
               'Mean Absolute Error' = 2)
  ) +
  scale_shape_manual(
    name = 'Error Type',
    breaks = c('Root Mean Square Error', 'Mean Absolute Error'),
    values = c('Root Mean Square Error' = 16,
               'Mean Absolute Error' = 18)
  ) +
  labs(
    x = "Sample Size",
    y = "Error",
    title = "Intercepts: Root-Square and Mean Absolute Error"
  ) +
  theme(
    panel.background   = element_rect(fill = NA, colour = "black", linetype = "solid", size = 1.5),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank(),
    strip.background = element_rect(fill = "darkgrey", linetype = "solid", size = 1.5),
    plot.title = element_text(family = "serif", face = "bold", size = 16, hjust = 0.5),
    axis.text = element_text(family = "serif", face = "bold", size = 12),
    axis.title = element_text(family = "serif", face = "bold", size = 14),
    strip.text = element_text(family = "serif", face = "bold", size = 14, color = "white"),
    legend.position = "bottom",
    legend.box = "horizontal",
    legend.text = element_text(family = "serif", size = 14)
  ) +
  guides(
    color = guide_legend(title = "Model", title.position = "top", title.hjust = 0.5,
                         title.theme = element_text(family = "serif", size = 14),
                         override.aes = list(shape = NA)),
    shape = guide_legend(title = "Error Type", title.position = "top", title.hjust = 0.5,
                         title.theme = element_text(family = "serif", size = 14)),
    linetype = guide_legend(title = "Error Type", title.position = "top", title.hjust = 0.5,
                         title.theme = element_text(family = "serif", size = 14))
  ) +
  facet_grid(
    ~ Parameters
  )

#* rmse and mae of coefficients plot ----
Rec_res %>%
  mutate(SampleSizeN = as.numeric(SampleSize)*2) %>%
  ggplot() +
  geom_point(
    aes(x = SampleSizeN, y = log_co1_rms, color = 'Unadjusted Logistic', shape = 'Root Mean Square Error'),
    size = 3
  ) +
  geom_line(
    aes(x = SampleSizeN, y = log_co1_rms, color = 'Unadjusted Logistic', linetype = 'Root Mean Square Error'),
    size = 1.15
  ) +
  geom_point(
    aes(x = SampleSizeN, y = log_co1_mae, color = 'Unadjusted Logistic', shape = 'Mean Absolute Error'),
    size = 3
  ) +
  geom_line(
    aes(x = SampleSizeN, y = log_co1_mae, color = 'Unadjusted Logistic', linetype = 'Mean Absolute Error'),
    size = 1.15
  ) +
  geom_point(
    aes(x = SampleSizeN, y = dsm_co1_rms, color = 'Adjusted Logistic', shape = 'Root Mean Square Error'),
    size = 3
  ) +
  geom_line(
    aes(x = SampleSizeN, y = dsm_co1_rms, color = 'Adjusted Logistic', linetype = 'Root Mean Square Error'),
    size = 1.15
  ) +
  geom_point(
    aes(x = SampleSizeN, y = dsm_co1_mae, color = 'Adjusted Logistic', shape = 'Mean Absolute Error'),
    size = 3
  ) +
  geom_line(
    aes(x = SampleSizeN, y = dsm_co1_mae, color = 'Adjusted Logistic', linetype = 'Mean Absolute Error'),
    size = 1.15
  ) +
  scale_x_continuous(
    breaks = c(2, 4, 6, 8), labels = levels(Rec_res$SampleSize),
    name = "Sample Size",
    minor_breaks = NULL
  ) +
  scale_y_continuous(
    n.breaks = 10
  ) +
  scale_color_manual(
    name='Model',
    breaks=c('Unadjusted Logistic', 'Adjusted Logistic'),
    values=c('Unadjusted Logistic' = RColorBrewer::brewer.pal(3, "Set2")[1],
             'Adjusted Logistic' = RColorBrewer::brewer.pal(3, "Set2")[2])
  ) +
  scale_linetype_manual(
    name = 'Error Type',
    breaks = c('Root Mean Square Error', 'Mean Absolute Error'),
    values = c('Root Mean Square Error' = 1,
               'Mean Absolute Error' = 2)
  ) +
  scale_shape_manual(
    name = 'Error Type',
    breaks = c('Root Mean Square Error', 'Mean Absolute Error'),
    values = c('Root Mean Square Error' = 16,
               'Mean Absolute Error' = 18)
  ) +
  labs(
    x = "Sample Size",
    y = "Error",
    title = "Coefficients: Root-Square and Mean Absolute Error"
  ) +
  theme(
    panel.background   = element_rect(fill = NA, colour = "black", linetype = "solid", size = 1.5),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank(),
    strip.background = element_rect(fill = "darkgrey", linetype = "solid", size = 1.5),
    plot.title = element_text(family = "serif", face = "bold", size = 16, hjust = 0.5),
    axis.text = element_text(family = "serif", face = "bold", size = 12),
    axis.title = element_text(family = "serif", face = "bold", size = 14),
    strip.text = element_text(family = "serif", face = "bold", size = 14, color = "white"),
    legend.position = "bottom",
    legend.box = "horizontal",
    legend.text = element_text(family = "serif", size = 14)
  ) +
  guides(
    color = guide_legend(title = "Model", title.position = "top", title.hjust = 0.5,
                         title.theme = element_text(family = "serif", size = 14),
                         override.aes = list(shape = NA)),
    shape = guide_legend(title = "Error Type", title.position = "top", title.hjust = 0.5,
                         title.theme = element_text(family = "serif", size = 14)),
    linetype = guide_legend(title = "Error Type", title.position = "top", title.hjust = 0.5,
                            title.theme = element_text(family = "serif", size = 14))
  ) +
  facet_grid(
    ~ Parameters
  )

#* ecr of intercepts and coefficients plot ----
Rec_res %>%
  mutate(SampleSizeN = as.numeric(SampleSize)*2) %>%
  ggplot() +
  geom_point(
    aes(x = SampleSizeN, y = log_int_ecr, color = 'Unadjusted Logistic', shape = 'Intercept'),
    size = 3
  ) +
  geom_line(
    aes(x = SampleSizeN, y = log_int_ecr, color = 'Unadjusted Logistic', linetype = 'Intercept'),
    size = 1.15
  ) +
  geom_point(
    aes(x = SampleSizeN, y = log_co1_ecr, color = 'Unadjusted Logistic', shape = 'Coefficients'),
    size = 3
  ) +
  geom_line(
    aes(x = SampleSizeN, y = log_co1_ecr, color = 'Unadjusted Logistic', linetype = 'Coefficients'),
    size = 1.15
  ) +
  geom_point(
    aes(x = SampleSizeN, y = dsm_int_ecr, color = 'Adjusted Logistic', shape = 'Intercept'),
    size = 3
  ) +
  geom_line(
    aes(x = SampleSizeN, y = dsm_int_ecr, color = 'Adjusted Logistic', linetype = 'Intercept'),
    size = 1.15
  ) +
  geom_point(
    aes(x = SampleSizeN, y = dsm_co1_ecr, color = 'Adjusted Logistic', shape = 'Coefficients'),
    size = 3
  ) +
  geom_line(
    aes(x = SampleSizeN, y = dsm_co1_ecr, color = 'Adjusted Logistic', linetype = 'Coefficients'),
    size = 1.15
  ) +
  geom_hline(
    aes(yintercept = 0.95),
    linetype = 2,
    size = 0.5
  ) +
  scale_x_continuous(
    breaks = c(2, 4, 6, 8), labels = levels(Rec_res$SampleSize),
    name = "Sample Size",
    minor_breaks = NULL
  ) +
  scale_y_continuous(
    n.breaks = 10
  ) +
  scale_color_manual(
    name='Model',
    breaks=c('Unadjusted Logistic', 'Adjusted Logistic'),
    values=c('Unadjusted Logistic' = RColorBrewer::brewer.pal(3, "Set2")[1],
             'Adjusted Logistic' = RColorBrewer::brewer.pal(3, "Set2")[2])
  ) +
  scale_linetype_manual(
    name = 'Parameter',
    breaks = c('Intercept', 'Coefficients'),
    values = c('Intercept' = 1,
               'Coefficients' = 2)
  ) +
  scale_shape_manual(
    name = 'Parameter',
    breaks = c('Intercept', 'Coefficients'),
    values = c('Intercept' = 16,
               'Coefficients' = 18)
  ) +
  labs(
    x = "Sample Size",
    y = "Coverage Rate",
    title = "Empirical Coverage Rates of Parameters"
  ) +
  theme(
    panel.background   = element_rect(fill = NA, colour = "black", linetype = "solid", size = 1.5),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank(),
    strip.background = element_rect(fill = "darkgrey", linetype = "solid", size = 1.5),
    plot.title = element_text(family = "serif", face = "bold", size = 16, hjust = 0.5),
    axis.text = element_text(family = "serif", face = "bold", size = 12),
    axis.title = element_text(family = "serif", face = "bold", size = 14),
    strip.text = element_text(family = "serif", face = "bold", size = 14, color = "white"),
    legend.position = "bottom",
    legend.box = "horizontal",
    legend.text = element_text(family = "serif", size = 14)
  ) +
  guides(
    color = guide_legend(title = "Model", title.position = "top", title.hjust = 0.5,
                         title.theme = element_text(family = "serif", size = 14),
                         override.aes = list(shape = NA)),
    shape = guide_legend(title = "Parameter", title.position = "top", title.hjust = 0.5,
                         title.theme = element_text(family = "serif", size = 14)),
    linetype = guide_legend(title = "Parameter", title.position = "top", title.hjust = 0.5,
                            title.theme = element_text(family = "serif", size = 14))
  ) +
  facet_grid(
    ~ Parameters
  )

#* break code folding ----

#sign and magnitude errors

# function for convenience
snmErr <- function(resData, param = "coef1", groupSS = TRUE, groupSN = TRUE) {

  # use function arguments to get labels
  log_est <- paste(rep(param, 3), c("_log_raw", "_log_cil", "_log_ciu"), sep = "")
  dsm_est <- paste(rep(param, 3), c("_dsm_raw", "_dsm_cil", "_dsm_ciu"), sep = "")
  tru_val <- paste(param, "_tru_raw", sep = "")

  # add significance and label errors
  dat <- resData %>%
    select(SampleSize, Sensitivity, Specificity, (starts_with(param) & matches('raw$|cil$|ciu$'))) %>%
    mutate(log_sig = factor(ifelse(sign(.[, log_est[2]]) == sign(.[, log_est[3]]), "Significant", "Non-Significant"),
                            levels = c("Significant", "Non-Significant"), labels = c("Significant", "Non-Significant")),
           dsm_sig = factor(ifelse(sign(.[, dsm_est[2]]) == sign(.[, dsm_est[3]]), "Significant", "Non-Significant"),
                            levels = c("Significant", "Non-Significant"), labels = c("Significant", "Non-Significant"))) %>%
    mutate(log_err = factor(ifelse(sign(.[, tru_val]) == sign(.[, log_est[1]]) & log_sig == "Significant" & .[, log_est[2]] <= .[, tru_val] & .[, log_est[3]] >= .[, tru_val], "No Error",
                                   ifelse(sign(.[, tru_val]) != sign(.[, log_est[1]]) & log_sig == "Significant", "Sign Error",
                                          ifelse(sign(.[, tru_val]) == sign(.[, log_est[1]]) & log_sig == "Significant" & .[, log_est[2]] >= .[, tru_val], "Magnitude Error (Over)",
                                                 ifelse(sign(.[, tru_val]) == sign(.[, log_est[1]]) & log_sig == "Significant" & .[, log_est[3]] <= .[, tru_val], "Magnitude Error (Under)", "Non-Significant"))))),
           dsm_err = factor(ifelse(sign(.[, tru_val]) == sign(.[, dsm_est[1]]) & log_sig == "Significant" & .[, dsm_est[2]] <= .[, tru_val] & .[, dsm_est[3]] >= .[, tru_val], "No Error",
                                   ifelse(sign(.[, tru_val]) != sign(.[, dsm_est[1]]) & log_sig == "Significant", "Sign Error",
                                          ifelse(sign(.[, tru_val]) == sign(.[, dsm_est[1]]) & log_sig == "Significant" & .[, dsm_est[2]] >= .[, tru_val], "Magnitude Error (Over)",
                                                 ifelse(sign(.[, tru_val]) == sign(.[, dsm_est[1]]) & log_sig == "Significant" & .[, dsm_est[3]] <= .[, tru_val], "Magnitude Error (Under)", "Non-Significant"))))))

  # group data by desired conditions
  if ( groupSS & groupSN ) {
    dat <- dat %>%
      group_by(SampleSize, Sensitivity, Specificity)
  }
  if ( groupSS & !groupSN ) {
    dat <- dat %>%
      group_by(SampleSize)
  }
  if ( !groupSS & groupSN ) {
    dat <- dat %>%
      group_by(Sensitivity, Specificity)
  }

  # compute errors
  dat <- dat %>%
    summarize(log_non_sig_rt = mean(log_err == "Non-Significant")*100,
              log_err_fre_rt = mean(log_err == "No Error")*100,
              log_sng_err_rt = mean(log_err == "Sign Error")*100,
              log_mgo_err_rt = mean(log_err == "Magnitude Error (Over)")*100,
              log_mgu_err_rt = mean(log_err == "Magnitude Error (Under)")*100,
              log_non_sig_ct = sum(log_err == "Non-Significant"),
              log_err_fre_ct = sum(log_err == "No Error"),
              log_sng_err_ct = sum(log_err == "Sign Error"),
              log_mgo_err_ct = sum(log_err == "Magnitude Error (Over)"),
              log_mgu_err_ct = sum(log_err == "Magnitude Error (Under)"),
              dsm_non_sig_rt = mean(dsm_err == "Non-Significant")*100,
              dsm_err_fre_rt = mean(dsm_err == "No Error")*100,
              dsm_sng_err_rt = mean(dsm_err == "Sign Error")*100,
              dsm_mgo_err_rt = mean(dsm_err == "Magnitude Error (Over)")*100,
              dsm_mgu_err_rt = mean(dsm_err == "Magnitude Error (Under)")*100,
              dsm_non_sig_ct = sum(dsm_err == "Non-Significant"),
              dsm_err_fre_ct = sum(dsm_err == "No Error"),
              dsm_sng_err_ct = sum(dsm_err == "Sign Error"),
              dsm_mgo_err_ct = sum(dsm_err == "Magnitude Error (Over)"),
              dsm_mgu_err_ct = sum(dsm_err == "Magnitude Error (Under)"))

  return(dat)

}

C_1P_SM <- C_1P
C_1P_SM$coef1_log_sig <- as.factor(ifelse(sign(C_1P$coef1_log_cil) == sign(C_1P$coef1_log_ciu), "Significant", "Non-Significant"))
C_1P_SM$coef1_dsm_sig <- as.factor(ifelse(sign(C_1P$coef1_dsm_cil) == sign(C_1P$coef1_dsm_ciu), "Significant", "Non-Significant"))
C_1P_SM$coef1_log_tru <- factor(ifelse(C_1P$coef1_log_cil <= C_1P$coef1_tru_raw & C_1P$coef1_log_ciu >= C_1P$coef1_tru_raw, "Within",
                                       ifelse(C_1P$coef1_log_ciu <= C_1P$coef1_tru_raw, "Below", "Above")),
                                levels = c("Below", "Within", "Above"), labels = c("Under Estimated", "Within Interval", "Over Estimated"))
C_1P_SM$coef1_dsm_tru <- factor(ifelse(C_1P$coef1_dsm_cil <= C_1P$coef1_tru_raw & C_1P$coef1_dsm_ciu >= C_1P$coef1_tru_raw, "Within",
                                       ifelse(C_1P$coef1_dsm_ciu <= C_1P$coef1_tru_raw, "Below", "Above")),
                                levels = c("Below", "Within", "Above"), labels = c("Under Estimated", "Within Interval", "Over Estimated"))

#* unadjusted logistic model (1P conditions) ----
C_1P_SM %>%
  filter(coef1_log_sig == "Significant") %>%
  arrange(coef1_log_raw) %>%
  group_by(SampleSize) %>%
  mutate(id = seq_len(n())) %>%
  ggplot() +
  geom_linerange(
    aes(x = id, ymin = coef1_log_cil, ymax = coef1_log_ciu),
    alpha = 0.25
  ) +
  geom_point(
    aes(x = id, y = coef1_tru_raw, color = coef1_log_tru),
    size = 0.75
  ) +
  geom_hline(
    aes(yintercept = 1.5),
    linetype = 2,
    size = 1.15
  ) +
  geom_hline(
    aes(yintercept = 0.5),
    linetype = 2,
    size = 1.15
  ) +
  scale_color_manual(
    name='coef1_log_tru',
    breaks=c('Under Estimated', 'Within Interval', 'Over Estimated'),
    values=c('Under Estimated' = RColorBrewer::brewer.pal(3, "Set2")[1],
             'Within Interval' = RColorBrewer::brewer.pal(3, "Set2")[2],
             'Over Estimated'  = RColorBrewer::brewer.pal(3, "Set2")[3])
  ) +
  labs(
    x = "Counts of Significant Results (Ordered from smallest to largest estimate)",
    y = "Coefficient Value",
    title = "Sign and Magnitude Errors of Unadjusted Logistic Regression Model"
  ) +
  theme(
    panel.background   = element_rect(fill = NA, colour = "black", linetype = "solid", size = 1.5),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank(),
    strip.background = element_rect(fill = "darkgrey", linetype = "solid", size = 1.5),
    plot.title = element_text(family = "serif", face = "bold", size = 16, hjust = 0.5),
    axis.text = element_text(family = "serif", face = "bold", size = 12),
    axis.title = element_text(family = "serif", face = "bold", size = 14),
    strip.text = element_text(family = "serif", face = "bold", size = 14, color = "white"),
    legend.position = "bottom",
    legend.box = "horizontal",
    legend.text = element_text(family = "serif", size = 14)
  ) +
  guides(
    color = guide_legend(title = "Parameter Value", title.position = "top", title.hjust = 0.5,
                         title.theme = element_text(family = "serif", size = 14),
                         override.aes = list(size = 2))
  ) +
  facet_grid(
  ~ SampleSize,
  labeller = labeller(
    SampleSize = labs_Ss
  ),
  scales = "free"
  )

#* adjusted logistic regression model (1P conditions) ----
C_1P_SM %>%
  filter(coef1_dsm_sig == "Significant") %>%
  arrange(coef1_dsm_raw) %>%
  group_by(SampleSize) %>%
  mutate(id = seq_len(n())) %>%
  ggplot() +
  geom_linerange(
    aes(x = id, ymin = coef1_dsm_cil, ymax = coef1_dsm_ciu),
    alpha = 0.25
  ) +
  geom_point(
    aes(x = id, y = coef1_tru_raw, color = coef1_dsm_tru),
    size = 0.75
  ) +
  geom_hline(
    aes(yintercept = 1.5),
    linetype = 2,
    size = 1.15
  ) +
  geom_hline(
    aes(yintercept = 0.5),
    linetype = 2,
    size = 1.15
  ) +
  scale_color_manual(
    name='coef1_log_tru',
    breaks=c('Under Estimated', 'Within Interval', 'Over Estimated'),
    values=c('Under Estimated' = RColorBrewer::brewer.pal(3, "Set2")[1],
             'Within Interval' = RColorBrewer::brewer.pal(3, "Set2")[2],
             'Over Estimated'  = RColorBrewer::brewer.pal(3, "Set2")[3])
  ) +
  labs(
    x = "Counts of Significant Results (Ordered from smallest to largest estimate)",
    y = "Coefficient Value",
    title = "Sign and Magnitude Errors of Adjusted Logistic Regression Model"
  ) +
  theme(
    panel.background   = element_rect(fill = NA, colour = "black", linetype = "solid", size = 1.5),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank(),
    strip.background = element_rect(fill = "darkgrey", linetype = "solid", size = 1.5),
    plot.title = element_text(family = "serif", face = "bold", size = 16, hjust = 0.5),
    axis.text = element_text(family = "serif", face = "bold", size = 12),
    axis.title = element_text(family = "serif", face = "bold", size = 14),
    strip.text = element_text(family = "serif", face = "bold", size = 14, color = "white"),
    legend.position = "bottom",
    legend.box = "horizontal",
    legend.text = element_text(family = "serif", size = 14)
  ) +
  guides(
    color = guide_legend(title = "Parameter Value", title.position = "top", title.hjust = 0.5,
                         title.theme = element_text(family = "serif", size = 14),
                         override.aes = list(size = 2))
  ) +
  facet_grid(
    ~ SampleSize,
    labeller = labeller(
      SampleSize = labs_Ss
    ),
    scales = "free"
  )

#* break code folding ----

#compute errors over various conditions and parameters
snmErr(resData = C_1P, param = "int", groupSN = FALSE)
snmErr(resData = C_1P, param = "coef1", groupSN = FALSE)

snmErr(resData = C_3P, param = "int", groupSN = FALSE)
snmErr(resData = C_3P, param = "coef1", groupSN = FALSE)
snmErr(resData = C_3P, param = "coef2", groupSN = FALSE)
snmErr(resData = C_3P, param = "coef3", groupSN = FALSE)

snmErr(resData = C_5P, param = "int", groupSN = FALSE)
snmErr(resData = C_5P, param = "coef1", groupSN = FALSE)
snmErr(resData = C_5P, param = "coef2", groupSN = FALSE)
snmErr(resData = C_5P, param = "coef3", groupSN = FALSE)
snmErr(resData = C_5P, param = "coef4", groupSN = FALSE)
snmErr(resData = C_5P, param = "coef5", groupSN = FALSE)

snmErr(resData = C_10P, param = "int", groupSN = FALSE)
snmErr(resData = C_10P, param = "coef1", groupSN = FALSE)
snmErr(resData = C_10P, param = "coef2", groupSN = FALSE)
snmErr(resData = C_10P, param = "coef3", groupSN = FALSE)
snmErr(resData = C_10P, param = "coef4", groupSN = FALSE)
snmErr(resData = C_10P, param = "coef5", groupSN = FALSE)
snmErr(resData = C_10P, param = "coef6", groupSN = FALSE)
snmErr(resData = C_10P, param = "coef7", groupSN = FALSE)
snmErr(resData = C_10P, param = "coef8", groupSN = FALSE)
snmErr(resData = C_10P, param = "coef9", groupSN = FALSE)
snmErr(resData = C_10P, param = "coef10", groupSN = FALSE)
