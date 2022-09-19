#
# RUN SIMULATION STUDY AND EXTRACT RESULTS
#


#* This script file assumes that the set-up script has already been run.
#* The various model syntax and functions needed for this particular script
#* script are available in the corresponding set-up script file.



# Run the Simulation Conditions -------------------------------------------

#
# First Set of Conditions: Sensitivity > Specificity, both marginally better than chance
#

#Condition 1: n = 1000, p = 1, sn = 0.75, sp = 0.60
C1_1 <- foreach(i = 1:1000, .packages = c("cmdstanr", "posterior", "pROC"), .combine = "rbind", .errorhandling = "remove") %dopar% simRun(N = 1000, P = 1, Sn_m = 0.75, Sp_m = 0.60)

#Condition 2: n = 500, p = 1, sn = 0.75, sp = 0.60
C1_2 <- foreach(i = 1:1000, .packages = c("cmdstanr", "posterior", "pROC"), .combine = "rbind", .errorhandling = "remove") %dopar% simRun(N = 500, P = 1, Sn_m = 0.75, Sp_m = 0.60)

#Condition 3: n = 250, p = 1, sn = 0.75, sp = 0.60
C1_3 <- foreach(i = 1:1000, .packages = c("cmdstanr", "posterior", "pROC"), .combine = "rbind", .errorhandling = "remove") %dopar% simRun(N = 250, P = 1, Sn_m = 0.75, Sp_m = 0.60)

#Condition 4: n = 100, p = 1, sn = 0.75, sp = 0.60
C1_4 <- foreach(i = 1:1000, .packages = c("cmdstanr", "posterior", "pROC"), .combine = "rbind", .errorhandling = "remove") %dopar% simRun(N = 100, P = 1, Sn_m = 0.75, Sp_m = 0.60)

#Condition 5: n = 1000, p = 3, sn = 0.75, sp = 0.60
C1_5 <- foreach(i = 1:1000, .packages = c("cmdstanr", "posterior", "pROC"), .combine = "rbind", .errorhandling = "remove") %dopar% simRun(N = 1000, P = 3, Sn_m = 0.75, Sp_m = 0.60)

#Condition 6: n = 500, p = 3, sn = 0.75, sp = 0.60
C1_6 <- foreach(i = 1:1000, .packages = c("cmdstanr", "posterior", "pROC"), .combine = "rbind", .errorhandling = "remove") %dopar% simRun(N = 500, P = 3, Sn_m = 0.75, Sp_m = 0.60)

#Condition 7: n = 250, p = 3, sn = 0.75, sp = 0.60
C1_7 <- foreach(i = 1:1000, .packages = c("cmdstanr", "posterior", "pROC"), .combine = "rbind", .errorhandling = "remove") %dopar% simRun(N = 250, P = 3, Sn_m = 0.75, Sp_m = 0.60)

#Condition 8: n = 100, p = 3, sn = 0.75, sp = 0.60
C1_8 <- foreach(i = 1:1000, .packages = c("cmdstanr", "posterior", "pROC"), .combine = "rbind", .errorhandling = "remove") %dopar% simRun(N = 100, P = 3, Sn_m = 0.75, Sp_m = 0.60)

#Condition 9: n = 1000, p = 5, sn = 0.75, sp = 0.60
C1_9 <- foreach(i = 1:1000, .packages = c("cmdstanr", "posterior", "pROC"), .combine = "rbind", .errorhandling = "remove") %dopar% simRun(N = 1000, P = 5, Sn_m = 0.75, Sp_m = 0.60)

#Condition 10: n = 500, p = 5, sn = 0.75, sp = 0.60
C1_10 <- foreach(i = 1:1000, .packages = c("cmdstanr", "posterior", "pROC"), .combine = "rbind", .errorhandling = "remove") %dopar% simRun(N = 500, P = 5, Sn_m = 0.75, Sp_m = 0.60)

#Condition 11: n = 250, p = 5, sn = 0.75, sp = 0.60
C1_11 <- foreach(i = 1:1000, .packages = c("cmdstanr", "posterior", "pROC"), .combine = "rbind", .errorhandling = "remove") %dopar% simRun(N = 250, P = 5, Sn_m = 0.75, Sp_m = 0.60)

#Condition 12: n = 100, p = 5, sn = 0.75, sp = 0.60
C1_12 <- foreach(i = 1:1000, .packages = c("cmdstanr", "posterior", "pROC"), .combine = "rbind", .errorhandling = "remove") %dopar% simRun(N = 100, P = 5, Sn_m = 0.75, Sp_m = 0.60)

#Condition 13: n = 1000, p = 10, sn = 0.75, sp = 0.60
C1_13 <- foreach(i = 1:1000, .packages = c("cmdstanr", "posterior", "pROC"), .combine = "rbind", .errorhandling = "remove") %dopar% simRun(N = 1000, P = 10, Sn_m = 0.75, Sp_m = 0.60)

#Condition 14: n = 500, p = 10, sn = 0.75, sp = 0.60
C1_14 <- foreach(i = 1:1000, .packages = c("cmdstanr", "posterior", "pROC"), .combine = "rbind", .errorhandling = "remove") %dopar% simRun(N = 500, P = 10, Sn_m = 0.75, Sp_m = 0.60)

#Condition 15: n = 250, p = 10, sn = 0.75, sp = 0.60
C1_15 <- foreach(i = 1:1000, .packages = c("cmdstanr", "posterior", "pROC"), .combine = "rbind", .errorhandling = "remove") %dopar% simRun(N = 250, P = 10, Sn_m = 0.75, Sp_m = 0.60)

#Condition 16: n = 100, p = 10, sn = 0.75, sp = 0.60
C1_16 <- foreach(i = 1:1000, .packages = c("cmdstanr", "posterior", "pROC"), .combine = "rbind", .errorhandling = "remove") %dopar% simRun(N = 100, P = 10, Sn_m = 0.75, Sp_m = 0.60)

#
# Second Set of Conditions: Sensitivity >> Specificity, specificity lower than 0.50
#

#Condition 1: n = 1000, p = 1, sn = 0.85, sp = 0.45
C2_1 <- foreach(i = 1:1000, .packages = c("cmdstanr", "posterior", "pROC"), .combine = "rbind", .errorhandling = "remove") %dopar% simRun(N = 1000, P = 1, Sn_m = 0.85, Sp_m = 0.45)

#Condition 2: n = 500, p = 1, sn = 0.85, sp = 0.45
C2_2 <- foreach(i = 1:1000, .packages = c("cmdstanr", "posterior", "pROC"), .combine = "rbind", .errorhandling = "remove") %dopar% simRun(N = 500, P = 1, Sn_m = 0.85, Sp_m = 0.45)

#Condition 3: n = 250, p = 1, sn = 0.85, sp = 0.45
C2_3 <- foreach(i = 1:1000, .packages = c("cmdstanr", "posterior", "pROC"), .combine = "rbind", .errorhandling = "remove") %dopar% simRun(N = 250, P = 1, Sn_m = 0.85, Sp_m = 0.45)

#Condition 4: n = 100, p = 1, sn = 0.85, sp = 0.45
C2_4 <- foreach(i = 1:1000, .packages = c("cmdstanr", "posterior", "pROC"), .combine = "rbind", .errorhandling = "remove") %dopar% simRun(N = 100, P = 1, Sn_m = 0.85, Sp_m = 0.45)

#Condition 5: n = 1000, p = 3, sn = 0.85, sp = 0.45
C2_5 <- foreach(i = 1:1000, .packages = c("cmdstanr", "posterior", "pROC"), .combine = "rbind", .errorhandling = "remove") %dopar% simRun(N = 1000, P = 3, Sn_m = 0.85, Sp_m = 0.45)

#Condition 6: n = 500, p = 3, sn = 0.85, sp = 0.45
C2_6 <- foreach(i = 1:1000, .packages = c("cmdstanr", "posterior", "pROC"), .combine = "rbind", .errorhandling = "remove") %dopar% simRun(N = 500, P = 3, Sn_m = 0.85, Sp_m = 0.45)

#Condition 7: n = 250, p = 3, sn = 0.85, sp = 0.45
C2_7 <- foreach(i = 1:1000, .packages = c("cmdstanr", "posterior", "pROC"), .combine = "rbind", .errorhandling = "remove") %dopar% simRun(N = 250, P = 3, Sn_m = 0.85, Sp_m = 0.45)

#Condition 8: n = 100, p = 3, sn = 0.85, sp = 0.45
C2_8 <- foreach(i = 1:1000, .packages = c("cmdstanr", "posterior", "pROC"), .combine = "rbind", .errorhandling = "remove") %dopar% simRun(N = 100, P = 3, Sn_m = 0.85, Sp_m = 0.45)

#Condition 9: n = 1000, p = 5, sn = 0.85, sp = 0.45
C2_9 <- foreach(i = 1:1000, .packages = c("cmdstanr", "posterior", "pROC"), .combine = "rbind", .errorhandling = "remove") %dopar% simRun(N = 1000, P = 5, Sn_m = 0.85, Sp_m = 0.45)

#Condition 10: n = 500, p = 5, sn = 0.85, sp = 0.45
C2_10 <- foreach(i = 1:1000, .packages = c("cmdstanr", "posterior", "pROC"), .combine = "rbind", .errorhandling = "remove") %dopar% simRun(N = 500, P = 5, Sn_m = 0.85, Sp_m = 0.45)

#Condition 11: n = 250, p = 5, sn = 0.85, sp = 0.45
C2_11 <- foreach(i = 1:1000, .packages = c("cmdstanr", "posterior", "pROC"), .combine = "rbind", .errorhandling = "remove") %dopar% simRun(N = 250, P = 5, Sn_m = 0.85, Sp_m = 0.45)

#Condition 12: n = 100, p = 5, sn = 0.85, sp = 0.45
C2_12 <- foreach(i = 1:1000, .packages = c("cmdstanr", "posterior", "pROC"), .combine = "rbind", .errorhandling = "remove") %dopar% simRun(N = 100, P = 5, Sn_m = 0.85, Sp_m = 0.45)

#Condition 13: n = 1000, p = 10, sn = 0.85, sp = 0.45
C2_13 <- foreach(i = 1:1000, .packages = c("cmdstanr", "posterior", "pROC"), .combine = "rbind", .errorhandling = "remove") %dopar% simRun(N = 1000, P = 10, Sn_m = 0.85, Sp_m = 0.45)

#Condition 14: n = 500, p = 10, sn = 0.85, sp = 0.45
C2_14 <- foreach(i = 1:1000, .packages = c("cmdstanr", "posterior", "pROC"), .combine = "rbind", .errorhandling = "remove") %dopar% simRun(N = 500, P = 10, Sn_m = 0.85, Sp_m = 0.45)

#Condition 15: n = 250, p = 10, sn = 0.85, sp = 0.45
C2_15 <- foreach(i = 1:1000, .packages = c("cmdstanr", "posterior", "pROC"), .combine = "rbind", .errorhandling = "remove") %dopar% simRun(N = 250, P = 10, Sn_m = 0.85, Sp_m = 0.45)

#Condition 16: n = 100, p = 10, sn = 0.85, sp = 0.45
C2_16 <- foreach(i = 1:1000, .packages = c("cmdstanr", "posterior", "pROC"), .combine = "rbind", .errorhandling = "remove") %dopar% simRun(N = 100, P = 10, Sn_m = 0.85, Sp_m = 0.45)

#
# Third Set of Conditions: Sensitivity = Specificity, both better than chance
#

#Condition 1: n = 1000, p = 1, sn = 0.70, sp = 0.70
C3_1 <- foreach(i = 1:1000, .packages = c("cmdstanr", "posterior", "pROC"), .combine = "rbind", .errorhandling = "remove") %dopar% simRun(N = 1000, P = 1, Sn_m = 0.70, Sp_m = 0.70)

#Condition 2: n = 500, p = 1, sn = 0.70, sp = 0.70
C3_2 <- foreach(i = 1:1000, .packages = c("cmdstanr", "posterior", "pROC"), .combine = "rbind", .errorhandling = "remove") %dopar% simRun(N = 500, P = 1, Sn_m = 0.70, Sp_m = 0.70)

#Condition 3: n = 250, p = 1, sn = 0.70, sp = 0.70
C3_3 <- foreach(i = 1:1000, .packages = c("cmdstanr", "posterior", "pROC"), .combine = "rbind", .errorhandling = "remove") %dopar% simRun(N = 250, P = 1, Sn_m = 0.70, Sp_m = 0.70)

#Condition 4: n = 100, p = 1, sn = 0.70, sp = 0.70
C3_4 <- foreach(i = 1:1000, .packages = c("cmdstanr", "posterior", "pROC"), .combine = "rbind", .errorhandling = "remove") %dopar% simRun(N = 100, P = 1, Sn_m = 0.70, Sp_m = 0.70)

#Condition 5: n = 1000, p = 3, sn = 0.70, sp = 0.70
C3_5 <- foreach(i = 1:1000, .packages = c("cmdstanr", "posterior", "pROC"), .combine = "rbind", .errorhandling = "remove") %dopar% simRun(N = 1000, P = 3, Sn_m = 0.70, Sp_m = 0.70)

#Condition 6: n = 500, p = 3, sn = 0.70, sp = 0.70
C3_6 <- foreach(i = 1:1000, .packages = c("cmdstanr", "posterior", "pROC"), .combine = "rbind", .errorhandling = "remove") %dopar% simRun(N = 500, P = 3, Sn_m = 0.70, Sp_m = 0.70)

#Condition 7: n = 250, p = 3, sn = 0.70, sp = 0.70
C3_7 <- foreach(i = 1:1000, .packages = c("cmdstanr", "posterior", "pROC"), .combine = "rbind", .errorhandling = "remove") %dopar% simRun(N = 250, P = 3, Sn_m = 0.70, Sp_m = 0.70)

#Condition 8: n = 100, p = 3, sn = 0.70, sp = 0.70
C3_8 <- foreach(i = 1:1000, .packages = c("cmdstanr", "posterior", "pROC"), .combine = "rbind", .errorhandling = "remove") %dopar% simRun(N = 100, P = 3, Sn_m = 0.70, Sp_m = 0.70)

#Condition 9: n = 1000, p = 5, sn = 0.70, sp = 0.70
C3_9 <- foreach(i = 1:1000, .packages = c("cmdstanr", "posterior", "pROC"), .combine = "rbind", .errorhandling = "remove") %dopar% simRun(N = 1000, P = 5, Sn_m = 0.70, Sp_m = 0.70)

#Condition 10: n = 500, p = 5, sn = 0.70, sp = 0.70
C3_10 <- foreach(i = 1:1000, .packages = c("cmdstanr", "posterior", "pROC"), .combine = "rbind", .errorhandling = "remove") %dopar% simRun(N = 500, P = 5, Sn_m = 0.70, Sp_m = 0.70)

#Condition 11: n = 250, p = 5, sn = 0.70, sp = 0.70
C3_11 <- foreach(i = 1:1000, .packages = c("cmdstanr", "posterior", "pROC"), .combine = "rbind", .errorhandling = "remove") %dopar% simRun(N = 250, P = 5, Sn_m = 0.70, Sp_m = 0.70)

#Condition 12: n = 100, p = 5, sn = 0.70, sp = 0.70
C3_12 <- foreach(i = 1:1000, .packages = c("cmdstanr", "posterior", "pROC"), .combine = "rbind", .errorhandling = "remove") %dopar% simRun(N = 100, P = 5, Sn_m = 0.70, Sp_m = 0.70)

#Condition 13: n = 1000, p = 10, sn = 0.70, sp = 0.70
C3_13 <- foreach(i = 1:1000, .packages = c("cmdstanr", "posterior", "pROC"), .combine = "rbind", .errorhandling = "remove") %dopar% simRun(N = 1000, P = 10, Sn_m = 0.70, Sp_m = 0.70)

#Condition 14: n = 500, p = 10, sn = 0.70, sp = 0.70
C3_14 <- foreach(i = 1:1000, .packages = c("cmdstanr", "posterior", "pROC"), .combine = "rbind", .errorhandling = "remove") %dopar% simRun(N = 500, P = 10, Sn_m = 0.70, Sp_m = 0.70)

#Condition 15: n = 250, p = 10, sn = 0.70, sp = 0.70
C3_15 <- foreach(i = 1:1000, .packages = c("cmdstanr", "posterior", "pROC"), .combine = "rbind", .errorhandling = "remove") %dopar% simRun(N = 250, P = 10, Sn_m = 0.70, Sp_m = 0.70)

#Condition 16: n = 100, p = 10, sn = 0.70, sp = 0.70
C3_16 <- foreach(i = 1:1000, .packages = c("cmdstanr", "posterior", "pROC"), .combine = "rbind", .errorhandling = "remove") %dopar% simRun(N = 100, P = 10, Sn_m = 0.70, Sp_m = 0.70)



#stop parallel processes
registerDoSEQ()
unregister()



# Extract Results ---------------------------------------------------------

#combine results for plotting
C_1P  <- do.call("rbind", list(C1_1, C1_2, C1_3, C1_4,
                               C2_1, C2_2, C2_3, C2_4,
                               C3_1, C3_2, C3_3, C3_4))

C_3P  <- do.call("rbind", list(C1_5, C1_6, C1_7, C1_8,
                               C2_5, C2_6, C2_7, C2_8,
                               C3_5, C3_6, C3_7, C3_8))

C_5P  <- do.call("rbind", list(C1_9, C1_10, C1_11, C1_12,
                               C2_9, C2_10, C2_11, C2_12,
                               C3_9, C3_10, C3_11, C3_12))

C_10P <- do.call("rbind", list(C1_13, C1_14, C1_15, C1_16,
                               C2_13, C2_14, C2_15, C2_16,
                               C3_13, C3_14, C3_15, C3_16))

#add condition indicators
C_1P$Sensitivity <- rep(c(0.75, 0.85, 0.70), each = 4000)
C_1P$Specificity <- rep(c(0.60, 0.45, 0.70), each = 4000)
C_1P$SampleSize  <- rep(rep(c(1000, 500, 250, 100), each = 1000), 3)

C_3P$Sensitivity <- rep(c(0.75, 0.85, 0.70), each = 4000)
C_3P$Specificity <- rep(c(0.60, 0.45, 0.70), each = 4000)
C_3P$SampleSize  <- rep(rep(c(1000, 500, 250, 100), each = 1000), 3)

C_5P$Sensitivity <- rep(c(0.75, 0.85, 0.70), each = 4000)
C_5P$Specificity <- rep(c(0.60, 0.45, 0.70), each = 4000)
C_5P$SampleSize  <- rep(rep(c(1000, 500, 250, 100), each = 1000), 3)

C_10P$Sensitivity <- rep(c(0.75, 0.85, 0.70), each = 4000)
C_10P$Specificity <- rep(c(0.60, 0.45, 0.70), each = 4000)
C_10P$SampleSize  <- rep(rep(c(1000, 500, 250, 100), each = 1000), 3)

#tidy environment
rm(C1_1, C1_2, C1_3, C1_4, C1_5, C1_6, C1_7, C1_8, C1_9, C1_10, C1_11, C1_12, C1_13, C1_14, C1_15, C1_16,
   C2_1, C2_2, C2_3, C2_4, C2_5, C2_6, C2_7, C2_8, C2_9, C2_10, C2_11, C2_12, C2_13, C2_14, C2_15, C2_16,
   C3_1, C3_2, C3_3, C3_4, C3_5, C3_6, C3_7, C3_8, C3_9, C3_10, C3_11, C3_12, C3_13, C3_14, C3_15, C3_16)
