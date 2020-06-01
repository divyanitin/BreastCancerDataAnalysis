wisc_bc_df <- read.csv("C:/Users/divya/OneDrive/Documents/SEM1/ABI/Project/breastcancerwisconsindata/cancerdata.csv", stringsAsFactors = F)


str(wisc_bc_df)

head(wisc_bc_df)

table(wisc_bc_df$diagnosis)

sum(is.na(wisc_bc_df$diagnosis))

wisc_bc_df$diagnosis <- factor(wisc_bc_df$diagnosis, levels = c("B", "M"), labels = c("benign", 
                                                                                      "malignant"))

normalize <- function(x) {
  y <- (x - min(x))/(max(x) - min(x))
  y
}

wbcd_n_L <- lapply(wisc_bc_df[, 3:32], normalize)
wbcd_n <- data.frame(wbcd_n_L)
wbcd_n[1:3, 1:4]


# Add id labels as rownames to keep track of the patient's data
rownames(wbcd_n) <- wisc_bc_df$id


BM_class <- wisc_bc_df[,2]
names(BM_class) <- wisc_bc_df$id
BM_class[1:3]




nrow(wisc_bc_df)
rand_permute <- sample(x = 1:569, size = 569)
rand_permute[1:5]
#save(rand_permute, file='rand_permute.RData')


load("rand_permute.RData")

all_id_random <- wisc_bc_df[rand_permute, "id"]
# Select the first third of these for validation


validate_id <- as.character(all_id_random[1:189])
training_id <- as.character(all_id_random[190:569])

wbcd_train <- wbcd_n[training_id, ]
wbcd_val <- wbcd_n[validate_id, ]
BM_class_train <- BM_class[training_id]
BM_class_val <- BM_class[validate_id]
table(BM_class_train)


table(BM_class_val)

library(class)
`?`(knn)

sqrt(nrow(wbcd_train))


k <- 19
knn_predict <- knn(wbcd_train, wbcd_val, BM_class_train, k = 19)
knn_predict[1:3]

table(knn_predict, BM_class_val)

prop.table(table(knn_predict, BM_class_val))

knn_predict_3 <- knn(wbcd_train, wbcd_val, BM_class_train, k = 3)
knn_predict_7 <- knn(wbcd_train, wbcd_val, BM_class_train, k = 7)
knn_predict_11 <- knn(wbcd_train, wbcd_val, BM_class_train, k = 11)
knn_predict_31 <- knn(wbcd_train, wbcd_val, BM_class_train, k = 31)


table(knn_predict_3, BM_class_val)

table(knn_predict_7, BM_class_val)
table(knn_predict_11, BM_class_val)
table(knn_predict_31, BM_class_val)


names(wbcd_train)

lm_1 <- lm(radius_mean ~ BM_class_train, data = wbcd_train)
summary(lm_1)

names(summary(lm_1))

summary(lm_1)$fstatistic

# The significance measure we want:
summary(lm_1)$fstatistic[1]

exp_var_fstat <- as.numeric(rep(NA, times = 30))
names(exp_var_fstat) <- names(wbcd_train)


exp_var_fstat["radius_mean"] <- summary(lm(radius_mean ~ BM_class_train, data = wbcd_train))$fstatistic[1]
exp_var_fstat["texture_mean"] <- summary(lm(texture_mean ~ BM_class_train, data = wbcd_train))$fstatistic[1]
exp_var_fstat["perimeter_mean"] <- summary(lm(perimeter_mean ~ BM_class_train, 
                                              data = wbcd_train))$fstatistic[1]
exp_var_fstat


exp_vars <- names(wbcd_train)
exp_var_fstat <- as.numeric(rep(NA, times = 30))
names(exp_var_fstat) <- exp_vars


for (j in 1:length(exp_vars)) {
  exp_var_fstat[exp_vars[j]] <- summary(lm(exp_vars[j] ~ BM_class_train, data = wbcd_train))$fstatistic[1]
}


for (j in 1:length(exp_vars)) {
  exp_var_fstat[exp_vars[j]] <- summary(lm(as.formula(paste(exp_vars[j], " ~ BM_class_train")), 
                                           data = wbcd_train))$fstatistic[1]
}


exp_var_fstat

exp_var_fstat2 <- sapply(exp_vars, function(x) {
  summary(lm(as.formula(paste(x, " ~ BM_class_train")), data = wbcd_train))$fstatistic[1]
})
exp_var_fstat2


names(exp_var_fstat2) <- exp_vars


wbcd_df_L <- lapply(exp_vars, function(x) {
  df <- data.frame(sample = rownames(wbcd_train), variable = x, value = wbcd_train[, 
                                                                                   x], class = BM_class_train)
  df
})
head(wbcd_df_L[[1]])


head(wbcd_df_L[[5]])

names(wbcd_df_L) <- exp_vars


library(plyr)
var_sig_fstats <- laply(wbcd_df_L, function(df) {
  fit <- lm(value ~ class, data = df)
  f <- summary(fit)$fstatistic[1]
  f
})
names(var_sig_fstats) <- names(wbcd_df_L)
var_sig_fstats[1:3]

most_sig_stats <- sort(var_sig_fstats, decreasing = T)
most_sig_stats[1:5]


most_sig_stats[25:30]

wbcd_train_ord <- wbcd_train[, names(most_sig_stats)]

length(training_id)


(2/3) * length(training_id)

length(training_id) - 253


# Use 253 as the training set size.
training_family_L <- lapply(1:1000, function(j) {
  perm <- sample(1:380, size = 380, replace = F)
  shuffle <- training_id[perm]
  trn <- shuffle[1:253]
  trn
})
#save(training_family_L, file='training_family_L.RData')
load("training_family_L.RData")
validation_family_L <- lapply(training_family_L, function(x) setdiff(training_id, 
                                                                     x))
N <- seq(from = 3, to = 29, by = 2)

sqrt(length(training_family_L[[1]]))

K <- seq(from = 3, to = 19, by = 2)

1000 * length(N) * length(K)

paramter_errors_df <- data.frame(mc_index = as.integer(rep(NA, times = 126000)), 
                                 var_num = as.integer(rep(NA, times = 126000)), k = as.integer(rep(NA, times = 126000)), 
                                 error = as.numeric(rep(NA, times = 126000)))

knn_test <- knn(train = wbcd_train_ord[training_family_L[[1]], 1:5], test = wbcd_train_ord[validation_family_L[[1]], 
                                                                                           1:5], cl = BM_class_train[training_family_L[[1]]], k = 7)
knn_test[1:3]

tbl_test <- table(knn_test, BM_class_train[validation_family_L[[1]]])
tbl_test

err_rate <- (tbl_test[1, 2] + tbl_test[2, 1])/length(validation_family_L[[1]])
err_rate

# j = index, n = length of range of variables, k=k
core_knn <- function(j, n, k) {
  knn_predict <- knn(train = wbcd_train_ord[training_family_L[[j]], 1:n], 
                     test = wbcd_train_ord[validation_family_L[[j]], 1:n], cl = BM_class_train[training_family_L[[j]]], 
                     k = k)
  tbl <- table(knn_predict, BM_class_train[validation_family_L[[j]]])
  err <- (tbl[1, 2] + tbl[2, 1])/length(validation_family_L[[j]])
  err
}
# sample
core_knn(1, 5, 7)

iter <- 1


str_time <- Sys.time()
for (j in 1:1000) {
  for (n in 1:length(N)) {
    for (m in 1:length(K)) {
      err <- core_knn(j, N[n], K[m])
      paramter_errors_df[iter, ] <- c(j, N[n], K[m], err)
      iter <- iter + 1
    }
  }
}
time_lapsed_for <- Sys.time() - str_time
save(paramter_errors_df, time_lapsed_for, file = "for_loop_paramter_errors.RData")
load("for_loop_paramter_errors.RData")
time_lapsed_for

param_df1 <- merge(data.frame(mc_index = 1:1000), data.frame(var_num = N))
param_df <- merge(param_df1, data.frame(k = K))
str(param_df)


knn_err_est_df_test <- ddply(param_df[1:20, ], .(mc_index, var_num, k), function(df) {
  err <- core_knn(df$mc_index[1], df$var_num[1], df$k[1])
  err
})
head(knn_err_est_df_test)

str_time <- Sys.time()
knn_err_est_df <- ddply(param_df, .(mc_index, var_num, k), function(df) {
  err <- core_knn(df$mc_index[1], df$var_num[1], df$k[1])
  err
})
time_lapsed <- Sys.time() - str_time
save(knn_err_est_df, time_lapsed, file = "knn_err_est_df.RData")
load("knn_err_est_df.RData")
time_lapsed
## Time difference of 6.811 mins
head(knn_err_est_df)

names(knn_err_est_df)[4] <- "error"

mean_ex_df <- subset(knn_err_est_df, var_num == 5 & k == 7)
head(mean_ex_df)

mean(mean_ex_df$error)

mean_errs_df <- ddply(knn_err_est_df, .(var_num, k), function(df) mean(df$error))
head(mean_errs_df)

names(mean_errs_df)[3] <- "mean_error"

library(ggplot2)
ggplot(data = mean_errs_df, aes(x = var_num, y = k, color = mean_error)) + geom_point(size = 10) + 
  theme_bw()

ggplot(data = subset(mean_errs_df, var_num >= 15), aes(x = var_num, y = k, color = mean_error)) + 
  geom_point(size = 10) + theme_bw()

subset(mean_errs_df, var_num == 17)

subset(mean_errs_df, var_num == 19)

subset(mean_errs_df, var_num == 21)

subset(mean_errs_df, var_num == 25)

mean_errs_df[which.min(mean_errs_df$mean_error), ]

names(wbcd_train_ord)

wbcd_val_ord <- wbcd_val[, names(wbcd_train_ord)]

bm_val_pred <- knn(train = wbcd_train_ord[, 1:27], wbcd_val_ord[, 1:27], BM_class_train, 
                   k = 3)
tbl_bm_val <- table(bm_val_pred, BM_class_val)
tbl_bm_val

(val_error <- tbl_bm_val[1, 2] + tbl_bm_val[2, 1])/length(BM_class_val)

#install.packages("doMC", repos="http://R-Forge.R-project.org")


library(doMC)

registerDoMC()
# How many cores are we using?
getDoParWorkers()

str_time <- Sys.time()
knn_err_est_df_par <- ddply(param_df, .(mc_index, var_num, k), function(df) {
  err <- core_knn(df$mc_index[1], df$var_num[1], df$k[1])
  err
}, .parallel = T)
time_lapsed_par <- Sys.time() - str_time
save(knn_err_est_df_par, time_lapsed_par, file = "knn_err_est_df_par.RData")
load("knn_err_est_df_par.RData")
time_lapsed_par






