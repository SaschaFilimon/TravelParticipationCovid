## 'Reiseanalyse' 2021

library(tidyverse)
library(foreign) # read.spss()
library(rsample) # initial_split()
library(gbm)
library(caret) # confusionmatrix()
library(ROCR) # prediction()
library(car) # Anova()
library(pROC) # ROC-graph, AUC-value etc.
library(mgcv) # gam()
library(gridExtra) # manageGrob()

# citation(package = "caret") # citation for Bibtex

#### Daten laden ##########################################################
reise_21_roh <- read.spss(
  "~/RProjekte/Reiseanalyse 2021/Gesis-2021.sav",
  to.data.frame = TRUE
)
# View(reise_21_roh)
colnames(reise_21_roh)

which(colnames(reise_21_roh) == "C6019_1")

## releveant variables
reise_21 <- reise_21_roh %>% select(
  "gereist" = "C6019_1",
  "geschlecht" = "C1003",
  "alter" = "N8",
  "herkunft" = "C1020",
  "ortsgroesse" = "C1011",
  "haushaltsgroesse" = "C1008",
  "kind_alter_0_5" = "C1019_1",
  "kind_alter_6_13" = "C1019_2",
  "kind_alter_14_17" = "C1019_3",
  "bildung_befragter" = "C1006", # for 'bildung'
  "bildung_hauptverdiener" = "C1016", # for 'bildung'
  "berufst_befragter" = "C1005", # for 'vollberufstaetig'
  "berufst_hauptverdiener" = "C1017", # for 'vollberufstaetig'
  "haushaltseinkommen" = "C19_16",
  "migration_ja" = "C21_16_01", # for 'migration'
  "migration_nein" = "C21_16_19", # for 'migration'
  "migration_ka" = "C21_16_20", # for 'migration'
  "gereist_01" = "C3028_13_01", # for 'gereist'
  "gereist_02" = "C3028_13_02", # for 'gereist'
  "gereist_03" = "C3028_13_03", # for 'gereist'
  "gereist_04" = "C3028_13_04", # for 'gereist'
  "gereist_05" = "C3028_13_05", # for 'gereist'
  "gereist_06" = "C3028_13_06", # for 'gereist'
  "gereist_07" = "C3028_13_07", # for 'gereist'
  "gereist_08" = "C3028_13_08", # for 'gereist'
  "gereist_09" = "C3028_13_09", # for 'gereist'
  "gereist_10" = "C3028_13_10", # for 'gereist'
  "gereist_11" = "C3028_13_11", # for 'gereist'
  "gereist_12" = "C3028_13_12" # for 'gereist'
)


### Preprocess Data ################################################
str(reise_21)


## determine: at least one of respondent and main-earner is fully employed?
# ('vollberufstaetig')
(levels_berufst <- levels(reise_21$berufst_befragter))
levels(reise_21$berufst_hauptverdiener)

ist_berufst_befragter <- reise_21$berufst_befragter == levels_berufst[1]
ist_berufst_hauptverdiener <- reise_21$berufst_hauptverdiener == levels_berufst[1]

reise_21$vollberufstaetig <- ifelse(
  ist_berufst_befragter | ist_berufst_hauptverdiener, "Ja", "Nein"
) %>%
  factor(levels = c("Nein", "Ja"))

reise_21 <- reise_21 %>%
  select(-berufst_befragter, -berufst_hauptverdiener)


## determine: higher education level between that of respondent and main-earner
# ('bildung')
(levels_bildung <- levels(reise_21$bildung_befragter))
head(reise_21$bildung_befragter)
bildung_befragter_num <- as.numeric(reise_21$bildung_befragter)
head(bildung_befragter_num)

levels(reise_21$bildung_hauptverdiener)
head(reise_21$bildung_hauptverdiener)
bildung_hauptverdiener_num <- as.numeric(reise_21$bildung_hauptverdiener)
head(bildung_hauptverdiener_num)

bildung_num <- apply(
  cbind(bildung_befragter_num, bildung_hauptverdiener_num), 1, max
)

reise_21$bildung <- sapply(bildung_num, function(i) {
  levels_bildung[i]
}) %>% factor(levels = levels_bildung)

# check
# cbind(bildung_befragter_num, bildung_hauptverdiener_num)[100:105, ]
# reise_21$bildung[100:105]

reise_21 <- reise_21 %>% select(-bildung_befragter, -bildung_hauptverdiener)


## determine: any migration background? ('migration')
head(select(reise_21, migration_ja, migration_nein, migration_ka))
apply(select(reise_21, migration_ja, migration_nein, migration_ka), 2, unique)

migration_ja <- reise_21$migration_ja == "Migrationshintergrund"
migration_nein <- reise_21$migration_nein == "Migrationshintergrund"
migration_ka <- reise_21$migration_ka == "Migrationshintergrund"
migration_matrix <- cbind(migration_ja, migration_nein, migration_ka)
head(migration_matrix)

# sum(rowSums(migration_matrix) != 1)

migration <- apply(migration_matrix, 1, function(i) {
  c("Ja", "Nein", NA)[i]
})
migration <- factor(migration, levels = c("Nein", "Ja"))
head(migration)

# check if migration_ka -> NA
# migration_matrix[which(migration_ka == TRUE), ]
# migration[which(migration_ka == TRUE)]

reise_21$migration <- migration

reise_21 <- reise_21 %>% select(
  -migration_ja, -migration_nein, -migration_ka
)


## determine: missing values
sum(is.na(reise_21))
lapply(reise_21, unique)
# missing values in the variable 'migration' found
sum(is.na(reise_21$migration)) # 242
# exclude observations with missing values
dim(reise_21)
reise_21_mit_migration_na <- reise_21
reise_21 <- reise_21[!is.na(reise_21$migration), ]
dim(reise_21)


## 'gereist_roh' as numeric
head(reise_21$gereist, 20)
reise_21$gereist <- as.numeric(reise_21$gereist == "Ja")
head(reise_21$gereist, 20)


## size of town as numeric
levels(reise_21$ortsgroesse)
head(reise_21$ortsgroesse)
ortsgroesse_fac <- factor(
  reise_21$ortsgroesse,
  labels = c("3000", "17500", "65000", "200000", "1000000")
) %>% 
  as.character() %>%
  as.numeric()
head(ortsgroesse_fac)
reise_21$ortsgroesse <- ortsgroesse_fac


## household size as numeric
levels(reise_21$haushaltsgroesse)
head(reise_21$haushaltsgroesse)
head(as.numeric(reise_21$haushaltsgroesse))
reise_21$haushaltsgroesse <- as.numeric(reise_21$haushaltsgroesse)


## household income as numeric
levels(reise_21$haushaltseinkommen)
head(reise_21$haushaltseinkommen)
head(factor(reise_21$haushaltseinkommen,
            labels = c(
              250, 625, 875, 1125, 1375, 1625, 1875, 2125, 2375, 2750,
              3250, 3750, 4250, 4750, 5500
            )
) %>%
  as.character() %>%
  as.numeric())

reise_21$haushaltseinkommen <- factor(reise_21$haushaltseinkommen,
                                      labels = c(
                                        250, 625, 875, 1125, 1375, 1625, 1875, 2125, 2375, 2750,
                                        3250, 3750, 4250, 4750, 5500
                                      )
) %>%
  as.character() %>%
  as.numeric()


## variables concerning children in the household
str(reise_21)
reise_21 <- reise_21 %>% mutate(
  kind_alter_0_5 = factor(ifelse(kind_alter_0_5 == "0", "Nein", "Ja"),
                          levels = c("Nein", "Ja")
  ),
  kind_alter_6_13 = factor(ifelse(kind_alter_6_13 == "0", "Nein", "Ja"),
                           levels = c("Nein", "Ja")
  ),
  kind_alter_14_17 = factor(ifelse(kind_alter_14_17 == "0", "Nein", "Ja"),
                            levels = c("Nein", "Ja")
  )
)


## exclude minors and persons older than 85
summary(reise_21$alter)
sum(reise_21$alter < 18)
sum(reise_21$alter > 85)

reise_21 <- reise_21 %>%
  filter(alter >= 18, alter <= 85)


## determine people, who traveled between once between March-December ('gereist')
# needed variables
gereist_matrix <- reise_21 %>%
  select(gereist_01:gereist_12)
head(gereist_matrix, 15)

# as logical values
gereist_matrix_bin <- ifelse(gereist_matrix == "0", FALSE, TRUE)
head(gereist_matrix_bin, 15)

# aggregate the columns for each person, if he/she traveled between March-December
gereist_nach_feb <- apply(gereist_matrix_bin, 1, function(x) {
  if (sum(is.na(x)) == 12) {
    return(NA)
  }
  sum(x[-c(1, 2)], na.rm = TRUE) > 0
})

head(gereist_nach_feb, 15)
sum(gereist_nach_feb)

# missing values: Are there cases where someone did not give any information
# for the specific months (meaning: did not travel) but stated that he/she
# traveled on the question if he/she traveled ('gereist_roh')?
vergleiche_variablen <- data.frame(
  gereist = reise_21$gereist,
  gereist_nach_feb = as.numeric(gereist_nach_feb)
)
head(vergleiche_variablen, 15)

vergleiche_variablen %>%
  filter(gereist == 1 & is.na(gereist_nach_feb))
# no, that case does not occur

# code NA as not-traveled
table(gereist_nach_feb, useNA = "always")
gereist_nach_feb[is.na(gereist_nach_feb)] <- FALSE
table(gereist_nach_feb, useNA = "always")

# adapt dataset
str(reise_21)

reise_21 <- reise_21 %>%
  select(-gereist_01:-gereist_12) %>%
  mutate(gereist = as.numeric(gereist_nach_feb))
str(reise_21)



#### Distribution of Variables ##################################################
variablen <- c(
  "gereist", 
  "alter", "haushaltseinkommen", "ortsgroesse", "haushaltsgroesse",
  "geschlecht", "herkunft", "kind_alter_0_5", "kind_alter_6_13",
  "kind_alter_14_17", "vollberufstaetig", "bildung", "migration"
)

variablen_verteilung_21 <- variablen %>% lapply(function(x) {
  if (x %in% c("alter", "haushaltseinkommen", "ortsgroesse")) {
    reise_21 %>% ggplot() +
      geom_boxplot(aes_string(x)) +
      xlab("") +
      ggtitle(x) 
  } else {
    reise_21 %>% ggplot(aes_string(x)) +
      geom_bar() +
      stat_count(geom = "text", colour = "white", size = 4,
                 mapping = aes(label = ..count..), position = position_stack(vjust = 0.5)) +
      xlab("") +
      ggtitle(x) 
  }
}) 
lay_mat <- matrix(1:15, 3, 5, TRUE)
variablen_verteilung_21_grid <- marrangeGrob(
  variablen_verteilung_21,
  layout_matrix = lay_mat
)
variablen_verteilung_21_grid
#ggsave("central_tendency_and_measure_of_dispersion_21.pdf", variablen_verteilung_21_grid,
#       width = 60, height = 25, units = "cm"
#)


## function for calculating summary metrics for numeric values
get_summaries <- function(x) {
  mode <- function(x) {                     
    unique_x <- unique(x)
    tabulate_x <- tabulate(match(x, unique_x))
    unique_x[tabulate_x == max(tabulate_x)]
  }
  as.table(c(min = min(x), max = max(x), mean = mean(x),
             median = median(x), sd = sd(x), mode = mode(x)))
}
variablen <- c(
  "alter", "haushaltseinkommen", "ortsgroesse"
)
sapply(variablen, function(x) {
  get_summaries(reise_21[, x])
})



#### Correlation Analysis #############################################################
library(vcd)
library(corrplot)

str(reise_21)

## categorial features
kategoriale_features <- reise_21 %>%
  select(-gereist, -alter, -haushaltseinkommen)

# mosaic plots
dim(kategoriale_features)
n_features <- dim(kategoriale_features)[2]
# for (i in seq_len(n_features - 1)) {
#   for (j in (i + 1):n_features) {
#     mosaicplot(
#       table(kategoriale_features[, i], kategoriale_features[, j]),
#       main = paste0(
#         colnames(kategoriale_features)[i],
#         " & ",
#         colnames(kategoriale_features[j])
#       )
#     )
#   }
# }

# initialize correlation matrix
korrelationsmatrix_leer <- matrix(
  ncol = length(kategoriale_features),
  nrow = length(kategoriale_features),
  dimnames = list(names(kategoriale_features), names(kategoriale_features))
)

# function for calculating the Cramer's V correlation matrix
berechne_cramer <- function(m, df) {
  for (r in seq(nrow(m))) {
    for (c in seq(ncol(m))) {
      m[[r, c]] <- assocstats(table(df[[r]], df[[c]]))$cramer
    }
  }
  return(m)
}

korrelationsmatrix_kategorial <- berechne_cramer(
  korrelationsmatrix_leer, kategoriale_features
)

# pdf(file = "korrelationsmatrix_kategorial.pdf", width = 8, height = 5)
corrplot(korrelationsmatrix_kategorial,
         type = "upper", method = "number", number.cex = .7, cl.cex = .7
)
# dev.off()


## numeric features
numerische_features <- reise_21 %>%
  select(alter, haushaltseinkommen, ortsgroesse, haushaltsgroesse)

# bivariate scatter plots
# plot(numerische_features$alter, numerische_features$haushaltseinkommen)
# plot(numerische_features$alter, numerische_features$ortsgroesse)
# plot(numerische_features$alter, numerische_features$haushaltsgroesse)
# plot(numerische_features$haushaltseinkommen, numerische_features$ortsgroesse)
# plot(numerische_features$haushaltseinkommen, numerische_features$haushaltsgroesse)
# plot(numerische_features$ortsgroesse, numerische_features$haushaltsgroesse)

# correlation
korrelationsmatrix_numerisch <- cor(numerische_features, method = "spearman")

# pdf(file = "Reiseanalyse 2020/korrelationsmatrix_numerisch.pdf", width = 8, height = 5)
corrplot(korrelationsmatrix_numerisch,
         type = "upper", method = "number", number.cex = .7, cl.cex = .7
)
# dev.off()



#### Model GBM ############################################################

### hyperparameter tuning
reise_21$gereist <- as.factor(reise_21$gereist)
levels(reise_21$gereist) <- c("no", "yes")

# split dataset to validation+training and testing set
# (in the end 60% should be used for training, 20% for validating, 20% for testing)set.seed(123456)
reise_21_split <- initial_split(reise_21, prop = 0.8)
reise_21_train_val <- training(reise_21_split)
reise_21_test <- testing(reise_21_split)

# set trainControl to use 75% of the validation+training set for training
# and the rest for validating
n_train_val <- nrow(reise_21_train_val)
reise_21_index_val <- sample(n_train_val, 0.25 * n_train_val)
train_control <- trainControl(
  method = "cv", number = 1,
  index = list(Fold1 = seq_len(n_train_val)[-reise_21_index_val]),
  returnData = FALSE
)

# define the tuning grid
search_grid <- expand.grid(
  n.trees = seq(50, 550, by = 100),
  interaction.depth = 1:10,
  shrinkage = seq(0.01, 0.1, by = 0.01),
  n.minobsinnode = seq(5, 100, by = 10)
)


## tune hyperparameters
# use the following line to use a already saved output of the tuning function,
# otherwise skip the next line
gbm_tuning_reise_21 <- readRDS("~/RProjekte/Reiseanalyse 2021/FinalModell/gbm_tuning_reise_21_acc_20210419.rds")

#Session -> Set Working Directory (not on R-Server)

set.seed(456123)
gbm_tuning_reise_21 <- caret::train(gereist ~ .,
                                    data = reise_21_train_val,
                                    method = "gbm",
                                    distribution = "bernoulli",
                                    trControl = train_control,
                                    tuneGrid = search_grid,
                                    metric = "Accuracy"
)
gbm_tuning_reise_21
# saveRDS(gbm_tuning_reise_21, file = "gbm_tuning_reise_21_acc_20210419.rds")

print(gbm_tuning_reise_21$finalModel)
par(mar = c(5, 25, 4, 2))
summary(gbm_tuning_reise_21$finalModel, las = 1)
par(mar = c(5, 4, 4, 2))


# train the model with optimized hyperparameters
gbm_tuning_reise_21$bestTune

head(reise_21_train_val$gereist)
reise_21_train_val$gereist <- ifelse(reise_21_train_val$gereist == "no", 0, 1)

set.seed(78912123)
gbm_reise_21 <- gbm(gereist ~ .,
                    distribution = "bernoulli",
                    data = reise_21_train_val[-reise_21_index_val, ],
                    n.trees = gbm_tuning_reise_21$bestTune$n.trees,
                    interaction.depth = gbm_tuning_reise_21$bestTune$interaction.depth,
                    shrinkage = gbm_tuning_reise_21$bestTune$shrinkage,
                    n.minobsinnode = gbm_tuning_reise_21$bestTune$n.minobsinnode
)
# saveRDS(gbm_reise_21, "gbm_reise_21_acc_20210419.rds")

print(gbm_reise_21)
par(mar = c(5, 10, 4, 2))
summary(gbm_reise_21, las = 1)
par(mar = c(5, 4, 4, 2))


# confusion matrix
gbm_reise_21_response <- predict.gbm(gbm_reise_21,
                                     newdata = reise_21_test,
                                     type = "response"
)

gbm_reise_21_schwelle <- mean(gbm_reise_21_response)
gbm_reise_21_schwelle

gbm_reise_21_prognose <- as.factor(ifelse(
  gbm_reise_21_response > gbm_reise_21_schwelle, "yes", "no"
))
confusionMatrix(gbm_reise_21_prognose, reise_21_test$gereist)

# AUC
gbm_reise_21_testing <- prediction(gbm_reise_21_response, reise_21_test$gereist)
gbm_reise_21_roc <- performance(gbm_reise_21_testing, "tpr", "fpr")
plot(gbm_reise_21_roc)
gbm_reise_21_auc_temp <- performance(gbm_reise_21_testing, "auc")
(gbm_reise_21_auc <- as.numeric(gbm_reise_21_auc_temp@y.values))

# another ROC plot 
# pdf(file = "roc_reise_20.pdf", width = 10, height = 8) 
roc(reise_21_test$gereist, gbm_reise_21_response,
    plot = TRUE, auc.polygon = TRUE, max.auc.polygon = TRUE, grid = TRUE,
    print.auc = TRUE, show.thres = TRUE,
    percent = TRUE, cex.axis = 1.3, cex.lab = 1.3, print.auc.cex = 1.3
)
# dev.off()



#### Interpretation of direction of effects in GBM ############################################################

# bernoulli response
plot(density(predict(gbm_reise_21, n.trees = gbm_tuning_reise_21$bestTune$n.trees, 
                     type = "response")), main = "Bernoulli - Response")

# plot partial dependence plots
plot.gbm(gbm_reise_21, i.var = "geschlecht", type = "response")
plot.gbm(gbm_reise_21, i.var = "alter", type = "response")
plot.gbm(gbm_reise_21, i.var = "herkunft", type = "response")
plot.gbm(gbm_reise_21, i.var = "ortsgroesse", type = "response")
plot.gbm(gbm_reise_21, i.var = "haushaltsgroesse", type = "response")
plot.gbm(gbm_reise_21, i.var = "kind_alter_0_5", type = "response")
plot.gbm(gbm_reise_21, i.var = "kind_alter_6_13", type = "response")
plot.gbm(gbm_reise_21, i.var = "kind_alter_14_17", type = "response")
plot.gbm(gbm_reise_21, i.var = "haushaltseinkommen", type = "response")
plot.gbm(gbm_reise_21, i.var = "vollberufstaetig", type = "response")
plot.gbm(gbm_reise_21, i.var = "bildung", type = "response")
plot.gbm(gbm_reise_21, i.var = "migration", type = "response")


## for interpretation: GBM corresponds to GAM (except for deviation at age = 80)


library(plotmo)
## check residuals
plotres(gbm_reise_21, which = 1:9, grid.col = TRUE)


## 2D-plots as above
# using  mean values:
plotmo(gbm_reise_21, type = "response", persp.ticktype = "detailed", ylim = c(0.4, 1), 
       degree2 = FALSE, all1 = TRUE, ngrid1 = 50, grid.col = TRUE)
# using partial dependence plots (PDP):
plotmo(gbm_reise_21, pmethod = "partdep", persp.ticktype = "detailed", ylim = c(0.40, 1), 
       degree2 = FALSE, all1 = TRUE, ngrid1 = 50, grid.col = TRUE)


## pseudo-3D-plots as PDP-plots
# specify the two variables to plot:
plotmo(gbm_reise_21,
       pmethod = "partdep", persp.ticktype = "detailed", ylim = c(0.4, 1),
       degree1 = FALSE, degree2 = c("migration", "haushaltseinkommen"), all2 = 2
)
plotmo(gbm_reise_21,
       pmethod = "partdep", persp.ticktype = "detailed", ylim = c(0.4, 1),
       degree1 = FALSE, degree2 = c("vollberufstaetig", "bildung"), all2 = 2
)
plotmo(gbm_reise_21,
       pmethod = "partdep", persp.ticktype = "detailed", ylim = c(0.4, 1),
       degree1 = FALSE, degree2 = c("haushaltseinkommen", "herkunft"), all2 = 2
)
plotmo(gbm_reise_21,
       pmethod = "partdep", persp.ticktype = "detailed", ylim = c(0.4, 1),
       degree1 = FALSE, degree2 = c("herkunft", "bildung"), all2 = 2
) # difference between east/west decreases with higher education level
plotmo(gbm_reise_21,
       pmethod = "partdep", persp.ticktype = "detailed", ylim = c(0.4, 1),
       degree1 = FALSE, degree2 = c("ortsgroesse", "haushaltsgroesse"), all2 = 2
)
plotmo(gbm_reise_21,
       pmethod = "partdep", persp.ticktype = "detailed", ylim = c(0.4, 1),
       degree1 = FALSE, degree2 = c("haushaltseinkommen", "ortsgroesse"), all2 = 2
)
plotmo(gbm_reise_21,
       pmethod = "partdep", persp.ticktype = "detailed", ylim = c(0.4, 1),
       degree1 = FALSE, degree2 = c("ortsgroesse", "vollberufstaetig"), all2 = 2
)
plotmo(gbm_reise_21,
       pmethod = "partdep", persp.ticktype = "detailed", ylim = c(0.4, 1),
       degree1 = FALSE, degree2 = c("haushaltsgroesse", "kind_alter_0_5"), all2 = 2
)
plotmo(gbm_reise_21,
       pmethod = "partdep", persp.ticktype = "detailed", ylim = c(0.4, 1),
       degree1 = FALSE, degree2 = c("haushaltsgroesse", "kind_alter_6_13"), all2 = 2
)
plotmo(gbm_reise_21,
       pmethod = "partdep", persp.ticktype = "detailed", ylim = c(0.4, 1),
       degree1 = FALSE, degree2 = c("haushaltsgroesse", "kind_alter_14_17"), all2 = 2
)
plotmo(gbm_reise_21,
       pmethod = "partdep", persp.ticktype = "detailed", ylim = c(0.4, 1),
       degree1 = FALSE, degree2 = c("haushaltsgroesse", "haushaltseinkommen"), all2 = 2
)
plotmo(gbm_reise_21,
       pmethod = "partdep", persp.ticktype = "detailed", ylim = c(0.4, 1),
       degree1 = FALSE, degree2 = c("haushaltsgroesse", "bildung"), all2 = 2, 
       ndiscrete = 10, xlim = 5
) # A small household with low level of education travels as often as a big household
# with high level of education
plotmo(gbm_reise_21,
       pmethod = "partdep", persp.ticktype = "detailed", ylim = c(0.4, 1),
       degree1 = FALSE, degree2 = c("kind_alter_0_5", "kind_alter_14_17"), all2 = 2
)
plotmo(gbm_reise_21,
       pmethod = "partdep", persp.ticktype = "detailed", ylim = c(0.4, 1),
       degree1 = FALSE, degree2 = c("kind_alter_0_5", "haushaltseinkommen"), all2 = 2
)
plotmo(gbm_reise_21,
       pmethod = "partdep", persp.ticktype = "detailed", ylim = c(0.4, 1),
       degree1 = FALSE, degree2 = c("kind_alter_14_17", "vollberufstaetig"), all2 = 2
) # interesting pair of variables
plotmo(gbm_reise_21,
       pmethod = "partdep", persp.ticktype = "detailed", ylim = c(0.4, 1),
       degree1 = FALSE, degree2 = c("alter", "herkunft"), all2 = 2
)
plotmo(gbm_reise_21,
       pmethod = "partdep", persp.ticktype = "detailed", ylim = c(0.4, 1),
       degree1 = FALSE, degree2 = c("geschlecht", "alter"), all2 = 2, swapxy = TRUE
)

# specify one variable (plot all possible pairs):
plotmo(gbm_reise_21,
       pmethod = "partdep", persp.ticktype = "detailed", ylim = c(0.4, 1),
       degree1 = FALSE, degree2 = "geschlecht", all2 = 2, swapxy = TRUE
)
plotmo(gbm_reise_21,
       pmethod = "partdep", persp.ticktype = "detailed", ylim = c(0.4, 1),
       degree1 = FALSE, degree2 = "alter", all2 = 2
)
plotmo(gbm_reise_21,
       pmethod = "partdep", persp.ticktype = "detailed", ylim = c(0.4, 1),
       degree1 = FALSE, degree2 = "herkunft", all2 = 2
)
plotmo(gbm_reise_21,
       pmethod = "partdep", persp.ticktype = "detailed", ylim = c(0.4, 1),
       degree1 = FALSE, degree2 = "ortsgroesse", all2 = 2
)
plotmo(gbm_reise_21,
       pmethod = "partdep", persp.ticktype = "detailed", ylim = c(0.4, 1),
       degree1 = FALSE, degree2 = "haushaltsgroesse", all2 = 2
)


## compare to 2D-plots using heatmaps
plotmo(gbm_reise_21,
       pmethod = "partdep", persp.ticktype = "detailed",
       ylim = c(0.4, 1), degree1 = FALSE, degree2 = "ortsgroesse", all2 = 2
)
plotmo(gbm_reise_21,
       pmethod = "partdep", persp.ticktype = "detailed", type2 = "image",
       ylim = c(0.4, 1), degree1 = FALSE, degree2 = "ortsgroesse", all2 = 2
)



#### Get Values in Partial Dependence Plots for Interpretation ############################################################

## variables with fewer or equal 5 possible outcomes
training_data <- reise_21_train_val[-reise_21_index_val, -1]
str(training_data)

predictions_list <- list()
for (i in seq_along(training_data)) {
  predictions <- c()
  
  if (length(unique(training_data[, i])) <= 5) {
    predictions <- sapply(unique(training_data[, i]), function(unique_level, data = training_data) {
      if (is.factor(training_data[, i])) {
        data[, i] <- factor(unique_level)
      } else {
        data[, i] <- unique_level
      }
      value <- mean(
        predict.gbm(gbm_reise_21,
                                newdata = data, type = "response",
                                n.trees = gbm_tuning_reise_21$bestTune$n.trees
      ))
      names(value) <- as.character(unique_level)
      value
    })
  }
  predictions_list[[i]] <- predictions
}
names(predictions_list) <- colnames(training_data)
predictions_list


## get mean of predicted response values for training data
training_data <- reise_21_train_val[-reise_21_index_val, -1]

mean(predict.gbm(gbm_reise_21,
                 newdata = training_data, type = "response",
                 n.trees = gbm_tuning_reise_20$bestTune$n.trees
))

# compare with real proportion in the training data
mean(reise_21_train_val[-reise_21_index_val, 1])


## numeric variables
# age
training_data <- reise_21_train_val[-reise_21_index_val, -1]

alter_seq <- seq(min(training_data$alter),
                 max(training_data$alter), length.out = 50)

predictions_alter <- sapply(alter_seq, function(x) {
  training_data$alter <- x
  mean(predict.gbm(gbm_reise_21,
                   newdata = training_data, type = "response",
                   n.trees = gbm_tuning_reise_21$bestTune$n.trees
  ))
})
names(predictions_alter) <- round(alter_seq, 1)
predictions_alter

# household income
training_data <- reise_21_train_val[-reise_21_index_val, -1]

haushaltseinkommen_seq <- seq(min(training_data$haushaltseinkommen),
                              max(training_data$haushaltseinkommen), length.out = 50)

predictions_haushaltseinkommen <- sapply(haushaltseinkommen_seq, function(x) {
  training_data$haushaltseinkommen <- x
  mean(predict.gbm(gbm_reise_21,
                   newdata = training_data, type = "response",
                   n.trees = gbm_tuning_reise_21$bestTune$n.trees
  ))
})
names(predictions_haushaltseinkommen) <- round(haushaltseinkommen_seq, 1)
predictions_haushaltseinkommen

# size of town
training_data <- reise_21_train_val[-reise_21_index_val, -1]

ortsgroesse_seq <- seq(min(training_data$ortsgroesse),
                       max(training_data$ortsgroesse), length.out = 50)

predictions_ortsgroesse <- sapply(ortsgroesse_seq, function(x) {
  training_data$ortsgroesse <- x
  mean(predict.gbm(gbm_reise_21,
                   newdata = training_data, type = "response",
                   n.trees = gbm_tuning_reise_21$bestTune$n.trees
  ))
})
names(predictions_ortsgroesse) <- round(ortsgroesse_seq, 1)
predictions_ortsgroesse




#### Model GLM ###########################################################
reise_21_train <- reise_21_train_val[-reise_21_index_val, ]

glm_reise_21 <- glm(gereist ~ .,
                    data = reise_21_train,
                    family = "binomial"
)
summary(glm_reise_21)
Anova(glm_reise_21)


## model quality
glm_reise_21_response <- predict(glm_reise_21, reise_21_test, type = c("response"))
reise_21_test$glm_reise_21_response <- glm_reise_21_response

# pseudo R^2
with(summary(glm_reise_21), 1 - deviance / null.deviance)

# ROC
glm_reise_21_roc <- roc(gereist ~ glm_reise_21_response, data = reise_21_test)
plot(glm_reise_21_roc, print.auc = TRUE)
auc(glm_reise_21_roc)

# confusion matrix
(glm_reise_21_schwelle <- as.numeric(coords(
  glm_reise_21_roc, "best",
  ret = "threshold", transpose = FALSE
)))

glm_reise_21_prognose <- ifelse(
  reise_21_test$glm_reise_21_response > glm_reise_21_schwelle, "yes", "no"
) %>%
  as.factor() %>%
  relevel(ref = "no")

reise_21_test$glm_reise_21_prognose <- glm_reise_21_prognose

confusionMatrix(reise_21_test$gereist,
                reise_21_test$glm_reise_21_prognose,
                positive = "yes"
)



#### Model GAM ###########################################################
str(reise_21)
gam_reise_21 <- gam(gereist ~ s(alter, bs = "ps") + geschlecht + herkunft +
                      ortsgroesse + haushaltsgroesse + kind_alter_0_5 +
                      kind_alter_6_13 + kind_alter_14_17 +
                      s(haushaltseinkommen, bs = "ps") +
                      vollberufstaetig + bildung + migration,
                    # + lebt_in_bayern,
                    data = reise_21_train,
                    family = binomial
)
summary(gam_reise_21)

plot(gam_reise_21,
     shade = TRUE, jit = TRUE
)


## model quality
gam_reise_21_response <- predict(gam_reise_21, reise_21_test, type = c("response"))
reise_21_test$gam_reise_21_response <- gam_reise_21_response

# ROC
gam_reise_21_roc <- roc(gereist ~ gam_reise_21_response, data = reise_21_test)
plot(gam_reise_21_roc, print.auc = TRUE)
auc(gam_reise_21_roc)

# confusion matrix
(gam_reise_21_schwelle <- as.numeric(coords(
  gam_reise_21_roc, "best",
  ret = "threshold", transpose = FALSE
)))

gam_reise_21_prognose <- ifelse(
  reise_21_test$gam_reise_21_response > gam_reise_21_schwelle, "yes", "no"
) %>%
  as.factor() %>%
  relevel(ref = "no")

reise_21_test$gam_reise_21_prognose <- gam_reise_21_prognose

confusionMatrix(reise_21_test$gereist,
                reise_21_test$gam_reise_21_prognose,
                positive = "yes"
)


## GAM-plots with scatter plot
library(mgcViz)

viz_object <- getViz(gam_reise_21)

# all variables
print(plot(viz_object, allTerms = TRUE), pages = 1)

# smooth terms: age, household income
plot_alter <- plot(sm(viz_object, 1))
plot_alter +
  l_fitLine(colour = "red") +
  l_rug(mapping = aes(x = x, y = y), alpha = 0.8) +
  l_ciLine(mul = 5, colour = "blue", linetype = 2) +
  l_points(shape = 19, size = 1, alpha = 0.1) +
  theme_classic()

plot_haushaltseinkommen <- plot(sm(viz_object, 2))
plot_haushaltseinkommen +
  l_fitLine(colour = "red") +
  l_rug(mapping = aes(x = x, y = y), alpha = 0.8) +
  l_ciLine(mul = 5, colour = "blue", linetype = 2) +
  l_points(shape = 19, size = 1, alpha = 0.1) +
  theme_classic()


## GAM-diagnostics
check(viz_object)
