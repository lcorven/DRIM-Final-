library(readxl)
library(tidyverse)
library(questionr)
library(fUnitRoots)
library(seastests)
library(tsoutliers)
library(seasonal)
library(FactoMineR)
library(lgarch)
library(gets)
library(glmnet)
library(rbridge)
library(ncvreg)
library(forecast)
library(lmtest)
library(mgcv)


D1 <- read_xlsx("D1.xlsx")
D1train <- D1[1:72,]
D1test <- D1[73:84,]
#D2 <- read_xlsx("D2.xlsx")

D1_1 <- read.csv2("Var_a_expliquer_diff1.csv")
D1_1 <- D1_1[,-1]

### Création de base train et test pour tester notre méthode alternative
D1_1train <- D1_1[1:71,]
D1_1test <- D1_1[72:83,]

### Préparation du script pour la base test 
#Basetest <- read.csv2("Base_test_var_explicative.csv")
#Basetest <- as.matrix(Basetest[,-1])
Base <- read.csv2("Base_train_var_explicative.csv")
Base <- Base[,-1]
Basetrain <- Base[1:71,]
Basetest <- Base[72:83,]
#Basetrain <- as.matrix(Basetrain)#On enlève la première ligne, car il nous 
#82 observations et non 83 pour l'instant
#Basetest <- as.matrix(Basetest)
str(Basetrain)

Base[,22:27] <- lapply(Base[,22:27],as.factor)
Basetest[,22:27] <- lapply(Basetest[,22:27],as.factor)
Basetrain[,22:27] <- lapply(Basetrain[,22:27],as.factor)


### Base test de la série à expliquer retardée d'un mois 
#DTest <- D1[72:83,] 
#DTest[1,] <- D1[71,] 

## Test de sélection de variables et optimisation d'une série ##

#Methode LASSO 
#### LASSO ####
Basetrain$y <- as.matrix(D1train[-1,1])
Basetrain$x <- as.matrix(D1_1train[,1])
model_LASSO <- glmnet(Basetrain[12:71,-c(1,18,19,21:27,35)],Basetrain[12:71,35], alpha = 1, standardize = T)
plot(model_LASSO)
lambdas_to_try <- 10^seq(-3, 5, length.out = 100)
lasso_cv <- cv.glmnet(as.matrix(Basetrain[12:71,-c(1,18,19,21:27,35)]),as.matrix(Basetrain[12:71,35]), alpha = 1, lambda = lambdas_to_try, standardize = T, nfolds = 10) # choix du meilleur lambda parmi 100
plot(lasso_cv)
lambda_lasso <- lasso_cv$lambda.1se

model_lasso <- glmnet(Basetrain[12:71,-c(1,18,19,21:27,35)],Basetrain[12:71,35], alpha = 1, lambda = lambda_lasso, standardize = T)
model_lasso$beta

var_Lasso <- which(! coef(model_lasso) == 0, arr.ind = TRUE)
var_Lasso


str(Basetrain)
resuR <- vector("numeric",110)
prev <- matrix(nrow = 12,ncol = 110)

##### Test sur le jeu d'entraînement et le jeu test #####
#### LASSO ####
for (i in 1:110){
  Basetrain$y <- as.matrix(D1train[-1,i])
  Basetrain$x <- as.matrix(D1_1train[,i])
  model_LASSO <- glmnet(Basetrain[12:71,-c(1,18,19,21:27,35)],Basetrain[12:71,35], alpha = 1, standardize = T)
  lambdas_to_try <- 10^seq(-3, 5, length.out = 100)
  lasso_cv <- cv.glmnet(as.matrix(Basetrain[12:71,-c(1,18,19,21:27,35)]),as.matrix(Basetrain[12:71,35]), alpha = 1, lambda = lambdas_to_try, standardize = T, nfolds = 10) # choix du meilleur lambda parmi 100
  lambda_lasso <- lasso_cv$lambda.1se
  model_lasso <- glmnet(Basetrain[12:71,-c(1,18,19,21:27,35)],Basetrain[12:71,35], alpha = 1, lambda = lambda_lasso, standardize = T)
  var_Lasso <- which(! coef(model_lasso) == 0, arr.ind = TRUE)
  print(i)
  print(var_Lasso)
  row <- rownames(var_Lasso)
  col <- row[!row %in% "(Intercept)"]
  if (length(col) > 0){
    Basetrain$y <- as.matrix(D1train[-1,i])
    Basetrain$x <- as.matrix(D1_1train[,i])
    gam <- gam(as.formula(paste0("y", " ~ ", paste(paste("s(", col, ")", sep = ""), collapse=" + "))), drop.intercept = FALSE,data=Basetrain)
    Basetest$x <- as.matrix(D1_1test[,i])
    Basetest$x_1v2 <- Basetest$x
    Basetest$Predx <- Basetest$x_1v2
    for (j in 1:11) {
      Basetest[j,37] <- predict(gam,Basetest[j,])
      Basetest[j+1,35] <- Basetest[j,37]
    }
    Basetest[12,37] <- predict(gam,Basetest[12,])
    prev[,i] <- Basetest[,37]
  }
  else{
    prev[,i] <- mean(as.matrix(D1train[,i]))
  }
  resuR[i] <- summary(gam)$r.sq 
}

resuR <- t(resuR)
resuR <- t(resuR)
plot(resuR)
summary(gam)

#### LASSO ####
for (i in 1:110){
  Basetrain$y <- as.matrix(D1train[-1,i])
  Basetrain$x <- as.matrix(D1_1train[,i])
  model_LASSO <- glmnet(Basetrain[12:71,-c(1,18,19,21:27,35)],Basetrain[12:71,35], alpha = 1, standardize = T)
  lambdas_to_try <- 10^seq(-3, 5, length.out = 100)
  lasso_cv <- cv.glmnet(as.matrix(Basetrain[12:71,-c(1,18,19,21:27,35)]),as.matrix(Basetrain[12:71,35]), alpha = 1, lambda = lambdas_to_try, standardize = T, nfolds = 10) # choix du meilleur lambda parmi 100
  lambda_lasso <- lasso_cv$lambda.1se
  model_lasso <- glmnet(Basetrain[12:71,-c(1,18,19,21:27,35)],Basetrain[12:71,35], alpha = 1, lambda = lambda_lasso, standardize = T)
  var_Lasso <- which(! coef(model_lasso) == 0, arr.ind = TRUE)
  print(i)
  print(var_Lasso)
  row <- rownames(var_Lasso)
  col <- row[!row %in% "(Intercept)"]
  if (length(col) > 0){
    Basetrain$y <- as.matrix(D1train[-1,i])
    Basetrain$x <- as.matrix(D1_1train[,i])
    gam <- gam(as.formula(paste0("y", " ~ ", paste(paste("s(", col, ")", sep = ""), collapse=" + "))), drop.intercept = FALSE,data=Basetrain)
    Basetest$x <- as.matrix(D1_1test[,i])
    Basetest$x_1v2 <- Basetest$x
    Basetest$Predx <- Basetest$x_1v2
    for (j in 1:11) {
      Basetest[j,37] <- predict(gam,Basetest[j,])
      Basetest[j+1,35] <- Basetest[j,37]
    }
    Basetest[12,37] <- predict(gam,Basetest[12,])
    prev[,i] <- Basetest[,37]
  }
  else{
    Basetrain$y <- as.matrix(D1train[-1,i])
    Basetrain$x <- as.matrix(D1_1train[,i])
    gam <- gam(y ~ x + date_T, drop.intercept = FALSE,data=Basetrain)
    Basetest$x <- as.matrix(D1_1test[,i])
    Basetest$x_1v2 <- Basetest$x
    Basetest$Predx <- Basetest$x_1v2
    for (j in 1:11) {
      Basetest[j,37] <- predict(gam,Basetest[j,])
      Basetest[j+1,35] <- Basetest[j,37]
    }
    Basetest[12,37] <- predict(gam,Basetest[12,])
    prev[,i] <- Basetest[,37]
  }
  resuR[i] <- summary(gam)$r.sq 
}

resuR <- t(resuR)
resuR <- t(resuR)
plot(resuR)
summary(gam)




##### Application sur la base final #####
resuRfinal <- vector("numeric",110)
prevFinal <- matrix(nrow = 36,ncol = 110)

for (i in 1:110){
  Base$y <- as.matrix(D1[-1,i])
  Base$x <- as.matrix(D1_1[,i])
  model_LASSO <- glmnet(Base[12:83,-c(1,18,19,21:27,35)],Base[12:83,35], alpha = 1, standardize = T)
  lambdas_to_try <- 10^seq(-3, 5, length.out = 100)
  lasso_cv <- cv.glmnet(as.matrix(Base[12:83,-c(1,18,19,21:27,35)]),as.matrix(Base[12:83,35]), alpha = 1, lambda = lambdas_to_try, standardize = T, nfolds = 10) # choix du meilleur lambda parmi 100
  lambda_lasso <- lasso_cv$lambda.1se
  model_lasso <- glmnet(Base[12:83,-c(1,18,19,21:27,35)],Base[12:83,35], alpha = 1, lambda = lambda_lasso, standardize = T)
  var_Lasso <- which(! coef(model_lasso) == 0, arr.ind = TRUE)
  print(i)
  print(var_Lasso)
  row <- rownames(var_Lasso)
  col <- row[!row %in% "(Intercept)"]
  if (length(col) > 0){
    Base$y <- as.matrix(D1[-1,i])
    Base$x <- as.matrix(D1_1[,i])
    gam <- gam(as.formula(paste0("y", " ~ ", paste(paste("s(", col, ")", sep = ""), collapse=" + "))), drop.intercept = FALSE,data=Base)
    basetest3$x <- as.matrix(D1_1train[1:36,i])
    basetest3$x[1] <- Base$y[83]
    basetest3$x_1v2 <- basetest3$x
    basetest3$Predx <- basetest3$x_1v2
    for (j in 1:35) {
      basetest3[j,40] <- predict(gam,basetest3[j,])
      basetest3[j+1,38] <- basetest3[j,40]
    }
    basetest3[36,40] <- predict(gam,basetest3[36,])
    r2 <- summary(gam)$r.sq
    if (r2 <0.4){
      prevFinal[,i] <- mean(as.matrix(D1[,i]))
    }
    else {
      prevFinal[,i] <- basetest3[,40]
    }
  }
  else{
    prevFinal[,i] <- mean(as.matrix(D1[,i]))
  }
  resuRfinal[i] <- summary(gam)$r.sq 
}

resuRfinal <- t(resuRfinal)
resuRfinal <- t(resuRfinal)
plot(resuRfinal)
summary(gam)
prevFinal
prevFinal[,90] <- mean(as.matrix(D1[,90]))
prevFinal[,93] <- mean(as.matrix(D1[,93]))

lst <- vector(mode = "list", length = 36)

for (i in 1:36){
  resuc1 <- t(as.data.frame(prevFinal[1,1:11]))
  resuc2 <- t(as.data.frame(prevFinal[i,12:22]))
  resuc3 <- t(as.data.frame(prevFinal[i,23:33]))
  resuc4 <- t(as.data.frame(prevFinal[i,34:44]))
  resuc5 <- t(as.data.frame(prevFinal[i,45:55]))
  resuc6 <- t(as.data.frame(prevFinal[i,56:66]))
  resuc7 <- t(as.data.frame(prevFinal[i,67:77]))
  resuc8 <- t(as.data.frame(prevFinal[i,78:88]))
  resuc9 <- t(as.data.frame(prevFinal[i,89:99]))
  resuc10 <- t(as.data.frame(prevFinal[i,100:110]))
  resuc <- rbind(resuc1, resuc2, resuc3, resuc4, resuc5, resuc6, resuc7, resuc8, resuc9, resuc10)
  write.csv(resuc,paste("./PITprojgam/",i,".csv", sep=""))
}

