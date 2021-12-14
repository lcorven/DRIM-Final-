###### avec we12 retardée : #####

##### Approche GETS #####

dlbase <- data.frame(Base_corrige3[13:84,c(2:21,29:35)])
mX = data.matrix(dlbase[,-1])

### Base test : 
base_test3 <- Base_corrige3[85:120,-1]

## Gets variable selection
# ARX model with AR(1)
Model <- arx(dlbase[13:84,1], mc = T, ar = 1, mxreg = mX[,c(17,18)], vcov.type = "ordinary")
modelgets <- getsm(Model) 
modelgets
coef_gets <- coef.arx(modelgets)						# Gets betas
var_gets <- names(coef.arx(modelgets))		# Get the name of relevant variables
var_gets <- var_gets[-1] # remove the AR(1) coef.
var_gets




#### Sélection de variables ####

##### R?gression p?nalis?es #####

#### RIDGE ####
model_ridge <- glmnet(mX, dlbase[,1], alpha = 0, standardize = T)
plot(model_ridge)
lambdas_to_try <- 10^seq(-3, 5, length.out = 100)
ridge_cv <- cv.glmnet(mX, dlbase[,1], alpha = 0, lambda = lambdas_to_try, standardize = T, nfolds = 10) 
plot(ridge_cv)
lambda_ridge <- ridge_cv$lambda.min
lambda_ridge

model_ridge <- glmnet(mX, dlbase[,1], alpha = 0, lambda = lambda_ridge, standardize = T)
summary(model_ridge)
model_ridge$beta

var_ridge <- which(! coef(model_ridge) == 0, arr.ind = TRUE)
var_ridge

#### LASSO ####
model_LASSO <- glmnet(mX, dlbase[,1], alpha = 1, standardize = T)
plot(model_LASSO)
lambdas_to_try <- 10^seq(-3, 5, length.out = 100)
lasso_cv <- cv.glmnet(mX, dlbase[,1], alpha = 1, lambda = lambdas_to_try, standardize = T, nfolds = 10) # choix du meilleur lambda parmi 100
plot(lasso_cv)
lambda_lasso <- lasso_cv$lambda.1se

model_lasso <- glmnet(mX, dlbase[,1], alpha = 1, lambda = lambda_lasso, standardize = T)
model_lasso$beta

var_Lasso <- which(! coef(model_lasso) == 0, arr.ind = TRUE)
var_Lasso


#### Elastic-Net ####
lambdas_to_try <- 10^seq(-3, 5, length.out = 100)
en_cv <- cv.glmnet(mX, dlbase[,1], alpha = 0.5, lambda = lambdas_to_try, standardize = T, nfolds = 10) 
plot(en_cv)
lambda_Elasticnet <- en_cv$lambda.1se

model_Elasticnet <- glmnet(mX, dlbase[,1], alpha = 0.5, lambda = lambda_Elasticnet, standardize = T)
model_Elasticnet$beta

var_Elasticnet <- which(! coef(model_Elasticnet) == 0, arr.ind = TRUE)
var_Elasticnet

## Recherche du meilleur alpha : 
# Choose alpha sequencially with 0 < alpha < 1: a = {0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9}
en_min <- NULL
lambdas_to_try <- 10^seq(-3, 5, length.out = 100)
alphalist <- seq(0.1,by=0.1)
elasticnet <- lapply(alphalist, function(a){
  cv.glmnet(mX, dlbase[,1], alpha=a, lambda = lambdas_to_try, standardize = T, nfolds = 10)
})
for (i in 1:9) {
  print(min(elasticnet[[i]]$cvm))
  en_min <- c(en_min, min(elasticnet[[i]]$cvm))
}
elasticnet_cvm <- min(en_min)
elasticnet_cvm

# Best lambda obtained from CV (lambda.1se) - other possibility: lambda.min
en_cv <- cv.glmnet(mX, dlbase[,1], alpha = 0.5, lambda = lambdas_to_try, standardize = T, nfolds = 10) 
plot(en_cv)
lambda_elasticnet <- en_cv$lambda.1se
model_elasticnet <- glmnet(mX, dlbase[,1], alpha = 0.5, lambda = lambda_elasticnet, standardize = T)

model_elasticnet$beta

var_elasticnet <- which(! coef(model_elasticnet) == 0, arr.ind = TRUE) 
var_elasticnet


#### Adaptive LASSO using Ridge pour les poids ####
### Basé sur Ridge pour calculer les poids ###

model_lassor <- glmnet(mX, dlbase[,1], alpha = 0, lambda = lambda_elasticnet, standardize = T)
coef_ridge <- predict(model_lassor,type="coef",s=lambda_elasticnet)

gamma = 0.5
w0 <- 1/(abs(coef_ridge) + (1/length(dlbase$we12)))
poids.ridge <- w0^(gamma)
poids <- poids.ridge@x

# Adaptive LASSO
fit_adalassor <- glmnet(mX, dlbase[,1], penalty.factor =poids[-1])
fit_cv_adalassor <- cv.glmnet(mX, dlbase[,1],penalty.factor=poids[-1])
plot(fit_cv_adalassor)

# Best lambda obtained from CV (lambda.1se) - other possibility: lambda.min
lambda_adalassor <- fit_cv_adalassor$lambda.1se

model_lassor <- glmnet(mX, dlbase[,1], alpha = 1, lambda = lambda_adalassor, standardize = T)
model_lassor$beta

var_adalassor <- which(! coef(model_lassor) == 0, arr.ind = TRUE)
var_adalassor


#### SCAD ####

fit_SCAD=ncvreg(mX, dlbase[,1], penalty = c("SCAD"))
plot(fit_SCAD)
summary(fit_SCAD, lambda=0.0038)

# Validation crois? pour le meilleur lambda 
cvfit_SCAD=cv.ncvreg(mX, dlbase[,1], penalty = c("SCAD"))
plot(cvfit_SCAD)

# On attribue le meilleur lambda 
lambda_SCAD <- cvfit_SCAD$lambda.min

#Modele finale 
SCAD_Final=ncvreg(mX, dlbase[,1], lambda=lambda_SCAD, alpha = 1)
SCAD_Final$beta

var_SCAD <- which(! coef(SCAD_Final) == 0, arr.ind = TRUE)
var_SCAD



#### R?captitulatif des diff?rents mod?les ####

### Approche GETS ###
modelgets
#Variables s?lectionn?es et leurs significativit?
beta_gets <- coeftest(modelgets)
var_gets
#Num?ro des variables : ar1 seulement

### Approche Ridge ###
model_ridge$beta
var_ridge
#Ici une grosse partie des variables ont ?t?s retenues
#car cette m?thode ne supprime pas n?cessairement des 
#variables ? l'origine

### Approche LASSO ###
model_lasso$beta
var_Lasso
#Num?ro des variables : 18 et 20

### Elastic-Net ###
model_elasticnet$beta
var_elasticnet
#Num?ro des variables : 6,7,10,11,18,19,20


### Adaptive Lasso - poids Ridge (Pr?f?r?) ###
model_lassor$beta
var_adalassor
#Num?ro des variables : 18 et 20


### SCAD ###
SCAD_Final$beta
var_SCAD
#Les variables sélectionnées: 6,7,8,9,11,12,15:19,21:23,25,27


###### sans we12 retardée : #####

#### Sélection de variables ####

##### R?gression p?nalis?es #####

mXs <- mX[,-c(17,18)]

#### RIDGE ####
model_ridge <- glmnet(mXs, dlbase$we12, alpha = 0, standardize = T)
plot(model_ridge)
lambdas_to_try <- 10^seq(-3, 5, length.out = 100)
ridge_cv <- cv.glmnet(mXs, dlbase$we12, alpha = 0, lambda = lambdas_to_try, standardize = T, nfolds = 10) 
plot(ridge_cv)
lambda_ridge <- ridge_cv$lambda.min
lambda_ridge

model_ridge <- glmnet(mXs, dlbase$we12, alpha = 0, lambda = lambda_ridge, standardize = T)
summary(model_ridge)
model_ridge$beta

var_ridge <- which(! coef(model_ridge) == 0, arr.ind = TRUE)
var_ridge

#### LASSO ####
model_LASSO <- glmnet(mXs, dlbase$we12, alpha = 1, standardize = T)
plot(model_LASSO)
lambdas_to_try <- 10^seq(-3, 5, length.out = 100)
lasso_cv <- cv.glmnet(mXs, dlbase$we12, alpha = 1, lambda = lambdas_to_try, standardize = T, nfolds = 10) # choix du meilleur lambda parmi 100
plot(lasso_cv)
lambda_lasso <- lasso_cv$lambda.1se

model_lasso <- glmnet(mXs, dlbase$we12, alpha = 1, lambda = lambda_lasso, standardize = T)
model_lasso$beta

var_Lasso <- which(! coef(model_lasso) == 0, arr.ind = TRUE)
var_Lasso

#### Elastic-Net ####
lambdas_to_try <- 10^seq(-3, 5, length.out = 100)
en_cv <- cv.glmnet(mXs, dlbase$we12, alpha = 0.5, lambda = lambdas_to_try, standardize = T, nfolds = 10) 
plot(en_cv)
lambda_Elasticnet <- en_cv$lambda.1se

model_Elasticnet <- glmnet(mXs, dlbase$we12, alpha = 0.5, lambda = lambda_Elasticnet, standardize = T)
model_Elasticnet$beta

var_Elasticnet <- which(! coef(model_Elasticnet) == 0, arr.ind = TRUE)
var_Elasticnet

## Recherche du meilleur alpha : 
# Choose alpha sequencially with 0 < alpha < 1: a = {0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9}
en_min <- NULL
lambdas_to_try <- 10^seq(-3, 5, length.out = 100)
alphalist <- seq(0.1,by=0.1)
elasticnet <- lapply(alphalist, function(a){
  cv.glmnet(mXs, dlbase$we12, alpha=a, lambda = lambdas_to_try, standardize = T, nfolds = 10)
})
for (i in 1:9) {
  print(min(elasticnet[[i]]$cvm))
  en_min <- c(en_min, min(elasticnet[[i]]$cvm))
}
elasticnet_cvm <- min(en_min)
elasticnet_cvm

# Best lambda obtained from CV (lambda.1se) - other possibility: lambda.min
en_cv <- cv.glmnet(mXs, dlbase$we12, alpha = 0.5, lambda = lambdas_to_try, standardize = T, nfolds = 10) 
plot(en_cv)
lambda_elasticnet <- en_cv$lambda.1se
model_elasticnet <- glmnet(mXs, dlbase$we12, alpha = 0.5, lambda = lambda_elasticnet, standardize = T)

model_elasticnet$beta

var_elasticnet <- which(! coef(model_elasticnet) == 0, arr.ind = TRUE) 
var_elasticnet


#### Adaptive LASSO using Ridge pour les poids ####
### Basé sur Ridge pour calculer les poids ###

model_lassor <- glmnet(mXs, dlbase$we12, alpha = 0, lambda = lambda_elasticnet, standardize = T)
coef_ridge <- predict(model_lassor,type="coef",s=lambda_elasticnet)

gamma = 0.5
w0 <- 1/(abs(coef_ridge) + (1/length(dlbase$we12)))
poids.ridge <- w0^(gamma)
poids <- poids.ridge@x

# Adaptive LASSO
fit_adalassor <- glmnet(mXs, dlbase$we12, penalty.factor =poids[-1])
fit_cv_adalassor <- cv.glmnet(mXs, dlbase$we12,penalty.factor=poids[-1])
plot(fit_cv_adalassor)

# Best lambda obtained from CV (lambda.1se) - other possibility: lambda.min
lambda_adalassor <- fit_cv_adalassor$lambda.1se

model_lassor <- glmnet(mXs, dlbase$we12, alpha = 1, lambda = lambda_adalassor, standardize = T)
model_lassor$beta

var_adalassor <- which(! coef(model_lassor) == 0, arr.ind = TRUE)
var_adalassor


#### SCAD ####

fit_SCAD=ncvreg(mXs, dlbase$we12, penalty = c("SCAD"))
plot(fit_SCAD)
summary(fit_SCAD, lambda=0.0038)

# Validation crois? pour le meilleur lambda 
cvfit_SCAD=cv.ncvreg(mXs, dlbase$we12, penalty = c("SCAD"))
plot(cvfit_SCAD)

# On attribue le meilleur lambda 
lambda_SCAD <- cvfit_SCAD$lambda.min

#Modele finale 
SCAD_Final=ncvreg(mXs, dlbase$we12, lambda=lambda_SCAD, alpha = 1)
SCAD_Final$beta

var_SCAD <- which(! coef(SCAD_Final) == 0, arr.ind = TRUE)
var_SCAD



#### R?captitulatif des diff?rents mod?les ####

### Approche GETS ###
modelgets
#Variables s?lectionn?es et leurs significativit?
beta_gets <- coeftest(modelgets)
var_gets
#Num?ro des variables : AR(1) seulement

### Approche Ridge ###
model_ridge$beta
var_ridge
#Ici une grosse partie des variables ont ?t?s retenues
#car cette m?thode ne supprime pas n?cessairement des 
#variables ? l'origine

### Approche LASSO ###
model_lasso$beta
var_Lasso
#Num?ro des variables : 6, 7, 10, 11, 20

### Elastic-Net ###
model_elasticnet$beta
var_elasticnet
#Num?ro des variables : 6, 7, 10, 11, 20, 33,34


### Adaptive Lasso - poids Ridge (Pr?f?r?) ###
model_lassor$beta
var_adalassor
#Num?ro des variables : 1, 2, 5:8, 10:13, 15, 20, 30,31,33,34


### SCAD ###
SCAD_Final$beta
var_SCAD
#Les variables sélectionnées: 5,6,7,8,9,11,12,13,16,17,20,28:31,33,34