#### Librairies ####
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
library(stargazer)
library(ggplot2)


#### Importation ####
Taux_defaut <- read_xlsx("WE12.xlsx")
Var <- read_xlsx("Base_variables_explicatives.xlsx")
Taux_defaut <- Taux_defaut[,-c(3:6)]

#### Transformation variable Date ####
Var[,2:17] <- lapply(Var[,2:17],as.numeric)
Taux_defaut$dtf_per_trt <- format(Taux_defaut$dtf_per_trt,"%Y-%m")
str(Taux_defaut)
Var$Date  <- as.Date(Var$Date)
Var$Date  <- format(Var$Date,"%Y-%m")
str(Var)

Taux_defaut <- rename.variable(Taux_defaut,"dtf_per_trt","Date")


#### Création base Var de 2010 à 2019 et Var 2020 ####
Var1019 <- Var[1:120,]
Var20 <- Var[121:132,]


#### Merge de la base des var explicative et var à expliquer ####
Base <- merge(Taux_defaut,Var1019,by = "Date")
cor <- cor(Base[,2:18])

#### Création des nouvelles variables ####
## Création we12_N_1 et we12_N_12 ##
Base$we12_N_1 <- Base$we12
Base$we12_N_12 <- Base$we12
Base[1,19] <- NA
Base[1:12,20] <- NA
Base[2:120,19] <- Base[1:119,2]
Base[13:120,20] <- Base[1:108,2]

## Création variables Rendement_obligations_N_1 et N_2 ##
Base$Rendement_obligations_N_1 <- Base$Rendement.des.obligations.d.états.sur.10ans
Base$Rendement_obligations_N_2 <- Base$Rendement.des.obligations.d.états.sur.10ans
Base[1:12,21] <- NA
Base[1:24,22] <- NA
Base[13:120,21] <- Base[1:108,10]
Base[25:120,22] <- Base[1:96,10]

## Création de la variable mois ##
date <- matrix(rep(1:12,10))
date <- as.data.frame(date)
Base$date_M <- date[1:120,]
Base$date_M <- as.factor(Base$date_M)
#Base$date_M <- as.numeric(Base$date_M)

## Création des variables trimestre ##
Base$date_T <- date[1:120,]
Base$date_T <- as.factor(Base$date_T)

Base$date_T[Base$date_T == 1|Base$date_T == 2|Base$date_T == 3] <- 1 
Base$date_T[Base$date_T == 4|Base$date_T == 5|Base$date_T == 6] <- 2 
Base$date_T[Base$date_T == 7|Base$date_T == 8|Base$date_T == 9] <- 3 
Base$date_T[Base$date_T == 10|Base$date_T == 11|Base$date_T == 12] <- 4 

Base$date_T1 <- "0"
Base$date_T1[Base$date_T == 1] <- "1"
Base$date_T1 <- as.factor(Base$date_T1)

Base$date_T2 <- "0"
Base$date_T2[Base$date_T == 2] <- "1"
Base$date_T2 <- as.factor(Base$date_T2)

Base$date_T3 <- "0"
Base$date_T3[Base$date_T == 3] <- "1"
Base$date_T3 <- as.factor(Base$date_T2)

Base$date_T4 <- "0"
Base$date_T4[Base$date_T == 4] <- "1"
Base$date_T4 <- as.factor(Base$date_T4)

Taux_defaut_obs <- read.csv2("dr_pit_serie_2010_2017.csv",sep=",")
Taux_defaut_obs <- Taux_defaut_obs[,-1]
Taux_defaut_obs <- as.numeric(Taux_defaut_obs)
Taux_defaut_diff <- Taux_defaut_obs[-84]
