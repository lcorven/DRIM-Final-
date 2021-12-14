#### Modélisation GAM sans correction ####
basetrain <- Base_corrige[2:84,-1]
basetest <- Base_corrige[85:120,-1]
basetrain$we12 <- Taux_defaut_obs[-1]
basetrain$we12_N_1 <- Taux_defaut_diff
str(basetrain$we12)
basetrain$we12_N_12[12:83] <- Taux_defaut_obs[1:72]
basetest$we12_N_1[1]<- Taux_defaut_obs[84]
basetest$we12_N_12[1:12]<- Taux_defaut_obs[73:84]
basetest$we12_N_1v2 <- basetest$we12_N_1 #A faire seulement une fois


str(basetrain$date_T3)

#### GAM version 5 ####
gam <- gam(we12 ~#Incertitude.Europe +
              #Incertitude.Italie+
              #Rendement.des.obligations.d.états.sur.10ans +
              #Rendement_obligations_N_18 +
              Rendement_obligations_N_1 +
              #Rendement_obligations_N_2+
              #Indice_prix_growth_pastyear_N_1+
              #Indice_prix_growth_pastyear_N_2+
              #Taux.de.change +
              #Taux.interbancaire+
              #Production.industrielle+
              #Indice.des.prix.à.la.production+
              #CCI +
              #Indice.des.prix.à.la.conso+
              d.CLI+
              #Indice.des.prix.à.la.conso.growth_pastyear+
              we12_N_12+
              #date1+
              #date2+
              #date3+
              #date4+
              #datesem2+
              #date+
              #date1_N_1+
              #date2_N_1+
              #date3_N_1+
              #date4_N_1+
              #date_N_1+
              #date_N3_1+
              #date1_N3_1+
              #date2_N3_1+
              #date3_N3_1+
              s(we12_N_1,by=date_T3), drop.intercept = FALSE ,data = basetrain[13:83,])
summary(gam)

basetest$we12_N_1 <- basetest$we12_N_1v2
basetest$PredAR <- basetest$we12_N_1v2

for (i in 1:24) {
  basetest[i,36] <- predict(gam,basetest[i,])
  basetest[i+1,18] <- basetest[i,36]
  basetest[i+12,19] <- basetest[i,36]
}
for (i in 25:35) {
  basetest[i,36] <- predict(gam,basetest[i,])
  basetest[i+1,18] <- basetest[i,36]
}
basetest[36,36] <- predict(gam,basetest[36,])
pred_GAM <- basetest$PredAR
basetest[,1]
pred_GAM
par(mfrow=c(1,1))
plot(pred_GAM)
plot(basetest[,1])
#plot(basetrain[,1])

##GAM##
observe <- basetest[,1]
err_GAM <- (observe - pred_GAM)/observe
err_GAMmoy <- mean(abs(err_GAM))
err_GAMmoy



#### Modélisation GAM avec correction atypique ####
basetrain2 <- Base_corrige2[2:84,-1]
basetest2 <- Base_corrige2[85:120,-1]

basetest2$we12_N_1v2 <- basetest2$we12_N_1 #A faire seulement une fois

basetrain2$date_T3 <- as.factor(basetrain2$date_T3)
basetest2$date_T3 <- as.factor(basetest2$date_T3)

gam2 <- gam(we12 ~#Incertitude.Europe +
             #Incertitude.Italie+
             #Rendement.des.obligations.d.états.sur.10ans +
             #Rendement_obligations_N_18 +
             Rendement_obligations_N_1 +
             #Rendement_obligations_N_2+
             #Indice_prix_growth_pastyear_N_1+
             #Indice_prix_growth_pastyear_N_2+
             #Taux.de.change +
             #Taux.interbancaire+
             #Production.industrielle+
             #Indice.des.prix.à.la.production+
             #CCI +
             #Indice.des.prix.à.la.conso+
             d.CLI+
             #Indice.des.prix.à.la.conso.growth_pastyear+
             we12_N_12+
             #date1+
             #date2+
             #date3+
             #date4+
             #datesem2+
             #date+
             #date1_N_1+
             #date2_N_1+
             #date3_N_1+
             #date4_N_1+
             #date_N_1+
           #date_N3_1+
           #date1_N3_1+
           #date2_N3_1+
           #date3_N3_1+
           s(we12_N_1,by=date_T3), drop.intercept = FALSE ,data = basetrain2[13:83,])
summary(gam2)

basetest2$we12_N_1 <- basetest2$we12_N_1v2
basetest2$PredAR <- basetest2$we12_N_1v2

for (i in 1:24) {
  basetest2[i,36] <- predict(gam2,basetest2[i,])
  basetest2[i+1,18] <- basetest2[i,36]
  basetest2[i+12,19] <- basetest2[i,36]
}
for (i in 25:35) {
  basetest2[i,36] <- predict(gam2,basetest2[i,])
  basetest2[i+1,18] <- basetest2[i,36]
}
basetest2[36,36] <- predict(gam2,basetest2[36,])
basetest2$Predict <- basetest2$PredAR
pred_GAM2 <- basetest2$Predict
basetest2[,1]
pred_GAM2
#par(mfrow=c(1,1))
plot(pred_GAM2)
plot(basetest2[,1])
#plot(basetrain[,1])

##GAM##
err_GAM2 <- (observe - pred_GAM2)/observe
err_GAMmoy2 <- mean(abs(err_GAM2))
err_GAMmoy2


#### Modélisation GAM avec correction atypique ####
basetrain3 <- data.frame(Base_corrige3[2:84,-1])
basetest3 <- Base_corrige3[85:120,-1]
basetrain3[,22:27] <- lapply(basetrain3[,22:27],as.factor)
basetest3[,22:27] <- lapply(basetest3[,22:27],as.factor)
#write.csv2(basetrain3,"Base_train_var_explicative.csv")
#write.csv2(basetest3,"Base_test_var_explicative.csv")
basetrain3$we12 <- Taux_defaut_obs[-1]
basetrain3$we12_N_1 <- Taux_defaut_diff
str(basetrain3$we12)
basetrain3$we12_N_12[12:83] <- Taux_defaut_obs[1:72]
basetest3$we12_N_1[1]<- Taux_defaut_obs[84]
basetest3$we12_N_12[1:12]<- Taux_defaut_obs[73:84]
basetest3$we12_N_1v2 <- basetest3$we12_N_1 #A faire seulement une fois


gam3 <- gam(we12 ~#Incertitude.Europe +
             #Incertitude.Italie+
             #Rendement.des.obligations.d.états.sur.10ans +
             #Rendement_obligations_N_18 +
             Rendement_obligations_N_1 +
             #Rendement_obligations_N_2+
             #Indice_prix_growth_pastyear_N_1+
             #Indice_prix_growth_pastyear_N_2+
             #Taux.de.change +
             #Taux.interbancaire+
             #Production.industrielle+
             #Indice.des.prix.à.la.production+
             #CCI +
             #Indice.des.prix.à.la.conso+
             #d.CLI+
             #Indice.des.prix.à.la.conso.growth_pastyear+
             we12_N_12+
             #date_T1+
             #date_T2+
             #date_T3+
             #date_T4+
             s(we12_N_1,by = date_T) , 
             drop.intercept = FALSE ,data = basetrain3[13:83,])
summary(gam3)

basetest3$we12_N_1 <- basetest3$we12_N_1v2
basetest3$PredAR <- basetest3$we12_N_1v2

for (i in 1:24) {
  basetest3[i,36] <- predict(gam3,basetest3[i,])
  basetest3[i+1,18] <- basetest3[i,36]
  basetest3[i+12,19] <- basetest3[i,36]
}
for (i in 25:35) {
  basetest3[i,36] <- predict(gam3,basetest3[i,])
  basetest3[i+1,18] <- basetest3[i,36]
}
basetest3[36,36] <- predict(gam3,basetest3[36,])
basetest3$Predict <- basetest3$PredAR
pred_GAM3 <- basetest3$Predict
basetest3[,1]
pred_GAM3
#par(mfrow=c(1,1))
plot(pred_GAM3)
plot(basetest3[,1])
#plot(basetrain[,1])

##GAM##
observe <- basetest3[,1]
err_GAM3 <- (observe - pred_GAM3)/observe
err_GAMmoy3 <- mean(abs(err_GAM3))
err_GAMmoy3


#### Modélisation GAM avec correction atypique pour y ####
basetrain4 <- data.frame(Base_corrige4[2:84,-1])
basetest4 <- Base_corrige4[85:120,-1]
basetrain4$date_T3 <- as.factor(basetrain4$date_T3)
basetest4$date_T3 <- as.factor(basetest4$date_T3)

basetest4$we12_N_1v2 <- basetest4$we12_N_1 #A faire seulement une fois


gam4 <- gam(we12 ~#Incertitude.Europe +
              #Incertitude.Italie+
              #Rendement.des.obligations.d.états.sur.10ans +
              #Rendement_obligations_N_18 +
              Rendement_obligations_N_1 +
              #Rendement_obligations_N_2+
              #Indice_prix_growth_pastyear_N_1+
              #Indice_prix_growth_pastyear_N_2+
              #Taux.de.change +
              #Taux.interbancaire+
              #Production.industrielle+
              #Indice.des.prix.à.la.production+
              #CCI +
              #Indice.des.prix.à.la.conso+
              d.CLI+
              #Indice.des.prix.à.la.conso.growth_pastyear+
              we12_N_12+
              #date_T1+
              #date_T2+
              #date_T3+
              #date_T4+
              s(we12_N_1, by = date_T3) , 
              drop.intercept = FALSE ,data = basetrain4[12:83,])
summary(gam4)

basetest4$we12_N_1 <- basetest4$we12_N_1v2
basetest4$PredAR <- basetest4$we12_N_1v2

for (i in 1:24) {
  basetest4[i,36] <- predict(gam4,basetest4[i,])
  basetest4[i+1,18] <- basetest4[i,36]
  basetest4[i+12,19] <- basetest4[i,36]
}
for (i in 25:35) {
  basetest4[i,36] <- predict(gam4,basetest4[i,])
  basetest4[i+1,18] <- basetest4[i,36]
}
basetest4[36,36] <- predict(gam4,basetest4[36,])
basetest4$Predict <- basetest4$PredAR
pred_GAM4 <- basetest4$Predict
basetest4[,1]
pred_GAM4
#par(mfrow=c(1,1))
plot(pred_GAM4)
plot(basetest4[,1])
#plot(basetrain[,1])

##GAM##
err_GAM4 <- (observe - pred_GAM4)/observe
err_GAMmoy4 <- mean(abs(err_GAM4))
err_GAMmoy4


#### Modélisation GAM avec selection de variables ####
basetrain3 <- data.frame(Base_corrige3[2:84,-1])
basetest3 <- Base_corrige3[85:120,-1]
basetrain3$date_T3 <- as.factor(basetrain3$date_T3)
basetest3$date_T3 <- as.factor(basetest3$date_T3)

basetest3$we12_N_1v2 <- basetest3$we12_N_1 #A faire seulement une fois


gam5 <- gam(we12 ~ #Incertitude.Europe +
              #Incertitude.Italie+
              #Rendement.des.obligations.d.états.sur.10ans +
              #Rendement_obligations_N_18 +
              Rendement_obligations_N_1 +
              #Rendement_obligations_N_2+
              #Indice_prix_growth_pastyear_N_1+
              #Indice_prix_growth_pastyear_N_2+
              #Taux.de.change +
              #Taux.de.change.reel.Italie +
              #Taux.interbancaire+
              #Production.industrielle+
              #Indice.des.prix.à.la.production+
              #CCI +
              #Indice.des.prix.à.la.conso+
              #d.CCI + 
              #d.CLI +
              #Indice.des.prix.à.la.conso.growth_pastyear+
              we12_N_12+
              #date_T1+
              #date_T2+
              #date_T3+
              #date_T4+
              s(we12_N_1,by = date_T) , 
              drop.intercept = FALSE ,data = basetrain3[13:83,])
summary(gam5)

basetest3$we12_N_1 <- basetest3$we12_N_1v2
basetest3$PredAR <- basetest3$we12_N_1v2

for (i in 1:24) {
  basetest3[i,36] <- predict(gam5,basetest3[i,])
  basetest3[i+1,18] <- basetest3[i,36]
  basetest3[i+12,19] <- basetest3[i,36]
}
for (i in 25:35) {
  basetest3[i,36] <- predict(gam5,basetest3[i,])
  basetest3[i+1,18] <- basetest3[i,36]
}
basetest3[36,36] <- predict(gam5,basetest3[36,])
basetest3$Predict <- basetest3$PredAR
pred_GAM5 <- basetest3$Predict
basetest3[,1]
pred_GAM5
#par(mfrow=c(1,1))
plot(pred_GAM5)
plot(basetest3[,1])
#plot(basetrain[,1])

##GAM##
err_GAM5 <- (observe - pred_GAM5)/observe
err_GAMmoy5 <- mean(abs(err_GAM5))
err_GAMmoy5

write.csv2(pred_GAM3,"predictions_gam.csv")
