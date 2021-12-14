#### Stationnarité, Atypicité et Saisonnalité  ####

Base_corrige <- Base

## Vérification graphique ##
par(mfrow=c(3,3))
Basep <- ts(Base_corrige[,-1],start=c(2010,01),frequency = 12)
for (i in 1:21){
  print(plot(Basep[,i]))
}

## Test adf avec H0 : la serie n'est pas stationnaire ##

for (i in 2:18) {
  m = ar(diff(Base_corrige[,i],method = "mle"))
  print(colnames(Base_corrige)[i])
  print(m$order)
  adf <-adfTest(Base_corrige[,i],lags=m$order)
  print(adf@test$p.value)
} 

Base_corrige$d.Ind_px_conso <- Base_corrige[,5]
Base_corrige$d.prod_indus <- Base_corrige[,6]
Base_corrige$d.Ind_px_prod <- Base_corrige[,7]
Base_corrige$d.Incertitude_EU <- Base_corrige[,8]
Base_corrige$d.BCI <- Base_corrige[,16]
Base_corrige$d.CLI <- Base_corrige[,17]
Base_corrige$d.CCI <- Base_corrige[,18]

Base_corrige[2:120,29] <- diff(Base_corrige[,5])
Base_corrige[2:120,30] <- diff(Base_corrige[,6])
Base_corrige[2:120,31] <- diff(Base_corrige[,7])
Base_corrige[2:120,32] <- diff(Base_corrige[,8])
Base_corrige[2:120,33] <- diff(Base_corrige[,16])
Base_corrige[2:120,34] <- diff(Base_corrige[,17])
Base_corrige[2:120,35] <- diff(Base_corrige[,18])
Base_corrige[1,29:35] <- NA

#Creation base qui ne sera pas modifiée par la correction de l'atypicité
#Base_corrige
#Creation base qui sera modifiée par la correction de l'atypicité
Base_corrige_aa <- Base_corrige

## Atypicité ##
#On commence a 3 car serie a explique ne sera pas modifiee
for (i in 3:18){
  yy <- ts(data = Base_corrige_aa[,i], start=c(2010,01),frequency=12)
  print(colnames(Base_corrige_aa)[i])
  fit<-tso(yy,types = c("AO","TC"))
  show(fit$outliers)
  Base_corrige_aa[,i] <- fit$yadj
}

#Pour les variables differenciee
for (i in 29:35){
  yy <- ts(data = Base_corrige_aa[-1,i], start=c(2010,02),frequency=12)
  print(colnames(Base_corrige_aa)[i])
  fit<-tso(yy,types = c("AO","TC"))
  show(fit$outliers)
  Base_corrige_aa[-1,i] <- fit$yadj
}

## Saisonnalité ##

for (i in 3:18){
  yy <- ts(data = Base_corrige_aa[,i], start=c(2010,01),frequency=12)
  ft <- fried(yy)
  is <- isSeasonal(yy, test="wo")
  kwt <- kw(yy)
  sd <- seasdum(yy)
  w <- welch(yy)
  print(colnames(Base_corrige_aa)[i])
  print(ft$Pval)
  print(is)
  print(kwt$Pval)
  print(sd$Pval)
  print(w$Pval)
}

##Correction de la saisonnalité##

#La base corrigée 2 prend donc en compte la correction des valeurs atypiques
#et la correction de la saisonnalité est ajoutée dans la base corrige3 
#contrairement à la base corrigée
Base_corrige2 <- Base_corrige_aa
Base_corrige3 <- ts(data = Base_corrige_aa, start=c(2010,01),frequency=12)

Base_corrige3[,3] <- final(seas(Base_corrige3[,3],  x11 = ""))
Base_corrige3[,5] <- final(seas(Base_corrige3[,5],  x11 = ""))
Base_corrige3[,8] <- final(seas(Base_corrige3[,8],  x11 = ""))
Base_corrige3[,12] <- final(seas(Base_corrige3[,12],  x11 = ""))
Base_corrige3[,14] <- final(seas(Base_corrige3[,14],  x11 = ""))

Base_corrige3 <- as.data.frame(Base_corrige3)

Base_corrige2[2:120,29] <- diff(Base_corrige2[,5])
Base_corrige2[2:120,30] <- diff(Base_corrige2[,6])
Base_corrige2[2:120,31] <- diff(Base_corrige2[,7])
Base_corrige2[2:120,32] <- diff(Base_corrige2[,8])
Base_corrige2[2:120,33] <- diff(Base_corrige2[,16])
Base_corrige2[2:120,34] <- diff(Base_corrige2[,17])
Base_corrige2[2:120,35] <- diff(Base_corrige2[,18])

Base_corrige3[2:120,29] <- diff(Base_corrige3[,5])
Base_corrige3[2:120,30] <- diff(Base_corrige3[,6])
Base_corrige3[2:120,31] <- diff(Base_corrige3[,7])
Base_corrige3[2:120,32] <- diff(Base_corrige3[,8])
Base_corrige3[2:120,33] <- diff(Base_corrige3[,16])
Base_corrige3[2:120,34] <- diff(Base_corrige3[,17])
Base_corrige3[2:120,35] <- diff(Base_corrige3[,18])


Base_corrige2[13:120,21] <- Base_corrige2[1:108,10]
Base_corrige2[25:120,22] <- Base_corrige2[1:96,10]

Base_corrige3[13:120,21] <- Base_corrige3[1:108,10]
Base_corrige3[25:120,22] <- Base_corrige3[1:96,10]
#On a donc 4 bases différentes : Base, Base_corrige, Base_corrige2 et Base_corrige3

#### Test de la correction de la variable à expliquer ####

## Atypicité ##
Base_corrige4 <- Base_corrige3
yy <- ts(data = Base_corrige4[1:84,2], start=c(2010,01),frequency=12)
fit<-tso(yy,types = c("AO","TC"))
show(fit$outliers)
Base_corrige4[1:84,2] <- fit$yadj
plot(Base_corrige3$we12)
plot(Base_corrige4$we12)

Base_corrige4[2:85,19] <- Base_corrige4[1:84,2]
Base_corrige4[13:96,20] <- Base_corrige4[1:84,2]


