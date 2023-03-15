############ MODELISATION #################

library(plyr)
# Création de df pour vides pour y rentrer les valeurs de nos modèles
a1 <- as.data.frame(t(combn(1:9,1)))
a2 <- as.data.frame(t(combn(1:9,2)))
a3 <- as.data.frame(t(combn(1:9,3)))
a4 <- as.data.frame(t(combn(1:9,4)))
a5 <- as.data.frame(t(combn(1:9,5)))
a6 <- as.data.frame(t(combn(1:9,6)))
a7 <- as.data.frame(t(combn(1:9,7)))
a8 <- as.data.frame(t(combn(1:9,8)))
a9 <- as.data.frame(t(combn(1:9,9)))

#Cols contiendra les combinaisons
Cols <- do.call(rbind.fill,list(a1,a2,a3,a4,a5,a6,a7,a8,a9))
# Array vide pour contenir nos BIC et AIC
AICX <- NULL
BICX <- NULL
# Librairie 
library(ROCR)
library(tidyverse)
## 25% TEST ET 75% ENTRAINEMENT
smp_size <- floor(0.75 * nrow(Contexte))
set.seed(123)
train_ind <- sample(seq_len(nrow(Contexte)), size = smp_size)
train <- Contexte[train_ind, ]
test <- Contexte[-train_ind, ]
#Liste vide qui contiendra nos modèles 
List <- list()
modx=list()
#Boucle qui teste toutes les combinaison de variables et fait des régressions log
for(i in 1:dim(Cols)[1])
{
  #Variables
  # Target le vote 
  vals <- as.vector(na.omit(t(Cols[i,])))
  target <- "Vote"
  vars <- names(Contexte)[vals]
  #Création des formules
  string <- paste(target,'~', paste(vars, collapse= "+"))
  fmla <- as.formula(paste(target,'~', paste(vars, collapse= "+")))
  #Modèle
  mod1 <- glm(formula = fmla,
              data = train,
              family = binomial(link = "logit"))
  pred_probs <- predict(mod1, newdata = test, type = "response")
  
  pred_obj <- ROCR::prediction(pred_probs, test$Vote)
  perf_obj <- ROCR::performance(pred_obj, measure = "tpr", x.measure = "fpr")
  
  auc <- performance(pred_obj, measure = "auc")
  auc <- auc@y.values[[1]]
  AICX <- c(AICX,AIC(mod1))
  BICX <- c(BICX, BIC(mod1))
  #Sauvegarder les résultats
  output <- data.frame(Model=string,auc=auc)
  output2 <- data.frame(AIC=AICX,BIC=BICX)
  #Les rentrer dans la liste
  List[[i]] <- output
}
#Transformer en DF
DFResult <- do.call(rbind,List)
final <-cbind(DFResult,output2)
# Avoir l'index des meilleurs modèles
which.min(final$BIC)
which.max(final$auc)
which.min(final$AIC)
final

#### SELECTION PAR METHODE DE SELECTION AIC/BIC POUR LES INTERACTIONS
mod<-glm(formula =Vote ~ RCRS2+RAGE+SEXE+RCRS15,
         data = train,family = binomial(link = "logit"))
library(MASS)
#BIC
step2<-stepAIC(mod,~.^2,direction="backward",k=log(nrow(train)))
step2$anova
pred_probs2 <- predict(mod, newdata = test, type = "response")
rmse<-rmse(test$Vote,pred_probs2)
mse<- mse(test$Vote,pred_probs2)
rmse
mse
#AIC
step2<-stepAIC(mod,~.^2,direction="backward",trace=FALSE)
step2$anova
Vote ~ RCRS2 + RCRS13 + RAGE + SEXE + RCRS15 + RAGE:SEXE + RCRS13:SEXE + 
  RAGE:RCRS15 + SEXE:RCRS15 + RCRS2:RCRS15
mod3<-glm(formula =Vote ~ RCRS2 + RAGE + SEXE + RCRS15 + RAGE:RCRS15 + RAGE:SEXE + 
            SEXE:RCRS15 + RCRS2:RAGE,
          data = train,family = binomial(link = "logit"))
pred_probs2 <- predict(mod3, newdata = test, type = "response")
rmse<-rmse(test$Vote,pred_probs2)
mse<- mse(test$Vote,pred_probs2)
rmse
mse
# MEME CHOSE POUR LES VARIABLES D'OPINIONS
library(plyr)
library(Metrics)
#Index
a1 <- as.data.frame(t(combn(1:11,1)))
a2 <- as.data.frame(t(combn(1:11,2)))
a3 <- as.data.frame(t(combn(1:11,3)))
a4 <- as.data.frame(t(combn(1:11,4)))
a5 <- as.data.frame(t(combn(1:11,5)))
a6 <- as.data.frame(t(combn(1:11,6)))
a7 <- as.data.frame(t(combn(1:11,7)))
a8 <- as.data.frame(t(combn(1:11,8)))
a9 <- as.data.frame(t(combn(1:11,9)))
a10 <- as.data.frame(t(combn(1:11,10)))
a11 <- as.data.frame(t(combn(1:11,11)))
#Cols contains the combinations
Cols <- do.call(rbind.fill,list(a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11))
AICX <- NULL
BICX <- NULL
## make it binary classification
library(ROCR)
## 75% of the sample size
smp_size <- floor(0.75 * nrow(Opinion))
## set the seed to make your partition reproducible
set.seed(123)
train_ind <- sample(seq_len(nrow(Opinion)), size = smp_size)
train <- Opinion[train_ind, ]
test <- Opinion[-train_ind, ]
#Now create a list
List <- list()
#Iterate over all posible values of Cols
for(i in 1:dim(Cols)[1])
{
  #Variables
  #Define target and covariates
  vals <- as.vector(na.omit(t(Cols[i,])))
  target <- "Vote"
  vars <- names(Opinion)[vals]
  #Create formulas
  string <- paste(target,'~', paste(vars, collapse= "+"))
  fmla <- as.formula(paste(target,'~', paste(vars, collapse= "+")))
  #Model 
  mod1 <- glm(formula = fmla,
              data = train,
              family = binomial(link = "logit"))
  pred_probs <- predict(mod1, newdata = test, type = "response")
  pred_obj <- ROCR::prediction(pred_probs, test$Vote)
  perf_obj <- ROCR::performance(pred_obj, measure = "tpr", x.measure = "fpr")
  
  auc <- performance(pred_obj, measure = "auc")
  auc <- auc@y.values[[1]]
  AICX <- c(AICX,AIC(mod1))
  BICX <- c(BICX, BIC(mod1))
  #Save results
  output <- data.frame(Model=string,auc=auc)
  output2 <- data.frame(AIC=AICX,BIC=BICX)
  #Feed into list
  List[[i]] <- output
}
#Format as dataframe
DFResult <- do.call(rbind,List)
final <-cbind(DFResult,output2)
which.min(final$BIC)
which.max(final$auc)
which.min(final$AIC)
final
options (max.print = 3000)
mod2<-glm(formula =Vote ~Q17E+Q12A+Q38+Q17D,
          data = train,family = binomial(link = "logit"))
library(MASS)
#BIC
step2<-stepAIC(mod2,~.^2,direction="forward",k=log(nrow(train)))
step2$anova
pred_probs2 <- predict(mod2, newdata = test, type = "response")
rmse<-rmse(test$Vote,pred_probs2)
mse<- mse(test$Vote,pred_probs2)
rmse
mse
#AIC
step2<-stepAIC(mod2,~.^2,direction="forward",trace=FALSE)
step2$anova

mod4<-glm(formula =Vote ~ Q17E + Q12A + Q38 + Q17D + Q38:Q17D + Q12A:Q38 + Q12A:Q17D,
          data = train,family = binomial(link = "logit"))
pred_probs2 <- predict(mod4, newdata = test, type = "response")


rmse<-rmse(test$Vote,pred_probs2)
mse<- mse(test$Vote,pred_probs2)
rmse
mse

############### RESEAU BAYESIEN #####################

### Création du DF qui avec qui nous feront le reseau

df <-cbind(Contexte,Opinion)
df <- subset( df, select = c(RAGE,RCRS15,SEXE,RCRS2,Q17E,Q12A,Q38,Q17D,Vote) )
df<-lapply(df,factor)
df<-data.frame(df)

library(grid)
library(Rgraphviz)
# BLACKLIST POUR LE RESEAU
exclusion <- as.data.frame(cbind(c("Q17E","Q17E","Q17E","Q17E",
                                   "Q12A","Q12A","Q12A","Q12A",
                                   "Q17D","Q17D","Q17D","Q17D",
                                   "Q38","Q38","Q38","Q38"
                                   ,"Vote","Vote","Vote","Vote","Vote","Vote","Vote","Vote"),
                                 c("RCRS2","RAGE","SEXE","RCRS15",
                                   "RCRS2","RAGE","SEXE","RCRS15",
                                   "RCRS2","RAGE","SEXE","RCRS15",
                                   "RCRS2","RAGE","SEXE","RCRS15",
                                   "Q17E","Q12A","Q17D","Q38",
                                   "RCRS2","RAGE","SEXE","RCRS15"
                                 )))
names(exclusion) <- c("from","to")
exclusion
# WHITELIST POUR LE RESEAU
inclusion <- as.data.frame(cbind(c("RAGE","RAGE","SEXE","RCRS2"),
                                 c("RCRS15","SEXE","RCRS15","RAGE")))
names(inclusion) <- c("from","to")
inclusion
library(bnlearn)
## RESEAU IAMB
df.gs <- iamb(df,blacklist=exclusion,whitelist=inclusion)
graphviz.plot(df.gs,layout="fdp")
## RESEAU HC
xd <- hc(df,blacklist=exclusion,whitelist=inclusion)
graphviz.plot(xd,layout="fdp")
# RESULTATS SCORES
fit=bn.fit(xd, data=df)