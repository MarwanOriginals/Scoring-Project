library(dplyr)
library(FNN)
library(glmnet)

setwd('C:/Users/jules/Documents/IMSD/Scoring')
df <- read.table("hmeq_cleaned.csv",header=TRUE, sep = ",")

#************************************
#Plus proche voisin : VARIABLES VALUE et MORTDUE
boxplot(df$MORTDUE)
print(sum(is.na(df$MORTDUE)))

query = df[is.na(df$MORTDUE),]
search = df[!is.na(df$MORTDUE) & !is.na(df$VALUE) ,]
neighs = get.knnx(search[,c("VALUE")],query[,c("VALUE")],k=1)
df[is.na(df$MORTDUE),"MORTDUE"]=search$MORTDUE[neighs$nn.index]

boxplot(df$MORTDUE)
print(sum(is.na(df$MORTDUE)))

boxplot(df$VALUE)
print(sum(is.na(df$VALUE)))

query = df[is.na(df$VALUE),]
search = df[!is.na(df$VALUE) & !is.na(df$MORTDUE) ,]
neighs = get.knnx(search[,c("MORTDUE")],query[,c("MORTDUE")],k=1)
df[is.na(df$VALUE),"VALUE"]=search$VALUE[neighs$nn.index]

boxplot(df$VALUE)
print(sum(is.na(df$VALUE)))
print(sum(is.na(df)))
#write.csv(df,"df_filled.csv", row.names = FALSE)


#************************************************************
#SELECTION DE VARAIBLES PAS A PAS BACKWARD

#modèle null
fit <- glm(BAD~1,data=df,family=binomial())
summary(fit)

#modèle avec toutes les variables
fit1 <- glm(BAD~LOAN + MORTDUE + VALUE + REASON_DebtCon + REASON_HomeImp + YOJ + JOB_Mgr + JOB_Office + JOB_ProfExe + JOB_Sales + JOB_Self + DEROG + DELINQ + CLAGE + NINQ + CLNO + DEBTINC,data=df,family=binomial())
summary(fit1)
print(BIC(fit1))
print(AIC(fit1))

# modèle 2 : sans JOB OFFICE
fit2 <- glm(BAD~LOAN + MORTDUE + VALUE + REASON_DebtCon + REASON_HomeImp + YOJ + JOB_Mgr + JOB_Other + JOB_ProfExe + JOB_Sales + JOB_Self + DEROG + DELINQ + CLAGE + NINQ + CLNO + DEBTINC,data=df,family=binomial())
summary(fit2)
print(BIC(fit2))
print(AIC(fit2))

#modèle 3 : sans REASON_HomeImp : BEST AIC
fit3 <- glm(BAD~LOAN + MORTDUE + VALUE + REASON_DebtCon + YOJ + JOB_Mgr + JOB_Other + JOB_ProfExe + JOB_Sales + JOB_Self + DEROG + DELINQ + CLAGE + NINQ + CLNO + DEBTINC,data=df,family=binomial())
summary(fit3)
print(BIC(fit3))
print(AIC(fit3))

#Probabilté d'erreur 

n <- nrow(df)
l <- 500
set.seed(123)
perm <- sample(n)
df_train <- df[perm[1:l],]
df_test <- df[-perm[1:l],]

model <- glm(BAD~LOAN + MORTDUE + VALUE + REASON_DebtCon + YOJ + JOB_Mgr + JOB_Other + JOB_ProfExe + JOB_Sales + JOB_Self + DEROG + DELINQ + CLAGE + NINQ + CLNO + DEBTINC,data=df_train,family=binomial())
pred <- round(predict(model, newdata = df_test, type = 'response'))
mean(pred!= df_test$BAD)

#modèle 4 : on enleve YOJ
fit4 <- glm(BAD~LOAN + MORTDUE + VALUE + REASON_DebtCon + JOB_Mgr + JOB_Other + JOB_ProfExe + JOB_Sales + JOB_Self + DEROG + DELINQ + CLAGE + NINQ + CLNO + DEBTINC,data=df,family=binomial())
summary(fit4)
print(BIC(fit4))
print(AIC(fit4))

#modèle 5 : on enleve MORTDUE 
fit5 <- glm(BAD~LOAN  + VALUE + REASON_DebtCon + JOB_Mgr + JOB_Other + JOB_ProfExe + JOB_Sales + JOB_Self + DEROG + DELINQ + CLAGE + NINQ + CLNO + DEBTINC,data=df,family=binomial())
summary(fit5)
print(BIC(fit5))
print(AIC(fit5))

#modèle 6 : On enlève VALUE : BEST BIC
fit6 <- glm(BAD~LOAN + REASON_DebtCon + JOB_Mgr + JOB_Other + JOB_ProfExe + JOB_Sales + JOB_Self + DEROG + DELINQ + CLAGE + NINQ + CLNO + DEBTINC,data=df,family=binomial())
summary(fit6)
print(BIC(fit6))
print(AIC(fit6))

#ANOVA sur les modèles 
anova(fit, fit6, test="Chisq")
anova(fit, fit6, test="LRT")

anova(fit, fit3, test="Chisq")
anova(fit, fit3, test="LRT")


#Probabilté d'erreur 
n <- nrow(df)
l <- 500
set.seed(123)
perm <- sample(n)
df_train <- df[perm[1:l],]
df_test <- df[-perm[1:l],]

model <- glm(BAD~LOAN + REASON_DebtCon + JOB_Mgr + JOB_Other + JOB_ProfExe + JOB_Sales + JOB_Self + DEROG + DELINQ + CLAGE + NINQ + CLNO + DEBTINC,data=df_train,family=binomial())
pred <- round(predict(model, newdata = df_test, type = 'response'))
mean(pred!= df_test$BAD)

#modele de Bestglm *****************************************
fit7 <- glm(BAD~LOAN + MORTDUE + VALUE + REASON_DebtCon +  JOB_Sales + JOB_Office + DEROG + DELINQ + CLAGE + NINQ + CLNO + DEBTINC,data=df,family=binomial())
summary(fit7)

print(BIC(fit7))
print(AIC(fit7))

#Probabilté d'erreur 

n <- nrow(df)
l <- 500
set.seed(1234)
perm <- sample(n)
df_train <- df[perm[1:l],]
df_test <- df[-perm[1:l],]

model <- glm(BAD~LOAN + MORTDUE + VALUE + REASON_DebtCon +  JOB_Sales + JOB_Office + DEROG + DELINQ + CLAGE + NINQ + CLNO + DEBTINC,data=df_train,family=binomial())
pred <- round(predict(model, newdata = df_test, type = 'response'))
mean(pred!= df_test$BAD)



#VIF**********************************************************
library(car)
vif(glm(BAD~LOAN + MORTDUE + VALUE + REASON_DebtCon + REASON_HomeImp + YOJ + JOB_Mgr + JOB_Office + JOB_Other + JOB_ProfExe + JOB_Sales + JOB_Self + DEROG + DELINQ + CLAGE + NINQ + CLNO + DEBTINC,data=df,family=binomial()))
#AIC
vif(glm(BAD~LOAN + MORTDUE + VALUE + REASON_DebtCon + JOB_Mgr + JOB_Other + JOB_ProfExe + JOB_Sales + JOB_Self + DEROG + DELINQ + CLAGE + NINQ + CLNO + DEBTINC,data=df,family=binomial()))
#bic
vif(glm(BAD~LOAN + REASON_DebtCon + JOB_Mgr + JOB_Other + JOB_ProfExe + JOB_Sales + JOB_Self + DEROG + DELINQ + CLAGE + NINQ + CLNO + DEBTINC,data=df,family=binomial()))
#BIC BEST
vif(glm(BAD~LOAN + MORTDUE + VALUE + REASON_DebtCon + JOB_Sales + JOB_Office + DEROG + DELINQ + CLAGE + NINQ + CLNO + DEBTINC,data=df,family=binomial()))




