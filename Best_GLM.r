setwd('/Users/fookinpelican/Downloads/')

df <- read.csv("hmeq_cleaned_m.csv",header=TRUE)
#regression logistique forward:

resall <- glm(BAD~.,data=df,family=binomial)

res0 <- glm(BAD~1,df,family = binomial)

resfor<- step(res0,list(upper=resall),direction = 'forward')

resfor_formula <- formula(resfor)

#regression logistique backward:

resbac<- step(resall, direction = 'backward')

resbac_formula <- formula(resbac)

#regression logistique both:

resboth <- step(resall, direction = 'both')

resboth_formula <- formula(resboth)

# Result 
print("----------------------")
print(resfor)
print("----------------------")
print(resbac)
print("----------------------")
print(resboth)
print("----------------------")

print(resbac_formula)
print(resboth_formula)

# Best AIC and Best BIC
install.packages('MASS')
install.packages('bestglm')
Xy = cbind(df[-1],df[1])
library("bestglm")
bestAIC <- bestglm(Xy, IC="AIC")
bestBIC <- bestglm(Xy, IC="BIC")
NAIC <- length(coef(bestAIC$BestModel))-1
NBIC <- length(coef(bestBIC$BestModel))-1
formula(bestBIC$BestModel)

formula(bestAIC$BestModel)





install.packages('BMA')
# test du BIC 
library("BMA")
resallBIC = bic.glm(BAD ~ LOAN + MORTDUE + VALUE + DEROG + DELINQ + CLAGE + NINQ + 
                  CLNO + DEBTINC + REASON_DebtCon + JOB_Office + JOB_Sales,data=df,glm.family=binomial)
summary(resallBIC)

# ---------- Ocean LePelican ---------------

n <- nrow(df)
l <- 5000
perm <- sample(n)

train <- df[perm[1:1], ]
test <- df[-perm[1:1], ]
model1 <- glm(BAD ~ LOAN + MORTDUE + VALUE + DEROG + DELINQ + CLAGE + NINQ + 
                CLNO + DEBTINC + REASON_DebtCon + JOB_Office + JOB_Sales, data=train, family=binomial)


model2 <- glm(BAD ~ LOAN + MORTDUE + VALUE + YOJ + DEROG + DELINQ + CLAGE + NINQ + 
                CLNO + DEBTINC + REASON_DebtCon + JOB_Mgr + JOB_Other + JOB_ProfExe + 
                JOB_Sales + JOB_Self, data=test, family=binomial)


prev1 <- round(predict(model1, newdata=test, type="response"))

prev2 <- round(predict(model2, newdata=test, type="response"))
mean(prev1 != test$BAD)
mean(prev2 != test$BAD)

deviance(model1)
deviance(model2)

1-pchisq(model1$deviance, model1$train.resid)

P1 <- sum(residuals(model1, type = "pearson" )^2)
1 - pchisq(P1, n-length(model1$coefficients))

P2 <- sum(residuals(model2, type = "pearson" )^2)
1 - pchisq(P2, n-length(model2$coefficients))


residPartiels1 <- residuals(model1, type="partial")
plot(train$LOAN, residPartiels1[, "LOAN"], cex=.5)
est <- loess(residPartiels1[,"LOAN" ]~train$LOAN)


