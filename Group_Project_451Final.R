#install packages, library
install.packages(c("ggplot2", "nnet", "pROC", "rpart", "party", "partykit", "forecast"))
library(ggplot2)
library(nnet)
library(pROC)
library(rpart) 
library(rpart.plot)
library(party) 
library(partykit)
library(forecast)

#Read data, turn to data frame, check for NA
mwuf = as.data.frame(read.csv("MWUF.csv"))
mwuf.new = as.data.frame(read.csv("MWUF_new.csv")) 
sum(is.na(mwuf))

#Check variable types and check if DONR is balanced
str(mwuf)
mwuf$home = factor(mwuf$home, levels = c(0,1), labels = c("Not a Homeowner","Homeowner"))
mwuf$hinc = factor(mwuf$hinc)
mwuf$genf = factor(mwuf$genf, levels = c(0,1), labels = c("Male","Female"))
mwuf$donr = factor(mwuf$donr, levels = c(0,1), labels = c("Non-donor","Donor"))

sum(mwuf$donr == "Non-donor")
sum(mwuf$donr == "Donor")

mwuf.new$home = factor(mwuf.new$home, levels = c(0,1), labels = c("Not a Homeowner","Homeowner"))
mwuf.new$hinc = factor(mwuf.new$hinc)
mwuf.new$genf = factor(mwuf.new$genf, levels = c(0,1), labels = c("Male","Female"))
mwuf.new$donr = factor(mwuf.new$donr, levels = c(0,1), labels = c("Non-donor","Donor"))

#DAMT correlation, present in table
mwuf.num <- subset(mwuf, select = -c(ID, reg1, reg2, reg3, reg4, home, hinc, genf, donr))
correlations <- as.data.frame(cor(mwuf.num))
cor.tab = as.table(cbind(names(mwuf.num),correlations$damt))
print(cor.tab)
#write.csv(cor.tab, "correlations.csv")

#DONR data visualization - 5 visualizations
p.chld <- ggplot(mwuf, aes(x=donr, y=chld, color = donr)) + 
  geom_boxplot() + geom_boxplot(outlier.size=3)
p.chld

p.wrat <- ggplot(mwuf, aes(x=donr, y=wrat, color = donr)) + 
  geom_boxplot() + geom_boxplot(outlier.size=3)
p.wrat

p.avhv <- ggplot(mwuf, aes(x=donr, y=avhv, color = donr)) + 
  geom_boxplot() + geom_boxplot(outlier.size=3)
p.avhv

p.incm <- ggplot(mwuf, aes(x=donr, y=incm, color = donr)) + 
  geom_boxplot() + geom_boxplot(outlier.size=3)
p.incm

p.inca <- ggplot(mwuf, aes(x=donr, y=inca, color = donr)) + 
  geom_boxplot() + geom_boxplot(outlier.size=3)
p.inca

p.plow <- ggplot(mwuf, aes(x=donr, y=plow, color = donr)) + 
  geom_boxplot() + geom_boxplot(outlier.size=3)
p.plow

p.npro <- ggplot(mwuf, aes(x=donr, y=npro, color = donr)) + 
  geom_boxplot() + geom_boxplot(outlier.size=3)
p.npro

p.tgif <- ggplot(mwuf, aes(x=donr, y=tgif, color = donr)) + 
  geom_boxplot() + geom_boxplot(outlier.size=3)
p.tgif

p.lgif <- ggplot(mwuf, aes(x=donr, y=lgif, color = donr)) + 
  geom_boxplot() + geom_boxplot(outlier.size=3)
p.lgif

p.rgif <- ggplot(mwuf, aes(x=donr, y=rgif, color = donr)) + 
  geom_boxplot() + geom_boxplot(outlier.size=3)
p.rgif

p.tdon <- ggplot(mwuf, aes(x=donr, y=tdon, color = donr)) + 
  geom_boxplot() + geom_boxplot(outlier.size=3)
p.tdon

p.tlag <- ggplot(mwuf, aes(x=donr, y=tlag, color = donr)) + 
  geom_boxplot() + geom_boxplot(outlier.size=3)
p.tlag

p.agif <- ggplot(mwuf, aes(x=donr, y=agif, color = donr)) + 
  geom_boxplot() + geom_boxplot(outlier.size=3)
p.agif

#####DONR Models#####
#LogR1: logistic regression model
mwuf.donr = subset(mwuf, select = -c(ID, reg1, reg2, reg3, reg4, damt))

set.seed(123)
train.index <- sample(c(1:dim(mwuf.donr)[1]), dim(mwuf.donr)[1]*0.7)  
train.donr <- mwuf.donr[train.index, ]
test.donr <- mwuf.donr[-train.index, ]
LogR1 <- glm(donr ~ ., data = train.donr, family = "binomial")
summary(LogR1)
  #ROC Curve and AUC Statistic
test.prob.LogR1 = predict(LogR1, newdata = test.donr, type = "response")
par(pty="s")
test.roc.LogR1 = roc(test.donr$donr ~ test.prob.LogR1, plot = TRUE, print.auc = TRUE)
par(pty="m")
auc(test.roc.LogR1)

#LogR2: feature selected regression model
LogR2 <- step(LogR1, direction = "both")
summary(LogR2)
  #ROC Curve and AUC Statistic
test.prob.LogR2 = predict(LogR2, newdata = test.donr, type = "response")
par(pty="s")
test.roc.LogR2 = roc(test.donr$donr ~ test.prob.LogR2, plot = TRUE, print.auc = TRUE)
par(pty="m")
auc(test.roc.LogR2)

#DT: Decision Tree
DT <- rpart(donr ~ ., method="class", data=train.donr)
plot(as.party(DT))
rpart.rules(DT)
  #ROC Curve and AUC Statistic
test.prob.DT = as.numeric(predict(DT, newdata = test.donr, type = "class"))
par(pty="s")
test.roc.DT = roc(test.donr$donr ~ test.prob.DT, plot = TRUE, print.auc = TRUE)
par(pty="m")
auc(test.roc.DT)

#PT: Pruned Decision Tree
m <- which.min(DT$cptable[, "xerror"])
DT$cptable[m, "CP"]
PT <- prune(DT, cp = DT$cptable[m, "CP"])
plot(as.party(PT))
rpart.rules(PT)
  #ROC Curve and AUC Statistic
test.prob.PT = as.numeric(predict(PT, newdata = test.donr, type = "class"))
par(pty="s")
test.roc.PT = roc(test.donr$donr ~ test.prob.PT, plot = TRUE, print.auc = TRUE)
par(pty="m")
auc(test.roc.PT)

#ANN1: Neural Network with 20 hidden nodes
ANN1 <- nnet(donr ~ ., data=train.donr,  size = 20)
  #ROC Curve and AUC Statistic
test.prob.ANN1 = (predict(ANN1, test.donr))
par(pty="s")
test.roc.ANN1 = roc(test.donr$donr ~ test.prob.ANN1, plot = TRUE, print.auc = TRUE)
par(pty="m")
auc(test.roc.ANN1)

#ANN2: Neural Network with 100 hidden nodes
ANN2 <- nnet(donr ~ ., data=train.donr,  size = 100, MaxNWts = 5000)
  #ROC Curve and AUC Statistic
test.prob.ANN2 = (predict(ANN2, test.donr))
par(pty="s")
test.roc.ANN2 = roc(test.donr$donr ~ test.prob.ANN2, plot = TRUE, print.auc = TRUE)
par(pty="m")
auc(test.roc.ANN2)

#Table with Model, Accuracy, Positive Predictive Value, AUC, F-1 Score

accuracy = function(tp, tn, fp, fn) {
  accuracy = (tp + tn) / (tp + fp + tn + fn)
  return(accuracy) 
}

ppv = function(tp, fp) {
  ppv = tp / (tp +fp)
  return(ppv)
}

f1 = function(tp, fp, fn) {
  precision = tp / (tp + fp)
  recall = tp / (tp + fn)
  f1 = (2 * precision * recall) / (precision + recall)
  return(f1)
}


pred.LogR1 <- predict(LogR1, test.donr, decision.values=TRUE, type="response")  
cm.LogR1 = table(test.donr$donr, ifelse(pred.LogR1>0.5, "Donor", "Non-donor"))
cm.LogR1

tp.LogR1 = cm.LogR1[2,1]
tn.LogR1 = cm.LogR1[1,2]
fp.LogR1 = cm.LogR1[1,1]
fn.LogR1 = cm.LogR1[2,2]
acc.LogR1 = accuracy(tp.LogR1, tn.LogR1, fp.LogR1, fn.LogR1)
ppv.LogR1 = ppv(tp.LogR1, fp.LogR1)
f1.LogR1 = f1(tp.LogR1, fp.LogR1, fn.LogR1)

pred.LogR2 <- predict(LogR2, test.donr, decision.values=TRUE, type="response")  
cm.LogR2 = table(test.donr$donr, ifelse(pred.LogR2>0.5, "Donor", "Non-donor"))
cm.LogR2

tp.LogR2 = cm.LogR2[2,1]
tn.LogR2 = cm.LogR2[1,2]
fp.LogR2 = cm.LogR2[1,1]
fn.LogR2 = cm.LogR2[2,2]
acc.LogR2 = accuracy(tp.LogR2, tn.LogR2, fp.LogR2, fn.LogR2)
ppv.LogR2 = ppv(tp.LogR2, fp.LogR2)
f1.LogR2 = f1(tp.LogR2, fp.LogR2, fn.LogR2)

pred.DT <- predict(DT, test.donr, decision.values=TRUE, type="class")  
cm.DT = table(pred = pred.DT, true = test.donr$donr)
cm.DT

tp.DT = cm.DT[2,2]
tn.DT = cm.DT[1,1]
fp.DT = cm.DT[2,1]
fn.DT = cm.DT[1,2]
acc.DT = accuracy(tp.DT, tn.DT, fp.DT, fn.DT)
ppv.DT = ppv(tp.DT, fp.DT)
f1.DT = f1(tp.DT, fp.DT, fn.DT)

pred.PT <- predict(PT, test.donr, decision.values=TRUE, type="class")  
cm.PT = table(pred = pred.PT, true = test.donr$donr)
cm.PT

tp.PT = cm.PT[2,2]
tn.PT = cm.PT[1,1]
fp.PT = cm.PT[2,1]
fn.PT = cm.PT[1,2]
acc.PT = accuracy(tp.PT, tn.PT, fp.PT, fn.PT)
ppv.PT = ppv(tp.PT, fp.PT)
f1.PT = f1(tp.PT, fp.PT, fn.PT)

pred.ANN1 <- predict(ANN1, test.donr, decision.values=TRUE, type="class")  
cm.ANN1 = table(pred = pred.ANN1, true = test.donr$donr)
cm.ANN1

tp.ANN1 = cm.ANN1[1,2]
tn.ANN1 = cm.ANN1[2,1]
fp.ANN1 = cm.ANN1[1,1]
fn.ANN1 = cm.ANN1[2,2]
acc.ANN1 = accuracy(tp.ANN1, tn.ANN1, fp.ANN1, fn.ANN1)
ppv.ANN1 = ppv(tp.ANN1, fp.ANN1)
f1.ANN1 = f1(tp.ANN1, fp.ANN1, fn.ANN1)

pred.ANN2 <- predict(ANN2, test.donr, decision.values=TRUE, type="class")  
cm.ANN2 = table(pred = pred.ANN2, true = test.donr$donr)
cm.ANN2

tp.ANN2 = cm.ANN2[1,2]
tn.ANN2 = cm.ANN2[2,1]
fp.ANN2 = cm.ANN2[1,1]
fn.ANN2 = cm.ANN2[2,2]
acc.ANN2 = accuracy(tp.ANN2, tn.ANN2, fp.ANN2, fn.ANN2)
ppv.ANN2 = ppv(tp.ANN2, fp.ANN2)
f1.ANN2 = f1(tp.ANN2, fp.ANN2, fn.ANN2)

table.names = c("Model", "Accuracy", "PPV", "AUC", "F1 Score")
table.LogR1 = c("LogR1", acc.LogR1, ppv.LogR1, auc(test.roc.LogR1), f1.LogR1)
table.LogR2 = c("LogR2", acc.LogR2, ppv.LogR2, auc(test.roc.LogR2), f1.LogR2)
table.DT = c("DT", acc.DT, ppv.DT, auc(test.roc.DT), f1.DT)
table.PT = c("PT", acc.PT, ppv.PT, auc(test.roc.PT), f1.PT)
table.ANN1 = c("ANN1", acc.ANN1, ppv.ANN1, auc(test.roc.ANN1), f1.ANN1)
table.ANN2 = c("ANN2", acc.ANN2, ppv.ANN2, auc(test.roc.ANN2), f1.ANN2)

table.models = rbind(table.names, table.LogR1, table.LogR2, table.DT, table.PT, table.ANN1, table.ANN2)
table.models
#write.csv(table.models, "models.csv")

#Profit Calculations; make table with cost and expected profit
donation.avg = 14.50
mail.cost = 2.00

revenue.actual = function(tp, donation.avg) {
  revenue.actual = tp * donation.avg
  return(revenue.actual)
}

rev.actual.LogR1 = revenue.actual(tp.LogR1, donation.avg)
rev.actual.LogR2 = revenue.actual(tp.LogR2, donation.avg)
rev.actual.DT = revenue.actual(tp.DT, donation.avg)
rev.actual.PT = revenue.actual(tp.PT, donation.avg)
rev.actual.ANN1 = revenue.actual(tp.ANN1, donation.avg)
rev.actual.ANN2 = revenue.actual(tp.ANN2, donation.avg)

cost.actual = function(tp, fp, mail.cost) {
  cost.actual = (tp + fp) * mail.cost
  return(cost.actual)
}

cost.actual.LogR1 = cost.actual(tp.LogR1, fp.LogR1, mail.cost)
cost.actual.LogR2 = cost.actual(tp.LogR2, fp.LogR2, mail.cost)
cost.actual.DT = cost.actual(tp.DT, fp.DT, mail.cost)
cost.actual.PT = cost.actual(tp.PT, fp.PT, mail.cost)
cost.actual.ANN1 = cost.actual(tp.ANN1, fp.ANN1, mail.cost)
cost.actual.ANN2 = cost.actual(tp.ANN2, fp.ANN2, mail.cost)

revenue.missed = function(fn, donation.avg, mail.cost) {
  revenue.missed = fn * (donation.avg - mail.cost)
  return(revenue.missed)
}

rev.missed.LogR1 = revenue.missed(fn.LogR1, donation.avg, mail.cost)
rev.missed.LogR2 = revenue.missed(fn.LogR2, donation.avg, mail.cost)
rev.missed.DT = revenue.missed(fn.DT, donation.avg, mail.cost)
rev.missed.PT = revenue.missed(fn.PT, donation.avg, mail.cost)
rev.missed.ANN1 = revenue.missed(fn.ANN1, donation.avg, mail.cost)
rev.missed.ANN2 = revenue.missed(fn.ANN2, donation.avg, mail.cost)

profit.oppcost = function(rev.actual, cost.actual, rev.missed) {
  profit.oppcost = rev.actual - cost.actual - rev.missed
  return(profit.oppcost)
}

prof.oc.LogR1 = profit.oppcost(rev.actual.LogR1, cost.actual.LogR1, rev.missed.LogR1)
prof.oc.LogR2 = profit.oppcost(rev.actual.LogR2, cost.actual.LogR2, rev.missed.LogR2)
prof.oc.DT = profit.oppcost(rev.actual.DT, cost.actual.DT, rev.missed.DT)
prof.oc.PT = profit.oppcost(rev.actual.PT, cost.actual.PT, rev.missed.PT)
prof.oc.ANN1 = profit.oppcost(rev.actual.ANN1, cost.actual.ANN1, rev.missed.ANN1)
prof.oc.ANN2 = profit.oppcost(rev.actual.ANN2, cost.actual.ANN2, rev.missed.ANN2)

table.names.2 = c("Model", "Actual Revenue", "Actual Cost", "Missed Revenue", "Expected Profit")
table.LogR1.2 = c("LogR1", rev.actual.LogR1, cost.actual.LogR1, rev.missed.LogR1, prof.oc.LogR1)
table.LogR2.2 = c("LogR2", rev.actual.LogR2, cost.actual.LogR2, rev.missed.LogR2, prof.oc.LogR2)
table.DT.2 = c("DT", rev.actual.DT, cost.actual.DT, rev.missed.DT, prof.oc.DT)
table.PT.2 = c("PT", rev.actual.PT, cost.actual.PT, rev.missed.PT, prof.oc.PT)
table.ANN1.2 = c("ANN1", rev.actual.ANN1, cost.actual.ANN1, rev.missed.ANN1, prof.oc.ANN1)
table.ANN2.2 = c("ANN2", rev.actual.ANN2, cost.actual.ANN2, rev.missed.ANN2, prof.oc.ANN2)

table.prof = rbind(table.names.2, table.LogR1.2, table.LogR2.2, table.DT.2, table.PT.2, table.ANN1.2, table.ANN2.2)
table.prof
#write.csv(table.prof, "profit.csv")

#####DAMT Model#####
#LR1: Ordinary Least Squares Regression
mwuf.damt = subset(mwuf, select = -c(ID, reg1, reg2, reg3, reg4, donr))
set.seed(123)
train.index <- sample(c(1:dim(mwuf.damt)[1]), dim(mwuf.damt)[1]*0.7)  
train.damt <- mwuf.damt[train.index, ]
test.damt <- mwuf.damt[-train.index, ]
LR1 <- lm(damt ~., data = train.damt)
summary(LR1)

#Stepwise Variable Selection
LR2 <- step(LR1, direction = "both")
summary(LR2)

#Evaluation with Mean Percent Error and test data
LR1.pred = predict(LR1, test.damt)
LR1.residuals = test.damt$damt - LR1.pred
LR1.percent.res = LR1.residuals / test.damt$damt
LR1.res.df = data.frame("Actual" = test.damt$damt, "Predicted" = LR1.pred, "Residual" = LR1.residuals, "Percent Residual" = LR1.percent.res)
head(LR1.res.df, 20)
mpe.LR1 = mean(LR1.res.df$Percent.Residual[LR1.res.df$Actual != 0]) * 100
mpe.LR1

LR2.pred = predict(LR2, test.damt)
LR2.residuals = test.damt$damt - LR2.pred
LR2.percent.res = LR2.residuals / test.damt$damt
LR2.res.df = data.frame("Actual" = test.damt$damt, "Predicted" = LR2.pred, "Residual" = LR2.residuals, "Percent Residual" = LR2.percent.res)
head(LR2.res.df)
mpe.LR2 = mean(LR2.res.df$Percent.Residual[LR2.res.df$Actual != 0]) * 100
mpe.LR2

#####Predictions#####

#Predict DONR
mwuf.new.pred.donr <- predict(PT, mwuf.new, decision.values=TRUE, type="class")  
mwuf.new$donr = mwuf.new.pred.donr

#Predict DAMT given DONR

mwuf.new.pred.damt = predict(LR2, mwuf.new[mwuf.new$donr == "Donor", ])
mwuf.new$damt[mwuf.new$donr == "Non-donor"] = 0
mwuf.new$damt[mwuf.new$donr == "Donor"] = mwuf.new.pred.damt

#compare average donation and predictors in both models
mean(mwuf.new$damt[mwuf.new$donr == "Donor"])
mean(mwuf$damt[mwuf$donr == "Donor"])
mean(mwuf.new$damt)
mean(mwuf$damt)

nrow(mwuf.new[mwuf.new$home == "Homeowner", ]) / nrow(mwuf.new)
nrow(mwuf[mwuf$home == "Homeowner", ]) / nrow(mwuf)
mean(mwuf.new$wrat)
mean(mwuf$wrat)
mean(mwuf.new$tdon)
mean(mwuf$tdon)
mean(mwuf.new$tlag)
mean(mwuf$tlag)

#calculate new profit with predicted model
sum(mwuf.new$damt) - 2 * nrow(mwuf.new)
