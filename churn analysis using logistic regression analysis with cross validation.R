
### newspaper readers' churn predicted by "whether had previous subscriptions", "any complaints in the last 40 days", without crossvalidation 
### lnpchurn$churn~lnpchurn$PreSub+lnpchurn$COMPLAINTS_IN_LAST_40_DAY
### no test/train

table(lnpchurn$COMPLAINTS_IN_LAST_40_DAYS)
table(lnpchurn$PreSub)

test1<-glm(lnpchurn$churn~lnpchurn$PreSub+lnpchurn$COMPLAINTS_IN_LAST_40_DAYS, data=lnpchurn, family = binomial)
summary(test1)

probs1<-predict(test1,type = "response")
probs1[1:10]

pred1=rep("NOCHURN",29689)
pred1[probs1>.5]="YESCHURN"

table(pred1, lnpchurn$churn)
mean(lnpchurn$churn==pred1)


### newspaper readers' churn predicted by "whether had previous subscriptions", "any complaints in the last 40 days", with crossvalidation; RoC curves applied
### lnpchurn$churn~lnpchurn$PreSub+lnpchurn$COMPLAINTS_IN_LAST_40_DAY
### yes test/train

sapply(lnpchurn, function(x) length(unique(x)))
sapply(lnpchurn,function(x) sum(is.na(x)))

smp_size<-(0.5*nrow(lnpchurn)+.5)
set.seed(124)
train_ind<-sample(seq(nrow(lnpchurn)), size = smp_size)
lnptrain<-lnpchurn[train_ind,]
lnptest<-lnpchurn[-train_ind,]

contrasts(lnpchurn$PreSub)
contrasts(lnpchurn$churn)
summary(lnpchurn$COMPLAINTS_IN_LAST_40_DAYS)

test2<-glm(churn~PreSub+COMPLAINTS_IN_LAST_40_DAYS, family = binomial, data=lnptrain)

anova(test2,test= "Chisq")

p2<-predict(test2,newdata=lnptest,type = "response")
summary(p2)
p2[1:10]
contrasts(lnptest$churn)

pred2=ifelse(p2 > 0.5,"YESCHURN","NOCHURN")
table(pred2, lnptest$churn)
ClasificRate <- mean(pred2==lnptest$churn)
print(paste('Accuracy',ClasificRate))

library(ROCR)
library(gplots)
p2<-predict(test2,newdata=lnptest,type = "response")
pr2 <- ROCR::prediction(p2, lnptest$churn)
prf2 <- ROCR::performance(pr2, measure = "tpr", x.measure = "fpr")
plot(prf2)
auc <- ROCR::performance(pr2, "auc")
auc <- auc@y.values[[1]]
auc


##########################################################################################################################
### newspaper readers' churn predicted by "START CATEGORY LAST START", "DAYS_SINCE_COMPLAINT", with crossvalidation; RoC curves applied
# " 


attach(lnptest)

test3<-qlm(churn~ `START CATEGORY LAST START`+ DAYS_SINCE_COMPLAINT , family = binomial, data = lnptest)
summary(test3)
contrasts(FLAG_NO_PAYMENT_SUBS)
contrasts(churn)
contrasts(`START CATEGORY LAST START`)
summary(FLAG_NO_PAYMENT_SUBS)
table(FLAG_NO_PAYMENT_SUBS, churn)

test3<-glm(churn~PreSub+COMPLAINTS_IN_LAST_30_DAYS, family = binomial, data=lnptrain)

anova(test3,test= "Chisq")

p3<-predict(test3,newdata=lnptest,type = "response")
summary(p3)
p3[1:10]
contrasts(lnptest$churn)

pred3=ifelse(p3 > 0.5,"YESCHURN","NOCHURN")
table(pred3, lnptest$churn)
ClasificRate <- mean(pred3==lnptest$churn)
print(paste('Accuracy',ClasificRate))

library(ROCR)
library(gplots)
p3<-predict(test3,newdata=lnptest,type = "response")
pr3 <- ROCR::prediction(p3, lnptest$churn)
prf3 <- ROCR::performance(pr3, measure = "tpr", x.measure = "fpr")
plot(prf3)
auc <- ROCR::performance(pr3, "auc")
auc <- auc@y.values[[1]]
auc
