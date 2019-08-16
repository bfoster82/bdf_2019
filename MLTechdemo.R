#stage environ
packages <- c("plyr", "caret", "car", "class", "corrplot", "dplyr", "e1071", "forecast", 
              "gbm", "ggridges", "ggplot2", "gridExtra", "ISLR", "magrittr", "MASS", 
              "randomForest", "ranger", "RColorBrewer", "tree", "glmnet","xray")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)
#load data, get a feel for it
charity <- read.csv(file.choose())
summary(charity)
fix(charity)
charity %>% 
  mutate(reg5=1-(reg1+reg2+reg3+reg4), 
         donrf=as.factor(donr), 
         homef=as.factor(home), 
         hincf=as.factor(hinc),
         genff=as.factor(genf), 
         wratf=as.factor(wrat),
         recent=as.factor(ifelse(tdon<12,1,0)),
        #last year
         donrdue=as.factor(ifelse((tdon>12)&(tdon<24),1,0)), 
         #b/t 1-2 y
         tgif.npro=tgif/npro, 
         #avg don
         incgif=as.factor(ifelse((rgif>=agif),1,0))) %>% 
 #most recent above avg
  filter(!is.na(donr)) -> charity.tv
# Filter out the data from the test set
distributions(charity)
distributions(charity.tv)

charity.tv %>%
  group_by(donr) %>%
  summarize(donr.reg1=sum(reg1),
            donr.reg2=sum(reg2),
            donr.reg3=sum(reg3),
            donr.reg4=sum(reg4),
            donr.reg5=sum(reg5)) -> donr.reg

#region 1&2 have higher donor rate
barplot(height=as.matrix(donr.reg[,-1]), col=c("pink","light blue"), 
        legend.text=c("Not Donor","Donor"))

#homeowners more likely to donate
ggplot(charity.tv, aes(donrf,homef)) + geom_jitter()

#fewer children more likely
ggplot(charity.tv, aes(donrf,chld)) + geom_boxplot()

#middle income more likely
ggplot(charity.tv, aes(hincf)) + geom_bar(aes(fill=donrf))

#gender can be removed as a predictor
ggplot(charity.tv, aes(genff)) +
  geom_bar(aes(fill=donrf), position="fill")

#higher wealth more likely
ggplot(charity.tv, aes(wratf)) +
  geom_bar(aes(fill=donrf), position="fill")






#slight negative corr
ggplot(charity.tv, aes(recent, donrf)) + geom_jitter()

# slight positive corr
ggplot(charity.tv, aes(donrdue, donrf)) + geom_jitter()

# unrelated
ggplot(charity.tv, aes(y=log(tgif.npro), x=donrf)) + geom_boxplot()

#unrelated
ggplot(charity.tv, aes(incgif, donrf)) + geom_jitter()

#get actual donors
charity.tv %>%
  filter(donr==1) %>% 
  mutate("region"=ifelse(reg1==1,"reg1",
                         ifelse(reg2==1,"reg2",
                                ifelse(reg3==1,"reg3",
                                       ifelse(reg4==1,"reg4","reg5"))))) %>%
  mutate("chldf"=as.factor(chld)) -> charity.tv1

#region 3 aand 4 slightly higher
ggplot(charity.tv1, aes(y=damt,x=region)) + geom_boxplot()

#home owners higher
ggplot(charity.tv1, aes(y=damt,x=homef)) + geom_boxplot()

#chilldren decrease it
ggplot(charity.tv1, aes(y=damt,x=chldf)) + geom_boxplot()

# amount postive to house income
ggplot(charity.tv1, aes(y=damt, x=hincf)) + geom_boxplot()

# unrelated
ggplot(charity.tv1, aes(y=damt, x=genff)) + geom_boxplot()

# midwealth slightly higher
ggplot(charity.tv1, aes(y=damt, x=wratf)) + geom_boxplot()



# not related
ggplot(charity.tv1, aes(x=npro,y=damt)) + geom_jitter()

#unrelated
ggplot(charity.tv1, aes(x=tdon, y=damt)) + geom_jitter()

#unrelated?
ggplot(charity.tv1, aes(x=tlag, y=damt)) + geom_jitter()

#unrelated
ggplot(charity.tv1, aes(x=recent, y=damt)) + geom_boxplot()

#unrelated
ggplot(charity.tv1, aes(x=donrdue, y=damt)) + geom_boxplot()

#slight positve
ggplot(charity.tv1, aes(x=incgif, y=damt)) + geom_boxplot()

#unrelated
ggplot(charity.tv1, aes(x=log(tgif.npro), y=damt)) + geom_jitter()

charity.tv1 %>%
  dplyr::select(damt, avhv, incm, inca, plow, npro, tgif, lgif, rgif, agif, tdon, tlag) %>%
  cor(use = 'pairwise.complete.obs') -> corrtab.y
print(corrtab.y)
corrplot(corrtab.y, type="lower", method="color", main="", 
         col=brewer.pal(n=8, name="RdBu"),diag=FALSE)
# transform predictors that are skewed using a log or Box-Cox transformation.
charity.t <- charity
charity.t$avhv <- log(charity.t$avhv)
charity.t$incm <- log(charity.t$incm)
charity.t$inca <- log(charity.t$inca)
charity.t$tgif <- BoxCox(charity.t$tgif, lambda.tgif)
charity.t$lgif <- BoxCox(charity.t$lgif, lambda.lgif)
charity.t$rgif <- BoxCox(charity.t$rgif, lambda.rgif)
charity.t$agif <- BoxCox(charity.t$agif, lambda.agif)

# Stage to analyze 
don.avg <- 14.5
mail.cost <- 2

data.train <- charity.t[charity$part=="train",]
x.train <- data.train[,2:21]
c.train <- data.train[,22] 
c.train.fact <- make.names(data.train[,22]) 

n.train.c <- length(c.train)
y.train <- data.train[c.train==1,23] 
n.train.y <- length(y.train)

data.valid <- charity.t[charity$part=="valid",]
x.valid <- data.valid[,2:21]
c.valid <- data.valid[,22]
c.valid.fact <- make.names(data.valid[,22]) 

n.valid.c <- length(c.valid)
y.valid <- data.valid[c.valid==1,23] 
n.valid.y <- length(y.valid) 

data.test <- charity.t[charity$part=="test",]
n.test <- dim(data.test)[1] 
x.test <- data.test[,2:21]

x.train.mean <- apply(x.train, 2, mean)
x.train.sd <- apply(x.train, 2, sd)
x.train.std <- t((t(x.train)-x.train.mean)/x.train.sd) 
apply(x.train.std, 2, mean)
apply(x.train.std, 2, sd) 
data.train.std.c <- data.frame(x.train.std, donr=c.train, donrf= c.train.fact) 
data.train.std.y <- data.frame(x.train.std[c.train==1,], damt=y.train) 

x.valid.std <- t((t(x.valid)-x.train.mean)/x.train.sd) 
data.valid.std.c <- data.frame(x.valid.std, donr=c.valid, donrf=c.valid.fact) 
data.valid.std.y <- data.frame(x.valid.std[c.valid==1,], damt=y.valid) 

x.test.std <- t((t(x.test)-x.train.mean)/x.train.sd) 
data.test.std <- data.frame(x.test.std)

library(MASS)
# 
model.lda1 <- lda(donr ~ reg1 + reg2 + reg3 + reg4 + home + chld + hinc + I(hinc^2) + genf + wrat + 
                    avhv + incm + inca + plow + npro + tgif + lgif + rgif + tdon + tlag + agif, 
                  data.train.std.c)

post.valid.lda1 <- predict(model.lda1, data.valid.std.c)$posterior[,2] 

# # calculate ordered profit function using average donation = $14.50 and mailing cost = $2
# 
profit.lda1 <- cumsum(14.5*c.valid[order(post.valid.lda1, decreasing=T)]-2)
plot(profit.lda1) 
n.mail.valid <- which.max(profit.lda1)
results.lda1=c(n.mail.valid, max(profit.lda1)) 

cutoff.lda1 <- sort(post.valid.lda1, decreasing=T)[n.mail.valid+1] 
chat.valid.lda1 <- ifelse(post.valid.lda1>cutoff.lda1, 1, 0)
table(chat.valid.lda1, c.valid) 

### LOG REG###
model.log2 <- glm(donr ~ reg1 + reg2 + reg3 + reg4 + home + chld + hinc + I(hinc^2) + genf + 
                    wrat + avhv + incm + inca + plow + npro + tgif + lgif + rgif + tdon + 
                    tlag + agif, data.train.std.c, family=binomial("logit"))
summary(model.log2)

post.valid.log2 <- predict(model.log2, data.valid.std.c, type="response") # n.valid post probs

profit.log2 <- cumsum(don.avg*c.valid[order(post.valid.log2, decreasing=T)]-mail.cost)

# plot profit as funtion of mailing
plot(profit.log2, type="l", main="Profits", xlab="Mailings", ylab="$")
n.mail.valid <- which.max(profit.log2) # #mailsing to max profit
results.log2 <- c(n.mail.valid, max(profit.log2)) # mail & profit
results.log2
# 1354 11647

cutoff.log2 <- sort(post.valid.log2, decreasing=T)[n.mail.valid+1] # set cutoff
truth <- factor(c.valid)
chat.valid.log2 <- ifelse(post.valid.log2>cutoff.log2, 1, 0) # mail to everyone above the cutoff
(csf.log2 <- confusionMatrix(factor(chat.valid.log2),truth)$table)
#          Reference
#Prediction   0   1
#         0 655   9
#         1 364 990

### KNN###
model.knn1 <- knn(data.train.std.c[,1:20], data.valid.std.c[,1:20], 
                  data.train.std.c[,21],k=200,prob=T)

post.valid.knn1 <- attr(model.knn1,"prob") # n.valid post probs

profit.knn1 <- cumsum(don.avg*c.valid[order(post.valid.knn1, decreasing=T)]-mail.cost)

# plot how profits change as more mailings are made
plot(profit.knn1, type="l", main="Profits", xlab="Mailings", ylab="$")
n.mail.valid <- which.max(profit.knn1) # mail to max profit
results.knn1 <- c(n.mail.valid, max(profit.knn1)) # mail and profit no
results.knn1
# 1927.0 10515.5

cutoff.knn1 <- sort(post.valid.knn1, decreasing=T)[n.mail.valid+1] # set cutoff based on n.mail.valid
chat.valid.knn1 <- ifelse(post.valid.knn1>cutoff.knn1, 1, 0) # mail to everyone above the cutoff
(cfs.knn1<-confusionMatrix(factor(chat.valid.knn1), truth)$table)
#          Reference
#Prediction   0   1
#         0  93  12
#         1 926 987

# Check number of mailings
sum(cfs.knn1[2,])
# Check potential profit
don.avg*cfs.knn1[2,2] - 
  mail.cost*sum(cfs.knn1[2,])
# lot of ties do to parameters
### R TREE###
model.tree1 <- rpart(donr ~ reg1 + reg2 + reg3 + reg4 + home + chld + hinc + I(hinc^2) + 
                      genf + wrat + avhv + incm + inca + plow + npro + tgif + lgif + 
                      rgif + tdon + tlag + agif, data=data.train.std.c)
summary(model.tree1)
rpart.plot(model.tree1)


cv.model.tree1 <- cv.tree(model.tree1)
cv.model.tree1
par(mfrow=c(1,2))
plot(cv.model.tree1$size, cv.model.tree1$dev, type="b")
plot(cv.model.tree1$k, cv.model.tree1$dev, type="b")

# no pruning req.

post.valid.tree1 <- predict(model.tree1,data.valid.std.c, type="vector") # n.valid post probs

profit.tree1 <- cumsum(don.avg*c.valid[order(post.valid.tree1, decreasing=T)]-mail.cost)

# plot how profits change as more mailings are made
plot(profit.tree1, type="l", main="Profits", xlab="Mailings", ylab="$")
n.mail.valid <- which.max(profit.tree1) # no mailings max profit
results.tree1 <- c(n.mail.valid, max(profit.tree1)) # report number of mailings and maximum profit
results.tree1
# 1391 11312

cutoff.tree1 <- sort(post.valid.tree1, decreasing=T)[n.mail.valid+1] # set cutoff based on n.mail.valid
chat.valid.tree1 <- ifelse(post.valid.tree1>cutoff.tree1, 1, 0) # mail to everyone above the cutoff
(cfs.tree1 <- confusionMatrix(factor(chat.valid.tree1), truth)$table)
#          Reference
#Prediction   0   1
#         0 645  37
#         1 374 962

# Check number of mailings
sum(cfs.tree1[2,])
# Check potential profit
don.avg*cfs.tree1[2,2] - 
  mail.cost*sum(cfs.tree1[2,])
#ties due to discrete branches


### BAGGING###
model.bag1 <- randomForest(donrf ~ reg1 + reg2 + reg3 + reg4 + home + chld + hinc + genf + 
                             wrat + avhv + incm + inca + plow + npro + tgif + lgif + 
                             rgif + tdon + tlag + agif, data=data.train.std.c, 
                           mtry=20, importance=T)


model.bag1

post.valid.bag1 <- predict(model.bag1, data.valid.std.c, type="prob")[,2] # n.valid post probs

profit.bag1 <- cumsum(don.avg*c.valid[order(post.valid.bag1, decreasing=T)]-mail.cost)

# plot how profits change as more mailings are made
plot(profit.bag1, type="l", main="Profits", xlab="Mailings", ylab="$")
n.mail.valid <- which.max(profit.bag1) # mail to maximize profit
results.bag1 <- c(n.mail.valid, max(profit.bag1)) # mail and profit
results.bag1
# 1293.0 11725.5


cutoff.bag1 <- sort(post.valid.bag1, decreasing=T)[n.mail.valid+1] # set cutoff based on n.mail.valid
chat.valid.bag1 <- ifelse(post.valid.bag1>cutoff.bag1, 1, 0) # mail to everyone above the cutoff
(cfs.bag1 <- confusionMatrix(factor(chat.valid.bag1), truth)$table)

###Boosting###

max(0.01, 0.1*min(1, nrow(data.train.std.c)/10000))

floor(sqrt(ncol(data.train.std.c)))

gbmGrid <-  expand.grid(interaction.depth = 1,
                        n.trees = (1:50)*50,
                        shrinkage = seq(.001, .01,.001),
                        n.minobsinnode = 10)
model.boost1 <- train(donr ~ reg1 + reg2 + reg3 + reg4 + home + chld + hinc + genf + wrat + 
                        avhv + incm + inca + plow + npro + tgif + lgif + rgif + tdon + 
                        tlag + agif, data=data.train.std.c, distribution="gaussian", 
                      method="gbm", verbose = F, tuneGrid=gbmGrid, 
                      trControl=trainControl(method = "cv", number = 5))
model.boost1
plot(model.boost1)

post.valid.boost1 <- predict(model.boost1, data.valid.std.c, n.trees=2500) 
# n.valid post probs

profit.boost1 <- cumsum(don.avg*c.valid[order(post.valid.boost1, decreasing=T)]-mail.cost)

# plot how profits change as more mailings are made
plot(profit.boost1, type="l", main="Profits", xlab="Mailings", ylab="$")
n.mail.valid <- which.max(profit.boost1) # number of mailings that maximizes profits
results.boost1 <- c(n.mail.valid, max(profit.boost1)) # report number of mailings and maximum profit
results.boost1
# 1241.0 11887.5


cutoff.boost1 <- sort(post.valid.boost1, decreasing=T)[n.mail.valid+1] # set cutoff based on n.mail.valid
chat.valid.boost1 <- ifelse(post.valid.boost1>cutoff.boost1, 1, 0) # mail to everyone above the cutoff
confusionMatrix(factor(chat.valid.boost1), truth)$table

### SVM ###
tune.out=tune(svm, donrf ~ reg1 + reg2 + reg3 + reg4 + home 
              + chld + hinc + I(hinc^2) + genf + wrat + avhv 
              + incm + inca + plow + npro + tgif + lgif + rgif 
              + tdon + tlag + agif, data=data.train.std.c, 
              kernel="linear", 
              ranges=list(cost=c(0.01, 0.1, 0.5, 1, 5, 10)), 
              scale=F, probability=T)
model.svm1 <- tune.out$best.model
summary(tune.out)

post.valid.svm1 <- attr(predict(model.svm1, data.valid.std.c, probability=T),"probabilities")[,2] # n.valid.c post probs

profit.svm1 <- cumsum(don.avg*c.valid[order(post.valid.svm1, decreasing=T)]-mail.cost)

# plot how profits change as more mailings are made
plot(profit.svm1, type="l", main="Profits", xlab="Mailings", ylab="$")
n.mail.valid <- which.max(profit.svm1) # number of mailings that maximizes profits
results.svm1 <- c(n.mail.valid, max(profit.svm1))# report number of mailings and maximum profit
results.svm1
#  1315 11638
cutoff.svm1 <- sort(post.valid.svm1, decreasing=T)[n.mail.valid+1] # set cutoff based on n.mail.valid
chat.valid.svm1 <- ifelse(post.valid.svm1>cutoff.svm1, 1, 0) # mail to everyone above the cutoff

confusionMatrix(factor(chat.valid.svm1), truth)$table

results.class <- as.data.frame(rbind(results.lda1, results.log2, 
                                     results.knn1, results.tree1, results.bag1, 
                                     results.boost1,results.svm1), 
                               row.names=c("Linear Discriminant Analysis",
                                           "Logistic Regression", 
                                           "K-nearest Neighbors", 
                                           "Decision Tree", "Bagging", 
                                           "Boosting", 
                                           "Support Vector Classifier"))
colnames(results.class) <- c("Number of Mailings","Profit")
results.class[order(results.class$Profit,decreasing=T),]

# calculate the posterior probabilites
post.test.boost1 <- predict(model.boost1, data.test.std, n.trees=2500) 

# Oversampling adjustment 
n.mail.valid <- which.max(profit.boost1)
tr.rate <- .1 # typical response rate is .1
vr.rate <- .5 # whereas validation response rate is .5
adj.test.1 <- (n.mail.valid/n.valid.c)/(vr.rate/tr.rate) # adjustment for mail yes
adj.test.0 <- ((n.valid.c-n.mail.valid)/n.valid.c)/((1-vr.rate)/(1-tr.rate)) # adjustment for mail no
adj.test <- adj.test.1/(adj.test.1+adj.test.0) # scale into a proportion
n.mail.test <- round(n.test*adj.test, 0) # calculate number of mailings for test set

# set a cutoff 
# sort the test set by probability of donaTE
cutoff.test <- sort(post.test.boost1, decreasing=T)[n.mail.test+1] 

# mail to everyone above the cutoff 
chat.test <- ifelse(post.test.boost1>cutoff.test, 1, 0) 
table(chat.test)
#    0    1 
# 1705  302
# based on this mail 302 highest post prob
# After update
#   0    1 
#1695  312

#### PREDICTIVE ####

### Least Squares ###
model.ls1 <- lm(damt ~ reg1 + reg2 + reg3 + reg4 + home + chld + hinc + genf + wrat + 
                  avhv + incm + inca + plow + npro + tgif + lgif + rgif + tdon + tlag + agif, 
                data.train.std.y)
# 
pred.valid.ls1 <- predict(model.ls1, newdata = data.valid.std.y) # validation predictions
mean((y.valid - pred.valid.ls1)^2) #mpe
# # 1.867523
sd((y.valid - pred.valid.ls1)^2)/sqrt(n.valid.y) # se
# # 0.1696615
# 

model.ls2 <- lm(damt ~ reg1 + reg2 + reg3 + reg4 + home + chld + hinc + genf + 
                  avhv + incm + inca + plow + npro + tgif + lgif + rgif + tdon + tlag + agif, 
                data.train.std.y)
# 
pred.valid.ls2 <- predict(model.ls2, newdata = data.valid.std.y) # validation predictions
MPE.ls1.y=mean((y.valid - pred.valid.ls2)^2) # mean prediction error
# # 1.867433
STDE.ls1.y=sd((y.valid - pred.valid.ls2)^2)/sqrt(n.valid.y) # std error
# # 0.1696498
results.ls1.y=c(MPE.ls1.y, STDE.ls1.y)
# # Results
# 
# # MPE  Model
# # 1.867523 LS1
# # 1.867433 LS2
# 
# # model ls.2 = best
# 
yhat.test <- predict(model.ls2, newdata = data.test.std) # test predictions

### LASSO ###
cv.out <- cv.glmnet(x=x.train.std[c.train==1,], y=y.train, alpha=1)
plot(cv.out)
bestlam =cv.out$lambda.min 

grid <- 10^seq(10,-2, length =100)
model.lasso1.y <- glmnet(x=x.train.std[c.train==1,], y=y.train, alpha=1, lambda=bestlam)
model.lasso1.y

# calculate the donation predictions on the validation data
pred.valid.lasso1.y <- predict(model.lasso1.y, s=bestlam, newx=x.valid.std[c.valid==1,])

pred.lasso1.coef <- predict(model.lasso1.y,type="coefficients", s=bestlam)[1:21,]
pred.lasso1.coef

(MPE.lasso1.y <- mean((y.valid - pred.valid.lasso1.y)^2)) # mean prediction error
# 1.538479
(STDE.lasso1.y <- sd((y.valid - pred.valid.lasso1.y)^2)/sqrt(n.valid.y)) # std error
# 0.1616434
results.lasso1.y <- c(MPE.lasso1.y, STDE.lasso1.y)


### RANDOM FOREST###
grid.rf1.y <- expand.grid(mtry=c(2,3,4,5,10,15,20), splitrule="extratrees", min.node.size=1)
model.rf1.y <- train(damt ~ reg1 + reg2 + reg3 + reg4 + home + chld + hinc + genf + wrat + 
                       avhv + incm + inca + plow + npro + tgif + lgif + rgif + tdon + 
                       tlag + agif, tuneGrid = grid.rf1.y, 
                     data=data.train.std.y, method = "ranger", importance="impurity", 
                     trControl = trainControl(method = "cv", number = 10, classProbs=F))
model.rf1.y
plot(model.rf1.y)
varImp(model.rf1.y)

# calculate the donation predictions on the validation data
pred.valid.rf1.y <- predict(model.rf1.y, data.valid.std.y, type="raw")
(MPE.rf1.y <- mean((y.valid - pred.valid.rf1.y)^2))

(STDE.rf1.y <- sd((y.valid - pred.valid.rf1.y)^2)/sqrt(n.valid.y))

results.rf1.y <- c(MPE.rf1.y, STDE.rf1.y)
### BOOSTING###

max(0.01, 0.1*min(1, nrow(data.train.std.y)/10000)) # 0.2

floor(sqrt(ncol(data.train.std.y))) # 4

gbmGrid <-  expand.grid(interaction.depth = 1,
                        n.trees = (1:10)*500,
                        shrinkage = seq(.001, .01,.001),
                        n.minobsinnode = 10)

model.boost1.y <- train(damt~., data=data.train.std.y, distribution="gaussian", 
                        method="gbm", verbose = F, tuneGrid=gbmGrid, 
                        trControl=trainControl(method = "cv", number = 5))
model.boost1.y$finalModel$n.trees
summary(model.boost1.y)
plot(model.boost1.y)

# calculate the donation predictions on the validation data
pred.valid.boost1.y <- predict(model.boost1.y, data.valid.std.y, 
                               n.trees=model.boost1.y$finalModel$n.trees) 
(MPE.boost1.y <- mean((y.valid - pred.valid.boost1.y)^2)) # mope

(STDE.boost1.y <- sd((y.valid - pred.valid.boost1.y)^2)/sqrt(n.valid.y))# se


results.boost1.y <- c(MPE.boost1.y, STDE.boost1.y)

###RPART
model.tree2.y <- rpart(damt ~ reg1 + reg2 + reg3 + reg4 + home + chld + hinc + I(hinc^2) + 
                        genf + wrat + avhv + incm + inca + plow + npro + tgif + lgif + 
                        rgif + tdon + tlag + agif, data=data.train.std.y)
summary(model.tree2.y)


# calculate the donation predictions on the validation data
pred.valid.tree2.y <- predict(model.tree2.y,newdata=data.valid.std.y, type="vector")
(MPE.tree2.y <- mean((y.valid - pred.valid.tree2.y)^2)) # mean prediction error
# 2.241075
(STDE.tree2.y <- sd((y.valid - pred.valid.tree2.y)^2)/sqrt(n.valid.y)) # std error
# 0.1920681
results.tree2.y <- c(MPE.tree2.y, STDE.tree2.y)

### Results ###
results.pred <- as.data.frame(rbind(results.ls1.y,  
                                    results.lasso1.y, results.rf1.y, results.boost1.y,results.tree2.y), 
                              row.names=c("Least Squares Regression", 
                                          "Lasso", "Random Forest", 
                                          "Boosting","Recursive Partitioning"))
colnames(results.pred) <- c("MPE","STDE")
results.pred[order(results.pred$MPE,decreasing=F),]

### FINAL###
# Profit if random mail
n.mail.test*(tr.rate*don.avg-mail.cost)
#predicted profit on test
sum(chat.test*yhat.test)-n.mail.test*mail.cost
# Avg donation on test
sum(chat.test*yhat.test)/n.mail.test
# This close to the expected donation that was input in the model.

# Save final results for both classification and regression
length(chat.test) #  length = 2007
length(yhat.test) #  length = 2007
chat.test[1:10] # 0s and 1s
yhat.test[1:10] # predictions of damt

ip <- data.frame(chat=chat.test, yhat=yhat.test) 
write.csv(ip, file="BrianFoster.csv", row.names=FALSE) 



