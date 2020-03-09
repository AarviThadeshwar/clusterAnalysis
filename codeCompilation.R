Code Compilation

data <-read.csv("BlackFriday.csv", header = TRUE)
summary(data)

data<-na.omit(data)

boxplot.stats(data$Purchase)$out  # outlier values.
boxplot(data$Purchase, main="Pressure Height", boxwex=0.1)
mtext(paste("Outliers: ", paste(outlier_values, collapse=", ")), cex=0.6)

smp5 <- floor(0.50 * nrow(data))
smp6 <- floor(0.60 * nrow(data))
smp7 <- floor(0.70 * nrow(data))
set.seed(123456)
idx5 <- sample(seq_len(nrow(data)), size = smp5)
idx6 <- sample(seq_len(nrow(data)), size = smp6)
idx7 <- sample(seq_len(nrow(data)), size = smp7)
train5 <- data[idx5, ]
test5 <- data[-idx5, ]
train6 <- data[idx6, ]
test4 <- data[-idx6, ]
train7 <- data[idx7, ]
test3 <- data[-idx7, ]

scale_test3 <- build_scales(dataSet = test3, cols = c("Purchase"), verbose = TRUE)
test3 <- fastScale(dataSet = test3, scales = scale_test3, verbose = TRUE)
print(head(test3[, c("Purchase")]))

scale_test4 <- build_scales(dataSet = test4, cols = c("Purchase"), verbose = TRUE)
test4 <- fastScale(dataSet = test4, scales = scale_test4, verbose = TRUE)
print(head(test4[, c("Purchase")]))

scale_test5 <- build_scales(dataSet = test5, cols = c("Purchase"), verbose = TRUE)
test5 <- fastScale(dataSet = test5, scales = scale_test5, verbose = TRUE)
print(head(test5[, c("Purchase")]))

scale_train5 <- build_scales(dataSet = train5, cols = c("Purchase"), verbose = TRUE)
train5 <- fastScale(dataSet = train5, scales = scale_train5, verbose = TRUE)
print(head(train5[, c("Purchase")]))

scale_train6 <- build_scales(dataSet = train6, cols = c("Purchase"), verbose = TRUE)
train6 <- fastScale(dataSet = train6, scales = scale_train6, verbose = TRUE)
print(head(train6[, c("Purchase")]))

scale_train7 <- build_scales(dataSet = train7, cols = c("Purchase"), verbose = TRUE)
train7 <- fastScale(dataSet = train7, scales = scale_train7, verbose = TRUE)
print(head(train7[, c("Purchase")]))

test3$Gender <- ifelse(test3$Gender=="M", 1, 0)
test4$Gender <- ifelse(test4$Gender=="M", 1, 0)
test5$Gender <- ifelse(test5$Gender=="M", 1, 0)
train5$Gender <- ifelse(train5$Gender=="M", 1, 0)
train6$Gender <- ifelse(train6$Gender=="M", 1, 0)
train7$Gender <- ifelse(train7$Gender=="M", 1, 0) 

levels(test4$Stay_In_Current_City_Years) <- 0:4
test4$Stay_In_Current_City_Years<- as.numeric(as.character(test4$Stay_In_Current_City_Years))

levels(train6$Stay_In_Current_City_Years) <- 0:4
train6$Stay_In_Current_City_Years<- as.numeric(as.character(train6$Stay_In_Current_City_Years))

test4$Age<- as.numeric(as.character(test4$Age))
train6$Age<- as.numeric(as.character(train6$Age))

sTest30$City_Category <- ifelse(sTest30$City_Category=='A', 1,
                            	ifelse(sTest30$City_Category == 'B', 2, 3))
sTest40$City_Category <- ifelse(sTest40$City_Category=='A', 1,
                            	ifelse(sTest40$City_Category == 'B', 2, 3))
sTest50$City_Category <- ifelse(sTest50$City_Category=='A', 1,
                            	ifelse(sTest50$City_Category == 'B', 2, 3))
sTrain50$City_Category <- ifelse(sTrain50$City_Category=='A', 1,
                            	ifelse(sTrain50$City_Category == 'B', 2, 3))
sTrain60$City_Category <- ifelse(sTrain60$City_Category=='A', 1,
                             	ifelse(sTrain60$City_Category == 'B', 2, 3))
sTrain70$City_Category <- ifelse(sTrain70$City_Category=='A', 1,
                             	ifelse(sTrain70$City_Category == 'B', 2, 3))

test4_pid <- subset(test4, select = c("User_ID","Gender","Age","Product_ID"))
test4_pur <- subset(test4, select = c("User_ID","Gender","Age","Purchase"))
test4_occ <- subset(test4, select = c("User_ID","Gender","Age","Occupation"))
test4_mar <- subset(test4, select = c("User_ID","Gender","Age","Marital_Status"))
test4_city <- subset(test4, select = c("User_ID","Gender","Age","City_Category"))
test4_prod1 <- subset(test4, select = c("User_ID","Gender","Age","Product_Category_1"))
test4_prod2 <- subset(test4, select = c("User_ID","Gender","Age","Product_Category_2"))
test4_prod3 <- subset(test4, select = c("User_ID","Gender","Age","Product_Category_3"))
test4_cur <- subset(test4, select = c("User_ID","Gender","Age","Stay_In_Current_City_Years"))

test4_all <- subset(test4, select = c("User_ID","Product_ID","Gender","Age","Stay_In_Current_City_Years","Occupation","Marital_Status","City_Category","Product_Category_1","Product_Category_2","Product_Category_3","Purchase"))

train6_pid <- subset(test4, select = c("Gender","Age","Product_ID"))
train6_pur <- subset(train6, select = c("User_ID","Gender","Age","Purchase"))
train6_cur <- subset(train6, select = c("User_ID","Gender","Age","Stay_In_Current_City_Years"))
train6_occ <- subset(train6, select = c("Gender","Age","Occupation"))
train6_mar <- subset(train6, select = c("Gender","Age","Marital_Status"))
train6_city <- subset(train6, select = c("User_ID","Gender","Age","City_Category"))
train6_prod1 <- subset(train6, select = c("User_ID","Gender","Age","Product_Category_1"))
train6_prod2 <- subset(train6, select = c("User_ID","Gender","Age","Product_Category_2"))
train6_prod3 <- subset(train6, select = c("User_ID","Gender","Age","Product_Category_3"))

train6_all <- subset(train6, select = c("User_ID","Product_ID","Gender","Age","Stay_In_Current_City_Years","Occupation","Marital_Status","City_Category","Product_Category_1","Product_Category_2","Product_Category_3","Purchase"))

test4_combo <-subset(test4, select = c("Gender","Age","Product_ID","Occupation","Marital_Status"))

train6_combo <-subset(train6, select = c("Gender","Age","Product_ID","Occupation","Marital_Status"))

kmeans(test4_pid, centers = 4)
kmeans(train6_pid, centers = 4)

kmeans(test4_occ, centers = 6)
kmeans(train6_occ, centers = 6)

kmeans(test4_mar, centers = 6)
kmeans(train6_mar, centers = 6)

kmeans(test4_combo, centers = 3)    
kmeans(test4_combo, centers = 5)
kmeans(test4_combo, centers = 7)
kmeans(train6_combo, centers = 3)
kmeans(train6_combo, centers = 5)
kmeans(train6_combo, centers = 7)

##kmeans##
pidTrain70_3<-kmeans(sTrain70_pid, centers = 3)
pidTrain70_5<-kmeans(sTrain70_pid, centers = 5)
pidTrain70_7<-kmeans(sTrain70_pid, centers = 7)

acckTrain70_3<-kmeans(sTrain70_occ, centers = 3, nstart=25)
acckTrain70_5<-kmeans(sTrain70_occ, centers = 5, nstart=25)
acckTrain70_7<-kmeans(sTrain70_occ, centers = 7, nstart=25)

markTrain70_3<-kmeans(sTrain70_mar, centers = 3, nstart=25)
markTrain70_5<-kmeans(sTrain70_mar, centers = 5, nstart=25)
markTrain70_7<-kmeans(sTrain70_mar, centers = 7, nstart=25)

allkTrain70_3<-kmeans(sTrain70_all, centers = 3, nstart=25)
allkTrain70_5<-kmeans(sTrain70_all, centers = 5, nstart=25)
allkTrain70_7<-kmeans(sTrain70_all, centers = 7, nstart=25)

data$Age <- mapvalues(data$Age, from=c("0-17", "18-25", "26-35", "36-45", "46-50", "51-55", 
                                       "55+"), to=c(0, 18, 26, 36, 46, 51, 55))
data$Age<- as.numeric(as.character(data$Age))

screedat<-scree(sTrain70_all, factors=TRUE)

s70.knn<-knn(sTrain70_occ, sTest30_occ, acckTrain70_5$cluster, k=4)
sTest30_occ_k4<-kmeans(sTest30_occ, 4)
View(sTest30_occ_k4)

train5_all <- subset(train5, select = c("User_ID","Product_ID","Gender","Age","Stay_In_Current_City_Years","Occupation","Marital_Status","City_Category","Product_Category_1","Product_Category_2","Product_Category_3","Purchase"))


wssplot <- function(data, nc=15, seed=1234)
{
  wss <- (nrow(data)-1)*sum(apply(data,2,var))
  for (i in 2:nc)
  {
	set.seed(seed)
	wss[i] <- sum(kmeans(data, centers=i)$withinss)
  }
  plot(1:nc, wss, type="b",
   	xlab="Number of Clusters",
   	ylab="Within groups sum of squares")
}


p3<-wssplot(train5_all)

#creating subsets for each of the listed attributes plus "User_ID","Gender","Age"
train5_pid <- subset(train5, select = c("User_ID","Gender","Age","Product_ID"))
train5_occ <- subset(train5, select = c("User_ID","Gender","Age","Occupation"))
train5_mar <- subset(train5, select = c("User_ID","Gender","Age","Marital_Status"))
p1<-wssplot(train5_pid)
p2<-wssplot(train5_occ)
p3<-wssplot(train5_mar)



#removing userid because it was making data look to similar
train5_pid <- subset(train5, select = c("Gender","Age","Product_ID"))
train5_occ <- subset(train5, select = c("Gender","Age","Occupation"))
train5_mar <- subset(train5, select = c("Gender","Age","Marital_Status"))
p1<-wssplot(train5_pid)
p2<-wssplot(train5_occ)
p3<-wssplot(train5_mar)



train_all_3 <-subset(train5, select = c("Gender","Age","Product_ID","Occupation","Marital_Status"))
plot(train_all_3)

kbest_pid_50<-kmeans(train5_pid, 4)
kbest_occ_50<-kmeans(train5_occ, 6)
kbest_mar_50<-kmeans(train5_mar, 6)

kbest_pid_50
kbest_occ_50
kbest_mar_50

k3_50<-kmeans(train50_all_3, 3)
k5_50<-kmeans(train50_all_3, 5)
k7_50<-kmeans(train50_all_3, 7)

iclust(train50_all_3, 3)
iclust(train50_all_3, 5)
iclust(train50_all_3, 7)
k3_50
k5_50
k7_50

##kmeans for 70:30

kbest_pid_70<-kmeans(sTrain70_pid, 4)
kbest_occ_70<-kmeans(sTrain70_occ, 6)
kbest_mar_70<-kmeans(sTrain70_mar, 6)

kbest_pid_50
kbest_occ_50
kbest_mar_50

k3_70<-kmeans(train_all_3, 3)
k5_70<-kmeans(train_all_3, 5)
k7_70<-kmeans(train_all_3, 7)

iclust(train70_all_3, 3)
iclust(train70_all_3, 5)
iclust(train70_all_3, 7)
k3_70
k5_70
k7_70

#60:40
#lm
fit1<- lm(formula=train6_all$Purchase~train6_all$Product_ID,data = train6_all)
summary.lm(fit1)
plot(fit1$fitted.values, fit1$residuals)

#glm
fit2<-glm(formula=train6_all$Purchase~train6_all$Product_ID,family=gaussian,
          	data = train6_all)
summary.glm(fit2)
plot(fit2$fitted.values, fit2$residuals)

fit2$pred<-predict.glm(fit2, NULL, type = c("link", "response", "terms"))
fit2$pred
newdata<-cbind(fit2$fitted.values, train6_all$Purchase)
View(newdata)
fit2con<-confint(fit2)
Fit2con

#svm
trainsvm<-svm(train6_all$Purchase~train6_all$Product_ID+train6_all$Product_Category_1
        	+train6_all$Product_Category_2+train6_all$Product_Category_3,
        	data=train6_all, kernel="linear")
trainsvm




###50-50
#lm
View(test5_all)
autofit1<- lm(formula=train5$Purchase~train5$Product_ID,data = train5)
summary.lm(autofit1)
plot(autofit1$fitted.values, autofit1$residuals)

#glm
auto.test.occ<-glm(formula=test5_occ$Gender~test5_occ$Occupation,family=gaussian,data=test5_occ)
summary.glm(auto.test.occ)
auto.test.occ<-glm(formula=test5_occ$Gender~test5_occ$Age,family=gaussian,data=test5_occ)
summary.glm(auto.test.occ)
auto.test.occ<-glm(formula=test5_occ$Age~test5_occ$Occupation,family=gaussian,data=test5_occ)
summary.glm(auto.test.occ)

autofit.testocc<-predict.glm(auto.test.occ,NULL,type=c("linik","response","terms"))
autofit.testocc

###70-30
###lm
View(sTrain70_all)
autofit1<- lm(formula=sTrain70_all$Purchase~sTrain70_all$Product_ID,data = sTrain70_all)
summary.lm(autofit1)
plot(autofit1$fitted.values, autofit1$residuals)

###glm
autofit2<-glm(formula=sTrain70_all$Purchase~sTrain70_all$Product_ID,family=gaussian,
              data = sTrain70_all)
summary.glm(autofit2)
plot(autofit2$fitted.values, autofit2$residuals)

###logistic regression
autofit2$pred<-predict.glm(autofit2, NULL, type = c("link", "response", "terms"))
autofit2$pred
newdata<-cbind(autofit2$fitted.values, sTrain70_all$Purchase)
View(newdata)
autofit2con<-confint(autofit2)
autofit2con

autofit1<- glm(formula=test5$Purchase~test5$Product_ID,family=gaussian,data = test5)
summary.glm(autofit1)
plot(autofit1$fitted.values, autofit1$residuals)

autofit1$pred<-predict.glm(autofit1, NULL, type = c("link", "response", "terms"))
autofit1$pred
newdata<-cbind(autofit1$fitted.values, train5$Purchase)
View(newdata)
autofit1con<-confint(autofit1)
autofit1con

##svm
datsvm<-svm(sTrain70_all$Purchase~sTrain70_all$Product_ID+sTrain70_all$Product_Category_1
            +sTrain70_all$Product_Category_2+sTrain70_all$Product_Category_3,
            data=sTrain70_all, kernel="linear")
datsvm







