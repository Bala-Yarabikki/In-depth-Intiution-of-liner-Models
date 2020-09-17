getwd()# get the working directory
setwd('C:/Users/balay/OneDrive/Bureau')#set the working directory
getwd()
dir()# to see all files in our working directory
data = read.table('data0.txt')#read the data
head(data)#display our data
colnames(data)=c('Y','X')#change data's column names for convi
head(data)
summary(data)#we have two variables x,y are numeric so we can see our summary of data
plot(data$X,data$Y)#draw a plot and observe the data,check linear correlation
abline(l,col='red')
l=lm(Y~X,data = data)#perform linear model
summary(l)#check quality of model or overall summary of model
#1)when we use linear model first we look on MULTI R Squared(0-1),if its close to 1 our model is good
#2)look quartile residuals whether there are symetric are not
#How to compute residuals
#names(l)check all names in our model, one way(l$residuals)
#2) res=(Y-predict(Yhat))

res=l$residuals
res=data$Y-predict(l)
#we look gaussanity model using qqnorm or plot linear model 
qqnorm(res)# look our qq plot x-axis is standared gaussian(SG) but Y not SG
#check vaiance of noise 1) look on resid stand error 2) var(res)
var(res)
s2=(nrow(data)-1)/(nrow(data)-2)*var(res)
# qq norm looks like a gaussian so our variance same for all our individuals
# check homoscedasticity and estimation of noise is equal to zero or not
plot(l)
boxplot(data$X)
boxplot(data$Y)
hist(res,freq = FALSE)
# we can do shapiro test or Kolmogorov-Smirnov test to check gaussanity
# Kolmogorov-Smirnov test
ks.test(res,pnorm)# comparision with SG but my residuals are not SG
# estimate vairnce of noise
var(res)#not good because perform on one variable not on residu come from LM
#To do correct estimattion n-1/n-2*variance of residuals
s2=(nrow(data)-1)/(nrow(data)-2)*var(res)
# perfom correct test
shapiro.test(res)
ks.test(res,pnorm,0,sqrt(s2))#Ho=gaussian H1=Not gaussian
#P-Value is very so big so don't reject Ho
#look fisher test if we're in simle linear model F-Statistic or below on both are same
#Coefficients:
#               Estimate  Std. Error  t value   Pr(>|t|)    
#(Intercept) 
#  X            1.250e-01  5.697e-05  2193.8   <2e-16 ***

#P-Value very small on F-test so reject Ho


# Perform Data1 initial notes are same as data 0
dir()
d1=read.table('data1.txt')
head(d1)
colnames(d1)=c('Y','X')
head(d1)
plot(d1$X,d1$Y)
hist(as.matrix(d1),freq = FALSE)
summary(d1)
l1=lm(Y~X,data=d1)
summary(l1)
abline(l1,col='red')
res1=d1$Y-predict(l1)
qqnorm(res1)# look at qq plot it seems to be there is no guasanity
# to recheck we should perform KS test or Shapiro test
shapiro.test(res1)# using shapiro test
# look at assumptions of homoscedasticity
plot(l1)
l1b=lm(Y[-70]~X[-70],data=d1)#remove outlier
plot(l1b)

head(predict(l1,interval = c('confidence')))# exp Y
head(predict(l1,interval = c('prediction')))# Y
# as we know that CI of Y is  bigger than exp Y
# but in this problem we shuld not consider these because we're not under gaussian assumption
hist(res1,freq = FALSE)


# data2
dir()
d2=read.table('data2.txt')
head(d2)
colnames(d2)=c('Y','X')
head(d2)
plot(d2$X,d2$Y)# it seems to be not linear, more like exponential distribution
#So we need to do transformation of Y random varible
X=d2$X
Z=log(d2$Y)#Tranformation of Y
plot(X,Z)
l2=lm(Z~X)
summary(l2)
#residuals seem to be symetric
#M-R squared is close to 1 so it seems to be gaussian but we should perform tests to confirm
res2=l2$residuals
shapiro.test(l2$residuals)
qqnorm(res2)
#last thing check with homoscedasticity
plot(l2)#when we draw plot it clearly shows problem with homoscedasticity
#we should perform clustering
A=cbind(X,Z)#to combine 
D=dist(A)#create a matrix distance between two individuals
H=hclust(D)#perform H-clust
plot(H)
#K-clust
K=kmeans(A,2)
a=which(K$cluster==1)
b=which(K$cluster==2)
A1=A[a,]
A2=A[b,]
L1=lm(A1[,1]~A1[,2])
L2=lm(A2[,1]~A2[,2])
plot(L1)
plot(L2)

#Multiple LinearRegression
dir()
d3=read.table('data3.txt')
head(d3)
colnames(d3)=c('Y',paste('X',1:6,sep = ''))#assigne values
head(d3)
cor(d3)#look correlation b/w variables
l3=lm(Y~.,data=d3)
res3=l3$residuals
qqnorm(res3)
plot(l3)
shapiro.test(res3)
summary(l3)
#Variable Selection
#Step by Step procedure and Penalised criterion,Exausitive but i take Pena
#Penalised criterion we have Lasso and Ridge Regression
#library(glmnet)
#using lasso 
las=glmnet(as.matrix(d3[,-1]),d3$Y)#compute lasso
plot(las)#all the time only we have 3 
#use cross-validation
cvlas=cv.glmnet(as.matrix(d3[,-1]),d3$Y)#compute cross-validation
names(cvlas)
#we have lambda and lambda.1se(small value and 1 sd)
lam=cvlas$lambda.1se# min cv value + 1sd
las1=glmnet(as.matrix(d3[,-1]),d3$Y,lambda = lam)
# just have one model with 3 varibles
names(las1)
las1$a0
las1$beta#select variables

d3b=d3[,-c(4,5,7)]
head(d3b)
lb=lm(Y~.,data=d3b)
# Matricial writing computational of transpose of XX
XX=t(as.matrix(d3b[,-1]))%*%as.matrix(d3b[,-1])

# compute eigen vales and eigen vectors assosiated to matrix XX
E=eigen(XX)
E$values

lb=lm(Y~X1+X2+X5-1,data=d3b)#(-1)means model without intercept
#always better to consider intercept otherwise we may have problem while perfroming tests


#perform several models on Ozone Dataset and select best one
dir()
oz=read.table('Dataset_ozone.txt',header = TRUE,sep = ';',dec = ',')
head(oz)
#Apply decesion tree model
#library(rpart)
ozs=oz[,-c(1)]#we can remove observation's id no use at all
ds=rpart(maxO3~.,data=oz)
#Apply maximal tree frist step
tm=rpart(maxO3~.,data=oz,control = rpart.control(minsplit = 2,cp=10^(-9)))
#apply second step
printcp(tm)
table=printcp(tm)#create an object
cver=table[,4]#extracting cross-validation error on purning step
#to see smaller value
a=which(cver==min(cver))#check smallest cross-validation error
#new threesold
nt=table[a,4]+table[a,5]#new threeshold for the 1se rule
min(cver)
#check cross-validation error is smaller than new threeshold
r=(cver<=nt)
rs=which(r==1)
s=rs[1]
cp=table[s,1]#the value of cp for which we need to prune
#Finale tree
Tf=prune(tm,cp=cp)
Tf
plot(Tf)
text(Tf)
summary(Tf)#T12 T15 T9 are importance variable


#LinearModel
head(oz)
dim(oz)
d=oz[,-c(12,13)]#remove categorical varible
head(d)
l=lm(maxO3~.,data = d)
summary(l)
res=l$residuals
qqnorm(res)
shapiro.test(res)#not really gaussian
cor(d)
laso=cv.glmnet(as.matrix(d[,-1]),d$maxO3)
names(laso)
lasf=las=glmnet(as.matrix(d[,-1]),d$maxO3,lambda=las$lambda.1se)
names(lasf)
lasf$a0
lasf$beta
#Final model
L=lm(maxO3~T12+T15+Ne9+Vx9+maxO3v,data = d)
summary(L)
#Comparison of two models
#Split the data into learning and validation error
n=nrow(d)
m=75
u=sample(1:n,75,replace = FALSE)
ozl=oz[u,]
ozv=oz[-u,]
tm=rpart(maxO3~.,data=ozl,control = rpart.control(minsplit = 2,cp=10^(-9)))
printcp(tm)
table=printcp(tm)
cver=table[,4]
a=which(cver==min(cver))
nt=table[a,4]+table[a,5]#new threeshold for the 1se rule
min(cver)
#check cross-validation error is smaller than new threeshold
r=(cver<=nt)
rs=which(r==1)
s=rs[1]
cp=table[s,1]#the value of cp for which we need to prune
#Finale tree
Tf=prune(tm,cp=cp)
Tf
ozl=oz[,-c(12,13)]#remove categorical varible
head(ozl)
l=lm(maxO3~.,data = ozl)
shapiro.test(l$residuals)
laso=cv.glmnet(as.matrix(ozl[,-1]),ozl$maxO3)
names(laso)
lasf=las=glmnet(as.matrix(ozl[,-1]),ozl$maxO3,lambda=las$lambda.1se)
names(lasf)
lasf$a0
lasf$beta
L=lm(maxO3~T12+T15+Ne9+Ne12+Vx9+Vx12+Vx15+maxO3v,data = ozl)
#computation of test error/validation
pc=predict(Tf,newdata = ozv[,-1])
pr=predict(L,newdata =ozv[,-c(1,13,14)])
pc==pr
errc=1/nrow(ozv)*sum((ozv[,1]-pc)^2)
errr=1/nrow(ozv)*sum((ozv[,1]-pr)^2)

#Model based on RandomForrest
#library(randomForest)
rf=randomForest(maxO3~.,data =ozl)
prf=predict(rf,newdata = ozv)
errrf=1/nrow(ozv)*sum((ozv[,1]-prf)^2)

library(VSURF)
rfs=VSURF(ozl[,-1],ozl[,1])
rfs
rfs$varselect.thres
dim(ozl[,-1])
si=rfs$varselect.interp
sp=rfs$varselect.pred
ozlb=ozl[,-1]
ozlbi=ozlb[,si]
ozlbp=ozlb[,sp]
ozli=cbind(ozl[,1],ozlbi)
tci=rpart(maxO3~.,ozli,minsplit=2,cp=10^(-9))
ozlp=cbind(ozl[,1],ozlbp)
tcp=rpart(maxO3~.,ozlp,minsplit=2,cp=10^(-9))
printcp(tci)
table=printcp(tci)
cver=table[,4]
a=which(cver==min(cver))
nt=table[a,4]+table[a,5]#new threeshold for the 1se rule
min(cver)
#check cross-validation error is smaller than new threeshold
r=(cver<=nt)
rs=which(r==1)
s=rs[1]
cp=table[s,1]#the value of cp for which we need to prune
#Finale tree
Tf=prune(tm,cp=cp)
cp==cp
