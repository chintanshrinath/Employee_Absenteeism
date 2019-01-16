rm(list = ls())
#For project, set working directory
setwd("E:/Subject/edwisor/project 3")

#Load required lib for project
x = c("ggplot2", "corrgram", "DMwR", "caret", "randomForest", "e1071",
      "DataCombine", "pROC")

library(utils)
install.packages("Rcpp")
library(readxl)
install.packages("readxl")

library(tibble)


#load required packages
lapply(x, require, character.only = TRUE)

# To read data from file, we have used readxl package
df = read_excel("Absenteeism_at_work_Project.xls")
#description of data
str(df)


#Renaming columns
colnames(df) = c("ID","Reason.for.absence","Month.of.absence","Day.of.the.week","Seasons","Transportation.expense"
                               ,"Distance.from.Residence.to.Work","Service.time","Age","Work.load.Average.day","Hit.target","Disciplinary.failure"
                               ,"Education","Son","Social.drinker","Social.smoker","Pet","Weight","Height","Body.mass.index","Absenteeism.time.in.hours")

#Analysis of missing values
missing_value = (as.data.frame(colSums(is.na(df)))*100/nrow(df))
colnames(missing_value) <- c("Missng Value Percentage")


#Reason.for.absence Vs. Absenteeism.time.in.hours -- box plot
ggplot(df,aes_string(y=df$Absenteeism.time.in.hours,x=as.factor(df$Reason.for.absence)))+geom_boxplot()+xlab('Reason.for.absence')+ylab('Absenteeism.time.in.hours')

#Impute the values in dataset columns

df$Reason.for.absence[is.na(df$Reason.for.absence)] = 27
#Zero category of 'Reason for absence' value has equal to 26
df$Reason.for.absence[df$Reason.for.absence==0] = 26
#Putting Month.of.absence null value equal to 10.
df$Month.of.absence[is.na(df$Month.of.absence)] = 10
#Finding ID column values for which there are missing values in Transportation.expense
df$ID[is.na(df$Transportation.expense)]

for (i in c(1,3,10,15,20,22)){
  df$Transportation.expense[is.na(df$Transportation.expense) & df$ID==i] = mean(df$Transportation.expense[df$ID==i],na.rm = T)
}

#to find rows for missing values
df$ID[is.na(df$Distance.from.Residence.to.Work)]
#As a return, we got 34,22 abd 28
for (i in c(34,22,28)){
    df$Distance.from.Residence.to.Work[is.na(df$Distance.from.Residence.to.Work) & df$ID==i] = mean(df$Distance.from.Residence.to.Work[df$ID==i],na.rm = T)
}
#Find missing values value in ID column
df$ID[is.na(df$Service.time)]
# As return we got 28, 34
for (i in c(34,28)){
  df$Service.time[is.na(df$Service.time) & df$ID==i] = mean(df$Service.time[df$ID==i],na.rm = T)
}
#Find missing values value in age
df$ID[is.na(df$Age)]
# [1] 28 24 24
for (i in c(24,28)){
  df$Age[is.na(df$Age) & df$ID==i] = mean(df$Age[df$ID==i],na.rm = T)
}

#Converting into numeric values
df$Work.load.Average.day = as.numeric(df$Work.load.Average.day)
#Work.load.Average.day missing values are imputed using Month.of.absence and Hit.target
df$Month.of.absence[is.na(df$Work.load.Average.day)]
df$Hit.target[is.na(df$Work.load.Average.day)]


df = data.frame(m=c(9,10,11,11,12,12,1,1,1,5),h=c(92,93,93,93,97,97,95,95,95,92))
for (i in 1:10){
  df$Work.load.Average.day[(is.na(df$Work.load.Average.day) & 
                                            df$Month.of.absence==df[i,1]) & df$Hit.target==df[i,2]] = 
    mean(df$Work.load.Average.day[df$Month.of.absence==df[i,1] & df$Hit.target==df[i,2]],na.rm = T)
}

#Impute value of hit target by month of absence and work load average day
df$Month.of.absence[is.na(df$Hit.target)]

df$Work.load.Average.day[is.na(df$Hit.target)]

final_data = data.frame(m1=c(11,12,1),w1=c(306345,261306,308593))
for (i in 1:3){
  df$Hit.target[(is.na(df$Hit.target) & df$Month.of.absence==final_data[i,1]) & df$Work.load.Average.day==final_data[i,2]] = mean(df$Hit.target[df$Month.of.absence==final_data[i,1] & df$Work.load.Average.day==final_data[i,2]],na.rm = T)
}
df$Disciplinary.failure[is.na(df$Disciplinary.failure)] = 0
#For education missing value for impute, we used id value again
df$ID[is.na(df$Education)]
# [1] 11 10 34 34 14 34 34 34 10 24
for (i in c(10,11,14,24,34)){
  df$Education[is.na(df$Education) & df$ID==i] = mean(df$Education[df$ID==i],na.rm=T)
}
#For son missing value, use id value
df$ID[is.na(df$Son)]
for (i in c(1,14,20,27,34)){
  df$Son[is.na(df$Son) & df$ID==i] = mean(df$Son[df$ID==i],na.rm=T)
}
df$ID[is.na(df$Social.drinker)]
for (i in c(10,14,17)){
  df$Social.drinker[is.na(df$Social.drinker) & df$ID==i] = mean(df$Social.drinker[df$ID==i],na.rm=T)
}
#For social smoker the id column has used
df$ID[is.na(df$Social.smoker)]
for (i in c(34,1,11,15)){
  df$Social.smoker[is.na(df$Social.smoker) & df$ID==i] = mean(df$Social.smoker[df$ID==i],na.rm=T)
}
#For fill pet missing value id column has used
df$ID[is.na(df$Pet)]
for (i in c(1,13)){
  df$Pet[is.na(df$Pet) & df$ID==i] = mean(df$Pet[df$ID==i],na.rm=T)
}
#For weight missing value, id has used
df$ID[is.na(df$Weight)]
for (i in c(27)){
  df$Weight[is.na(df$Weight) & df$ID==i] = mean(df$Weight[df$ID==i],na.rm=T)
}
df$ID[is.na(df$Height)]
for (i in c(20,10,28,34,27,11,5,22,13,24,32)){
  df$Height[is.na(df$Height) & df$ID==i] = mean(df$Height[df$ID==i],na.rm=T)
}
df$ID[is.na(df$Body.mass.index)]
for (i in c(3,24,11,30,2,19,34,28,13,36,14,20,18,17,15,22,5)){
  df$Body.mass.index[is.na(df$Body.mass.index) & df$ID==i] = mean(df$Body.mass.index[df$ID==i],na.rm=T)
}
df$Reason.for.absence[is.na(df$Absenteeism.time.in.hours)]
for (i in c(23,14,10,22,26,6,28,11,13)){
  df$Absenteeism.time.in.hours[is.na(df$Absenteeism.time.in.hours) & df$Reason.for.absence==i] = mean(df$Absenteeism.time.in.hours[df$Reason.for.absence==i],na.rm=T)
}

# Distribution for Continuous Variables
#First ,Transportation expense 
hist(df$Transportation.expense,prob = TRUE,xlab = 'Transportation.expense')
lines(density(df$Transportation.expense))
# second, Distance from Residence to Work
hist(df$Distance.from.Residence.to.Work,prob = TRUE,xlab = 'Distance.from.Residence.to.Work')
lines(density(df$Distance.from.Residence.to.Work))
#third, Service time
hist(df$Service.time,prob = TRUE,xlab = 'Service.time')
lines(density(df$Service.time))
#Age
hist(df$Age,prob = TRUE,xlab = 'Age')
lines(density(df$Age))
#For, Work load Average day
hist(df$Work.load.Average.day,prob = TRUE,xlab = 'Work.load.Average.day')
lines(density(df$Work.load.Average.day))
#next, Hit target
hist(df$Hit.target,prob = TRUE,xlab = 'Hit.target')
lines(density(df$Hit.target))
#Also,Weight
hist(df$Weight,prob = TRUE,xlab = 'Weight')
lines(density(df$Weight))
#Height
hist(df$Height,prob = TRUE,xlab = 'Height')
lines(density(df$Height))
#Body.mass.index
hist(df$Body.mass.index,prob = TRUE,xlab = 'Body.mass.index')
lines(density(df$Body.mass.index))


#Independent variable's Outlier analsis
num_col =c('Weight', 'Height', 'Body.mass.index','Absenteeism.time.in.hours','Transportation.expense',
           'Distance.from.Residence.to.Work', 'Service.time', 'Age','Hit.target','Work.load.Average.day')
cat_col = c('')
for (i in 1:length(num_col))
{
  assign(paste0("gn",i),ggplot(aes_string(y = (num_col[i]), x = 'Absenteeism.time.in.hours'),data = df) +
           stat_boxplot(geom = "errorbar", width = 0.5) +geom_boxplot(outlier.colour="blue", fill = "skyblue",
                                                                      outlier.shape=18,outlier.size=1, notch=FALSE) +labs(y=num_col[i],x="Absenteeism in Hours")+
           ggtitle(paste("Box plot of responded for",num_col[i])))
}

# Plotting
gridExtra::grid.arrange(gn1,gn2,gn3,ncol=3)
gridExtra::grid.arrange(gn4,gn5,gn6,ncol=3)
gridExtra::grid.arrange(gn7,gn8,gn9,ncol=3)


#converting into a dataframe 
df = as.data.frame(df)
for (i in c('Transportation.expense','Service.time','Age','Work.load.Average.day','Hit.target','Height','Absenteeism.time.in.hours')){
  qnt = quantile(df[,i], probs=c(.25, .75), na.rm = T)
  iqr1 = qnt[2]-qnt[1]
  min1 = qnt[1]-1.5*iqr1
  max1 = qnt[2]+1.5*iqr1
  df[,i][df[,i]<min1] = min1
  df[,i][df[,i]>max1] = max1
}
#Correlation Analysis of variables and Convert categorical into catcols
catcols = c('Reason.for.absence','Month.of.absence','Day.of.the.week','Seasons','Disciplinary.failure','Education','Son','Social.drinker','Social.smoker','Pet')

for (i in catcols){
  df[,i] = as.factor(df[,i])
}
str(df)
#Measur of  Chi-square test
pval = c()

#Calculating & storing p-values in vector pval from chisquare test

for(i in catcols){
  for(j in catcols){
    chi2 = chisq.test(df[,i],df[,j])
    pval = c(pval,chi2$p.value)
  }
}

length(pval)#100

#convertiino matrix m1

m1 = matrix(pval,ncol=10)

#Convert m1 to dataframe chi_df
chi_df = data.frame(m1)
#catcols to row names
row.names(chi_df) = catcols
#catcols - column names
colnames(chi_df) = catcols

#As per values <0.05. Many column doesn't fit into the criteria. SO all will drop out from that

df[,c('Month.of.absence','Seasons','Disciplinary.failure','Education','Son','Social.drinker','Social.smoker','Pet')] = list(NULL)

#continuous independent variables correlation
cor(df[,4:13])

#As per values, there is no relation between independent and dependent variable(i<0.95 and d ,0.2)
#Therefore, aggreate dependent variablen and reason for absence
Reasons = aggregate(df$Absenteeism.time.in.hours, by=list(Category=df$Reason.for.absence), FUN=sum)


Reasons$Absence = (Reasons$x/sum(df$Absenteeism.time.in.hours))*100
Reasons = Reasons[order(Reasons$Absence),]
Reasons

barplot(Reasons$Absence,names.arg=Reasons$Category,xlab="Reason.for.absence",ylab="Absence",col="blue")

#Again read data file
final_data = read_excel('Absenteeism.xls')
#Same, rename to column names
colnames(final_data) = c("ID","Reason.for.absence","Month.of.absence","Day.of.the.week","Seasons","Transportation.expense"
                                ,"Distance.from.Residence.to.Work","Service.time","Age","Work.load.Average.day","Hit.target","Disciplinary.failure"
                                ,"Education","Son","Social.drinker","Social.smoker","Pet","Weight","Height","Body.mass.index","Absenteeism.time.in.hours")

#Impute missing values as required
final_data$Month.of.absence[is.na(final_data$Month.of.absence)] = 10
final_data$Reason.for.absence[is.na(final_data$Reason.for.absence)] = 27
final_data$Reason.for.absence[final_data$Reason.for.absence==0] = 26

for (i in c(23,14,10,22,26,6,28,11,13)){
  final_data$Absenteeism.time.in.hours[is.na(final_data$Absenteeism.time.in.hours) & final_data$Reason.for.absence==i] = median(final_data$Absenteeism.time.in.hours[final_data$Reason.for.absence==i],na.rm=T)
}

#Converting Month.of.absence to factor
final_data$Month.of.absence = as.factor(final_data$Month.of.absence)
#Making a timeseries aggregating Absenteeism.time.in.hours by Month.of.absence
monthly_absence = aggregate(final_data$Absenteeism.time.in.hours,by=list(Category=final_data$Month.of.absence),FUN=sum)
monthly_absence = monthly_absence[2:13,]
monthly_absence

#Calculating dfeeism time as percent of total time in column dfhours
monthly_absence$dfhours = monthly_absence$x/3
row.names(monthly_absence) = monthly_absence$Category
monthly_absence

# Modelling time series using arima
tsdata = ts(monthly_absence$dfhours)
class(tsdata)

#Display timeseries data
plot(tsdata)

#Checking stationarity - Augmented Dickey-Fuller Test
library(tseries)
adf.test(tsdata, alternative="stationary", k=0)

# Augmented Dickey-Fuller Test
#Subtract shifted time series from original time series.
tsdata2 = tsdata - stats::lag((tsdata),1)
plot(tsdata2)
#Dickey-Fuller Test again
adf.test(tsdata2, alternative="stationary", k=0)
#plot for ACF
acf(tsdata2)
#plot for PACF 
pacf(tsdata2)
library(forecast)
model = arima(tsdata2,c(4,0,9))
fit1 = fitted(model)
residuals1 = tsdata2 - fit1
sum(residuals1**2)

plot(tsdata2)
lines(fit1)
df2011 = predict(model,n.ahead = 12)

#Scaling df2011 back to original
absence_2011 = cumsum(df2011$pred)
absence_2011_2 = absence_2011 + rep(tsdata[4],12)
as.data.frame(absence_2011_2)
ts_2011 = ts(absence_2011_2)
final_data = as.data.frame(absence_2011_2)
row.names(final_data) = c(13:24)
ts_2011 = ts(final_data$absence_2011_2,start=13)

#Display original timeseries & predicted values

plot(tsdata,xlim=c(1,24))
lines(ts_2011)
