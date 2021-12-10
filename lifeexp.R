
## THE OBJECTIVE HERE IS TO PREDICT THE AVERAGE LIFE EXPECTANCY OF PEOPLE FOR THE UPCOMING YEAR

## HYPOTHESIS:We are trying to predict the future Life_Expectancy age and also the variables that effect it and how they help in the prediction


## Our Target variable is a continuous  variable and we are trying to predict the future value of the target variable  thus  we are performing  linear regression

mydata<-read.csv(choose.files())## importing the data set
dim(mydata)#2938 rows   23 columns
class(mydata)#data.frame
colnames(mydata)
#"Country"                         "Year"                           
# "Status"                          "Life_Expectancy"                
# "Adult_Mortality"                 "Infant_Deaths"                  
# "Alcohol"                         "Percentage_Expenditure"         
# "Hepatitis_B"                     "Measles"                        
# "BMI"                             "Under.five_Deaths"              
# "Polio"                           "Total_Expenditure"              
# "Diphtheria"                      "HIV.AIDS"                       
# "GDP"                             "Per_Capita_GDP"                 
# "Population"                      "Thinness_1.19_Years"            
# "Thinness_5.9_Years"              "Income_Composition_of_Resources"
# "Schooling"

## OUT OF THE 23 COLUMNS Life_Expectancy IS OUR TARGET VARIABLE

str(mydata)
# Country                        : chr  "Afghanistan" "Afghanistan" "Afghanistan" "Afghanistan" ...
# Year                           : int  2015 2014 2013 2012 2011 2010 2009 2008 2007 2006 ...
#Status                         : chr  "Developing" "Developing" "Developing" "Developing" ...
#Life_Expectancy                : num  65 59.9 59.9 59.5 59.2 58.8 58.6 58.1 57.5 57.3 ...
# Adult_Mortality                : int  263 271 268 272 275 279 281 287 295 295 ...
# Infant_Deaths                  : int  62 64 66 69 71 74 77 80 82 84 ...
# Alcohol                        : num  0.01 0.01 0.01 0.01 0.01 0.01 0.01 0.03 0.02 0.03 ...
# Percentage_Expenditure         : num  71.3 73.5 73.2 78.2 7.1 ...
# Hepatitis_B                    : int  65 62 64 67 68 66 63 64 63 64 ...
# Measles                        : int  1154 492 430 2787 3013 1989 2861 1599 1141 1990 ...
# BMI                            : num  19.1 18.6 18.1 17.6 17.2 16.7 16.2 15.7 15.2 14.7 ...
# Under.five_Deaths              : int  83 86 89 93 97 102 106 110 113 116 ...
# Polio                          : int  6 58 62 67 68 66 63 64 63 58 ...
# Total_Expenditure              : num  8.16 8.18 8.13 8.52 7.87 9.2 9.42 8.33 6.73 7.43 ...
# Diphtheria                     : int  65 62 64 67 68 66 63 64 63 58 ...
# HIV.AIDS                       : num  0.1 0.1 0.1 0.1 0.1 0.1 0.1 0.1 0.1 0.1 ...
# GDP                            : num  1.99e+10 2.05e+10 2.06e+10 2.00e+10 1.78e+10 ...
# Per_Capita_GDP                 : num  578 614 637 642 591 ...
# Population                     : int  34413603 33370794 32269589 31161376 30117413 29185507 28394813 27722276 27100536 26433049 ...
# Thinness_1.19_Years            : num  17.2 17.5 17.7 17.9 18.2 18.4 18.6 18.8 19 19.2 ...
# Thinness_5.9_Years             : num  17.3 17.5 17.7 18 18.2 18.4 18.7 18.9 19.1 19.3 ...
# Income_Composition_of_Resources: num  0.479 0.476 0.47 0.463 0.454 0.448 0.434 0.433 0.415 0.405 ...
# Schooling                      : num  10.1 10 9.9 9.8 9.5 9.2 8.9 8.7 8.4 8.1 ...

#Country,Year,Status,Schooling doesn't help in deriving the Life_expectancy value thus we remove them
##except for Country and Status all the other variables are continuous



## Removing the useless Columns(Country,Year,Status,Schooling)
mydata <-  mydata[, -c(1,2,3,23)]
head(mydata)
#Life_Expectancy Adult_Mortality Infant_Deaths Alcohol Percentage_Expenditure
#Hepatitis_B Measles  BMI Under.five_Deaths Polio Total_Expenditure Diphtheria
#HIV.AIDS         GDP Per_Capita_GDP Population Thinness_1.19_Years
#Thinness_5.9_Years Income_Composition_of_Resources

## All the variables are in proper form no onversion needed


##Step 4:

##Checking and treating missing values
colSums(is.na(mydata))
#Life_Expectancy             Adult_Mortality 
#10                              10 
#Infant_Deaths                Alcohol 
#0                             194 
#Percentage_Expenditure       Hepatitis_B 
#0                             553 
#Measles                        BMI 
#0                              34 
#Under.five_De                 Polio 
#0                              19 
#Total_Expenditure            Diphtheria 
#226                              19 
#HIV.AIDS                      GDP 
#0                              62 
#Per_Capita_GDP                Population 
#62                               6 
#Thinness_1.19_Years           Thinness_5.9_Years 
#34                              34 
#Income_Composition_of_Resources 
#167 


#let find how much percent data are missing in these columns
colSums(is.na(mydata))/nrow(mydata)

## We can see teh columns Life_Expectancy ,Adult_Mortality,Income_Composition_of_Resources,Thinness_1.19_Years,
#Thinness_5.9_Years,Per_Capita_GDP,Population,GDP,Total_Expenditure, Diphtheria,Polio,BMI, Hepatitis_B,Alcohol  
## Contains Missing Value

## Missing Value Treatment

boxplot(mydata$Life_Expectancy)##outliers present
median(mydata$Life_Expectancy,na.rm=T)##72.1
mydata$Life_Expectancy[which(is.na(mydata$Life_Expectancy))] <-72.1

boxplot(mydata$Adult_Mortality)##outliers present
median(mydata$Adult_Mortality,na.rm=T)##144
mydata$Adult_Mortality[which(is.na(mydata$Adult_Mortality))] <-144

boxplot(mydata$Alcohol)##no outliers
mean(mydata$Alcohol,na.rm=T)##4.602861
mydata$Alcohol[which(is.na(mydata$Alcohol))] <-4.602861

boxplot(mydata$Hepatitis_B)#outliers present
median(mydata$Hepatitis_B,na.rm=T)## 92
mydata$Hepatitis_B[which(is.na(mydata$Hepatitis_B))] <- 92

boxplot(mydata$BMI)#no outliers
mean(mydata$BMI,na.rm=T)#38.32125
mydata$BMI[which(is.na(mydata$BMI))] <-38.32125

boxplot(mydata$Polio)## outliers present
median(mydata$Polio,na.rm=T)##93
mydata$Polio[which(is.na(mydata$Polio))] <-93

boxplot(mydata$Total_Expenditure)## outliers present
median(mydata$Total_Expenditure,na.rm=T)##5.755
mydata$Total_Expenditure[which(is.na(mydata$Total_Expenditure))] <-5.755

boxplot(mydata$Diphtheria)## outliers present
median(mydata$Diphtheria,na.rm=T)##93
mydata$Diphtheria[which(is.na(mydata$Diphtheria))] <-93

boxplot(mydata$GDP)#outliers present
median(mydata$GDP,na.rm=T)##21122408668
mydata$GDP[which(is.na(mydata$GDP))] <-21122408668

boxplot(mydata$Per_Capita_GDP)## outliers present
median(mydata$Per_Capita_GDP,na.rm=T)##3738.52
mydata$Per_Capita_GDP[which(is.na(mydata$Per_Capita_GDP))] <-3738.52

boxplot(mydata$Population)#outliers present
median(mydata$Population,na.rm=T)##8171061
mydata$Population[which(is.na(mydata$Population))] <-8171061

boxplot(mydata$Thinness_1.19_Years)## outliers present
median(mydata$Thinness_1.19_Years,na.rm=T)## 3.3
mydata$Thinness_1.19_Years[which(is.na(mydata$Thinness_1.19_Years))] <- 3.3

boxplot(mydata$Thinness_5.9_Years)##outliers present
median(mydata$Thinness_5.9_Years,na.rm=T)##3.3
mydata$Thinness_5.9_Years[which(is.na(mydata$Thinness_5.9_Years))] <-3.3

boxplot(mydata$Income_Composition_of_Resources)## no outliers
mean(mydata$Income_Composition_of_Resources,na.rm=T)##0.6275511
mydata$Income_Composition_of_Resources[which(is.na(mydata$Income_Composition_of_Resources))] <-0.6275511


colSums(is.na(mydata))
#Life_Expectancy                 Adult_Mortality 
#0                               0 
#Infant_Deaths                         Alcohol 
#0                               0 
#Percentage_Expenditure                     Hepatitis_B 
#0                               0 
#Measles                             BMI 
#0                               0 
#Under.five_Deaths                           Polio 
#0                               0 
#Total_Expenditure                      Diphtheria 
#0                               0 
#HIV.AIDS                             GDP 
#0                               0 
#Per_Capita_GDP                      Population 
#0                               0 
#Thinness_1.19_Years              Thinness_5.9_Years 
#0                               0 
#Income_Composition_of_Resources 
#0 

##Missing Value treatment done.No more missing data


## Outliers Treatment
boxplot(mydata$Life_Expectancy)## Negative Outliers present
quantile(mydata$Life_Expectancy,seq(0,1,0.02))
mydata$Life_Expectancy <- ifelse(mydata$Life_Expectancy<49.800,49.800,mydata$Life_Expectancy)
boxplot(mydata$Life_Expectancy)

boxplot(mydata$Adult_Mortality)## positive outliers present
quantile(mydata$Adult_Mortality,seq(0,1,0.02))
mydata$Adult_Mortality <- ifelse(mydata$Adult_Mortality>382.00,382.00,mydata$Adult_Mortality)
boxplot(mydata$Adult_Mortality)

boxplot(mydata$Infant_Deaths)##outliers present
quantile(mydata$Infant_Deaths,seq(0,1,0.02))
mydata$Infant_Deaths <- ifelse(mydata$Infant_Deaths>52.00,52.00,mydata$Infant_Deaths)
boxplot(mydata$Infant_Deaths)

boxplot(mydata$Alcohol)#outliers  present
quantile(mydata$Alcohol,seq(0,1,0.02))
mydata$Alcohol <- ifelse(mydata$Alcohol>13.015200,13.015200,mydata$Alcohol)
boxplot(mydata$Alcohol)

boxplot(mydata$Percentage_Expenditure)##outliers present
quantile(mydata$Percentage_Expenditure,seq(0,1,0.02))
mydata$Percentage_Expenditure <- ifelse(mydata$Percentage_Expenditure>1002.646624,1002.646624,mydata$Percentage_Expenditure)
boxplot(mydata$Percentage_Expenditure)

boxplot(mydata$Hepatitis_B)##outliers present
quantile(mydata$Hepatitis_B,seq(0,1,0.02))
mydata$Hepatitis_B <- ifelse(mydata$Hepatitis_B<63.00,63.00,mydata$Hepatitis_B)
boxplot(mydata$Hepatitis_B)

boxplot(mydata$Measles)##outliers present
quantile(mydata$Measles,seq(0,1,0.02))
mydata$Measles <- ifelse(mydata$Measles>740.60,740.60,mydata$Measles)
boxplot(mydata$Measles)

boxplot(mydata$BMI)#not present

boxplot(mydata$Under.five_Deaths)##outliers present
quantile(mydata$Under.five_Deaths,seq(0,1,0.02))
mydata$Under.five_Deaths<- ifelse(mydata$Under.five_Deaths>65.82 ,65.82 ,mydata$Under.five_Deaths)
boxplot(mydata$Under.five_Deaths)

boxplot(mydata$Polio)##outliers present
quantile(mydata$Polio,seq(0,1,0.02))
mydata$Polio<- ifelse(mydata$Polio<50,50,mydata$Polio)
boxplot(mydata$Polio)

boxplot(mydata$Total_Expenditure)##outliers present
quantile(mydata$Total_Expenditure,seq(0,1,0.02))
mydata$Total_Expenditure<- ifelse(mydata$Total_Expenditure>11.6156,11.6156,mydata$Total_Expenditure)
boxplot(mydata$Total_Expenditure)

boxplot(mydata$Diphtheria)##outliers present
quantile(mydata$Diphtheria,seq(0,1,0.02))
mydata$Diphtheria<- ifelse(mydata$Diphtheria<50,50,mydata$Diphtheria)
boxplot(mydata$Diphtheria)

boxplot(mydata$HIV.AIDS)##outliers present
quantile(mydata$HIV.AIDS,seq(0,1,0.02))
mydata$HIV.AIDS<- ifelse(mydata$HIV.AIDS>1.50,1.500 ,mydata$HIV.AIDS)
boxplot(mydata$HIV.AIDS)

boxplot(mydata$GDP)##outliers present
quantile(mydata$GDP,seq(0,1,0.02))
mydata$GDP<- ifelse(mydata$GDP>2.865796e+11,2.865796e+11,mydata$GDP)
boxplot(mydata$GDP)

boxplot(mydata$Per_Capita_GDP)##outliers present
quantile(mydata$Per_Capita_GDP,seq(0,1,0.02))
mydata$Per_Capita_GDP<- ifelse(mydata$Per_Capita_GDP> 27480.1612, 27480.1612,mydata$Per_Capita_GDP)
boxplot(mydata$Per_Capita_GDP)

boxplot(mydata$Population)##outliers present
quantile(mydata$Population,seq(0,1,0.02))
mydata$Population<- ifelse(mydata$Population>4.779301e+07,4.779301e+07,mydata$Population)
boxplot(mydata$Population)

boxplot(mydata$Thinness_1.19_Years)##outliers present
quantile(mydata$Thinness_1.19_Years,seq(0,1,0.02))
mydata$Thinness_1.19_Years<- ifelse(mydata$Thinness_1.19_Years> 14.600,14.600,mydata$Thinness_1.19_Years)
boxplot(mydata$Thinness_1.19_Years)

boxplot(mydata$Thinness_5.9_Years)##outliers present
quantile(mydata$Thinness_5.9_Years,seq(0,1,0.02))
mydata$Thinness_5.9_Years<- ifelse(mydata$Thinness_5.9_Years>15.000,15.000,mydata$Thinness_5.9_Years)
boxplot(mydata$Thinness_5.9_Years)

boxplot(mydata$Income_Composition_of_Resources)#not present




###UNIVARIATE AND BIVARIATE ANALYSIS

# Strength of Relationship between predictor and target variable
# Continuous Vs Continuous ---- Correlation test
#correlation for checking continous variable
# Correlation for multiple columns at once

colnames(mydata)
ContinuousCols = c("Life_Expectancy","Adult_Mortality","Infant_Deaths","Alcohol","Percentage_Expenditure","Hepatitis_B","Measles","BMI" ,"Under.five_Deaths","Polio","Total_Expenditure","Diphtheria","HIV.AIDS","GDP","Per_Capita_GDP","Population","Thinness_1.19_Years","Thinness_5.9_Years","Income_Composition_of_Resources")
cor(mydata[, ContinuousCols], use = "complete.obs")
CorrData=cor(mydata[, ContinuousCols], use = "complete.obs")
CorrData

abs(CorrData["Life_Expectancy",])>0.5

names(CorrData["Life_Expectancy",])
names(CorrData["Life_Expectancy",][abs(CorrData["Life_Expectancy",])>0.5])
# as here we have fixed the thresold value >0.5
#The Good PREDICTORS:
# "Life_Expectancy"                 "Adult_Mortality"                
# "Infant_Deaths"                   "BMI"                            
# "Under.five_Deaths"               "Polio"                          
# "Diphtheria"                      "HIV.AIDS"                       
# "Per_Capita_GDP"                  "Thinness_1.19_Years"            
# "Thinness_5.9_Years"              "Income_Composition_of_Resources"

##"Alcohol","Percentage_Expenditure","Hepatitis_B","Measles" ,"Total_Expenditure","GDP","Population" are bad predictors

str(mydata)
mydata <-  mydata[, -c(4,5,6,7,11,14,16)]
str(mydata)

##continuous column- histogram
ColsForHist=c("Life_Expectancy","Adult_Mortality","Infant_Deaths","BMI")
par(mfrow=c(3,2))
library(RColorBrewer)
for (ColumnName in ColsForHist){
  hist(mydata[,c(ColumnName)], main=paste(' Histogram of:', ColumnName), 
       col=brewer.pal(8,"Paired"))
}
ColsHist=c("Polio","Diphtheria","HIV.AIDS","Per_Capita_GDP","Income_Composition_of_Resources")
par(mfrow=c(3,2))
for (ColumnName in ColsHist){
  hist(mydata[,c(ColumnName)], main=paste('Histogram of:', ColumnName), 
       col=brewer.pal(8,"Paired"))
}
#Bivariate analysis


# Continuous Vs Continuous --- Scatter plot
# For multiple columns at once
ContinuousCols = c("Life_Expectancy","Adult_Mortality","Infant_Deaths","BMI")
par(mfrow=c(1,1))
plot(mydata[, ContinuousCols], col='blue')

CCols = c("Polio","Diphtheria","HIV.AIDS","Per_Capita_GDP","Income_Composition_of_Resources")
par(mfrow=c(1,1))
plot(mydata[, CCols], col='blue')

##STEP 5:BUILD THE MODEL
##splitting the data
library(caTools)
set.seed(100)
split<- sample.split(mydata$Life_Expectancy,SplitRatio = 0.70)
split

table(split)
training<-subset(mydata,split==TRUE)
nrow(training) #2075
testing<-subset(mydata,split==FALSE)
nrow(testing) #863


ln_rg <- lm(Life_Expectancy~.,data=training)
ln_rg
summary(ln_rg)

#Residual standard error: 3.52 on 2063 degrees of freedom
#Multiple R-squared:  0.856,	Adjusted R-squared:  0.8552 
#F-statistic:  1114 on 11 and 2063 DF,  p-value: < 2.2e-16
##removing the insignificant variable Thinness_1.19_Years(having p value 0.356317),BMI(Having p value 0.213879) that has high p-value


ln_rg1<-lm(Life_Expectancy~.-Thinness_1.19_Years-BMI,data=training)
summary(ln_rg1)
#Residual standard error: 3.52 on 2065 degrees of freedom
#Multiple R-squared:  0.8559,	Adjusted R-squared:  0.8552 
#F-statistic:  1362 on 9 and 2065 DF,  p-value: < 2.2e-16


lin_predict<- predict(ln_rg1, newdata = testing)
lin_predict

lin_predict_cbind <- cbind(testing$Life_Expectancy,lin_predict)
lin_predict_cbind


library(lmtest)
library(faraway)
library(car)

## Check for auto-corelation
dwtest(ln_rg1)
#DW =0.65899, p-value < 2.2e-16
#alternative hypothesis: true autocorrelation is greater than 0
#no-autocorelation exist


## Check for Multicolinearity
vif(ln_rg1)
#Adult_Mortality                   Infant_Deaths 
#1.700247                      102.478995 
#Under.five_Deaths                           Polio 
#107.738105                        3.589593 
#Diphtheria                        HIV.AIDS 
#3.525128                        2.105931 
#Per_Capita_GDP              Thinness_5.9_Years 
#1.897971                        1.412849 
#Income_Composition_of_Resources 
#2.089632 


##Removing Under.five_Deaths from our model which has high variance inflation factor and reason fpr multicolinearity
ln_rg2<-lm(Life_Expectancy~.-Under.five_Deaths-Thinness_1.19_Years-BMI,data=training)
summary(ln_rg2)##building model again

#Residual standard error: 3.574 on 2066 degrees of freedom
#Multiple R-squared:  0.8513,	Adjusted R-squared:  0.8507 
#F-statistic:  1479 on 8 and 2066 DF,  p-value: < 2.2e-16



##Check for autocorelation
dwtest(ln_rg2)
#data:  ln_rg2
#DW =  0.67128, p-value < 2.2e-16
#alternative hypothesis: true autocorrelation is greater than 0
##NO autocorelation exist

##Check for Multi-colinearity
vif(ln_rg2)
#Adult_Mortality                   Infant_Deaths 
#1.698316                        1.531439 
#Polio                      Diphtheria 
#3.587585                        3.525090 
#HIV.AIDS                  Per_Capita_GDP 
#1.931740                        1.889450 
#Thinness_5.9_Years Income_Composition_of_Resources 
#1.412725                        2.032209 
                       
##No multicolinearity

ln_predict<- predict(ln_rg2, newdata = testing)
ln_predict

ln_predict_cbind <- cbind(testing$Life_Expectancy,ln_predict)
ln_predict_cbind

bptest(ln_rg2)
ncvTest(ln_rg2)
plot((ln_rg2))

shapiro.test(ln_rg2)

## MAPE 
testing$Pred_LM=predict(ln_rg2,testing)
head(testing)

testing$LM_APE= 100 *(abs(testing$Life_Expectancy-testing$Pred_LM)/testing$Life_Expectancy)
head(testing)

MeanAPE=mean(testing$LM_APE)
MedianAPE=median(testing$LM_APE)

print(paste('### Mean Accuracy of Linear Regression Model is: ', 100 - MeanAPE))
"### Mean Accuracy of Linear Regression Model is:  96.0690183360353"
print(paste('### Median Accuracy of Linear Regression Model is: ', 100 - MedianAPE))
"### Median Accuracy of Linear Regression Model is:  97.2630023747313"




# THE Multiple R-squared IS 0.8513 AND THE Adjusted R-squared IS 0.8507
# THE VALUE lies between 0.5 to 1 AND IT IS NEARER TO 1 ABOVE THAN 0.8
## HERE it is seen that the fitness of model is good.
