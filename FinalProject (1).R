#1.Descriptive statistics

#Data(PWD) summary
summary(PWD)
#A)Calculate: mean, median, minimum, maximum, first and third quartile, using separate commands for numerical variables

#1)Pen
mean(PWD$Pen,na.rm=TRUE)
median(PWD$Pen,na.rm=TRUE)
min(PWD$Pen,na.rm=TRUE)
max(PWD$Pen,na.rm=TRUE)
quantile(PWD$Pen,na.rm=TRUE,0.25)
quantile(PWD$Pen,na.rm=TRUE,0.75)

#2)Feeder
mean(PWD$Feeder,na.rm=TRUE)
median(PWD$Feeder,na.rm=TRUE)
min(PWD$Feeder,na.rm=TRUE)
max(PWD$Feeder,na.rm=TRUE)
quantile(PWD$Feeder,na.rm=TRUE,0.25)
quantile(PWD$Feeder,na.rm=TRUE,0.75)

#3)W0
mean(PWD$W0,na.rm=TRUE)
median(PWD$W0,na.rm=TRUE)
min(PWD$W0,na.rm=TRUE)
max(PWD$W0,na.rm=TRUE)
quantile(PWD$W0,na.rm=TRUE,0.25)
quantile(PWD$W0,na.rm=TRUE,0.75)

#4)P0
mean(PWD$P0,na.rm=TRUE)
median(PWD$P0,na.rm=TRUE)
min(PWD$P0,na.rm=TRUE)
max(PWD$P0,na.rm=TRUE)
quantile(PWD$P0,na.rm=TRUE,0.25)
quantile(PWD$P0,na.rm=TRUE,0.75)

#5)ADWG0021
mean(PWD$ADWG0021,na.rm=TRUE)
median(PWD$ADWG0021,na.rm=TRUE)
min(PWD$ADWG0021,na.rm=TRUE)
max(PWD$ADWG0021,na.rm=TRUE)
quantile(PWD$ADWG0021,na.rm=TRUE,0.25)
quantile(PWD$ADWG0021,na.rm=TRUE,0.75)

#6)ADWG2150
mean(PWD$ADWG2150,na.rm=TRUE)
median(PWD$ADWG2150,na.rm=TRUE)
min(PWD$ADWG2150,na.rm=TRUE)
max(PWD$ADWG2150,na.rm=TRUE)
quantile(PWD$ADWG2150,na.rm=TRUE,0.25)
quantile(PWD$ADWG2150,na.rm=TRUE,0.75)

#7)ADWG0050
mean(PWD$ADWG0050,na.rm=TRUE)
median(PWD$ADWG0050,na.rm=TRUE)
min(PWD$ADWG0050,na.rm=TRUE)
max(PWD$ADWG0050,na.rm=TRUE)
quantile(PWD$ADWG0050,na.rm=TRUE,0.25)
quantile(PWD$ADWG0050,na.rm=TRUE,0.75)
#////////////////////////////////////////////////////////////////////////
#B)Calculate a frequency table for the categorical variable existing
table(PWD$Sex)
table(PWD$Treatment)
#///////////////////////////////////////////////////////////////////
#c)Calculate the correlation coefficient (ADWG0021 and ADWG2150)

# cor.test() returns a list containing p.value and estimate: the correlation coefficient 
Corr <- cor.test(PWD$ADWG0021, PWD$ADWG2150, method = "pearson")
Corr$estimate

#Calculate the correlation coefficient (ADWG0021and ADWG0050)
Corr <- cor.test(PWD$ADWG0021, PWD$ADWG0050, method = "pearson")
Corr$estimate
##################################################################################################################################################
#2.Graphics
#generate a bar chart of a categorical variable for the gender (Sex parameter).
freqTableForSex <- table(PWD$Sex)
barplot(freqTableForSex)
title("Gender counter")
#//////////////////////////////////////////////////////////////////////
#Generate a bar chart graph with mean ADWG0021 in  males and females 
barplot(table(tapply(PWD$ADWG0021,c(Sex=PWD$Sex),mean,na.rm=TRUE)), xlab="Gender",ylab="Mean age")
#/////////////////////////////////////////////////////////////////////////
#Make a histogram of a continuous variable: “ADWG2150” as well as “ADWG0021”.

hist(PWD$ADWG2150)
hist(PWD$ADWG0021)
#//////////////////////////////////////////////////////////////////////////
#•	Make a scatterplot of 2 continuous variables ADWG0050 and ADWG0021, and add the regression lines for each gender
# Load the ggplot2 package
# Load the ggplot2 package
library(ggplot2)
PWD$Sex<-factor(PWD$Sex,labels=c("male","female"))
# Create a scatter plot of ADWG0021 versus ADWG0050, with separate regression lines for each sex
ggplot(PWD, aes(x = ADWG0050, y = ADWG0021, color = Sex)) +
  geom_point() +
  stat_smooth(aes(group = Sex), method = "lm", se = FALSE)
#///////////////////////////////////////////////////
#Make a boxplot of ADWG0021 in and a separate boxplots per Treatment (as.factors). 
boxplot(ADWG0021~Treatment, data = PWD )  
########################################################################################

#3.outliers:
library(outliers)
boxplot(ADWG0021~Treatment, data = PWD)     
title("ADWG0021")
#we can find outliers in A

boxplot(ADWG2150~Treatment, data = PWD)     
title("ADWG2150")
#we can find outliers in B

boxplot(ADWG0050~Treatment, data = PWD)     
title("ADWG0050")
#we can find outliers in B

##There are no outliers in C,D and E since their means are close to each other.

##################################################################################################
# 4. Testing for normality and homoscedasticity
#checking normality:
#checking normality using Q-Q plot: (The ADWG):
# Create QQ plots of ADWG0021, ADWG0050, and ADWG2150 columns with red points and line
qqnorm(PWD$ADWG0021, main="ADWG0021 QQ Plot", col="darkgreen")
qqline(PWD$ADWG0021, col="red")
qqnorm(PWD$ADWG0050, main="ADWG0050 QQ Plot", col="darkgreen")
qqline(PWD$ADWG0050, col="red")
qqnorm(PWD$ADWG2150, main="ADWG2150 QQ Plot", col="darkgreen")
qqline(PWD$ADWG2150, col="red")
#checking normality using Shapiro test:
#check normality for ADWG0021,ADWG0050,ADWG2150 using Shapiro test
shapiro.test(PWD$ADWG0021)#p-value(0.7305)>0.05 : it can't reject H0(normally distributed)
shapiro.test(PWD$ADWG0050)#p-value(0.9086)>0.05 : it can't reject H0(normally distributed)
shapiro.test(PWD$ADWG2150)#p-value(0.9037)>0.05 : it can't reject H0(normally distributed)
#/////////////////////////////////////////////////////////////////////////////////////////////////
#checking homoscedasticity: 
#we can test homosecdasticity using(Levene test , Bartlet test , resudule plots)

#check the homoscedasticity using Bartlett's test 
bartlett.test(PWD$ADWG0021, PWD$Treatment)
bartlett.test(PWD$ADWG0050, PWD$Treatment)
bartlett.test(PWD$ADWG2150, PWD$Treatment)
##check the homoscedasticity using Levene's test 
#The null hypothesis of the Levene test is that the variances of the groups in the data are equal. 
#The alternative hypothesis is that the variances of the groups are not equal.
install.packages("car")
library(car)
leveneTest(ADWG0021~Treatment, data=PWD)
leveneTest(ADWG2150~Treatment, data=PWD)
leveneTest(ADWG0050~Treatment, data=PWD)
#p-value>0.05, we can't reject null hypothesis.
#using residual plots
model=lm(ADWG0021 ~ ADWG0050, data = PWD) #for ADWG0021 aganist ADWG0050
plot(model, 1)
model2=lm(ADWG0021 ~ ADWG2150, data = PWD) #for ADWG0021 aganist ADWG2150
plot(model2, 1)
model3=lm(ADWG0050 ~ ADWG2150, data = PWD) #for ADWG0050 aganist ADWG2150
plot(model3, 1)
#///////////////////////////////////////////////////////////////////////////////
#After performing the above test , I observe that the data are normally distributed and their variance are equal(homoscedasticity)
#####################################################################################################

#	5. Statistical Inference 
#	Calculate the 90%, 95%, 99% confidence interval for the means of ADWG0021per each gender.

means <- tapply(PWD$ADWG0021,list(Sex=PWD$Sex),mean,na.rm=T)# males:144.0724 , females:142.1528
sd <- tapply(PWD$ADWG0021,list(Sex=PWD$Sex),sd,na.rm=T)
x = means
s = sd
n = 20

#90%
error <- qnorm(0.951)*s/sqrt(n) #males: 6.464626 , females: 7.803972
lowerinterval <- x - error #males : 137.6078 , females: 134.3488
upperinterval <- x + error #males : 150.5370 , females: 149.9568

#95
error <- qnorm(0.975)*s/sqrt(n) #males:7.657573  , females: 9.244075 
lowerinterval <- x - error #males: 136.4148  , females: 132.9087 
upperinterval <- x + error #males : 151.7300  , females: 151.3969 

#99
error <- qnorm(0.9995)*s/sqrt(n) # males : 12.85608  , females :15.51961 
lowerinterval <- x - error # males: 131.2163 , females: 126.6332 
upperinterval <- x + error #males: 156.9285  , females: 157.6724 
lowerinterval
upperinterval

#How would you describe those inferences?
#confidence interval for the means of the average daily weight gain (g/day) 
#in the period between 0 and 21 days post-weaning per each gender shows that 
#with 90% confidence interval in males: values is between : 139.5012 - 148.6436
#with 95% confidence interval in males: values are between :138.6577 - 149.4871 


#what do you observe in terms of the interval width when request higher confidence?
#we noticed that by increasing the confidence interval the range is wider(width increases) , 
#therefore the error range increase but with more certainty to have the true value included in the interval


#____________________________________________________________________________#
#6 hypothesis testing :
#6.1:hypothesis that ADWG0021is different between male vs female
table(PWD$Sex)
Male=(PWD$ADWG0021[PWD$Sex==2])
Female=(PWD$ADWG0021[PWD$Sex==1])
t.test(ADWG0021~Sex, data=PWD)
#null hypothesis : male and female are not different 
#alternative : male and female are  different 
#PVALUE= 0.7 > 0.05  We don't have enough evidence to reject the null hypothesis and they are different
#####################################################################################################################3
#6.2:•Assess whether the previous test assumptions have been meet for the test
##normality test 
hist(PWD[PWD$Sex=='1',]$ADWG0021,)
hist(PWD[PWD$Sex=='2',]$ADWG0021,)
#####shapiro test for males:
qqnorm(PWD[PWD$Sex=='2',]$ADWG0021, main='2')
qqline(PWD[PWD$Sex=='2',]$ADWG0021)  ## Since there is no deviation from the straight line so they are normally distributed
hist(PWD[PWD$Sex=='2',]$ADWG0021, main='2')
shapiro.test(PWD[PWD$Sex=='2',]$ADWG0021)
#####shapiro test for females:
qqnorm(PWD[PWD$Sex=='1',]$ADWG0021, main='1')
qqline(PWD[PWD$Sex=='1',]$ADWG0021)   ## Since there is no deviation from the straight line so they are normally distributed
hist(PWD[PWD$Sex=='1',]$ADWG0021,main='1')
shapiro.test(PWD[PWD$Sex=='1',]$ADWG0021)

##The null hypothesis : data is  normally distributed
##The alternative hypothesis : data is not normally distributed
##since in p-values of both males and females > alpha(0.05) , then 
##we don't have enough evidence to reject the null hypothesis and support the alternative hypothesis.
##We assume normality(no enough evidence to support non normality)

##Therefore we proved normality using qqplot and shapiro


####checking homosc... : (i.e :Same variance)
model <- lm(Sex~ADWG0021, data = PWD) 
par(mfrow = c(2, 2))
plot(model)
##In the lower left graph , Scale-location graph is used to check the assumption of constant 
##variance (i.e., homoscedasticity) of the residuals.
##A typical scale-location plot has a horizontal line with a constant spread of the
##residuals across the range of the fitted values.This indicated homoscedacity.
#####Bartlet test
data <- PWD
head(PWD,343)
bartlett.test(ADWG0021 ~ Sex, data = data)

#To test the homogeneity of variance, we use Bartlett's
#test of homoscedasticity. In Bartlett's test the null
#hypothesis (HO) means there is no difference in
#variance, the alternative (H1) means there is differences
#in variance (Heteroscedastic). From the test results we
#can see that p-value equals to 0.4129 which is larger
#than the significant level alpha(0.05) so we do not have
#enough evidence to reject the null hypothesis in
#support of alternative hypothesis. This concludes that we can assume
#our data is homoscedastic (they do have the same variance).

###################################thirdpoint#########

#We hypothesis that ADWG0021is “different” in the 
#group receiving Treatment A compared to the Treatment B 


table(PWD$Treatment)
A=(PWD$ADWG0021[PWD$Treatment=="A"])
B=(PWD$ADWG0021[PWD$Treatment=="B"])

diff = (A-B)
hist(diff)
qqnorm(diff)
qqline(diff)

# Paired t-test
t.test(A, B, paired= T, var.equal = F)
#mean difference 24.59077 
# mean difference is not equal to 0

bartlett.test(list(A[A == A],
                   B[B == B]))

#To test the homogeneity of variance, we use Bartlett's
#test of homoscedasticity. In Bartlett's test the null
#hypothesis (HO) means there is no difference in
#variance, the alternative (H1) means there is differences
#in variance (Heteroscedastic). From the test results we
#can see that p-value equals to 0.955 which is larger
#than the significant level alpha(0.05) so we do not have
#enough evidence to reject the null hypothesis in
#support of alternative hypothesis. This concludes that
#our data is homoscedastic (they have the same variance).
###########################################################################################################################################
##6.5

#ADWG0021is different between the different Treatments
#perform comparison between the different groups
install.packages("car")  # Install the car package
library(car)  # Load the car package
install.packages("rsq")  # Install the rsq package
library(rsq)  # Load the rsq package

PWD$Treatment<-factor(PWD$Treatment,labels=c('A','B','C','D','E'))

leveneTest (ADWG0021~Treatment, PWD)
##since the result of levene test is p-value > alpha 
## we don't have enough evidence to reject the null hypothesis
##that they have same varince , therefore we can state that they are homosedacity

AnovaModel = aov(ADWG0021~Treatment,data= PWD)
summary(AnovaModel)
#report(AnovaModel)
coef(AnovaModel)
##since the result of anova test is p-value > alpha 
## we don't have enough evidence to reject the null hypothesis
##that they have same varince , therefore we can state that they are homosedacity
##(ANOVA) is a statistical formula used to compare variances across the means (or average) of different groups

#posthoc

TukeyHSD(AnovaModel) 
pairwise.t.test(PWD$ADWG0021, PWD$Treatment, p.adjust.method = "bonferroni")
#######################################################################################33
#7.	Linear model
PWD$Sex<-as.integer(PWD$Sex)
plot(PWD$Sex, PWD$ADWG0021)
regression <- lm(ADWG0021 ~ Sex , data= PWD)
regression #coefficients a and b (intercept = 144.07 and slope = -1.92), therefore that the intercept is a predicted value of Y when X is zero (unit is the same as in Y), while the slope is the rate of change in y (age) as x changes.
summary(regression)
abline(regression, col="red")
#it is obvious that there is no linear relation ship between the explanatory variable sex with the response variable ADWG0021 as the sex is categorical (all males equal to one and females equal to 2) and ADVG0021 is continuous so the regression line is horizontal seems to be zero so it means that y(ADVG0021) can't be predicted by x (sex) as there is no linearity between them
#the results of regression models shows that the residuals result is too bad which is 1.23 so the error is too large, also the p value is equal to 0.756 which is much greater than the significance level 0.05 so we do not have enough evidence to reject the null so the slope is equal to zero so y can't be predicted by x (there is no linear relationship between the response variable and the explanatory variable), also the residual standard error(mean square error) equal 19.37 which is too large which is not good and f value is too small 0.09824 which results in the large p value and finally the adjusted R squares is too small -0.02367 which is a not good at all(it explains -0.02 from the variability of y)
#linear regression is between continuous and continuous but here all used in hypothesis were continuous and categorical

#Calculate and interpret a 95% confidence interval of the regression slope, (bonus)
confint(regression, 'Sex', level=0.95) #conf int is with the regression model and x axis
#here we are confident 95 percent that the true population mean falls between 2.5% (-14.31805) and 97.5% (10.47877) of the sampling distribution of sample means
#this means that the zero value is in half of the interval which indicates that we do not have enough evidence to reject the null hypothesis and in linear regression the null is that the slope equal zero(no linearity and y can't be predicted by x) so this assures that ADWG0021 can't be predicted by sex


#now we will perform linear regression but 2 continuous variables as it is supposed to be done like that 
#so we will perform ADGW2150(y axis response variable) with ADWG0021(x axis explanatory variable) 
plot(PWD$ADWG2150, PWD$ADWG0021)
regression <- lm(ADWG0021 ~ ADWG2150 , data= PWD)
regression #coefficients a and b (intercept = 103.25181 and slope = 0.07958), therefore that the intercept is a predicted value of Y when X is zero (unit is the same as in Y), while the slope is the rate of change in y (age) as x changes.
summary(regression)
#the results of linear regression between the 2 continuous variables ADWG0021(y) and ADWG2150(x) gives a bad model as the residuals median is too large(2.849) and the p values is equal to 0.158882 which is much greater than 0.05 so we do not have enough evidence to reject the null hypothesis so y(ADWG0021) can't be predicted by x(ADWG2150) as there is no linearity between them also the regression line when fitted shows that it is horizontally so that means that the slope is close to zero (null hypothesis), also here the adjusted R squared is more accurate than the R squared and the adjusted gives a very small value which is 0.02659(2.5%) which means that the regression model explains(capture) only 2.5% of the total variation of y(ADGW0021) which is not good at all, so there is no linearity
abline(regression, col="red")
#Calculate and interpret a 95% confidence interval of the regression slope, (bonus)
confint(regression, 'ADWG2150', level=0.95) #conf int is with the regression model and x axis
#here we are confident 95 percent that the true population mean falls between 2.5% (-0.03252242) and 97.5% (0.191674) of the sampling distribution of sample means
#this means that the zero value is in half of the interval which indicates that we do not have enough evidence to reject the null hypothesis and in linear regression the null is that the slope equal zero(no linearity and y can't be predicted by x) so this assures that ADWG0021 can't be predicted by sex


##now we will perform linear regression the same 2 continuous variables but exchange x with y
#so we will perform ADGW2150(x axis response variable) with ADWG0021(y axis explanatory variable) 
plot(PWD$ADWG0021, PWD$ADWG2150)
regression <- lm(ADWG2150 ~ ADWG0021 , data= PWD)
regression #coefficients a and b (intercept = 408.2149 and slope = 0.6477), therefore that the intercept is a predicted value of Y when X is zero (unit is the same as in Y), while the slope is the rate of change in y (age) as x changes.
summary(regression)
#the results of linear regression between the 2 continuous variables ADWG0021(x) and ADWG2150(y) gives also a bad model as the residuals median is too large(4.802) and the p values is equal to 0.159 which is much greater than 0.05 so we do not have enough evidence to reject the null hypothesis so y(ADWG2150) can't be predicted by x(ADWG0021) as there is no linearity between them also the regression line when fitted shows that it is horizontally so that means that the slope is close to zero (null hypothesis), so that means that the slope is close to zero (null hypothesis), also here the adjusted R squared is more accurate than the R squared and the adjusted gives a very small value which is 0.02659(2.5%) which means that the regression model explains(capture) only 2.5% of the total variation of y(ADGW2150) which is not good at all, so there is no linearity.
abline(regression, col="red")
#Calculate and interpret a 95% confidence interval of the regression slope, (bonus)
confint(regression, 'ADWG0021', level=0.95) #conf int is with the regression model and x axis
#here we are confident 95 percent that the true population mean falls between 2.5% (-0.2647333) and 97.5% (1.560231) of the sampling distribution of sample means
#this means that the zero value is in half of the interval which indicates that we do not have enough evidence to reject the null hypothesis and in linear regression the null is that the slope equal zero(no linearity and y can't be predicted by x) so this assures that ADWG0021 can't be predicted by sex

# Estimating the average ADWG0021 change for with changing the gender from 1 to 2 (bonus).
#now we will perform multiple linear regression with the 2 continuous variables ADGW0021 and ADGW2150 with the existence of Sex to see wether sex affect or not

# Load the car package
library(car)
# Fit a multiple linear regression model
multiple_regression <- lm(ADWG0021~ ADWG2150+Sex, data = PWD)# performing model so we plot the response variable (ADWG0021) against the the 2 explainatory variables (sex+ADWG2150)
multiple_regression # coefficients (intercept = 102.97717 and slope of ADWG2150= 0.07981 , slope of Sex = 0.10592)
summary(multiple_regression)
# Create added variable plots for the model
avPlots(multiple_regression)
