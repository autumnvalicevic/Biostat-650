require(sas7bdat)
require(ggplot2)
require(dplyr)

depression<-read.sas7bdat("~/Downloads/completedata.sas7bdat")

# QUESTION 1

## a)Obtain a scatterplot, with title and axis labels, Depression (y-axis) vs. Fatalism (x-axis). 
## Comment on what you see.

ggplot(depression, aes(x=Fatalism, y=Depression)) + geom_point()
# There appears to be a slight positive relationship between Fatalism and Depression.
# However the data is appears quite randomly scattered

## b)Fit  a  linear  regression  model  to  estimate the  association  
## (provide  point  and interval  estimates), 
## between the   predictor Fatalism and the outcome Depression 
## (do  this  using proc reg in SAS,  or lm() in R), and 
## interpret the association & confidence interval in plain language.

depression.mod1 <- lm(Depression ~ Fatalism, data = depression)
summary(depression.mod1)
depression.mod1$coefficients[2] + 1.96 * coef(summary(depression.mod1))[2,2]
depression.mod1$coefficients[2] - 1.96 * coef(summary(depression.mod1))[2,2]
# The slope is 0.246. 95% CI: 0.168, 0.323
# Interpretation: Dpression and fatalism are significantly associated. 
# Those with a one point increase in fatalism scores on average have a 
# 0.25 point increase in depression score (CI 95%: 0.17, 0.32)


## c)Re-do b) with your own code 
## (either SAS proc iml, in R, or other). 
## Submit your code. 
x.bar <- mean(depression$Fatalism)
y.bar <- mean(depression$Depression)

ssx <- 0
ssxy <- 0
for (i in 1:length(depression$Fatalism)){
  ssx <- ssx + (depression$Fatalism[i] - x.bar)^2
  ssxy <- ssxy + (depression$Fatalism[i] - x.bar)*depression$Depression[i]
}

beta.1 <- ssxy/ssx
beta.0 <- y.bar-beta.1*x.bar

y.hat <- beta.0 + beta.1*depression$Fatalism
sse <- sum((y.hat - depression$Depression)^2)
sig2.hat <- sse/(length(depression$Fatalism)-2)

se.beta.1 <- sqrt(sig2.hat/ssx)
se.beta.0 <- sqrt(sig2.hat*(1/length(depression$Fatalism)+x.bar^2/ssx))


## d)Are depression and fatalism significantly associated? 
## Write out the formal hypothesis test: Fullystate the null and alternative hypotheses, 
## report the results of the test in statistical terms, and writeout the conclusion in plain language, and supported with evidence (p-value). 

# H0: beta.1 equals 0 HA: beta.1 does not equal 0
t.beta.1 <- beta.1/se.beta.1 ## 6.23 
df <- length(depression$Fatalism) - 2 ## df: n-2 
2 * pt(-abs(t.beta.1), df) ## p < 0.001
# Higher fatalism scores are significantly associated with higher depression scores (p < 0.001)

## e)Report and interpret the R -squared statistic 
## (the coefficient of determination). f)
## Obtain the interquartile range (IQR) for fatalism (IQR is Q3-Q1). 
## Make a  new variableFatalism_IQR, equal to Fatalism centered at its median, and divided by the IQR. 
## This is analternative way of standardizing a variable, and can be used for variables that have symmetric orskewed distribution. 
## Re-run the regression from (b) using this new variable, and interpret itscoefficient. Which of the following are the same as in 
## 1b: intercept, slope, pvalues for intercept and slope, R-squared?

summary(depression.mod2)
# R-squared = 0.05977 which means only 6% of the variation in the depression variable
# is explained by the linear model containing the Fatalism variable.
# It does not appear that Fatalism scores alone are a great predictor of Depression scores


## f) obtain the IQR for fatalism. Make a new variable Fatalism_IQR equal to fatalism
## centered at its median and divided by the IQR.
## Rerun and interpret. How is it the same or different from above model

quantile(depression$Fatalism)
depression$Fatalism_IQR <- (depression$Fatalism - median(depression$Fatalism))/(21.25-13)
depression.mod2 <- lm(Depression ~ Fatalism_IQR, data = depression)
summary(depression.mod2)
depression.mod2$coefficients[2] + 1.96 * coef(summary(depression.mod2))[2,2]
depression.mod2$coefficients[2] - 1.96 * coef(summary(depression.mod2))[2,2]
# The new coefficient for fatalism is 2.028
# Those with 8.5 higher fatalism score have on average 2.09 higher points on the depression scale (CI 95%: 1.39, 2.67)
# The point estimate and intercept as well as standard errors are different from the previous model
# The r-squared value remained the same. 

## QUESTION 2
## Calculate the sample mean of depression for males and females separately, 
## and calculate the difference between females and males (mean from females – mean for males). 
## Using a two-sample t- test, compare the differences in depression scores for females vs males: 
## Calculate the pvalue 1) assuming equal variances, 2) not assuming equal variances. 


d.avg <- depression %>% group_by(Sex) %>% summarise(d.average = mean(Depression)) # Men are 0. Female are 1.
# Mean depression score average: 5.22. Women depression score average: 5.97
t.test(Depression~Sex, data = depression) ## Assumes unequal variances: p = 0.079
t.test(Depression~Sex, data = depression, var.equal =TRUE) ## Assumes equal variance/pooled variance: p = 0.080


## QUESTION 3
## Fit a  linear regression model with depression as the outcome and X=0 for males and 1 for females as the predictor. 
## Compare the coefficient with the difference in sample means in question (2) (same ordifferent?). 
## Compare the p-value for X with the pvalues for question (2). 
## Does the pvalue from regression equal the pvalue from the two sample test 1) assuming equal variances, or 2) not assuming equal variances?

depression.mod3 <- lm(Depression ~ Sex, data = depression)
summary(depression.mod3) ## coefficient of Sex: 0.7512 = 5.97 - 5.22 and the p value is the same as the pooled variance 0.080
## we can do regression with dummy variables and obtain the same answer as t-test that assumes equal variance


## QUESTION 4
## Continue with the dataset from Homework 1. Estimate and interpret the association between depression and fatalism after adjusting for 
## demographic variables (age, sex, and race/ethnicity (MA vs NHW)).  
## Give R-squared value

depression.mod4 <- lm(Depression ~ Fatalism + Age + Sex + R_E, data = depression)
summary(depression.mod4)
depression.mod4$coefficients[2] + 1.96 * coef(summary(depression.mod4))[2,2]
depression.mod4$coefficients[2] - 1.96 * coef(summary(depression.mod4))[2,2]
## One higher point in fatalism is associated with 0.25 point increase in depression score keeping all other covariates constant (CI 95%: 0.18, 0.33)
## This is very similar to the unadjusted association
## Fatalism and demographic variables explain 10.65% of the variance of depression which is higher than the model with just fatalism

## QUESTION 5
## Clinical collaborators on this project want to fit a model with their existing data to help decide what variables 
## will allow them to explain the largest amount of variance in future studies of depression. 
## What is the most appropriate way of testing in this case: partial or sequential? Write the hypotheses being tested.
## i. Demographic variables (age, sex, and race/ethnicity) 
## ii.Education 
## iii.   Stroke risk factors (Hypertension, High cholesterol, Atrial fibrillation, Coronary artery disease, Diabetes, Current smoking, History of stroke)
## iv.   Stroke severity (NIHSS score) or other comorbidities (comorbidity1)
## v.Fatalism
## Conduct the tests required to decide which of the groups of variables in i.-iv. would be most important to collect in the collaborators’ future studies. 

# We want to do sequential testing
depression.mod5 <-lm(Depression ~ Age + Sex + R_E + Education + Htn + HiChol + Afib + Cad + Db + CurrentSmoker + HxStroke + NIHScore + Comorbidity1 + Fatalism, data = depression)
summary(depression.mod5)
anova(depression.mod5)

# SS(B_age, B_sex, B_race|Beta.0) / 3/ MSE
test.stat1 <- (640.1 + 87.59 + 9.35)/3/24.40
df(test.stat1, 3, 612-15)

# SS(B_edu|B_age, B_sex, B_race,Beta.0) / 1 / MSE
test.stat2 <- (107.95)/1/24.40
df(test.stat2, 1, 597)

# SS(B_htn, B_hichol, B_afib, B_cad, B_curr, B_hxstroke| B_edu, B_age, B_sex, B_race,Beta.0))
test.stat3 <- (561.34 + 16.47 + 22.66 + 4.22 + 186.47 + 41.59 + 48.01)/7/24.4
df(test.stat3, 7, 597)

# SS(B_nihscore, B_comorb | B_htn, B_hichol, B_afib, B_cad, B_curr, B_hxstroke, B_edu, B_age, B_sex, B_race,Beta.0))
test.stat4 <- (42.55 + 20.16) / 2/ 24.4
df(test.stat4, 2, 597)
## We stop here because we do not reject these variables

# We end up not collecting information on Fatalism

## QUESTION 6 
## Stroke  researchers  are  interested  in  knowing  whether,  among  strokepatients, the association between fatalism (predictor) and depression (outcome) 
## differs by sex (i.e., fatalism slope differs by sex), in models adjusted for age and race/ethnicity.  



## a) Use  appropriately  “centered”  and/or  “scaled”  covariates  and  write  down  a  single model 
## equation that would allow you to test the null hypothesis that the fatalism-depression  association  among  females  is  not  different  from  the  fatalism-depression association among males.  
## The alternative hypothesis is that there is a difference inthis association.  
## Write the null and alternative hypotheses in terms of a parameterfrom the model.

# H0: B_5 = 0 HA: B_5 /= 0
depression.mod7 <- lm(Depression ~ Fatalism_IQR + I((Age-mean(Age))/10) + Sex + Fatalism_IQR * Sex + R_E, data = depression)
summary(depression.mod7)

## b) Use the stroke dataset to fit the model in (a), and carry out the test.  
## Give the value ofthe parameter being tested, the test statistic, the distribution and degrees of freedomof the test statistic, and give statistical conclusion (Reject/not reject Ho.).

test.stat.beta.6 <- depression.mod7$coefficients[6]/coef(summary(depression.mod7))[6,2]
2 * pt(-abs(test.stat.beta.6), df = 606) ## p = 0.044 < 0.05
# We reject H0 and conclude that B_3 /= 0

## c) Use results from the fitted model to obtain point estimates and confidence intervalsfor the fatalism-depression association 
## among females, and a separate point estimate and CI for the fatalism-depression association among males.

summary(depression.mod7)$cov.unscaled
B.female.fatal <- depression.mod7$coefficients[2] + depression.mod7$coefficients[6]

se.female.fatal <- sqrt(0.4114^2 + 0.6385^2 + )


## d) Write a 1 sentence intepretation of the test in (b), as would be given in a researcharticle, 
## and then give another sentence interpreting the associations for males and for females.








