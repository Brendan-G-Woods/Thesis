##EMERGE##

library(dplyr)
load("\\Users\\brend\\Downloads\\data")
ls
View(tmpDat)
thesis.df<-tmpDat

Metformin_Insulin_proportion<-sum(tmpDat$InsulinYN[tmpDat$Group== "Metformin"] == "Yes", na.rm=T)/sum(tmpDat$Group == "Metformin")
Metformin_gw32_mean<-mean(tmpDat$gw32[tmpDat$Group == "Metformin"], na.rm=T)
Metformin_gw38_mean<-mean(tmpDat$gw38[tmpDat$Group == "Metformin"], na.rm=T)

## T-values and Z-scores for the original study ##
StudyInsulinTest<- prop.test(x = c(sum(tmpDat$InsulinYN[tmpDat$Group== "Metformin"] == "Yes", na.rm=T),
                              sum(tmpDat$InsulinYN[tmpDat$Group== "Placebo"] == "Yes", na.rm=T)),
                        n = c(sum(tmpDat$Group == "Metformin"),sum(tmpDat$Group == "Placebo")))
z_score_squared_insulin<-sqrt(StudyInsulinTest$statistic)                              


gw32_t_value<-t.test(tmpDat$gw32[tmpDat$Group == "Metformin"], tmpDat$gw32[tmpDat$Group == "Placebo"])$statistic
gw38_t_value<-t.test(tmpDat$gw38[tmpDat$Group == "Metformin"], tmpDat$gw38[tmpDat$Group == "Placebo"])$statistic

##Composites for Original study##
##Squared##
study_squared_composite<-z_score_squared_insulin+(gw32_t_value^2)+(gw38_t_value^2)
##Not squared##
study_non_squared_composite<-sqrt(z_score_squared_insulin)+gw32_t_value+gw38_t_value


## Previous permutation codes using mean ##
permutation_function<-function(x){
  permutation_result<-numeric(10000)
  i <-1
  repeat{
    thesis.df$Group<-sample(thesis.df$Group, 535, replace = F)
    permutation_result[i]<-mean(x[thesis.df$Group == "Metformin"], na.rm=T) 
    i<- i +1
    if(i > 10000) break}
  return(permutation_result)
}
summary(permutation_function(thesis.df$gw32))
hist(permutation_function(thesis.df$gw32), breaks = 100, main= "Histogram of Permuted Means of Fasting Glucose Level at Week 32", xlab= "Mean")
summary(permutation_function(thesis.df$gw38))
hist(permutation_function(thesis.df$gw38), breaks = 100, main= "Histogram of Permuted Means of Fasting Glucose Level at Week 38", xlab= "Mean")
InsulinBinary<-ifelse(thesis.df$InsulinYN == "Yes", 1, 0)
summary(permutation_function(InsulinBinary))
hist(permutation_function(InsulinBinary), breaks = 100, main= "Histogram of Permuted Mean Insulin", xlab= "Proportion Insulin")

##Permutation Code for t-values for continuous data (gw32, gw38) and for z-score for binary data (InsulinYN) ##

## Z-score for Insulin ##
proportion_permutation_function<-function(x){
  permutation_chisquare<-numeric(10000)
  i <-1
  repeat{
    thesis.df$Group<-sample(thesis.df$Group, 535, replace = F)
    permutation_result<-prop.test(x = c(sum(x[thesis.df$Group== "Metformin"] == "Yes", na.rm=T),
                                        sum(x[thesis.df$Group== "Placebo"] == "Yes", na.rm=T)),
                                  n = c(sum(thesis.df$Group == "Metformin"),sum(thesis.df$Group == "Placebo"))) 
    permutation_chisquare[i]<-permutation_result$statistic
    i<- i +1
    if(i > 10000) break}
  return(permutation_chisquare)
}

summary(proportion_permutation_function(thesis.df$InsulinYN))
hist(proportion_permutation_function(thesis.df$InsulinYN),
     breaks = 500, main= "Histogram of Permuted Z-scores of Insulin comparing Treatment Groups",
     xlab= "Z-scores Insulin")

##T-values for gw32, gw 38 ##
continuous_permutation_function<-function(x){
  permutation_t_value<-numeric(10000)
  i <-1
  repeat{
    thesis.df$Group<-sample(thesis.df$Group, 535, replace = F)
    permutation_result<-t.test(x[thesis.df$Group == "Metformin"], x[thesis.df$Group == "Placebo"])
    permutation_t_value[i]<-permutation_result$statistic
    i<- i +1
    if(i > 10000) break}
  return(permutation_t_value)
}

summary(continuous_permutation_function(thesis.df$gw32))
hist(continuous_permutation_function(thesis.df$gw32),
     breaks = 500, main= "Histogram of Permuted t-values of gw32 comparing Treatment Groups",
     xlab= "T-values gw32")

summary(continuous_permutation_function(thesis.df$gw38))
hist(continuous_permutation_function(thesis.df$gw38),
     breaks = 500, main= "Histogram of Permuted t-values of gw38 comparing Treatment Groups",
     xlab= "T-values gw38")

## Creating the Composite ##
Zi+t32+t38
## The squared method ##

squared_composite<-proportion_permutation_function(thesis.df$InsulinYN) +
  ((continuous_permutation_function(thesis.df$gw32))^2) +
  ((continuous_permutation_function(thesis.df$gw38))^2)

## Non-squared method ##
non_squared_composite<-sqrt(proportion_permutation_function(thesis.df$InsulinYN)) +
  continuous_permutation_function(thesis.df$gw32) +
  continuous_permutation_function(thesis.df$gw38)

