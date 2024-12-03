##EMERGE##


load("\\Users\\brend\\Downloads\\data")
ls
View(tmpDat)
thesis.df<-tmpDat

Metformin_Insulin<-sum(tmpDat$InsulinYN[tmpDat$Group== "Metformin"] == "Yes", na.rm=T)/sum(tmpDat$Group == "Metformin")
Metformin_gw32_mean<-mean(tmpDat$gw32[tmpDat$Group == "Metformin"], na.rm=T)
Metformin_gw38_mean<-mean(tmpDat$gw38[tmpDat$Group == "Metformin"], na.rm=T)

library(dplyr)

result1<-numeric(10000)
result0<-numeric(10000)
i <-1
repeat{
  thesis.df$Group<-sample(thesis.df$Group, 535, replace = F)
  result1[i]<-mean(thesis.df$gw32[thesis.df$Group == "Metformin"], na.rm=T) 
  result0[i]<-mean(thesis.df$gw32[thesis.df$Group == "Placebo"], na.rm=T)
  i<- i +1
  if(i > 10000) break}






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
