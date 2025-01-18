View(tmpDat)
thesis.df<-tmpDat
View(thesis.df)

gw32_t_value<-t.test(tmpDat$gw32[tmpDat$Group == "Metformin"], tmpDat$gw32[tmpDat$Group == "Placebo"])$statistic



power_function <- function(x, y, z) {
  power_result <- numeric(1000)  
  i <- 1
  repeat {
placebo_group <- tmpDat[tmpDat$Group == "Placebo", ]
clean_placebo_group1<-as.vector(na.omit(placebo_group[[x]]))
clean_placebo_group2<-as.vector(na.omit(placebo_group[[y]]))
clean_placebo_group3<-as.vector(na.omit(placebo_group[[z]]))
sampled_placebo1 <- sample(clean_placebo_group1, 15, replace = T)
sampled_placebo2 <- sample(clean_placebo_group2, 15, replace = T)
sampled_placebo3 <- sample(clean_placebo_group3, 15, replace = T)

metformin_group <- tmpDat[tmpDat$Group == "Metformin", ]
clean_metformin_group1<-as.vector(na.omit(metformin_group[[x]]))
clean_metformin_group2<-as.vector(na.omit(metformin_group[[y]]))
clean_metformin_group3<-as.vector(na.omit(metformin_group[[z]]))

sampled_metformin1 <- sample(clean_metformin_group1, 15, replace = T)
sampled_metformin2 <- sample(clean_metformin_group2, 15, replace = T)
sampled_metformin3 <- sample(clean_metformin_group3, 15, replace = T)

t_test_result1<- t.test(sampled_metformin1, sampled_placebo1)
t_test_result2<- t.test(sampled_metformin2, sampled_placebo2)
t_test_result3<- t.test(sampled_metformin3, sampled_placebo3)


power_result[i] <- (((t_test_result1$statistic)^2)+((t_test_result2$statistic)^2)+((t_test_result3$statistic)^2))
i<- i +1
if(i > 1000) break}
  return(sum((power_result>quantile(squared_composite, 0.95))/ 1000))
}

power_function("gw32", "gw38", "binary_insulin")

## @500000 samples, power was 0.220298, 0.220396. Code takes >5mins to run ##


str(thesis.df$gw32)
metformin_group <- thesis.df[thesis.df$Group == "Metformin", ]
clean_metformin_group<-na.omit(metformin_group$gw32)
sum(is.na(thesis.df$gw32[thesis.df$Group == "Metformin"]))




metformin_group <- thesis.df[thesis.df$Group == "Metformin", ]
clean_metformin_group<-as.vector(na.omit(metformin_group$gw32))
sampled_metformin <- sample(clean_metformin_group, 15, replace = T)
print(sampled_metformin)

placebo_group <- thesis.df[thesis.df$Group == "Placebo", ]
clean_placebo_group<-as.vector(na.omit(placebo_group$gw32))
sampled_placebo <- sample(clean_placebo_group, 15, replace = T)
print(sampled_placebo)

t_test_result<- t.test(sampled_metformin, sampled_placebo)
power_result <- t_test_result$statistic
print(power_result)



placebo_group <- thesis.df[thesis.df$Group == "Placebo", ]
clean_placebo_group<-as.vector(na.omit(placebo_group[["gw32"]]))
sampled_placebo <- sample(clean_placebo_group, 15, replace = T)

metformin_group <- thesis.df[thesis.df$Group == "Metformin", ]
clean_metformin_group<-as.vector(na.omit(metformin_group[["gw32"]]))
sampled_metformin <- sample(clean_metformin_group, 15, replace = T)

t_test_result<- t.test(sampled_metformin, sampled_placebo)
print(t_test_result$statistic)

##For the composites##





## Graph for Poster##


# Add a label for the line
text(percentile_95, 50, labels=paste("95th percentile:", round(percentile_95, 2)), col="red", pos=4)

squared_composite<-((continuous_permutation_function(thesis.df$binary_insulin))^2) +
  ((continuous_permutation_function(thesis.df$gw32))^2) +
  ((continuous_permutation_function(thesis.df$gw38))^2)

hist(squared_composite, main = "Permuted Squared Composite Test Statistics",
     xlab = "Squared Composite Test Statistic",
     col = "lavender",
     border = "black",
     breaks = 300)

percentile_95<-quantile(squared_composite, 0.95)
abline(v=percentile_95, col="red", lwd=1)

color_list <- c("lightpink", "salmon", "peachpuff", "lavender", "mistyrose",
                "wheat", "beige", "ivory", "lightgray", "gray", "silver", 
                "gainsboro", "darkred", "firebrick", "darkslategray", "crimson", 
                "burgundy", "olivedrab", "darkolivegreen", "palegreen", 
                "mediumseagreen", "gold", "yellow", "lime", "turquoise", "teal", 
                "slateblue")

# Create a color palette to visualize the colors
par(mfrow=c(5,5), mar=c(1,1,1,1)) # 5x5 grid layout
for (color in color_list) {
  plot(1, 1, col=color, pch=15, cex=6, main=color, xlab="", ylab="")
}

