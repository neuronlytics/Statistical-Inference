#' ---
#' title: "Statistical Inference Project 2"
#' author: "Dennis Oriaifo"
#' date: "June 25th, 2017"
#' ---

rm(list=ls())
cat('\014')
library(knitr)
library(ggplot2)
library(datasets)

data(ToothGrowth)
ToothGrowth$dose = as.factor(ToothGrowth$dose)

summary(ToothGrowth)

supVC = ToothGrowth[1:30,]
supOJ = ToothGrowth[31:60,]

VCvsOJ = supOJ$len - supVC$len
bothSups = data.frame(VCvsOJ, as.numeric(as.character(supOJ$dose)))
names(bothSups) = c("VCvsOJ", "Dose")
bothSups

# Using T tests to compare Supplements and dosage efficacy

# Dose of 0.5
Dose05 = subset(bothSups, Dose == 0.5)
t.test(Dose05)

# CI is  0.7483243 5.0016757 and  p-value = 0.01071
# OJ and VC are different and OJ is more effective

# Dose of 1.0
Dose10 = subset(bothSups, Dose == 1.0)
t.test(Dose10)

# CI is 1.318017 5.611983 and p-value = 0.003158
# OJ and VC are different and OJ is more effective

# Dose of 2.0
Dose20 = subset(bothSups, Dose == 2.0)
t.test(Dose20)

#CI is -1.01732  2.93732 and p-value = 0.3223
# OJ and VC are have very similar efficacy

# Visual comparison of the above t test

ggplot(data=ToothGrowth, aes(x=as.factor(dose), y=len, fill=supp)) + geom_bar(stat="identity") + 
  facet_grid(. ~ supp) + xlab("Dose") + ylab("Tooth Growth Length") + 
  guides(fill=guide_legend(title="Supplement Type"))
#  OJ is more effective at the 0.5 and 1 dose level

# Box plots for OJ and VC
ggplot(ToothGrowth, aes(x=dose, y=len)) + 
  ggtitle("Tooth Growth Length by Supplement Type") + 
  geom_boxplot(aes(fill=factor(dose))) + geom_jitter() + facet_grid(.~supp)


## CONCLUSION
# We can see from both the t tests and the visualizations that there is a positive
#   relationship between dosage and tooth growth length. i.e as dosage increases so does tooth growth length
# OJ is much more effective at the 0.5 and 1 dosage levels.
