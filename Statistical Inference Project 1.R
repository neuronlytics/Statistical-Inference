#' ---
#' title: "Statistical Inference Project 1"
#' author: "Dennis Oriaifo"
#' date: "June 25th, 2017"
#' ---

rm(list=ls())
cat('\014')
library(knitr)
library(plyr)
library(ggplot2)

# Build dataset
set.seed(625)
n = 40
lambda = 0.2
multiples = 1000

# Define simulation set
sim = matrix(data=rexp(n=40 * 1000, rate = .2),1000,40)
means = rowMeans(sim)

# Define variables for calculations and graphing
# Simulation set variables
simMean = mean(means)
simSD = sd(simMean)
simVar = var(means)
simInterval = round(mean(means) + c(-1,1)*1.96*sd(means)/sqrt(n),3)

# Theoretical set variables
TheoreticalMean = 1/lambda
TheoreticalSD = sd(TheoreticalMean)
TheoreticalVar = (1/lambda)^2/n
TheoreticalInterval = TheoreticalMean + c(-1,1) * 1.96 * sqrt(TheoreticalVar)/sqrt(n)

# Difference two means 
simMean - TheoreticalMean

# Looking at the theoretical confidence interval
tConfInterval =  TheoreticalMean + c(-1,1)*1.96 * TheoreticalInterval/n
tConfInterval

# Looking at simulation confidence interval
sConfInterval = simMean + c(-1,1) * 1.96 * simInterval/n
sConfInterval

# The distribution is not normally distributed

# Question 3: Show that the distribution is approximately normal.
hist(means,  xlab="Simulation Mean", ylab = "Density", main="Mean Dist.", breaks=n, prob=TRUE)
lines(density(means),lwd=4)
abline(v = TheoreticalMean, col = "red", lty = 1,lwd=3)
x_fit = seq(min(means), max(means), length = 50)
y_fit = dnorm(x_fit, mean = TheoreticalMean, sd = (TheoreticalMean/sqrt(n)))
lines(x_fit, y_fit, pch = 50, col = "blue", lty = 2,lwd=3)

# Plotting theoretical quantiles 
summary(means)
qqnorm(means)
qqline(means)
