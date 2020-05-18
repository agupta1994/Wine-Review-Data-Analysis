library(tidyverse)
library(ggplot2)
library(MCMCpack)

##Read Data
Data<- read.csv("../input/wine-reviews/winemag-data-130k-v2.csv")
library(dplyr)
Data_ch <- filter(Data, price == '15' & country == 'Chile' & variety == 'Chardonnay')
Data_sb <- filter(Data, price == '15' & country == 'South Africa' & variety == 'Sauvignon Blanc')
Data_comb <- rbind(Data_sb,Data_ch)

##Boxplot for both variety of wine
ggplot(Data_comb) +
geom_boxplot(aes(variety, points, fill = variety)) +
geom_jitter(aes(variety, points, shape = variety))
Data_comb <- within(Data_comb, variety <- factor(variety, labels = c(1, 2)))
tapply(Data_comb$points, Data_comb$variety, mean)
tapply(Data_comb$points, Data_comb$variety, median)
tapply(Data_comb$points, Data_comb$variety, sd)

##t-test implementation
t.test(points ~ variety, data=Data_comb, var.equal = TRUE)

##Gibbs Sampling
compare_2_gibbs <- function(y, ind, mu0 = 50, tau0 = 1/500, del0 = 0, gamma0 = 1/500, a0 = 1, b0 = 1/500, maxiter = 5000)
{
y1 <- y[ind == 1]
y2 <- y[ind == 2]
n1 <- length(y1)
n2 <- length(y2)
##### starting values
mu <- (mean(y1) + mean(y2)) / 2
del <- (mean(y1) - mean(y2)) / 2
mat_store <- matrix(0, nrow = maxiter, ncol = 3)
#####
##### Gibbs sampler
an <- a0 + (n1 + n2)/2
for(s in 1 : maxiter)
{
##update tau
bn <- b0 + 0.5 * (sum((y1 - mu - del) ^ 2) + sum((y2 - mu + del) ^ 2))
tau <- rgamma(1, an, bn)
##
##update mu
taun <- tau0 + tau * (n1 + n2)
mun <- (tau0 * mu0 + tau * (sum(y1 - del) + sum(y2 + del))) / taun
mu <- rnorm(1, mun, sqrt(1/taun))
##
##update del
gamman <- tau0 + tau*(n1 + n2)
deln <- ( del0 * tau0 + tau * (sum(y1 - mu) - sum(y2 - mu))) / gamman
del<-rnorm(1, deln, sqrt(1/gamman))
##
## store parameter values
mat_store[s, ] <- c(mu, del, tau)
}
colnames(mat_store) <- c("mu", "del", "tau")
return(mat_store)
}

#fitting data into Gibbs Sampler
fit <- compare_2_gibbs(Data_combined$points, as.factor(Data_combined$variety))

# Analysing the fit
plot(as.mcmc(fit))
raftery.diag(as.mcmc(fit))
apply(fit, 2, sd)
apply(fit, 2, mean)
mean(1/sqrt(fit[, 3]))
sd(1/sqrt(fit[, 3]))
y1_sim <- rnorm(5000, fit[, 1] + fit[, 2], sd = 1/sqrt(fit[, 3]))
y2_sim <- rnorm(5000, fit[, 1] - fit[, 2], sd = 1/sqrt(fit[, 3]))
ggplot(data.frame(y_sim_diff = y1_sim - y2_sim)) + stat_bin(aes(y_sim_diff))

#getting Probability
mean(y1_sim < y2_sim)
ggplot(data.frame(y1_sim, y2_sim)) + geom_point(aes(y1_sim, y2_sim), alpha = 0.3) + geom_abline(slope = 1, intercept = 0)