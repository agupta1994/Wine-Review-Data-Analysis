library(tidyverse)
df <- read.csv("../input/winereview/winemag-data-130k-v2.csv")
df <- df %>% filter(price<20,country == "Italy")
df <- df %>% group_by(region_1) %>% mutate(freq = n()) %>% ungroup() %>%
filter(freq>=4, region_1 != "",points != "")
df <- df[c(1,4:5,8)]
df <- df[with(df,order(df$region_1)),]
df$points <- df$points + rnorm(nrow(df), 1, 1)/10000

#Gibbs Sampling
compare_m_gibbs <- function(y, ind, mu0 = 50, tau0 = 1/400,
a0 = 1, b0 = 50, alpha0 =1, beta0 = 50, maxiter = 1000)
{
### weakly informative priors
a0 <- 1/2 ; b0 <- 50 ## tau_w hyperparameters
alpha0 <-1/2 ; beta0 <- 50 ## tau_b hyperparameters
mu0<-50 ; tau0 <- 1/25
###
### starting values
#m <- nlevels(ind)
m <- length(unique(ind))
reg <- unique(ind)
ybar <- theta <- tapply(y, ind, mean)
tau_w <- mean(1 / tapply(y, ind, var)) ##within group precision
mu <- mean(theta)
tau_b <-var(theta) ##between group precision
n_m <- tapply(y, ind, length)
alphan <- alpha0 + sum(n_m)/2
###
### setup MCMC
theta_mat <- matrix(0, nrow=maxiter, ncol=m)
mat_store <- matrix(0, nrow=maxiter, ncol=3)
###
### MCMC algorithm
for(s in 1:maxiter)
{
# sample new values of the thetas
for(j in 1:m)
{
taun <- n_m[j] * tau_w + tau_b
thetan <- (ybar[j] * n_m[j] * tau_w + mu * tau_b) / taun
theta[j]<-rnorm(1, thetan, 1/sqrt(taun))
}
#sample new value of tau_w
ss <- 0
for(j in 1:m){
ss <- ss + sum((y[ind == reg[j]] - theta[j])^2)
}
betan <- beta0 + ss/2
tau_w <- rgamma(1, alphan, betan)
#sample a new value of mu
taum <- m * tau_b + tau0
mum <- (mean(theta) * m * tau_b + mu0 * tau0) / taum
mu <- rnorm(1, mum, 1/ sqrt(taum))
# sample a new value of tau_b
am <- a0 + m/2
bm <- b0 + sum((theta - mu)^2) / 2
tau_b <- rgamma(1, am, bm)
#store results
theta_mat[s,] <- theta
mat_store[s, ] <- c(mu, tau_w, tau_b)
}
colnames(mat_store) <- c("mu", "tau_w", "tau_b")
colnames(theta_mat) <- c(reg)
return(list(params = mat_store, theta = theta_mat))
}

##generating data using Gibbs Sampler
fit2 <- compare_m_gibbs(df$points,as.character(df$region_1))

## Region wise mean
theta_hat <- apply(fit2$theta, 2, mean)
names(theta_hat) <- 1:100
sort(theta_hat, decreasing = TRUE)
mean_region <- mean(fit2$params[,1])

#Filtering out regions
region_name <- names(theta_hat)[theta_hat>mean_region]