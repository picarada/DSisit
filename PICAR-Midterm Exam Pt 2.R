"DIVINE ADA L. PICAR"
"BSSDS-3A"


#21-22
library(ggplot2)
set.seed(2122)
x <- 0:4
p <- c(0.1,0.2,0.2,0.2,0.3)
cp <- cumsum(p)
m <- 1000
r <- numeric(m)
r <- x[findInterval(runif(m),cp)+1]
table_r <- table(r)
print(table_r)

tp <- p*1000
names(tp) <- x
print(tp)
a <- data.frame(x,freq = c(105,193,205,200,297,100,200,200,200,300),type = rep(c('random sample','theoretical sample'),each = 5))
ggplot(a,aes(x = x ,y =freq ,fill = type))+geom_col(position = 'dodge')



#23-24
set.seed(2324)
library(ggplot2)
beta_arm <- function(n,a,b){
  beta_pdf <- function(x){
    (1/beta(a,b))*(x)^(a-1)*(1-x)^(b-1) 
  }
  j <- 0 
  k <- 0 
  y <- numeric(n) 
  while (k < n){
    u <- runif(1) 
    j <- j + 1 
    x <- runif(1) 
    if ((a-1)*x^(a-2) * (b-1)*(1-x)^(b-2) > u) { 
      #accept x 
      k <- k + 1 
      y[k] <- x
    }
  }
  return(y)
}
sample_beta <- beta_arm(1000,3,2) 
head(sample_beta,20)
sample_beta_theo <- rbeta(1000,3,2) 
data_beta <- data.frame(sample = c(sample_beta,sample_beta_theo),class = rep(c('empirical','theoretical'),each = 1000))
ggplot(data_beta,aes(x = sample,fill = class)) + stat_bin(bins = 30)



#25-26
set.seed(2526)
n = 1000
m = 5
s = 2
mu = log(m^2/sqrt(s^2+m^2))
sig = sqrt(log((s^2/m^2)+1))
x= rnorm(n, mu, sig)
y= exp(x)
hist(x, prob= TRUE, main= "Normal")
hist(y, prob= TRUE, main= "Lognormal")
a= seq(0, 15, 0.01)
lines(a, dlnorm(a, mu, sig), col="red")
summary(y)



#27-28
set.seed(2728)
m <- 1000
x <- runif (m , min =2 , max =4)
thetahat <- mean (exp( - x )) * 2
print ( thetahat)
print (exp ( -2) - exp ( -4))
cat("Estimated value: ", thetahat)
cat("Exact value of the integral: ", (exp ( -2) - exp ( -4)))



#29-30
#Generating theta hat and variance
thetahat <- function(n){
  u <- runif(n, min = 0, max = .5)
  that <- exp(-u)*.5
  varthat <- var(that)
  print(paste0("Theta hat is ", mean(that), ", and the variance is ", varthat, "."))
}
thetahat(1000)

#Generating theta star
thetastar <- function(n){
  x <- rexp(n,1)
  g <- (exp(-x)*(x>0)*(x<.5))/(exp(-x))
  thetastar <- mean(g)
  vartstar <- var(g)
 
  print(paste0("Theta star is ", thetastar, ", and the variance is ", vartstar, "."))
}
thetastar(1000)