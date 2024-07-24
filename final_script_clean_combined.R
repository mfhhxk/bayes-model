library(readxl)
africa <- read_xlsx('./data/all_regions.xlsx',sheet=1)
america <- read_xlsx('./data/all_regions.xlsx',sheet=2)
asia <- read_xlsx('./data/all_regions.xlsx',sheet=3)
eu <- read_xlsx('./data/all_regions.xlsx',sheet=4)
oceania <- read_xlsx('./data/all_regions.xlsx',sheet=5)

#### Cost of living and rent ####

#### Equal Mean ####
Y1.e <- NULL
Y1.e <- rbind(Y1.e,
              c(africa$colnrent
                ,america$colnrent
                ,asia$colnrent
                ,eu$colnrent
                ,oceania$colnrent))
length(Y1.e)

##### split train and test #####
# train index
set.seed(111)
index <- sample(seq(1,578,1),405)

Y1.e.train <- NULL
Y1.e.train <- Y1.e[index]
Y1.e.test <- Y1.e[-index]

n = length(Y1.e.train)
ybar = mean(Y1.e.train)
sv2 = var(Y1.e.train)

n.test = length(Y1.e.test)
ybar.test = mean(Y1.e.test)
sv2.test = var(Y1.e.test)


##### overall mean #####
#Gibbs sampler code
S<-10000

y = Y1.e.train

mu0<-100 ; t20<-40^2  # for mu
s20<-40^2 ; nu0<-1  # for sigma2
# 95% CI for y = 100+ 2*40 = 20
# sigma2 ~ invGamma(1/2, 50)

PHI.e<-matrix(nrow=S,ncol=2)
phi<-c( ybar, sv2)
YS1.e <- NULL

set.seed(112)
### Gibbs sampling
for(s in 1:S) {
  # new theta 
  t2n<- 1/( 1/t20 + n/phi[2] )
  mun<-  ( mu0/t20 + n*ybar/phi[2] )* t2n
  phi[1]<-rnorm(1, mun, sqrt(t2n) )
  
  # new 1/sigma^2 
  nun<- nu0+n
  s2n<- (nu0*s20 + (n-1)*sv2 + n*(ybar-phi[1])^2 ) /nun
  phi[2]<- 1/rgamma(1, nun/2, nun*s2n/2)
  
  ys1.e <- rnorm(1,phi[1],sqrt(phi[2]))
  
  YS1.e <- rbind(YS1.e,ys1.e)
  PHI.e[s,]<-phi         }
###


##### compare error #####
# general predictive draws 
Y.e.pred <- apply(YS1.e,2,mean)
length(Y.e.pred)
mean( (Y1.e.test - Y.e.pred)^2)   # [1] 381.0604





#### Separate Mean ####
Y1.s<- NULL
Y1.s[[1]] <- africa$colnrent 
Y1.s[[2]] <- america$colnrent
Y1.s[[3]] <- asia$colnrent
Y1.s[[4]] <- eu$colnrent
Y1.s[[5]] <- oceania$colnrent


##### split train and test #####
# train index
set.seed(121)
index_africa <- sample(seq(1,25,1),18) ## 18/25
index_america <- sample(seq(1,164,1),115) ## 115/164
index_asia <- sample(seq(1,136,1),95) ## 95/136
index_eu <- sample(seq(1,237,1),166) ## 166/237
index_oceania <- sample(seq(1,16,1),11) ## 11/16

Y1.s.train <- NULL
Y1.s.train[[1]] <- africa$colnrent[index_africa]
Y1.s.train[[2]] <- america$colnrent[index_america]
Y1.s.train[[3]] <- asia$colnrent[index_asia]
Y1.s.train[[4]] <- eu$colnrent[index_eu]
Y1.s.train[[5]] <- oceania$colnrent[index_oceania]

Y1.s.test <- NULL
Y1.s.test[[1]] <- africa$colnrent[-index_africa]
Y1.s.test[[2]] <- america$colnrent[-index_america]
Y1.s.test[[3]] <- asia$colnrent[-index_asia]
Y1.s.test[[4]] <- eu$colnrent[-index_eu]
Y1.s.test[[5]] <- oceania$colnrent[-index_oceania]

J=length(Y1.s)
n <- ybar <- sv2 <- rep(0,J)     # stat for each regions
for(j in 1:J) {
  ybar[j]<-mean(Y1.s.train[[j]])
  n[j]<-length(Y1.s.train[[j]])
  sv2[j]<-var(Y1.s.train[[j]])
}



#####
S<-10000
mu0<-100 ; t20<-40^2  # for mu
s20<-20^2 ; nu0<-1  # for sigma2
# 95% CI for y = 100+ 2*40 = 20
# sigma2 ~ invGamma(1/2, 50)

YS.s1 <- YS.s2 <- YS.s3 <- YS.s4 <- YS.s5 <- NULL
ys.s1 <- ys.s2 <- ys.s3 <- ys.s4 <- ys.s5 <- NULL

##### africa ######
Mean1<- Sigma21<-NULL
mean <- ybar[1]; var <- sv2[1]
set.seed(122)
### Gibbs sampling
for(s in 1:S) {
  # generate a new theta value from its full conditional
  t2n<- 1/( 1/t20 + n[1]/var)
  mun<-  ( mu0/t20 + n[1]*ybar/var) *t2n
  mean<-rnorm(1, mun, sqrt(t2n) )
  
  # generate a new 1/sigma^2 value from its full conditional
  nun<- nu0+n[1]
  s2n<- (nu0*s20 + (n[1]-1)*sv2[1] + n[1]*(ybar-mean)^2 ) /nun
  var<- 1/rgamma(1, nun/2, nun*s2n/2)
  
  ys.s1 <- rnorm(1,mean,sqrt(var))
  
  YS.s1 <- rbind(YS.s1, ys.s1)
  Mean1 <- rbind(Mean1,mean)
  Sigma21 <- rbind(Sigma21,var)
}




##### america #####
Mean2<- Sigma22<-NULL
mean <- ybar[2]; var <- sv2[2]
set.seed(123)
### Gibbs sampling
for(s in 1:S) {
  # generate a new theta value from its full conditional
  t2n<- 1/( 1/t20 + n[2]/var)
  mun<-  ( mu0/t20 + n[2]*ybar/var)*t2n
  mean <- rnorm(1, mun, sqrt(t2n) )
  
  # generate a new 1/sigma^2 value from its full conditional
  nun<- nu0+n[2]
  s2n<- (nu0*s20 + (n[2]-1)*sv2[2] + n[2]*(ybar-mean)^2 ) /nun
  var<- 1/rgamma(1, nun/2, nun*s2n/2)
  
  ys.s2 <- rnorm(1,mean,sqrt(var))
  
  YS.s2 <- rbind(YS.s2, ys.s2)
  Mean2 <- rbind(Mean2,mean)
  Sigma22 <- rbind(Sigma22,var)
}





##### asia #####
Mean3<- Sigma23<-NULL 
mean <- ybar[3]; var <- sv2[3]
set.seed(124)
### Gibbs sampling
for(s in 1:S) {
  # generate a new theta value from its full conditional
  t2n<- 1/( 1/t20 + n[3]/var)
  mun<-  ( mu0/t20 + n[3]*ybar/var) *t2n
  mean<-rnorm(1, mun, sqrt(t2n) )
  
  # generate a new 1/sigma^2 value from its full conditional
  nun<- nu0+n[3]
  s2n<- (nu0*s20 + (n[3]-1)*sv2[3] + n[3]*(ybar-mean)^2 ) /nun
  var<- 1/rgamma(1, nun/2, nun*s2n/2)
  
  ys.s3 <- rnorm(1,mean,sqrt(var))
  
  YS.s3 <- rbind(YS.s3, ys.s3)
  Mean3 <- rbind(Mean3,mean)
  Sigma23 <- rbind(Sigma23,var)
}






##### eu #####
Mean4<- Sigma24<-NULL
mean <- ybar[4]; var <- sv2[4]
set.seed(125)
### Gibbs sampling
for(s in 1:S) {
  # generate a new theta value from its full conditional
  t2n<- 1/( 1/t20 + n[4]/var)
  mun<-  ( mu0/t20 + n[4]*ybar/var)*t2n
  mean<-rnorm(1, mun, sqrt(t2n) )
  
  # generate a new 1/sigma^2 value from its full conditional
  nun<- nu0+n[4]
  s2n<- (nu0*s20 + (n[4]-1)*sv2[4] + n[4]*(ybar-mean)^2 ) /nun
  var<- 1/rgamma(1, nun/2, nun*s2n/2)
  
  ys.s4 <- rnorm(1,mean,sqrt(var))
  
  YS.s4 <- rbind(YS.s4, ys.s4)
  Mean4 <- rbind(Mean4,mean)
  Sigma24 <- rbind(Sigma24,var)
}


##### oceania #####
Mean5<- Sigma25<-NULL
mean <- ybar[5]; var <- sv2[5]
set.seed(126)
### Gibbs sampling
for(s in 1:S) {
  # generate a new theta value from its full conditional
  t2n<- 1/( 1/t20 + n[5]/var)
  mun<-  ( mu0/t20 + n[5]*ybar/var) *t2n
  mean<-rnorm(1, mun, sqrt(t2n) )
  
  # generate a new 1/sigma^2 value from its full conditional
  nun<- nu0+n[5]
  s2n<- (nu0*s20 + (n[5]-1)*sv2[5] + n[5]*(ybar-mean)^2 ) /nun
  var<- 1/rgamma(1, nun/2, nun*s2n/2)
  
  ys.s5 <- rnorm(1,mean,sqrt(var))
  
  YS.s5 <- rbind(YS.s5, ys.s5)
  Mean5 <- rbind(Mean5,mean)
  Sigma25 <- rbind(Sigma25,var)
}

Mean.s = cbind(Mean1, Mean2, Mean3, Mean4, Mean5)
Sigma2.s = cbind(Sigma21, Sigma22, Sigma23, Sigma24, Sigma25)





###

library(coda)
mean_sep = apply(Mean.s,2,mean)
mean_sep
# [1] 28.66123 28.58981 28.54824 28.51357 29.06416
sig_sep = apply(Sigma2.s,2,mean)
sig_sep
# [1] 85.59536 356.57544 274.22999 262.54768 161.03640


##### compare error #####
# general predictive draws 
Y.pred_sep <- NULL
mean_err_sep <- NULL
YS1.s <- cbind(YS.s1,YS.s2,YS.s3,YS.s4,YS.s5)

for(j in 1:J){
  Y.pred_sep <- apply(YS1.s,2,mean)
  mean_err_sep[j] <- mean( (Y1.s.test[[j]] - Y.pred_sep[j])^2)
}
mean_err_sep
# [1]  22.09373 909.72000 271.91258 619.08923 776.33264





#### Hier ####
Y1.h<- NULL
Y1.h[[1]] <- africa$colnrent
Y1.h[[2]] <- america$colnrent
Y1.h[[3]] <- asia$colnrent
Y1.h[[4]] <- eu$colnrent
Y1.h[[5]] <- oceania$colnrent


##### split train and test #####
# train index
set.seed(131)
index_africa <- sample(seq(1,25,1),18) ## 18/25
index_america <- sample(seq(1,164,1),115) ## 115/164
index_asia <- sample(seq(1,136,1),95) ## 95/136
index_eu <- sample(seq(1,237,1),166) ## 166/237
index_oceania <- sample(seq(1,16,1),11) ## 11/16

Y1.h.train <- NULL
Y1.h.train[[1]] <- africa$colnrent[index_africa]
Y1.h.train[[2]] <- america$colnrent[index_america]
Y1.h.train[[3]] <- asia$colnrent[index_asia]
Y1.h.train[[4]] <- eu$colnrent[index_eu]
Y1.h.train[[5]] <- oceania$colnrent[index_oceania]

Y1.h.test <- NULL
Y1.h.test[[1]] <- africa$colnrent[-index_africa]
Y1.h.test[[2]] <- america$colnrent[-index_america]
Y1.h.test[[3]] <- asia$colnrent[-index_asia]
Y1.h.test[[4]] <- eu$colnrent[-index_eu]
Y1.h.test[[5]] <- oceania$colnrent[-index_oceania]

J=length(Y1.h)
n <- ybar <- sv2 <- rep(0,J)     # stat for each regions
for(j in 1:J) {
  ybar[j]<-mean(Y1.h.train[[j]])
  n[j]<-length(Y1.h.train[[j]])
  sv2[j]<-var(Y1.h.train[[j]])
}


##### mean and variance  vary #####
m<-5

## weakly informative priors
# tau ~ invGamma(1/2, 50)
eta0<-1 ; t20<-100  

# mu ~ normal(100, 40)
mu0<-100 ; g20<-40^2  

# for sigma2 ~ Gamma(1,1/100) - improper 
a0<-1 ; b0<-1/100 ;  

# for nu0
alpha<-1  


## starting values
theta<-ybar; sigma2<-sv2 
mu<-mean(theta)
tau2<-var(theta)
s20<-1/mean(1/sv2)
nu0<-10


##### MCMC #####
S<-50000; odens<-S/10000
## setup MCMC
SIGMA2<-THETA<- YS1.h <- NULL
MTSN<-NULL
nu0s<-1:5000
ys1.h <- mtsn <- NULL

set.seed(132)
for(s in 1:S){
  # thetas - mean of Y
  for(j in 1:5){
    vtheta<-1/(n[j]/sigma2[j]+1/tau2)
    etheta<-vtheta*(ybar[j]*n[j]/sigma2[j] + mu/tau2)
    theta[j]<-rnorm(1,etheta,sqrt(vtheta))
  }
  
  # sigma2s - var of Y
  for(j in 1:5) { 
    nun<-nu0+n[j]
    ss<-nu0*s20+ sum((Y1.h.train[[j]]-theta[j])^2)
    sigma2[j]<-1/rgamma(1,nun/2,ss/2)
  }
  
  # s20 - var of sigma2s
  s20<-rgamma(1, a0+m*nu0/2, b0+nu0*sum(1/sigma2)/2)
  
  # nu0s - mean of sigma2s
  lpnu0<- .5*nu0s*m*log(s20*nu0s/2)-m*lgamma(nu0s/2)+(nu0s/2-1)*sum(log(1/sigma2)) -
    nu0s*s20*sum(1/sigma2)/2   - alpha*nu0s
  
  nu0<-sample(nu0s,1,prob=exp( lpnu0-max(lpnu0)) )
  
  # mu - mean of thetas
  vmu<- 1/(m/tau2+1/g20)
  emu<- vmu*(m*mean(theta)/tau2 + mu0/g20)
  mu<-rnorm(1,emu,sqrt(vmu))
  
  # tau2 - var of thetas
  etam<-eta0+m
  ss<- eta0*t20 + sum( (theta-mu)^2 )
  tau2<-1/rgamma(1,etam/2,ss/2)
  
  for(j in 1:5){
    ys1.h[j] <- rnorm(1,theta[j],sqrt(sigma2[j]))
  }
  
  mtsn <- c(mu,tau2,s20,nu0)
  if(s%%odens==0){
    #store results
    YS1.h <- rbind(YS1.h, ys1.h)
    THETA <- rbind(THETA, theta)
    SIGMA2<-rbind(SIGMA2, sigma2)
    MTSN <- rbind(MTSN, mtsn)
  }
}


mean_hier = apply(THETA,2,mean)
mean_hier
# [1] 28.52344 53.01722 27.82402 46.20123 57.69117

sig_hier = apply(SIGMA2,2,mean)
sig_hier
# [1]  58.93735 328.32395 241.66792 281.74305  28.57518


# > n
# [1]  18 115  95 166  11

##### compare error #####
# general predictive draws 
Y.pred_hier <- NULL
mean_err_hier <- NULL

Y.pred.hier <- apply(YS1.h,2,mean)
Y1.h.err <- NULL
for(j in 1:J){
  Y1.h.err[j] = mean((Y1.h.test[[j]] - Y.pred.hier[j])^2)
}
Y1.h.err
# [1] 36.77522 335.53593 338.98960 215.17478 197.68126


##### diagnosis #####
par(mfrow=c(3,2))
plot(THETA[,1])
plot(THETA[,2])
plot(THETA[,3])
plot(THETA[,4])
plot(THETA[,5])

par(mfrow=c(3,2))
acf(THETA[,1])
acf(THETA[,2])
acf(THETA[,3])
acf(THETA[,4])
acf(THETA[,5])

par(mfrow=c(3,2))
plot(density(THETA[,1]),main='', xlab=expression(theta[1]))
plot(density(THETA[,2]),main='', xlab=expression(theta[2]))
plot(density(THETA[,3]),main='', xlab=expression(theta[3]))
plot(density(THETA[,4]),main='', xlab=expression(theta[4]))
plot(density(THETA[,5]),main='', xlab=expression(theta[5]))

apply(THETA,2,effectiveSize)
apply(THETA,2,mean)

par(mfrow=c(3,2))
plot(SIGMA2[,1])
plot(SIGMA2[,2])
plot(SIGMA2[,3])
plot(SIGMA2[,4])
plot(SIGMA2[,5])

par(mfrow=c(3,2))
acf(SIGMA2[,1])
acf(SIGMA2[,2])
acf(SIGMA2[,3])
acf(SIGMA2[,4])
acf(SIGMA2[,5])

par(mfrow=c(3,2))
plot(density(SIGMA2[,1]),main='', xlab=expression(sigma[1]^2))
plot(density(SIGMA2[,2]),main='', xlab=expression(sigma[2]^2))
plot(density(SIGMA2[,3]),main='', xlab=expression(sigma[3]^2))
plot(density(SIGMA2[,4]),main='', xlab=expression(sigma[4]^2))
plot(density(SIGMA2[,5]),main='', xlab=expression(sigma[5]^2))

apply(SIGMA2,2,effectiveSize)
apply(SIGMA2,2,mean)


# MTSN[s,]<-c(mu,tau2,s20,nu0)
par(mfrow=c(2,2))
plot(MTSN[,1],ylab=expression(mu))
plot(MTSN[,2],ylab=expression(tau^2))
plot(MTSN[,3],ylab=expression(sigma[0]^2))
plot(MTSN[,4],ylab=expression(nu[0]))

par(mfrow=c(2,2))
acf(MTSN[,1],ylab=expression(mu),main='')
acf(MTSN[,2],ylab=expression(tau^2),main='')
acf(MTSN[,3],ylab=expression(sigma[0]^2),main='')
acf(MTSN[,4],ylab=expression(nu[0]),main='')

par(mfrow=c(2,2))
plot(density(MTSN[,1]), main='', xlab=expression(mu))
plot(density(MTSN[,2]), main='', xlab=expression(tau^2))
plot(density(MTSN[,3]), main='', xlab=expression(sigma[0]^2))
plot(density(MTSN[,4]), main='', xlab=expression(nu[0]))

apply(MTSN,2,effectiveSize)
apply(MTSN,2,mean)




##### shrinkage #####
shrink <- factor <- matrix(ncol=S, nrow=m)

# MTSN[s,]<-c(mu,tau2,s20,nu0)
for(j in 1:m){
  factor[j,] = (SIGMA2[,j]/n[j]) / (SIGMA2[,j]/n[j] + MTSN[,2])
  
  shrink[j,] <- (1-factor[j,])*ybar[j] + factor[j]*MTSN[,1]
}
shrink

mean_factor = apply(factor,1,mean)
par(mfrow=c(1,1))
plot(n,mean_factor, 
     xlab = 'sample size', ylab='mean shrinkage factor')
text(n,mean_factor, 1:5, cex=0.6, pos=3, col="red")


##### plots (param shrinkage) #####
par(mfrow=c(1,2))
apply(SIGMA2,2,mean) -> sigma2.hat
plot(sv2,sigma2.hat ,
     ylab='Posterior SIGMA2',
     xlab='Sample Variance')
abline(0,1)
text(sv2, sigma2.hat, 1:5, cex=0.6, pos=3, col="red")

# par(mar = c(5.1, 5, 2, 2.1))
plot(n, sv2-sigma2.hat,ylim=c(-16,1),
     xlab = 'Sample Size',
     ylab=expression(s^2-hat(sigma^2)))
abline(h=0, lty=2)
text(n, sv2-sigma2.hat, 1:5, cex=0.6, pos=3, col="red")

dev.off()















########################################################
#### LPP ####

#### Equal Mean ####
Y2.e <- NULL
Y2.e <- rbind(Y2.e,
              c(africa$localpp
                ,america$localpp
                ,asia$localpp
                ,eu$localpp
                ,oceania$localpp))
length(Y2.e)

##### split train and test #####
# train index
set.seed(212)
index <- sample(seq(1,578,1),405)

Y2.e.train <- NULL
Y2.e.train <- Y2.e[index]
Y2.e.test <- Y2.e[-index]

n = length(Y2.e.train)
ybar = mean(Y2.e.train)
sv2 = var(Y2.e.train)


##### overall mean #####
#Gibbs sampler code
S<-10000

y = Y2.e.train

mu0<-100 ; t20<-40^2  # for mu
s20<-40^2 ; nu0<-1  # for sigma2
# 95% CI for y = 100+ 2*40 = 20
# sigma2 ~ invGamma(1/2, 50)

PHI1.e<-matrix(nrow=S,ncol=2)
phi1<-c( ybar, sv2)
YS2.e <- ys2.e <- NULL

set.seed(211)
### Gibbs sampling
for(s in 1:S) {
  # new theta 
  t2n<- 1/( 1/t20 + n/phi1[2] )
  mun<-  ( mu0/t20 + n*ybar/phi1[2] )* t2n
  phi1[1]<-rnorm(1, mun, sqrt(t2n) )
  
  # new 1/sigma^2 
  nun<- nu0+n
  s2n<- (nu0*s20 + (n-1)*sv2 + n*(ybar-phi1[1])^2 ) /nun
  phi1[2]<- 1/rgamma(1, nun/2, nun*s2n/2)
  
  ys2.e <- rnorm(1,phi1[1],sqrt(phi1[2]))
  
  YS2.e <- rbind(YS2.e,ys2.e)
  PHI1.e[s,]<-phi1         }
###


##### compare error #####
# general predictive draws 
Y.e.pred <- apply(YS2.e,2,mean)
length(Y.e.pred)
mean( (Y2.e.test - Y.e.pred)^2)   # [1] 1291.827

#### Separate Mean2 ####
Y2.s<- NULL
Y2.s[[1]] <- africa$localpp 
Y2.s[[2]] <- america$localpp
Y2.s[[3]] <- asia$localpp
Y2.s[[4]] <- eu$localpp
Y2.s[[5]] <- oceania$localpp


##### split train and test #####
# train index
set.seed(221)
index_africa <- sample(seq(1,25,1),18) ## 18/25
index_america <- sample(seq(1,164,1),115) ## 115/164
index_asia <- sample(seq(1,136,1),95) ## 95/136
index_eu <- sample(seq(1,237,1),166) ## 166/237
index_oceania <- sample(seq(1,16,1),11) ## 11/16

Y2.s.train <- NULL
Y2.s.train[[1]] <- africa$localpp[index_africa]
Y2.s.train[[2]] <- america$localpp[index_america]
Y2.s.train[[3]] <- asia$localpp[index_asia]
Y2.s.train[[4]] <- eu$localpp[index_eu]
Y2.s.train[[5]] <- oceania$localpp[index_oceania]

Y2.s.test <- NULL
Y2.s.test[[1]] <- africa$localpp[-index_africa]
Y2.s.test[[2]] <- america$localpp[-index_america]
Y2.s.test[[3]] <- asia$localpp[-index_asia]
Y2.s.test[[4]] <- eu$localpp[-index_eu]
Y2.s.test[[5]] <- oceania$localpp[-index_oceania]

J=length(Y2.s)
n <- ybar <- sv2 <- rep(0,J)     # stat for each regions
for(j in 1:J) {
  ybar[j]<-mean(Y2.s.train[[j]])
  n[j]<-length(Y2.s.train[[j]])
  sv2[j]<-var(Y2.s.train[[j]])
}



#####
S<-10000
mu0<-100 ; t20<-40^2  # for mu
s20<-20^2 ; nu0<-1  # for sigma2
# 95% CI for y = 100+ 2*40 = 20
# sigma2 ~ invGamma(1/2, 50)

YS2.s1 <- YS2.s2 <- YS2.s3 <- YS2.s4 <- YS2.s5 <- NULL
ys2.s1 <- ys2.s2 <- ys2.s3 <- ys2.s4 <- ys2.s5 <- NULL

##### africa ######
Mean21<- Sigma2_21<-NULL
mean <- ybar[1]; var <- sv2[1]
set.seed(222)
for(s in 1:S) {
  # generate a new theta value from its full conditional
  t2n<- 1/( 1/t20 + n[1]/var)
  mun<-  ( mu0/t20 + n[1]*ybar/var) *t2n
  mean<-rnorm(1, mun, sqrt(t2n) )
  
  # generate a new 1/sigma^2 value from its full conditional
  nun<- nu0+n[1]
  s2n<- (nu0*s20 + (n[1]-1)*sv2[1] + n[1]*(ybar-mean)^2 ) /nun
  var<- 1/rgamma(1, nun/2, nun*s2n/2)
  
  ys2.s1 <- rnorm(1,mean,sqrt(var))
  
  YS2.s1 <- rbind(YS2.s1, ys2.s1)
  Mean21 <- rbind(Mean21,mean)
  Sigma2_21 <- rbind(Sigma2_21,var)
}




##### america #####
Mean22<- Sigma2_22<-NULL
mean <- ybar[2]; var <- sv2[2]
set.seed(223)
### Gibbs sampling
for(s in 1:S) {
  # generate a new theta value from its full conditional
  t2n<- 1/( 1/t20 + n[2]/var)
  mun<-  ( mu0/t20 + n[2]*ybar/var)*t2n
  mean <- rnorm(1, mun, sqrt(t2n) )
  
  # generate a new 1/sigma^2 value from its full conditional
  nun<- nu0+n[2]
  s2n<- (nu0*s20 + (n[2]-1)*sv2[2] + n[2]*(ybar-mean)^2 ) /nun
  var<- 1/rgamma(1, nun/2, nun*s2n/2)
  
  ys2.s2 <- rnorm(1,mean,sqrt(var))
  
  YS2.s2 <- rbind(YS2.s2, ys2.s2)
  Mean22 <- rbind(Mean22,mean)
  Sigma2_22 <- rbind(Sigma2_22,var)
}





##### asia #####
Mean23<- Sigma2_23<-NULL 
mean <- ybar[3]; var <- sv2[3]
set.seed(224)
### Gibbs sampling
for(s in 1:S) {
  # generate a new theta value from its full conditional
  t2n<- 1/( 1/t20 + n[3]/var)
  mun<-  ( mu0/t20 + n[3]*ybar/var) *t2n
  mean<-rnorm(1, mun, sqrt(t2n) )
  
  # generate a new 1/sigma^2 value from its full conditional
  nun<- nu0+n[3]
  s2n<- (nu0*s20 + (n[3]-1)*sv2[3] + n[3]*(ybar-mean)^2 ) /nun
  var<- 1/rgamma(1, nun/2, nun*s2n/2)
  
  ys2.s3 <- rnorm(1,mean,sqrt(var))
  
  YS2.s3 <- rbind(YS2.s3, ys2.s3)
  Mean23 <- rbind(Mean23,mean)
  Sigma2_23 <- rbind(Sigma2_23,var)
}






##### eu #####
Mean24<- Sigma2_24<-NULL
mean <- ybar[4]; var <- sv2[4]
set.seed(225)
### Gibbs sampling
for(s in 1:S) {
  # generate a new theta value from its full conditional
  t2n<- 1/( 1/t20 + n[4]/var)
  mun<-  ( mu0/t20 + n[4]*ybar/var)*t2n
  mean<-rnorm(1, mun, sqrt(t2n) )
  
  # generate a new 1/sigma^2 value from its full conditional
  nun<- nu0+n[4]
  s2n<- (nu0*s20 + (n[4]-1)*sv2[4] + n[4]*(ybar-mean)^2 ) /nun
  var<- 1/rgamma(1, nun/2, nun*s2n/2)
  
  ys2.s4 <- rnorm(1,mean,sqrt(var))
  
  YS2.s4 <- rbind(YS2.s4, ys2.s4)
  Mean24 <- rbind(Mean24,mean)
  Sigma2_24 <- rbind(Sigma2_24,var)
}


##### oceania #####
Mean25<- Sigma2_25<-NULL
mean <- ybar[5]; var <- sv2[5]
set.seed(226)
### Gibbs sampling
for(s in 1:S) {
  # generate a new theta value from its full conditional
  t2n<- 1/( 1/t20 + n[5]/var)
  mun<-  ( mu0/t20 + n[5]*ybar/var) *t2n
  mean<-rnorm(1, mun, sqrt(t2n) )
  
  # generate a new 1/sigma^2 value from its full conditional
  nun<- nu0+n[5]
  s2n<- (nu0*s20 + (n[5]-1)*sv2[5] + n[5]*(ybar-mean)^2 ) /nun
  var<- 1/rgamma(1, nun/2, nun*s2n/2)
  
  ys2.s5 <- rnorm(1,mean,sqrt(var))
  
  YS2.s5 <- rbind(YS2.s5, ys2.s5)
  Mean25 <- rbind(Mean25,mean)
  Sigma2_25 <- rbind(Sigma2_25,var)
}

Mean2.s = cbind(Mean21, Mean22, Mean23, Mean24, Mean25)
Sigma2_2.s = cbind(Sigma2_21, Sigma2_22, Sigma2_23, Sigma2_24, Sigma2_25)





###

library(coda)
mean_sep = apply(Mean2.s,2,mean)
mean_sep
# [1] 36.63033 35.87715 35.54366 35.45861 37.12423
sig_sep = apply(Sigma2_2.s,2,mean)
sig_sep
# [1] 569.3414 1726.0820  567.1149  530.9891  520.3140


##### compare error #####
# general predictive draws 
Y.pred_sep <- NULL
mean_err_sep <- NULL
YS2.s <- cbind(YS2.s1,YS2.s2,YS2.s3,YS2.s4,YS2.s5)

for(j in 1:J){
  Y.pred_sep <- apply(YS2.s,2,mean)
  mean_err_sep[j] <- mean( (Y2.s.test[[j]] - Y.pred_sep[j])^2)
}
mean_err_sep
# [1]  538.6271 5241.2049  516.8833 2234.0655 4650.3766


#### Hier ####
Y2.h<- NULL
Y2.h[[1]] <- africa$localpp
Y2.h[[2]] <- america$localpp
Y2.h[[3]] <- asia$localpp
Y2.h[[4]] <- eu$localpp
Y2.h[[5]] <- oceania$localpp


##### split train and test #####
# train index
set.seed(231)
index_africa <- sample(seq(1,25,1),18) ## 18/25
index_america <- sample(seq(1,164,1),115) ## 115/164
index_asia <- sample(seq(1,136,1),95) ## 95/136
index_eu <- sample(seq(1,237,1),166) ## 166/237
index_oceania <- sample(seq(1,16,1),11) ## 11/16

Y2.h.train <- NULL
Y2.h.train[[1]] <- africa$localpp[index_africa]
Y2.h.train[[2]] <- america$localpp[index_america]
Y2.h.train[[3]] <- asia$localpp[index_asia]
Y2.h.train[[4]] <- eu$localpp[index_eu]
Y2.h.train[[5]] <- oceania$localpp[index_oceania]

Y2.h.test <- NULL
Y2.h.test[[1]] <- africa$localpp[-index_africa]
Y2.h.test[[2]] <- america$localpp[-index_america]
Y2.h.test[[3]] <- asia$localpp[-index_asia]
Y2.h.test[[4]] <- eu$localpp[-index_eu]
Y2.h.test[[5]] <- oceania$localpp[-index_oceania]

J=length(Y2.h)
n <- ybar <- sv2 <- rep(0,J)     # stat for each regions
for(j in 1:J) {
  ybar[j]<-mean(Y2.h.train[[j]])
  n[j]<-length(Y2.h.train[[j]])
  sv2[j]<-var(Y2.h.train[[j]])
}


##### mean and variance  vary #####
m<-5

## weakly informative priors
# tau ~ invGamma(1/2, 50)
eta0<-1 ; t20<-100  

# mu ~ normal(100, 40)
mu0<-100 ; g20<-40^2  

# for sigma2 ~ Gamma(1,1/100) - improper 
a0<-1 ; b0<-1/100 ;  

# for nu0
alpha<-1  


## starting values
theta<-ybar; sigma2<-sv2 
mu<-mean(theta)
tau2<-var(theta)
s20<-1/mean(1/sv2)
nu0<-10


##### MCMC #####
set.seed(232)
S<-50000; odens<-S/10000
## setup MCMC
SIGMA2.pp<-THETA.pp<- YS2.h.pp <- NULL
MTSN.pp<-NULL
nu0s<-1:5000
ys2.h <- mtsn <- NULL

for(s in 1:S){
  # thetas - mean of Y
  for(j in 1:5){
    vtheta<-1/(n[j]/sigma2[j]+1/tau2)
    etheta<-vtheta*(ybar[j]*n[j]/sigma2[j] + mu/tau2)
    theta[j]<-rnorm(1,etheta,sqrt(vtheta))
  }
  
  # sigma2s - var of Y
  for(j in 1:5) { 
    nun<-nu0+n[j]
    ss<-nu0*s20+ sum((Y2.h.train[[j]]-theta[j])^2)
    sigma2[j]<-1/rgamma(1,nun/2,ss/2)
  }
  
  # s20 - var of sigma2s
  s20<-rgamma(1, a0+m*nu0/2, b0+nu0*sum(1/sigma2)/2)
  
  # nu0s - mean of sigma2s
  lpnu0<- .5*nu0s*m*log(s20*nu0s/2)-m*lgamma(nu0s/2)+(nu0s/2-1)*sum(log(1/sigma2)) -
    nu0s*s20*sum(1/sigma2)/2   - alpha*nu0s
  
  nu0<-sample(nu0s,1,prob=exp( lpnu0-max(lpnu0)) )
  
  # mu - mean of thetas
  vmu<- 1/(m/tau2+1/g20)
  emu<- vmu*(m*mean(theta)/tau2 + mu0/g20)
  mu<-rnorm(1,emu,sqrt(vmu))
  
  # tau2 - var of thetas
  etam<-eta0+m
  ss<- eta0*t20 + sum( (theta-mu)^2 )
  tau2<-1/rgamma(1,etam/2,ss/2)
  
  for(j in 1:5){
    ys2.h[j] <- rnorm(1,theta[j],sqrt(sigma2[j]))
  }
  
  mtsn <- c(mu,tau2,s20,nu0)
  if(s%%odens==0){
    #store results
    YS2.h.pp <- rbind(YS2.h.pp, ys2.h)
    THETA.pp <- rbind(THETA.pp, theta)
    SIGMA2.pp<-rbind(SIGMA2.pp, sigma2)
    MTSN.pp <- rbind(MTSN.pp, mtsn)
  }
}


mean_hier = apply(THETA.pp,2,mean)
mean_hier
# [1] 38.43116 91.17847 47.44920 72.87207 96.99121

sig_hier = apply(SIGMA2.pp,2,mean)
sig_hier
# [1] 693.9381 1715.5199  472.5908  531.0008  517.4287


# > n
# [1]  18 115  95 166  11

##### compare error #####
# general predictive draws 
Y.pred_hier <- NULL
mean_err_hier <- NULL

Y.pred.hier <- apply(YS2.h.pp,2,mean)
Y2.h.err <- NULL
for(j in 1:J){
  Y2.h.err[j] = mean((Y2.h.test[[j]] - Y.pred.hier[j])^2)
}
Y2.h.err
# [1] 263.3537 1419.0438  616.3927  630.2744  122.7762


##### diagnosis #####
par(mfrow=c(3,2))
plot(THETA.pp[,1])
plot(THETA.pp[,2])
plot(THETA.pp[,3])
plot(THETA.pp[,4])
plot(THETA.pp[,5])

par(mfrow=c(3,2))
acf(THETA.pp[,1])
acf(THETA.pp[,2])
acf(THETA.pp[,3])
acf(THETA.pp[,4])
acf(THETA.pp[,5])

par(mfrow=c(3,2))
plot(density(THETA.pp[,1]),main='', xlab=expression(theta[1]))
plot(density(THETA.pp[,2]),main='', xlab=expression(theta[2]))
plot(density(THETA.pp[,3]),main='', xlab=expression(theta[3]))
plot(density(THETA.pp[,4]),main='', xlab=expression(theta[4]))
plot(density(THETA.pp[,5]),main='', xlab=expression(theta[5]))

apply(THETA.pp,2,effectiveSize)
apply(THETA.pp,2,mean)

par(mfrow=c(3,2))
plot(SIGMA2.pp[,1])
plot(SIGMA2.pp[,2])
plot(SIGMA2.pp[,3])
plot(SIGMA2.pp[,4])
plot(SIGMA2.pp[,5])

par(mfrow=c(3,2))
acf(SIGMA2.pp[,1])
acf(SIGMA2.pp[,2])
acf(SIGMA2.pp[,3])
acf(SIGMA2.pp[,4])
acf(SIGMA2.pp[,5])

par(mfrow=c(3,2))
plot(density(SIGMA2.pp[,1]),main='', xlab=expression(sigma[1]^2))
plot(density(SIGMA2.pp[,2]),main='', xlab=expression(sigma[2]^2))
plot(density(SIGMA2.pp[,3]),main='', xlab=expression(sigma[3]^2))
plot(density(SIGMA2.pp[,4]),main='', xlab=expression(sigma[4]^2))
plot(density(SIGMA2.pp[,5]),main='', xlab=expression(sigma[5]^2))

apply(SIGMA2.pp,2,effectiveSize)
apply(SIGMA2.pp,2,mean)


# MTSN.pp[s,]<-c(mu,tau2,s20,nu0)
par(mfrow=c(2,2))
plot(MTSN.pp[,1],ylab=expression(mu))
plot(MTSN.pp[,2],ylab=expression(tau^2))
plot(MTSN.pp[,3],ylab=expression(sigma[0]^2))
plot(MTSN.pp[,4],ylab=expression(nu[0]))

par(mfrow=c(2,2))
acf(MTSN.pp[,1],ylab=expression(mu),main='')
acf(MTSN.pp[,2],ylab=expression(tau^2),main='')
acf(MTSN.pp[,3],ylab=expression(sigma[0]^2),main='')
acf(MTSN.pp[,4],ylab=expression(nu[0]),main='')

par(mfrow=c(2,2))
plot(density(MTSN.pp[,1]), main='', xlab=expression(mu))
plot(density(MTSN.pp[,2]), main='', xlab=expression(tau^2))
plot(density(MTSN.pp[,3]), main='', xlab=expression(sigma[0]^2))
plot(density(MTSN.pp[,4]), main='', xlab=expression(nu[0]))

apply(MTSN.pp,2,effectiveSize)
apply(MTSN.pp,2,mean)




##### shrinkage #####
shrink_lpp <- factor_lpp <- matrix(ncol=S, nrow=m)

# MTSN_lpp[s,]<-c(mu,tau2,s20,nu0)

for(j in 1:m){
  factor_lpp[j,] = (SIGMA2.pp[,j]/n[j]) / (SIGMA2.pp[,j]/n[j] + MTSN.pp[,2])
  
  shrink_lpp[j,] <- (1-factor_lpp[j,])*ybar[j] + factor_lpp[j]*MTSN.pp[,1]
}
shrink_lpp

mean_factor = apply(factor_lpp,1,mean)
par(mfrow=c(1,1))
plot(n,mean_factor, 
     xlab = 'sample size', ylab='mean shrinkage factor_lpp')
text(n,mean_factor, 1:5, cex=0.6, pos=3, col="red")





##### plots (param shrinkage) #####
par(mfrow=c(1,2))
apply(SIGMA2.pp,2,mean) -> sigma2_lpp.hat
plot(sv2,sigma2_lpp.hat ,
     ylab='Posterior SIGMA2.pp',
     xlab='Sample Variance')
abline(0,1)
text(sv2, sigma2_lpp.hat, 1:5, cex=0.6, pos=3, col="red")


par(mar = c(5.1, 5, 2, 2.1))
plot(n, sv2-sigma2_lpp.hat,ylim=c(-70,1),
     xlab = 'Sample Size',
     ylab=expression(s^2-hat(sigma^2)))
abline(h=0, lty=2)
text(n, sv2-sigma2_lpp.hat, 1:5, cex=0.6, pos=3, col="red")


#############################################################
#### Comparison ####
# colnrent
    # #store results
    # YS1.h <- rbind(YS1.h, ys1.h)
    # THETA <- rbind(THETA, theta)
    # SIGMA2<-rbind(SIGMA2, sigma2)
    # MTSN <- rbind(MTSN, mtsn)
# lpp
    # #store results
    # YS2.h.pp <- rbind(YS2.h.pp, ys2.h)
    # THETA.pp <- rbind(THETA.pp, theta)
    # SIGMA2.pp<-rbind(SIGMA2.pp, sigma2)
    # MTSN.pp <- rbind(MTSN.pp, mtsn)

##### THETA #####
###### africa ######
mean(THETA.pp[,1]>THETA[,1])
mean(THETA.pp[,1]-THETA[,1])
# 94.25% cities in africa have a higher pp than colnrent by mean of 9.911729

###### america ######
mean(THETA.pp[,2]>THETA[,2])
mean(THETA.pp[,2]-THETA[,2])
# all cities have a higher pp by mean of 38.1775

###### asia ######
mean(THETA.pp[,3]>THETA[,3])
mean(THETA.pp[,3]-THETA[,3])
# all cities have a higher pp by mean of 19.63594

###### eu ######
mean(THETA.pp[,4]>THETA[,4])
mean(THETA.pp[,4]-THETA[,4])
# all cities have a higher pp by mean of 26.68194

####### oceania ######
mean(THETA.pp[,5]>THETA[,5])
mean(THETA.pp[,5]-THETA[,5])
# all cities have a higher pp by mean of 39.29561

###### all colnrent ######
plot(density(THETA[,1]),xlim=c(15,70),col=1,ylim=c(0,0.4),
     main='Densities of colnrent')
lines(density(THETA[,2]),col=2)
lines(density(THETA[,3]),col=3)
lines(density(THETA[,4]),col=4)
lines(density(THETA[,5]),col=5)
legend('topleft',legend=c('Africa','America','Asia','Europe','Oceania'), 
       lty=1:1, col=1:5, cex=0.7)

###### all localpp ######
plot(density(THETA.pp[,1]),xlim=c(10,130),col=1,ylim=c(0,0.3),
     main='Densities of localpp')
lines(density(THETA.pp[,2]),col=2)
lines(density(THETA.pp[,3]),col=3)
lines(density(THETA.pp[,4]),col=4)
lines(density(THETA.pp[,5]),col=5)
legend('topleft',legend=c('Africa','America','Asia','Europe','Oceania'), 
       lty=1:1, col=1:5, cex=0.7)


##### YS #####
###### africa ######
mean(YS2.h.pp[,1]>YS1.h[,1])
mean(YS2.h.pp[,1]-YS1.h[,1])
# 64.28% cities in africa have a higher predicted pp 
# than colnrent by mean of 10.12581

###### america ######
mean(YS2.h.pp[,2]>YS1.h[,2])
mean(YS2.h.pp[,2]-YS1.h[,2])
# 80.04% cities have a higher predicted pp by mean of 37.93745

###### asia ######
mean(YS2.h.pp[,3]>YS1.h[,3])
mean(YS2.h.pp[,3]-YS1.h[,3])
# 76.5% cities have a higher predicted pp by mean of 19.43804

###### eu ######
mean(YS2.h.pp[,4]>YS1.h[,4])
mean(YS2.h.pp[,4]-YS1.h[,4])
# 82.83% cities have a higher predicted pp by mean of 26.7654

####### oceania ######
mean(YS2.h.pp[,5]>YS1.h[,5])
mean(YS2.h.pp[,5]-YS1.h[,5])
# 94.71% cities have a higher predicted pp by mean of 39.42027

###### all colnrent ######
plot(density(YS1.h[,1]),xlim=c(-50,140),
     col='black',ylim=c(0,0.1),
     main='Densities of \nposterior predicted colnrent')
lines(density(YS1.h[,2]),col='red')
lines(density(YS1.h[,3]),col=3)
lines(density(YS1.h[,4]),col=4)
lines(density(YS1.h[,5]),col=5)
legend('topleft',legend=c('Africa','America','Asia','Europe','Oceania'), 
       lty=1:1, col=1:5, cex=0.7)

###### all localpp ######
plot(density(YS2.h.pp[,1]),xlim=c(-100,300),col=1,ylim=c(0,0.03),
     main='Densities of \nposterior predicted localpp')
lines(density(YS2.h.pp[,2]),col=2)
lines(density(YS2.h.pp[,3]),col=3)
lines(density(YS2.h.pp[,4]),col=4)
lines(density(YS2.h.pp[,5]),col=5)
legend('topleft',legend=c('Africa','America','Asia','Europe','Oceania'), 
       lty=1:1, col=1:5, cex=0.7)


###### in 1 plot  ######
par(mfrow=c(2,2),mar=c(4,3,2,0.5),mgp=c(1.9,1,0))
plot(density(THETA[,1]),xlim=c(10,130),
     col=1, ylim=c(0,0.4),lty=2,main=''
     ,xlab='colnrent',ylab='')
lines(density(THETA[,2]),col=2,lty=2)
lines(density(THETA[,3]),col=3,lty=2)
lines(density(THETA[,4]),col=4,lty=2)
lines(density(THETA[,5]),col=5,lty=2)

legend('topright',bty='n', xpd=NA,
       legend=c('Africa','America','Asia','Europe','Oceania'), 
       col=1:5, lty=2:2, cex=0.6)

mtext("Posterior Mean", side = 3, line = -1.5, outer = TRUE)




plot(density(THETA.pp[,1]),xlim=c(10,130),
     col=1, ylim=c(0,0.4),
     main='',xlab='localpp',ylab='')
lines(density(THETA.pp[,2]),col=2)
lines(density(THETA.pp[,3]),col=3)
lines(density(THETA.pp[,4]),col=4)
lines(density(THETA.pp[,5]),col=5)

legend('topright',bty='n', xpd=NA,
       legend=c('Africa','America','Asia','Europe','Oceania'), 
       col=1:5, lty=1:1, cex=0.6)







plot(density(YS1.h[,1]),xlim=c(-100,300),ylim=c(0,0.1),
     col=1, xlab='colnrent',ylab='',
     main='',lty=2)
lines(density(YS1.h[,2]),col=2,lty=2)
lines(density(YS1.h[,3]),col=3,lty=2)
lines(density(YS1.h[,4]),col=4,lty=2)
lines(density(YS1.h[,5]),col=5,lty=2)

legend('topright',bty='n', xpd=NA,
       legend=c('Africa','America','Asia','Europe','Oceania'), 
       col=1:5,lty=2:2, cex=0.6)


mtext("Posterior Predicted", line = -16, outer = TRUE)


plot(density(YS1.h[,1]),xlim=c(-100,300),ylim=c(0,0.1),
     col=1, xlab='localpp',
     main='',ylab='')
# lines(density(YS2.h.pp[,1]),col='grey3')
lines(density(YS2.h.pp[,2]),col=2)
lines(density(YS2.h.pp[,3]),col=3)
lines(density(YS2.h.pp[,4]),col=4)
lines(density(YS2.h.pp[,5]),col=5)

legend('topright',bty='n', xpd=NA,
       legend=c('Africa','America','Asia','Europe','Oceania'), 
       col=1:5, lty=1:1, cex=0.6)














