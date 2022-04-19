set.seed(29482)
load(file="data/sim.data")
data <- imp1

covs.cont.n=c("bw","b.head","preterm","birth.o","nnhealth","momage")
covs.cat.n=c("sex","twin","b.marr","mom.lths","mom.hs",	"mom.scoll","cig","first","booze","drugs","work.dur","prenatal","ark","ein","har","mia","pen","tex","was")
p=length(c(covs.cont.n,covs.cat.n))
Trt=data$treat

covs.ols = c(covs.cont.n,covs.cat.n)
X = data[,covs.ols]
#X = na.omit(X)
# now standardize the continuous variables

X[,covs.cont.n]=as.data.frame(t((t(X[,covs.cont.n])-unlist(lapply(X[,covs.cont.n],mean)))/sqrt(unlist(lapply(X[covs.cont.n],var)))))
# record numbers of units and covariates
N = nrow(X)
dimx = ncol(X)
Xmat = as.matrix(X)

### now create matrix of all interactions etc for third response surface
ytmp=rnorm(N)
mod.bal <- glm(formula=ytmp~(bw+b.head+preterm+birth.o+nnhealth+momage+sex+twin+b.marr+mom.lths+mom.hs+mom.scoll+cig+first+booze+drugs+work.dur+prenatal+ark+ein+har+mia+pen+tex+was)^2 + I(bw^2) + I(b.head^2) + I(preterm^2) + I(birth.o^2) + I(nnhealth^2) + I(momage^2),x=T,data=cbind.data.frame(Xmat))
coefs <- mod.bal$coef[-1]
XX <- mod.bal$x[,-1]
XX <- XX[,!is.na(coefs)]
XXXmat=cbind(rep(1,N),XX)


error = 1
tau = 0
#
# main effects coefficients
betaC.m0 = sample(c(0,1,2),p+1,replace=T,prob=c(.6,.3,.1))
betaC.m1 = sample(c(0,1,2),p+1,replace=T,prob=c(.6,.3,.1))
# quadratic coefficients
#these we make pretty rare since they really represent 3-way interactions
betaC.q0 = sample(c(0,.5,1),ncol(XXXmat)-(p+1),replace=TRUE,prob=c(.8,.15,.05))
betaC.q1 = sample(c(0,.5,1),ncol(XXXmat)-(p+1),replace=TRUE,prob=c(.8,.15,.05))
#
betaC0 = c(betaC.m0,betaC.q0)
betaC1 = c(betaC.m1,betaC.q1)
yc0hat = (XXXmat) %*% betaC0
yc1hat = (XXXmat) %*% betaC1 
offset = c(mean(yc1hat[Trt==1] - yc0hat[Trt==1])) - 0
yc1hat = (XXXmat) %*% betaC1 - offset
YC0 = rnorm(N, yc0hat, error)
YC1 = rnorm(N, yc1hat, error)
# YC is the vector of observed responses
YC = YC1; YC[Trt==0] = YC0[Trt==0]
tauCis = yc1hat[Trt==1] - yc0hat[Trt==1]
tauC = mean(tauCis)
data$YC  <- YC






