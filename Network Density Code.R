library(rmutil)

# Two vector representing network densities before and after shifts. This code is specific to Figure 7, but the parameter can be changes to generate the output for Figure 6
den1=rep(0,1000)
den2=rep(0,1000)

# Difference in density saved in another vector
dendiff=rep(0,1000)

# Generate densities
for (i in 1:1000){
    # in-control
thetai = rpareto(1, 1, 3)
thetaj = rpareto(1, 1, 3)
P = matrix(NA,nrow=2,ncol=2)

# baseline propensitiy of communications within the subnetwork and among the nodes across subnetwork
P[1,1] = P[2,2]=5
P[1,2] = P[2,1]=P[1,1]/2
foo = 0
for (r in 1:2){
  for (s in 1:2){
    foo = foo +  exp(-thetai*thetaj*P[r,s])
    }
}
q = foo/4
den1[i] = 1-q

# after shift densities
# Shifted propensitiy of communications within the subnetwork
P[1,1] = P[2,2]=25
foo = 0
for (r in 1:2){
  for (s in 1:2){
    foo = foo +  exp(-thetai*thetaj*P[r,s])
  }
}
q = foo/4
den2[i] = 1-q
dendiff[i] = max(0,den2[i]-den1[i])
}
par(mfrow=c(2,3))


# Showing boxplots of densities shown in Figures 6 and 7
boxplot(den1, main=expression(bold(atop("Baseline density "*'P'['rr']*'=5', '|V|=20'))))
boxplot(den2, main=expression(bold(atop("Anomalous density "*'P'['11']^{"'"}*'=(1+4)x'*'P'['11'],'|V|=20'))))
boxplot(dendiff, main="Difference in density")

den=0
for (i in 1:1000){
  thetai = runif(1,min=0.5,max=1.5)
  thetaj = runif(1,min=0.5,max=1.5)
  P = matrix(NA,nrow=2,ncol=2)
  P[1,1] = P[2,2]=1
  P[1,2] = P[2,1]=P[1,1]/2
  P[1,1] = P[2,2]=1.5 
  foo = 0
  for (r in 1:2){
    for (s in 1:2){
      foo = foo +  exp(-thetai*thetaj*P[r,s])
    }
  }
  q = foo/4
  den = den+1-q}
den/100