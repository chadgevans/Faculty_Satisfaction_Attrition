# Convergence problems, so normalize features to a mean of zero and sd of 1
temp<-df$ACE
numvars<-sapply(df, is.numeric)
df[numvars]<-sapply(df[numvars], function(x){((x-mean(x, na.rm=T))/sd(x, na.rm=T))})
df$ACE<-temp