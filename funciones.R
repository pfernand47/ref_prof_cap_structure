# Funciones necesarias

cl=function(dat,fm,cluster){ # this function calculates p-values of regression coefficients using clustered standard errors
  attach(dat, warn.conflicts = F)
  if(length(fm$na.action>0)) cluster=factor(cluster[-fm$na.action]) # added this line, remove observations with NA
  M <- length(unique(cluster))
  N <- length(cluster)  	
  K <- fm$rank		
  dfc <- (M/(M-1))*((N-1)/(N-K))
  uj  <- apply(estfun(fm),2,function(x) tapply(x, cluster, sum))
  vcovCL <- dfc*sandwich(fm, meat=crossprod(uj)/N)
  coeftest(fm, vcovCL)
}

cl_vcov=function(dat,fm,cluster){ # this function calculates the covariance matrix of regression coefficients using clustered standard errors
  attach(dat, warn.conflicts = F)
  if(length(fm$na.action>0)) cluster=factor(cluster[-fm$na.action]) # added this line, remove observations with NA
  M <- length(unique(cluster))
  N <- length(cluster)  	
  K <- fm$rank		
  dfc <- (M/(M-1))*((N-1)/(N-K))
  uj  <- apply(estfun(fm),2,function(x) tapply(x, cluster, sum))
  vcovCL <- dfc*sandwich(fm, meat=crossprod(uj)/N)
  return(vcovCL)
}

mfx <- function(x,sims=1000){ # this function calculates the marginal effects
  set.seed(1984)
  pdf <- ifelse(as.character(x$call)[3]=="binomial(link = \"probit\")",mean(dnorm(predict(x, type = "link"))),mean(dlogis(predict(x, type = "link"))))
  pdfsd <- ifelse(as.character(x$call)[3]=="binomial(link = \"probit\")",sd(dnorm(predict(x, type = "link"))),sd(dlogis(predict(x, type = "link"))))
  marginal.effects <- pdf*coef(x)
  sim <- matrix(rep(NA,sims*length(coef(x))), nrow=sims)
  for(i in 1:length(coef(x))){
    sim[,i] <- rnorm(sims,coef(x)[i],diag(vcov(x)^0.5)[i])
  }
  pdfsim <- rnorm(sims,pdf,pdfsd)
  sim.se <- pdfsim*sim
  res <- cbind(marginal.effects,apply(sim.se,2,sd))
  colnames(res)[2] <- "standard.error"
  ifelse(names(x$coefficients[1])=="(Intercept)",return(res[2:nrow(res),]),return(res))
}