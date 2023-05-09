# ---------------------------------
# Table 4: Different (asymmetric) thresholds for the rebalancing dummy
# ---------------------------------
t.d = c(0.05,0.03,0.07,0.03,0.07,0.05,-Inf,0.05)
t.e = c(0.07,0.07,0.05,0.05,0.03,0.03,0.05,-Inf)

covariates=c("profit","inter")
covariates_nice=c("Profit","Dum $\\times$ Profit")
rnames=c()
for(i in 1:length(covariates)){
  rnames=c(rnames,covariates[i])
  rnames=c(rnames,paste("SE",covariates[i],sep=""))
}
rnames=c("debt.th","equity.th",rnames,"Controls","Wald","Quarter FE","Adj. R^2","Refin. Obs.","Total Obs.")
finalrnames=c()
for(i in 1:length(covariates)){
  finalrnames=c(finalrnames,covariates_nice[i])
  finalrnames=c(finalrnames,paste("\\color{white}",covariates_nice[i],sep=""))
}
finalrnames=c("\\color{white} debt.th","\\color{white} equity.th",finalrnames,"Controls","Wald","Quarter FE","Adj. $R^2$","Refin. Obs.","Total Obs.")
cnames = as.character(1:length(t.d))
finalcnames=c()
for(i in 1:length(t.d)){
  temp=paste("(",i,")",sep="")
  finalcnames=c(finalcnames,temp)
}
m=matrix(NA,nrow=length(rnames),ncol=length(cnames))
dimnames(m)=list(rnames,cnames)

Div.temp = d$Div
Rep.temp = d$Rep
StkIss.temp = d$StkIss
Div.temp[is.na(Div.temp)] = 0 # we will need the sum of these 3 variables, so if one of them is NA then the sum would be NA
Rep.temp[is.na(Rep.temp)] = 0
StkIss.temp[is.na(StkIss.temp)] = 0

for(r in 1:length(t.d)){
  th.e = t.e[r]
  th.d = t.d[r]
  
  p = pdata.frame(d,index = c('gvkey','time'),row.names=FALSE) # create a 'panel dataframe' from d
  debt.issues = d$chg.net.debt > th.d
  equity.payouts = Div.temp + Rep.temp - StkIss.temp > th.e
  dum = debt.issues * equity.payouts
  dum[is.na(dum)] = 0
  
  inter = Lag(p$profit) * dum
  d$dum = dum
  d$inter = inter
  p = pdata.frame(d,index = c('gvkey','time'),row.names=FALSE) # this creates a special dataframe, a 'panel data frame' with firm and time indices
  p = lapply(p, function(x){attr(x, "index")<- NULL; x}) # make a normal dataframe out of p
  p = data.frame(p)
  p$profit = Lag(p$profit)
  p$risk = Lag(p$risk)
  p$size = Lag(p$size)
  p$mtb = Lag(p$mtb)
  p$ta = Lag(p$ta)
  
  formula = nmlev ~ profit + dum + inter + risk + size + mtb + ta + factor(time)
  reg = lm(formula, data=p)
  summary(reg)
  reg.cl = cl(p,reg,p$gvkey)
  vcov = cl_vcov(p,reg,p$gvkey)
  
  pos_prof<-2
  pos_int<-4
  summ<-summary(reg)
  
  R<-c(rep(0,(pos_prof-1)),1,rep(0,length(pos_prof:pos_int)-2),1,rep(0,length(coefficients(reg))-pos_int))
  tstat<-(t(R)%*%coefficients(summ)[,1]) * solve(t(R)%*% vcov %*%R) * (t(R)%*%coefficients(summ)[,1])
  wtest<-1-pchisq(tstat, 1, ncp=0, lower.tail = TRUE, log.p = FALSE)
  wtest
  rf=sum(p$dum[-reg$na.action]) # number of refinancing firms, excluding observations with NA
  rf
  
  rr = reg
  s = summ
  
  for(j in 1:length(covariates)){
    covariate=covariates[j]
    if(!is.na(rr$coefficients[covariate])){
      m[covariate,r]=as.character(formatC(rr$coefficients[covariate],digits=3,format="f"))
      if(reg.cl[covariate,"Pr(>|t|)"]<0.01) m[covariate,r]=paste("$",m[covariate,r],"^{***}$",sep="")
      else if(reg.cl[covariate,"Pr(>|t|)"]<0.05) m[covariate,r]=paste("$",m[covariate,r],"^{**\\color{white}*}$",sep="")
      else if(reg.cl[covariate,"Pr(>|t|)"]<0.1) m[covariate,r]=paste("$",m[covariate,r],"^{*\\color{white}**}$",sep="")
      else m[covariate,r]=paste("$",m[covariate,r],"^{\\color{white}***}$",sep="")
      temp=as.character(formatC(reg.cl[covariate,'Std. Error'],digits=3,format="f"))
      temp2 = which(rownames(m)==covariate)
      m[temp2+1,r]=paste("$(",temp,")","^{\\color{white}***}$",sep="")
    }
  }
  
  m["debt.th",r] = if(th.d==-Inf) NA else paste("$T_d=",th.d,"$",sep="")
  m["equity.th",r] = if(th.e==-Inf) NA else paste("$T_e=",th.e,"$",sep="")
  m["Controls",r] = "yes"
  m["Wald",r] = as.character(formatC(wtest,digits=3,format="f"))
  m["Quarter FE",r] = "yes"
  m["Adj. R^2",r] = as.character(formatC(s$adj.r.squared,digits=2,format="f"))
  m["Refin. Obs.",r] = as.character(floor(rf))
  m["Total Obs.",r] = as.character(floor(s$df[1]+s$df[2]))
}

dimnames(m)=list(finalrnames,finalcnames)

cap=c("\\centerline{\\textbf{Regression Results: Asymmetric Refinancing Firm Definition}}")
lab=c("tab:asymmetric")
table=xtable(m,caption=cap,label=lab)
align(table)=c("r",rep("c",ncol(m)))
path = paste(getwd(),"/Results/tabla_4.tex",sep='')
print(table,file=path,hline.after=c(-1,0,2,(nrow(m)-6),nrow(m)),caption.placement="top",sanitize.text.function=function(x){x})
