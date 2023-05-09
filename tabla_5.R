# --------------------------------------
# Table 5: Predictive regressions for rebalancing
# --------------------------------------
load('data.Rdata') # import dataset

p = pdata.frame(d,index = c('gvkey','time'),row.names=FALSE) # this creates a special dataframe, a 'panel data frame' with firm and time indices
p = lapply(p, function(x){attr(x, "index")<- NULL; x}) # make a normal dataframe out of p
p = data.frame(p)

p$profit = Lag(p$profit)
p$risk = Lag(p$risk)
p$size = Lag(p$size)
p$mtb = Lag(p$mtb)
p$ta = Lag(p$ta)

difference.lengths = c(4,8,12,16,20)
diff.prof.4q = diff(p$profit,lag=4)
diff.prof.8q = diff(p$profit,lag=8)
diff.prof.12q = diff(p$profit,lag=12)
diff.prof.16q = diff(p$profit,lag=16)
diff.prof.20q = diff(p$profit,lag=20)
diff.prof = list(diff.prof.4q,diff.prof.8q,diff.prof.12q,diff.prof.16q,diff.prof.20q)

covariates=c('(Intercept)',"d.prof",'profit[-(1:(4 * i))]','risk[-(1:(4 * i))]','size[-(1:(4 * i))]','mtb[-(1:(4 * i))]','ta[-(1:(4 * i))]')
covariates_nice=c('Intercept',"$\\Delta$ (Profit)",'Profit','Risk','Size','MTB','TA')
rnames=c()
for(i in 1:length(covariates)){
  rnames=c(rnames,covariates[i])
  rnames=c(rnames,paste("SE",covariates[i],sep=""))
}
rnames=c(rnames,"debt.th","equity.th","McFadden's R^2","Refin. Obs.","Total Obs.")
finalrnames=c()
for(i in 1:length(covariates)){
  finalrnames=c(finalrnames,covariates_nice[i])
  finalrnames=c(finalrnames,paste("\\color{white}",covariates_nice[i],sep=""))
}
finalrnames=c(finalrnames,"$T_d$","$T_e$","McFadden's $R^2$","Refin. Obs.","Total Obs.")
cnames = c()
for(i in 1:length(diff.prof)){
  cnames = c(cnames,as.character(i),paste(i,'mfx'))
}
finalcnames=c()
for(i in 1:length(diff.prof)){
  temp=paste(i*4,'Quart.',sep=" ")
  temp2 = 'Marg. Eff.'
  finalcnames=c(finalcnames,temp,temp2)
}

m=matrix(NA,nrow=length(rnames),ncol=length(cnames))
dimnames(m)=list(rnames,cnames)

for(i in 1:length(diff.prof)){
  difference.length = difference.lengths[i]
  p.profit = pdata.frame(d[,c('gvkey','time','profit')],index = c('gvkey','time'),row.names=FALSE)
  p$profit = Lag(p.profit$profit,k=(difference.length+1))
  d.prof = diff.prof[[i]]
  rr = glm(dum[-(1:(4*i))] ~ d.prof + profit[-(1:(4*i))] + risk[-(1:(4*i))] + size[-(1:(4*i))] + mtb[-(1:(4*i))] + ta[-(1:(4*i))] ,data=p,family = binomial(link=logit))
  print(summary(rr))
  marginal.effects = mfx(rr)
  s = summary(rr)
  
  rf=sum(p$dum[-rr$na.action]) # number of refinancing firms, excluding observations with NA
  print(rf)
  
  r = as.character(i)
  
  for(j in 1:length(covariates)){
    covariate=covariates[j]
    if(!is.na(rr$coefficients[covariate])){
      m[covariate,r]=as.character(formatC(rr$coefficients[covariate],digits=3,format="f"))
      if(s$coefficients[covariate,"Pr(>|z|)"]<0.01) m[covariate,r]=paste("$",m[covariate,r],"^{***}$",sep="")
      else if(s$coefficients[covariate,"Pr(>|z|)"]<0.05) m[covariate,r]=paste("$",m[covariate,r],"^{**\\color{white}*}$",sep="")
      else if(s$coefficients[covariate,"Pr(>|z|)"]<0.1) m[covariate,r]=paste("$",m[covariate,r],"^{*\\color{white}**}$",sep="")
      else m[covariate,r]=paste("$",m[covariate,r],"^{\\color{white}***}$",sep="")
      temp=as.character(formatC(s$coefficients[covariate,'Std. Error'],digits=3,format="f"))
      temp2 = which(rownames(m)==covariate)
      m[temp2+1,r]=paste("$(",temp,")","^{\\color{white}***}$",sep="")
      if(covariate != '(Intercept)') {
        m[covariate,paste(i,'mfx')] = as.character(formatC(marginal.effects[covariate,'marginal.effects'],digits=3,format="f"))
        m[covariate,paste(i,'mfx')] = paste('$',m[covariate,paste(i,'mfx')],'$')
      }
    }
  }
  m["debt.th",r] = 0.05
  m["equity.th",r] = 0.05
  m["McFadden's R^2",r] = as.character(formatC(pR2(rr)['McFadden'],digits=2,format="f"))
  m["Refin. Obs.",r] = as.character(floor(rf))
  m["Total Obs.",r] = as.character(floor(s$df[1]+s$df[2]))
}

dimnames(m)=list(finalrnames,finalcnames)

cap=c("\\centerline{\\textbf{Binary Predictive Regressions for Refinancing Events: Logit Models}}")
lab=c("tab:predictive_logit")
table=xtable(m,caption=cap,label=lab)
align(table)=c("r",rep("c",ncol(m)))
path = paste(getwd(),"/Results/tabla_5.tex",sep='')
print(table,file=path,hline.after=c(-1,0,(nrow(m)-5),nrow(m)),caption.placement="top",sanitize.text.function=function(x){x})
