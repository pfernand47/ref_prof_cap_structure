# tabla 2
# ---------------------------------
# Table 2: Base case regressions (Table 1 in the paper contains summary statistics)
# ---------------------------------
regs = c('bc','x','ife','fe')
covariates=c("(Intercept)","profit","dum","inter","risk","size","mtb","ta","rating","hhi","ilev")
covariates_nice=c("Intercept","Profit","Dum","Dum $\\times$ Profit","Risk","Size","MTB","TA","Rating","HHI","ILev")
rnames=c()
for(i in 1:length(covariates)){
  rnames=c(rnames,covariates[i])
  rnames=c(rnames,paste("SE",covariates[i],sep=""))
}
rnames=c(rnames,"Wald","Quarter FE","Industry FE","Firm FE","Adj. R^2","Refin. Obs.","Total Obs.")
finalrnames=c()
for(i in 1:length(covariates)){
  finalrnames=c(finalrnames,covariates_nice[i])
  finalrnames=c(finalrnames,paste("\\color{white}",covariates_nice[i],sep=""))
}
finalrnames=c(finalrnames,"Wald","Quarter FE","Industry FE","Firm FE","Adj. $R^2$","Refin. Obs.","Total Obs.")
cnames=regs
finalcnames=c()
for(i in 1:length(regs)){
  temp=paste("(",i,")",sep="")
  finalcnames=c(finalcnames,temp)
}
m=matrix(NA,nrow=length(rnames),ncol=length(cnames))
dimnames(m)=list(rnames,cnames)


for(r in regs){
  if(r=='bc') {
    formula = nmlev ~ profit + dum + inter + risk + size + mtb + ta + factor(time)
  }
  
  if(r=='x') formula = nmlev ~ profit + dum + inter + risk + size + mtb + ta + hhi + rating + ilev + factor(time)
  
  if(r=='ife') formula = nmlev ~ profit + dum + inter + risk + size + mtb + ta + factor(sic2) + factor(time)
  
  if(r=='bc' | r=='x' | r=='ife'){
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
  }
  
  if(r=='fe'){
    formula = nmlev ~ profit + dum + inter + risk + size + mtb + ta + factor(time) # uses quarter FE
    p.fe=p[,c("gvkey","nmlev","profit","dum","inter","risk","size","mtb","ta","fyearq","time")]
    na.index=is.na(p.fe)
    na.index=rowSums(na.index)
    p.fe = p.fe[na.index==0,] # only pick the rows with no NA
    
    variables.demean = c("nmlev","profit","dum","inter","risk","size","mtb","ta")
    for(variable in variables.demean){
      p.fe[,variable] = p.fe[,variable] - ave(p.fe[,variable],p.fe$gvkey,FUN=mean)
    }
    
    p.fe$gvkey = factor(p.fe$gvkey)
    reg = lm(formula, data=p.fe)
    summary(reg)
    reg.cl = cl(p.fe,reg,p.fe$gvkey)
    vcov = cl_vcov(p.fe,reg,p.fe$gvkey)
    
    pos_prof<-2
    pos_int<-4
    summ<-summary(reg)
    
    R<-c(rep(0,(pos_prof-1)),1,rep(0,length(pos_prof:pos_int)-2),1,rep(0,length(coefficients(reg))-pos_int))
    tstat<-(t(R)%*%coefficients(summ)[,1]) * solve(t(R)%*% vcov %*%R) * (t(R)%*%coefficients(summ)[,1])
    wtest<-1-pchisq(tstat, 1, ncp=0, lower.tail = TRUE, log.p = FALSE)
    wtest
    rf=sum(p$dum[na.index==0]) # number of refinancing firms, excluding observations with NA
    rf
  }
  
  rr = reg
  s = summ
  for(j in seq(1,(length(covariates)*2-1),by=2)){
    covariate=rnames[j]
    if(!is.na(rr$coefficients[covariate])){
      m[j,r]=as.character(formatC(rr$coefficients[covariate],digits=3,format="f"))
      if(reg.cl[covariate,"Pr(>|t|)"]<0.01) m[j,r]=paste("$",m[j,r],"^{***}$",sep="")
      else if(reg.cl[covariate,"Pr(>|t|)"]<0.05) m[j,r]=paste("$",m[j,r],"^{**\\color{white}*}$",sep="")
      else if(reg.cl[covariate,"Pr(>|t|)"]<0.1) m[j,r]=paste("$",m[j,r],"^{*\\color{white}**}$",sep="")
      else m[j,r]=paste("$",m[j,r],"^{\\color{white}***}$",sep="")
      temp=as.character(formatC(reg.cl[covariate,'Std. Error'],digits=3,format="f"))
      m[j+1,r]=paste("$(",temp,")","^{\\color{white}***}$",sep="")
    }
  }
  m["Wald",r]=as.character(formatC(wtest,digits=3,format="f"))
  m["Quarter FE",r]= "yes"
  m["Adj. R^2",r]=as.character(formatC(s$adj.r.squared,digits=2,format="f"))
  m["Refin. Obs.",r]=as.character(floor(rf))
  m["Total Obs.",r]=as.character(floor(s$df[1]+s$df[2]))
  if(r=='bc') temp = 'no'; if(r=='x') temp = 'no'; if(r=='ife') temp = 'yes'; if(r=='fe') temp = 'no';
  m["Industry FE",r]= temp
  if(r=='bc') temp = 'no'; if(r=='x') temp = 'no'; if(r=='ife') temp = 'no'; if(r=='fe') temp = 'yes';
  m["Firm FE",r]= temp
}

dimnames(m)=list(finalrnames,finalcnames)

cap=c("\\centerline{\\textbf{Basecase regression results}}")
lab=c("tab:base_case")
table=xtable(m,caption=cap,label=lab)
align(table)=c("r",rep("c",ncol(m)))
path = paste(getwd(),'/Results/tabla_2.tex',sep='')
print(table,file=path,hline.after=c(-1,0,(nrow(m)-7),nrow(m)),caption.placement="top",sanitize.text.function=function(x){x})

