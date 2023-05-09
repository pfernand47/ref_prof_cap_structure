load('data.Rdata') # import dataset

p = pdata.frame(d,index = c('gvkey','time'),row.names=FALSE) # this creates a special dataframe, a 'panel data frame' with firm and time indices
p = lapply(p, function(x){attr(x, "index")<- NULL; x}) # make a normal dataframe out of p
p = data.frame(p)

p$profit = Lag(p$profit) # Change function lag() (of library zoom) for Lag() (og library quantmod)
p$risk = Lag(p$risk)
p$size = Lag(p$size)
p$mtb = Lag(p$mtb)
p$ta = Lag(p$ta)
p$hhi = Lag(p$hhi)
p$rating = Lag(p$rating)
p$ilev = Lag(p$ilev)