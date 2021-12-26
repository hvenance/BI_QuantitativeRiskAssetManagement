rm(list=ls())
library(dplyr)
library(zoo)
library(moments)
library(stats4)


#link to GlobalEquity
GlobalEquity <- read.csv('GlobalEquityReturns.csv')

#QUESTION 1
#Consider the period 198501-202005
GlobalEquity <- GlobalEquity[GlobalEquity[,1]>198412,]

#Adjust the returns, as they're computed for t+1
#GlobalEquity <- 
 #GlobalEquity %>%
  #group_by(countrynumber) %>%
  #mutate(adjusted_lrtn = dplyr::lag(excessret, n = 1, default = NA))
#to ensure that we have the right data for the right date and that NA data are not taken into account
#GlobalEquity <- GlobalEquity[complete.cases(GlobalEquity), ]
#adjusted_lrtn <- GlobalEquity[,9]

#mean(GlobalEquity$adjusted_lrtn, na.rm = T)*12


#
dats <- as.Date(as.yearmon(format(GlobalEquity$date), "%Y%m"))
uniqueDats <- unique(dats)
year <- substring(dats,1,4)
month <- substring(dats,6,7)

##
adjusted_lrtn <- GlobalEquity[,3]

#excessret <- GlobalEquity[,3]
n <- length(GlobalEquity$date)
country <- GlobalEquity[,2]
momentum <- GlobalEquity[,5]

#mean(excessret)*12


mydt <- data.frame()
mydt <- data.frame(country =country, year=year[1:n], month=month[1:n], adjusted_lrtn=adjusted_lrtn)
#to ensure that we have the right data for the right date and that NA data are not taken into account
#mydt <- mydt[complete.cases(mydt), ]
#head(mydt) 
#colnames(mydt)



#Mean function on 'temp' to get the monthly mean for all countries
monthly_lrtrn <- data.frame() #monthly returns
  for(y in unique(year)){
    for(m in sort(unique(month))){ 
      temp <- mydt[mydt[,'year'] == y & mydt[,'month'] == m, 'adjusted_lrtn']
      if(length(temp) > 0){ monthly_lrtrn <- rbind(monthly_lrtrn, data.frame(year = y, month = m, monthly_lrtrn = mean(temp), monthly_return=mean(exp(temp)-1))) } 
    }
  }

#exact same computation
#df <- group_by(mydt, date)
#df <- summarise(df, year, month, result=mean(adjusted_lrtn))
#df <- group_by(df, year)
#df <- summarise(df, result=mean(result)*12)
#mean(df$result)

#Aggregate for the annual return
annual_lrtrn <- data.frame() #annual returns
for(y in unique(year)){
    temp <- monthly_lrtrn[monthly_lrtrn[,'year'] == y, 'monthly_lrtrn']
    if(length(temp) > 0){ annual_lrtrn <- rbind(annual_lrtrn, data.frame(year = y, annual_lrtrn = mean(temp)*12, annual_return=mean(exp(temp)-1)*12)) } 
}


#More or less same computations, but now I want to retrieve VOLATILITY
vol12m <- GlobalEquity['vol12m']

mydt_vol <- data.frame(year=year[1:n], month=month[1:n], vola=vol12m)
head(mydt_vol) 

#We assume the annual volatility (t) is the one measured in December (t) as it records the previous 12 months 
annual_vol <- data.frame()

for(y in unique(year)){
  for(m in max(sort(month))){ 
    temp <- mydt_vol[mydt_vol[,'year'] == y & mydt_vol[,'month'] == m, 'vol12m']
    if(length(temp) > 0){ annual_vol <- rbind(annual_vol, data.frame(year = y, month = m, annual_vol = mean(temp)))} 
  }
}
 

#PLOT of avg log returns and avg volatility through time
#Issue: we still do not have the volatility for end 2020 so we will use latest data
annual_vol_2020 <- mydt_vol$vol12m[n]
total_annual_vol <- c(annual_vol$annual_vol, annual_vol_2020)

plot(unique(year),annual_lrtrn$annual_lrtrn,type="l",lwd=3,col='black', xlab='Year', ylab = 'Annual log rert and volatility')
lines(unique(year),total_annual_vol,lwd=3, col='red')
legend('topright', inset = 0.02, legend = c('Avg Log Returns','Avg Volatility'), col = c('black','red'), lty = rep(1,3), lwd = 3)


#Correlations
tbl1 <- data.frame(cor(cbind(annual_lrtrn[,'annual_return'],total_annual_vol)))
colnames(tbl1) <- c('Return','Volatility')
rownames(tbl1) <- c('Return','Volatility')
tbl1



#QUESTION 2
#i)Transform log returns to returns 
#ii)Average the returns of all countries --> thanks to the mean
EW_portfolio <- data.frame()
for(y in unique(year)){
  for(m in unique(month)){ 
    temp <- mydt[mydt[,'year'] == y & mydt[,'month'] == m, 'adjusted_lrtn']
    if(length(temp) > 0){ EW_portfolio <- rbind(EW_portfolio, data.frame(year = y, month = m, logreturn =mean(temp), returns = mean(exp(temp)-1)))} 
  }
}
#Add the date in the proper format
EW_portfolio$date <- uniqueDats

View(EW_portfolio)

mean(EW_portfolio$returns)*12
#mean(store$annual_returns)
#1)
#here for the annual_logreturn we use mean()*12, as some years do not have 12 months of data
#store <- data.frame()
#for(y in unique(EW_portfolio$year)){
 # temp <- EW_portfolio[EW_portfolio[,'year'] == y, 'logreturn']
  #if(length(temp) > 0){ store <- rbind(store, data.frame(year = y, annual_lrtrn = mean(temp)*12, annual_returns =mean(exp(temp)-1)*12))} 
#}
#View(store)

#logrtrn <- store$annual_lrtrn
#clr <- cumsum(logrtrn)
#clr

logrtrn <- EW_portfolio$logreturn
clr <- cumsum(logrtrn)
clr

#cumulative wealth 
wealth <- exp(clr)

#Plot of cumulative log returns and cumulative wealth for the equal-weighted portfolio
t1 = 'Cumulative log returns'
#plot(store$year,clr, type = 'l',main = t1, xlab='Year', col = "black")  
plot(EW_portfolio$date,clr, type = 'l', main = t1, xlab='Year', col = "black")  
t1 = 'Cumulative wealth'
#plot(store$year,wealth,type = 'l', main = t1 ,col ="green",xlab='Year')
plot(EW_portfolio$date,wealth,type = 'l', main = t1 ,col ="green",xlab='Year')


#Compute mean (for returns)
returns <-   EW_portfolio$returns
EW_rtrn <- mean(returns)*12
EW_rtrn

#Compute std
EW_sd <- sd(returns)*sqrt(12)
EW_sd

#Compute Sharpe Ratio
EW_SharpeRatio <- EW_rtrn/EW_sd
EW_SharpeRatio

resl = data.frame(rbind(c(EW_rtrn,EW_sd,EW_SharpeRatio)))
colnames(resl) = c('return','std','Sharpe') 
rownames(resl) = 'EW_portfolio'
resl

#2)Compute empirical VaR and ES at 0.95
#Computations on aggregate returns of countries (i.e. EW_portfolio)
lossesEmpirical <- -EW_portfolio$returns
srtlEmpirical <- sort(lossesEmpirical)

varEmpirical <- srtlEmpirical[0.95*length(lossesEmpirical)]
eshfEmpirical <- mean(srtlEmpirical[(0.95*length(lossesEmpirical)):length(lossesEmpirical)])



#3)
#Fit a mixture of two student-T distributions
# Step 1: define minus the log-likelihood (minus since we are going to call mle(), which is defined as mle(minuslogl,start,...))
pdfStudent = function(x,m,v,dof){
  s = sqrt(v)                 #because the variable is like y=m+sigma*x so its pdf is like p(x) = p(y)*dy/dx
  y = (x-m)/s
  p = dt(y, df = dof)/s      # notice that we divide by s
  return(p)
}

minusLogPdfMt2 = function(m1,m2,logv1,logv2,logitprob,logdof1,logdof2){
  v1 = exp(logv1)
  v2 = exp(logv2)
  prob = exp(logitprob) / (1+exp(logitprob))
  dof1 = exp(logdof1)
  dof2 = exp(logdof2)
  pdfMt = prob*pdfStudent(x,m1,v1,dof1) + (1-prob)*pdfStudent(x,m2,v2,dof2)
  return(-sum(log(pdfMt)))
}

# Step 2: define starting values
#Preliminary step. R's built-in 'mle' function requires (annoyingly!) the starting values for parameter estimates to be
# given as a named list.
makeNamedList = function(param){
  namedList = list()
  namedList$m1 = as.numeric(param[1])
  namedList$m2 = as.numeric(param[2])
  namedList$logv1 = as.numeric(param[3])
  namedList$logv2 = as.numeric(param[4])
  namedList$logitprob = as.numeric(param[5])
  namedList$logdof1 = as.numeric(param[6])
  namedList$logdof2 = as.numeric(param[7])
  return(namedList)
}

disaggregateParam <- function(mleCoefs){
  coef <- list()
  coef$m1 <- mleCoefs[1]
  coef$m2 <- mleCoefs[2]
  coef$v1 <- exp(mleCoefs[3])
  coef$v2 <- exp(mleCoefs[4])
  coef$prob <- exp(mleCoefs[5])/(1+exp(mleCoefs[5]))
  coef$dof1 <- exp(mleCoefs[6])
  coef$dof2 <- exp(mleCoefs[7])
  return(coef)
}

#initial parameter guess
#(using log returns would have given a better fit)
x <- EW_portfolio$logreturn

mean1 <- mean(x)*12
mean2 <- mean1
var1 <- var(x)*0.5*sqrt(12)
var2 <- var1*4
prob <- 0.8
dof1 <- 7
dof2 <- 7

# Step 3: optimize
# make named list of parameters
param0 <- makeNamedList(c(mean1,mean2,log(var1),log(var2),log(prob/(1-prob)),log(dof1),log(dof2)))
# use SANN (derivatives-free) method to improve starting values for parameters
init <- mle(minusLogPdfMt2, start = param0, method = 'SANN', control = list(maxit = 1000))
# make named list of parameters
param1 <- makeNamedList(init@coef)
# final stage ML estimation
finl <- mle(minusLogPdfMt2, start = param1, method = 'BFGS', control = list(reltol = 1e-8))

resl = disaggregateParam(finl@coef)
annu = c(resl$m1, resl$m2, sqrt(resl$v1), sqrt(resl$v2))
tble = data.frame(c(annu, resl$prob, resl$dof1, resl$dof2))
colnames(tble) = c(paste('MLE estimate of a mixture of two student-t'))
rownames(tble) = c('m1','m2','sqrt(v1)','sqrt(v2)','prob','dof1','dof2')
tble


#Compute the mixture VaR and ES at 0.95
ndraws = 100000 
x1 = sqrt(resl$v1)*rt(ndraws, df = resl$dof1) + resl$m1    # draw ndraws data points from t distribution with mean, variance, and dof from ML estimates
x2 = sqrt(resl$v2)*rt(ndraws, df = resl$dof2) + resl$m2    # draw ndraws data points from t distribution with mean, variance, and dof from ML estimates
u = runif(ndraws) < resl$prob                              # draw ndraws uniformly distributed numbers between 0 and 1 to determine if x comes from x1 or x1 based on the estimated prob from MLE
data_simul = x1*u + x2*(1-u)                               # generate simulated data from mix of the two t-distributions
losses = -data_simul                                       # convert x to losses
srtl = sort(losses)                                        # sort losses from smallest to largest (in order to get simulated VaR and ES)
varMixture = srtl[ndraws*0.95]                         # take sorted loss element number ndraws*conf[i] to represent VaR at conf[i] confidence
eshfMixture= mean(srtl[(ndraws*0.95):length(srtl)])    # take mean of sorted loss elements at and above ndraws*conf[i] to represent ES at conf[i] confidence

resl1 = cbind(varEmpirical, eshfEmpirical, varMixture, eshfMixture)
rownames(resl1) = c('0.95')
colnames(resl1) = c('Empirical VaR','Empirical ES','Mixture VaR', 'Mixture ES')
resl1


#4) Half-Kelly portfolio
alpha = 0.5           # fractional Kelly alpha. e.g. 0.5 for half-Kelly.

f_star = EW_rtrn/EW_sd^2  # optimal allocation (leverage) according to (quadratic approximation to) Kelly criterion
leverage = alpha*f_star # actual position  
leverage

#with these new strategy, here is how the returns and wealth will evolve
Rs <- leverage*EW_portfolio$returns

rs <- log(1 + Rs)
cumul_rs <- cumsum(rs)
V_t <- exp(cumsum(rs))
p     =  cumsum(EW_portfolio$logreturn)            # log price(t) = r(1) + .... + r(t)
P     = exp(p)


t1 = 'Kelly Strat: Asset price (black) and cumulative wealth (green)'
plot(EW_portfolio$date,P, type = 'l',main = t1,col = "black", ylim = c(min(c(V_t,P)),max(c(V_t,P))))
lines(EW_portfolio$date,V_t, col = "green", main = t1)
lines(EW_portfolio$date,rep(1,length(EW_portfolio$year)), col = "black")



#QUESTION 3
#Equal volatility portfolio 
#i) retrieve the data from 'mydt dataframe', column name: 'adjusted_lrtn.1'
mydt$returns <- exp(mydt$adjusted_lrtn)-1
#to ensure that we have the right data for the right date and that NA data are not taken into account
mydt <- mydt[complete.cases(mydt), ]
head(mydt) 

#ii)Compute the weight
#First add the volatility
avgvol <- GlobalEquity[,6]
mydtQ3 <- data.frame(year=year[1:n], month=month[1:n], adjusted_lrtn=adjusted_lrtn, volatility=avgvol) 
#to ensure that we have the right data for the right date and that NA data are not taken into account
mydtQ3 <- mydtQ3[complete.cases(mydtQ3), ]
head(mydtQ3) 

#weight in the dataframe
mydtQ3$EVol_w <- 1/mydtQ3$volatility
head(mydtQ3)

#Normalize weights to sum up to 1 each month
#Create new dataframe as columns do not match 
EV_prtf <- data.frame()
for(y in unique(year)){
  for(m in unique(month)){ 
    temp <- mydtQ3[mydtQ3[,'year'] == y & mydtQ3[,'month'] == m, 'EVol_w']
    if(length(temp) > 0){ EV_prtf <- rbind(EV_prtf, data.frame(year = y, month = m, normalized_EVol_w= temp/sum(temp)))} 
  }
}
head(EV_prtf)


#add the returns
#ATTENTION we need to reorder the returns in the same one as those stored in EV_prtf
ordered_mydt <- mydtQ3[order(mydtQ3$year,mydtQ3$month),]

EV_prtf$log_returns <- ordered_mydt$adjusted_lrtn
head(EV_prtf)




#1)
#New monthly returns for the Equal Volatility Portfolio, i.e. aggregate per country with their respective weights
EV_prtf$weighted_lrtn <- EV_prtf$normalized_EVol_w*EV_prtf$log_returns
head(EV_prtf)

#Here, we sum for the monthly returns as they're already weighted by a factor for each country
EV_portfolio <- data.frame()
for(y in unique(year)){
  for(m in unique(month)){ 
    temp <- EV_prtf[EV_prtf[,'year'] == y & EV_prtf[,'month'] == m, 'weighted_lrtn']
    if(length(temp) > 0){ EV_portfolio <- rbind(EV_portfolio, data.frame(year = y, month = m, monthly_lrtrn= sum(temp), monthly_returns = sum(exp(temp)-1)))} 
  }
}
#Add the date in the proper format
EV_portfolio$date <- uniqueDats
head(EV_portfolio)

#here for the annual_logreturn we use mean()*12, as some years do not have 12 months
#storeQ3 <- data.frame()
#for(y in unique(EV_portfolio$year)){
 # temp <- EV_portfolio[EV_portfolio[,'year'] == y, 'monthly_lrtrn']
  #if(length(temp) > 0){ storeQ3 <- rbind(storeQ3, data.frame(year = y, annual_lrtrn = mean(temp)*12, annual_returns =mean(exp(temp)-1)*12))} 
#}
#View(storeQ3)

#logrtrnQ3 <- storeQ3$annual_lrtrn
#clrQ3 <- cumsum(logrtrnQ3)
#clrQ3

logrtrnQ3 <- EV_portfolio$monthly_lrtrn
clrQ3 <- cumsum(logrtrnQ3)
clrQ3

#cumulative wealth 
wealthQ3 <- exp(clrQ3)

#Plot of cumulative log returns and cumulative wealth for the equal-weighted portfolio
t1 = 'Cumulative log returns'
#plot(storeQ3$year,clrQ3, type = 'l',main = t1, xlab='Year', col = "black")  
plot(EV_portfolio$date,clrQ3, type = 'l',main = t1, xlab='Year', col = "black")  
t1 = 'Cumulative wealth'
#plot(storeQ3$year,wealthQ3,type = 'l', main = t1 ,col ="green",xlab='Year')
plot(EV_portfolio$date,wealthQ3,type = 'l', main = t1 ,col ="green",xlab='Year')

#Conclusion: results are a bit better when targeting the volatility


#Compute mean (for returns)
returnsQ3 <- EV_portfolio$monthly_returns #OR mean(storeQ3$annual_returns)
EV_rtrn <- mean(returnsQ3)*12
EV_rtrn

#Compute std
EV_sd <- sd(returnsQ3)*sqrt(12)
EV_sd

#Compute Sharpe Ratio
EV_SharpeRatio <- EV_rtrn/EV_sd
EV_SharpeRatio

reslQ3 = data.frame(rbind(c(EV_rtrn,EV_sd,EV_SharpeRatio)))
colnames(reslQ3) = c('return','std','Sharpe') 
rownames(reslQ3) = 'EV_portfolio'
reslQ3

#2)Compute empirical VaR and ES at 0.95
#Computations on aggregate returns of countries (i.e. EW_portfolio)
lossesEmpiricalQ3 <- -EV_portfolio$monthly_lrtrn  #OR storeQ3$annual_returns
srtlEmpiricalQ3 <- sort(lossesEmpiricalQ3)

varEmpiricalQ3 <- srtlEmpiricalQ3[0.95*length(lossesEmpiricalQ3)]
eshfEmpiricalQ3 <- mean(srtlEmpiricalQ3[(0.95*length(lossesEmpiricalQ3)):length(lossesEmpiricalQ3)])


#3)
#initial parameter guess
#(using log returns would have given a better fit)
x <- EV_portfolio$monthly_lrtrn

mean1 <- mean(x)*12
mean2 <- mean1
var1 <- var(x)*0.5*sqrt(12)
var2 <- var1*4
prob <- 0.8
dof1 <- 7
dof2 <- 7

# Step 3: optimize
# make named list of parameters
param0 <- makeNamedList(c(mean1,mean2,log(var1),log(var2),log(prob/(1-prob)),log(dof1),log(dof2)))
# use SANN (derivatives-free) method to improve starting values for parameters
init <- mle(minusLogPdfMt2, start = param0, method = 'SANN', control = list(maxit = 1000))
# make named list of parameters
param1 <- makeNamedList(init@coef)
# final stage ML estimation
finl <- mle(minusLogPdfMt2, start = param1, method = 'BFGS', control = list(reltol = 1e-8))

resl = disaggregateParam(finl@coef)
annu = c(resl$m1, resl$m2, sqrt(resl$v1), sqrt(resl$v2))
tble = data.frame(c(annu, resl$prob, resl$dof1, resl$dof2))
colnames(tble) = c(paste('MLE estimate of a mixture of two student-t'))
rownames(tble) = c('m1','m2','sqrt(v1)','sqrt(v2)','prob','dof1','dof2')
tble


#Compute the mixture VaR and ES at 0.95
ndraws = 100000 
x1 = sqrt(resl$v1)*rt(ndraws, df = resl$dof1) + resl$m1    # draw ndraws data points from t distribution with mean, variance, and dof from ML estimates
x2 = sqrt(resl$v2)*rt(ndraws, df = resl$dof2) + resl$m2    # draw ndraws data points from t distribution with mean, variance, and dof from ML estimates
u = runif(ndraws) < resl$prob                              # draw ndraws uniformly distributed numbers between 0 and 1 to determine if x comes from x1 or x1 based on the estimated prob from MLE
data_simul = x1*u + x2*(1-u)                               # generate simulated data from mix of the two t-distributions
losses = -data_simul                                       # convert x to losses
srtl = sort(losses)                                        # sort losses from smallest to largest (in order to get simulated VaR and ES)
varMixture = srtl[ndraws*0.95]                         # take sorted loss element number ndraws*conf[i] to represent VaR at conf[i] confidence
eshfMixture= mean(srtl[(ndraws*0.95):length(srtl)])    # take mean of sorted loss elements at and above ndraws*conf[i] to represent ES at conf[i] confidence

resl1 = cbind(varEmpiricalQ3, eshfEmpiricalQ3, varMixture, eshfMixture)
rownames(resl1) = c('0.95')
colnames(resl1) = c('Empirical VaR','Empirical ES','Mixture VaR', 'Mixture ES')
resl1


#4) Half-Kelly portfolio
alpha = 0.5           # fractional Kelly alpha. e.g. 0.5 for half-Kelly.

f_starQ3 = EV_rtrn/EV_sd^2  # optimal allocation (leverage) according to (quadratic approximation to) Kelly criterion
f_starQ3
leverageQ3 = alpha*f_starQ3 # actual position  
leverageQ3 

leverage

#with these new strategy, here is how the returns and wealth will evolve
RsQ3 <- leverageQ3*EV_portfolio$monthly_returns
rsQ3 <- log(1 + RsQ3)
cumul_rsQ3 <- cumsum(rsQ3)
V_tQ3 <- exp(cumsum(rsQ3))

pQ3     = cumsum(EV_portfolio$monthly_lrtrn)            # log price(t) = r(1) + .... + r(t)
PQ3     = exp(pQ3)

t1 = 'Asset price (black) and cumulative wealth (green)'
plot(EV_portfolio$date,PQ3, type = 'l',main = t1,col = "black",ylim = c(0,max(c(V_tQ3,PQ3))))
lines(EV_portfolio$date,V_tQ3, col = "green", main = t1)
lines(EV_portfolio$date,rep(1,length(EV_portfolio$year)), col = "black")

#Conclusions: leverage Q3 is 0.9 while leverage was initially (Q2) 0.49. 
#That is the reason why we have less wealth (lower than the asset price) than before 




#QUESTION 4
#EW portfolio of the top 1/3 of countries in terms of momentum
#First add the momentum
momentum <- GlobalEquity[,5]
mydtQ4 <- data.frame(date=dats[1:n], year=year[1:n], month=month[1:n], momentum=momentum, adjusted_lrtn=adjusted_lrtn)
#mydtQ4 <- data.frame(date=date[1:n], year=year[1:n], month=month[1:n], momentum=momentum, adjusted_lrtn=excessret)
mydtQ4 <- mydtQ4[complete.cases(mydtQ4), ]
head(mydtQ4) 

#Here we took only the 1/3 highest momentum
training_set <- mydtQ4  %>%
                  group_by(date) %>%
                     arrange(desc(momentum))  %>%
                      slice(1:(1/3*n()))

training_set <- as.data.frame(training_set)

#ReDf <- group_by(training_set, date)
#ReDf <- summarise(ReDf, result=mean(adjusted_lrtn))


#1)
#Aggregate by countries to get the mean of this strategy each month
EW_mom_portfolio <- data.frame()
for(y in unique(year)){
  for(m in sort(unique(month))){ 
      temp <- training_set[training_set[,'year'] == y & training_set[,'month'] == m, 'adjusted_lrtn']
      if(length(temp) > 0){ EW_mom_portfolio <- rbind(EW_mom_portfolio, data.frame(year = y, month = m, monthly_lrtrn= mean(temp), monthly_returns = mean(exp(temp)-1)))}
    }
}
#Add the date in the proper format
EW_mom_portfolio$date <- uniqueDats
head(EW_mom_portfolio)

#here for the annual_logreturn we use mean()*12, as some years do not have 12 months
#storeQ4 <- data.frame()
#for(y in unique(EW_mom_portfolio$year)){
 # temp <- EW_mom_portfolio[EW_mom_portfolio[,'year'] == y, 'monthly_lrtrn']
  #if(length(temp) > 0){ storeQ4 <- rbind(storeQ4, data.frame(year = y, annual_lrtrn = mean(temp)*12, annual_returns =mean(exp(temp)-1)*12))} 
#}
#View(storeQ4)

#logrtrnQ4 <- storeQ4$annual_lrtrn
#clrQ4 <- cumsum(logrtrnQ4)
#clrQ4

logrtrnQ4 <- EW_mom_portfolio$monthly_lrtrn
clrQ4 <- cumsum(logrtrnQ4)
clrQ4

#cumulative wealth 
wealthQ4 <- exp(clrQ4)

#Plot of cumulative log returns and cumulative wealth for the equal-weighted portfolio
t1 = 'Cumulative log returns'
#plot(storeQ4$year,clrQ4, type = 'l',main = t1, xlab='Year', col = "black")  
plot(EW_mom_portfolio$date,clrQ4, type = 'l',main = t1, xlab='Year', col = "black")  
t1 = 'Cumulative wealth'
#plot(storeQ4$year,wealthQ4,type = 'l', main = t1 ,col ="green",xlab='Year')
plot(EW_mom_portfolio$date,wealthQ4,type = 'l', main = t1 ,col ="green",xlab='Year')

#Conclusion: results are way higher than previous strategies. It show us the 
#Regarding the astonishing cumulative wealth we can mention the power compounding and the lower performance result of momentum in the last few years


#Compute mean (for returns)
returnsQ4 <- EW_mom_portfolio$monthly_returns #OR mean(storeQ4$annual_returns)
EV_rtrnQ4 <- mean(returnsQ4)*12
EV_rtrnQ4

#Compute std
EV_sdQ4 <- sd(returnsQ4)*sqrt(12)
EV_sdQ4

#Compute Sharpe Ratio
EV_SharpeRatioQ4 <- EV_rtrnQ4/EV_sdQ4
EV_SharpeRatioQ4

reslQ4 = data.frame(rbind(c(EV_rtrnQ4,EV_sdQ4,EV_SharpeRatioQ4)))
colnames(reslQ4) = c('return','std','Sharpe') 
rownames(reslQ4) = 'EW momentum portfolio'
reslQ4


#2)Compute empirical VaR and ES at 0.95
#Computations on aggregate returns of countries (i.e. EW_portfolio)
lossesEmpiricalQ4 <- -EW_mom_portfolio$monthly_lrtrn  #OR storeQ3$annual_returns
srtlEmpiricalQ4 <- sort(lossesEmpiricalQ4)

varEmpiricalQ4 <- srtlEmpiricalQ4[0.95*length(lossesEmpiricalQ4)]
eshfEmpiricalQ4 <- mean(srtlEmpiricalQ4[(0.95*length(lossesEmpiricalQ4)):length(lossesEmpiricalQ4)])



#3)Fit a mixture of 2-student distributions
#initial parameter guess
#(using log returns would have given a better fit)
x <- EW_mom_portfolio$monthly_lrtrn

mean1 <- mean(x)*12
mean2 <- mean1
var1 <- var(x)*0.5*sqrt(12)
var2 <- var1*4
prob <- 0.8
dof1 <- 7
dof2 <- 7

# Step 3: optimize
# make named list of parameters
param0 <- makeNamedList(c(mean1,mean2,log(var1),log(var2),log(prob/(1-prob)),log(dof1),log(dof2)))
# use SANN (derivatives-free) method to improve starting values for parameters
init <- mle(minusLogPdfMt2, start = param0, method = 'SANN', control = list(maxit = 1000))
# make named list of parameters
param1 <- makeNamedList(init@coef)
# final stage ML estimation
finl <- mle(minusLogPdfMt2, start = param1, method = 'BFGS', control = list(reltol = 1e-8))

resl = disaggregateParam(finl@coef)
annu = c(resl$m1, resl$m2, sqrt(resl$v1), sqrt(resl$v2))
tble = data.frame(c(annu, resl$prob, resl$dof1, resl$dof2))
colnames(tble) = c(paste('MLE estimate of a mixture of two student-t'))
rownames(tble) = c('m1','m2','sqrt(v1)','sqrt(v2)','prob','dof1','dof2')
tble


#Compute the mixture VaR and ES at 0.95
ndraws = 100000 
x1 = sqrt(resl$v1)*rt(ndraws, df = resl$dof1) + resl$m1    # draw ndraws data points from t distribution with mean, variance, and dof from ML estimates
x2 = sqrt(resl$v2)*rt(ndraws, df = resl$dof2) + resl$m2    # draw ndraws data points from t distribution with mean, variance, and dof from ML estimates
u = runif(ndraws) < resl$prob                              # draw ndraws uniformly distributed numbers between 0 and 1 to determine if x comes from x1 or x1 based on the estimated prob from MLE
data_simul = x1*u + x2*(1-u)                               # generate simulated data from mix of the two t-distributions
losses = -data_simul                                       # convert x to losses
srtl = sort(losses)                                        # sort losses from smallest to largest (in order to get simulated VaR and ES)
varMixture = srtl[ndraws*0.95]                         # take sorted loss element number ndraws*conf[i] to represent VaR at conf[i] confidence
eshfMixture= mean(srtl[(ndraws*0.95):length(srtl)])    # take mean of sorted loss elements at and above ndraws*conf[i] to represent ES at conf[i] confidence

resl1 = cbind(varEmpiricalQ4, eshfEmpiricalQ4, varMixture, eshfMixture)
rownames(resl1) = c('0.95')
colnames(resl1) = c('Empirical VaR','Empirical ES','Mixture VaR', 'Mixture ES')
resl1


#4) Half-Kelly portfolio
alpha = 0.5           # fractional Kelly alpha. e.g. 0.5 for half-Kelly.

f_starQ4 = EV_rtrnQ4/EV_sdQ4^2  # optimal allocation (leverage) according to (quadratic approximation to) Kelly criterion
f_starQ4
leverageQ4 = alpha*f_starQ4 # actual position  
leverageQ4

#with these new strategy, here is how the returns and wealth will evolve
RsQ4 <- leverageQ4*EW_mom_portfolio$monthly_returns
rsQ4 <- log(1 + RsQ4)
cumul_rsQ4 <- cumsum(rsQ4)
V_tQ4 <- exp(cumsum(rsQ4))

pQ4     = cumsum(EW_mom_portfolio$monthly_lrtrn)            # log price(t) = r(1) + .... + r(t)
PQ4     = exp(pQ4)

t1 = 'Asset price (black) and cumulative wealth (green)'
plot(EW_mom_portfolio$date,PQ4, type = 'l',main = t1,col = "black",ylim = c(0,max(c(V_tQ4,PQ4))))
lines(EW_mom_portfolio$date,V_tQ4, col = "green", main = t1)
lines(EW_mom_portfolio$date,rep(1,length(EV_portfolio$year)), col = "black")

#Conclusions: leverage Q3 is 0.9 while levarage was initially (Q2) 0.49. 
#Here we have a crazy amount of leverage (Q4) 4.65 , as such it undermines our final wealth 
#The asset price however has increased exponentially and a more cautious approach would have been more efficient





#QUESTION 5
#Equal-volatility portfolio of the top 1/3 of countries in terms of momentum
#First add the volatility
avgvol <- GlobalEquity[,6]

mydtQ5 <- data.frame(date=dats[1:n], year=year[1:n], month=month[1:n], momentum=momentum, volatility=avgvol, adjusted_lrtn=adjusted_lrtn)
mydtQ5 <- mydtQ5[complete.cases(mydtQ5), ]
head(mydtQ5) 

training_setQ5 <- mydtQ5  %>%
                      group_by(date) %>%
                            arrange(desc(momentum))  %>%
                                    slice(1:(1/3*n()))

training_setQ5 <- as.data.frame(training_setQ5)
head(training_setQ5)

#weight in the new training set dataframe
training_setQ5$EVol_w <- 1/training_setQ5$avgvol
head(training_setQ5)


#Normalize weights to sum up to 1 each month
#Create new dataframe as columns do not match 
EV_mom_prtf <- data.frame()
for(y in unique(year)){
  for(m in unique(month)){ 
    temp <- training_setQ5[training_setQ5[,'year'] == y & training_setQ5[,'month'] == m, 'EVol_w']
    if(length(temp) > 0){ EV_mom_prtf <- rbind(EV_mom_prtf, data.frame(year = y, month = m, normalized_EVol_w= temp/sum(temp)))} 
  }
}
head(EV_mom_prtf)

#add the returns
#ATTENTION we need to reorder the returns in the same one as those stored in EV_prtf
ordered_mydtQ5 <- training_setQ5[order(training_setQ5$year,training_setQ5$month),]

EV_mom_prtf$log_returns <- ordered_mydtQ5$adjusted_lrtn
head(EV_mom_prtf)

#1)
#New monthly returns for the Equal Volatility Portfolio, i.e. aggregate per country with their respective weights
EV_mom_prtf$weighted_lrtn <- EV_mom_prtf$normalized_EVol_w*EV_mom_prtf$log_returns
head(EV_mom_prtf)

#Here, we sum for the monthly returns as they're already weighted by a factor for each country
EV_mom_portfolio <- data.frame()
for(y in unique(year)){
  for(m in unique(month)){ 
    temp <- EV_mom_prtf[EV_mom_prtf[,'year'] == y & EV_mom_prtf[,'month'] == m, 'weighted_lrtn']
    if(length(temp) > 0){ EV_mom_portfolio <- rbind(EV_mom_portfolio, data.frame(year = y, month = m, monthly_lrtrn= sum(temp), monthly_returns = sum(exp(temp)-1)))} 
  }
}
#Add the date in the proper format
EV_mom_portfolio$date <- uniqueDats
head(EV_mom_portfolio)

#here for the annual_logreturn we use mean()*12, as some years do not have 12 months
#storeQ5 <- data.frame()
#for(y in unique(EV_mom_portfolio$year)){
 # temp <- EV_mom_portfolio[EV_mom_portfolio[,'year'] == y, 'monthly_lrtrn']
  #if(length(temp) > 0){ storeQ5 <- rbind(storeQ5, data.frame(year = y, annual_lrtrn = mean(temp)*12, annual_returns =mean(exp(temp)-1)*12))} 
#}
#View(storeQ5)

#logrtrnQ5 <- storeQ5$annual_lrtrn
#clrQ5 <- cumsum(logrtrnQ5)
#clrQ5

logrtrnQ5 <- EV_mom_portfolio$monthly_lrtrn
clrQ5 <- cumsum(logrtrnQ5)
clrQ5

#cumulative wealth 
wealthQ5 <- exp(clrQ5)

#Plot of cumulative log returns and cumulative wealth for the equal-weighted portfolio
t1 = 'Cumulative log returns'
#plot(storeQ5$year,clrQ5, type = 'l',main = t1, xlab='Year', col = "black") 
plot(EV_mom_portfolio$date,clrQ5, type = 'l',main = t1, xlab='Year', col = "black")  
t1 = 'Cumulative wealth'
#plot(storeQ5$year,wealthQ5,type = 'l', main = t1 ,col ="green",xlab='Year')
plot(EV_mom_portfolio$date,wealthQ5,type = 'l', main = t1 ,col ="green",xlab='Year')
#Conclusion: results are a bit lower (surprisingly?) than when we only use momentum as the guideline of the portfolio strategy
#However, volatility will probably be lower than our Sharpe Ratio will be improved in the end 



#Compute mean (for returns)
returnsQ5 <- EV_mom_portfolio$monthly_returns #OR storeQ3$annual_returns 
EV_rtrnQ5 <- mean(returnsQ5)*12
EV_rtrnQ5

#Compute std
EV_sdQ5 <- sd(returnsQ5)*sqrt(12)
EV_sdQ5

#Compute Sharpe Ratio
EV_SharpeRatioQ5 <- EV_rtrnQ5/EV_sdQ5
EV_SharpeRatioQ5

EV_SharpeRatioQ4

reslQ5 = data.frame(rbind(c(EV_rtrnQ5,EV_sdQ5,EV_SharpeRatioQ5)))
colnames(reslQ5) = c('return','std','Sharpe') 
rownames(reslQ5) = 'EV_portfolio'
reslQ5



#2)Compute empirical VaR and ES at 0.95
#Computations on aggregate returns of countries (i.e. EW_portfolio)
lossesEmpiricalQ5 <- -EV_mom_portfolio$monthly_lrtrn  #OR storeQ3$annual_returns
srtlEmpiricalQ5 <- sort(lossesEmpiricalQ5)

varEmpiricalQ5 <- srtlEmpiricalQ5[0.95*length(lossesEmpiricalQ5)]
eshfEmpiricalQ5 <- mean(srtlEmpiricalQ5[(0.95*length(lossesEmpiricalQ5)):length(lossesEmpiricalQ5)])



#3)
#initial parameter guess
#(using log returns would have given a better fit)
x <- EV_mom_portfolio$monthly_lrtrn

mean1 <- mean(x)*12
mean2 <- mean1
var1 <- var(x)*0.5*sqrt(12)
var2 <- var1*4
prob <- 0.8
dof1 <- 7
dof2 <- 7

# Step 3: optimize
# make named list of parameters
param0 <- makeNamedList(c(mean1,mean2,log(var1),log(var2),log(prob/(1-prob)),log(dof1),log(dof2)))
# use SANN (derivatives-free) method to improve starting values for parameters
init <- mle(minusLogPdfMt2, start = param0, method = 'SANN', control = list(maxit = 1000))
# make named list of parameters
param1 <- makeNamedList(init@coef)
# final stage ML estimation
finl <- mle(minusLogPdfMt2, start = param1, method = 'BFGS', control = list(reltol = 1e-8))

resl = disaggregateParam(finl@coef)
annu = c(resl$m1, resl$m2, sqrt(resl$v1), sqrt(resl$v2))
tble = data.frame(c(annu, resl$prob, resl$dof1, resl$dof2))
colnames(tble) = c(paste('MLE estimate of a mixture of two student-t'))
rownames(tble) = c('m1','m2','sqrt(v1)','sqrt(v2)','prob','dof1','dof2')
tble


#Compute the mixture VaR and ES at 0.95
ndraws = 100000 
x1 = sqrt(resl$v1)*rt(ndraws, df = resl$dof1) + resl$m1    # draw ndraws data points from t distribution with mean, variance, and dof from ML estimates
x2 = sqrt(resl$v2)*rt(ndraws, df = resl$dof2) + resl$m2    # draw ndraws data points from t distribution with mean, variance, and dof from ML estimates
u = runif(ndraws) < resl$prob                              # draw ndraws uniformly distributed numbers between 0 and 1 to determine if x comes from x1 or x1 based on the estimated prob from MLE
data_simul = x1*u + x2*(1-u)                               # generate simulated data from mix of the two t-distributions
losses = -data_simul                                       # convert x to losses
srtl = sort(losses)                                        # sort losses from smallest to largest (in order to get simulated VaR and ES)
varMixture = srtl[ndraws*0.95]                         # take sorted loss element number ndraws*conf[i] to represent VaR at conf[i] confidence
eshfMixture= mean(srtl[(ndraws*0.95):length(srtl)])    # take mean of sorted loss elements at and above ndraws*conf[i] to represent ES at conf[i] confidence

resl1 = cbind(varEmpiricalQ5, eshfEmpiricalQ5, varMixture, eshfMixture)
rownames(resl1) = c('0.95')
colnames(resl1) = c('Empirical VaR','Empirical ES','Mixture VaR', 'Mixture ES')
resl1



#4) Half-Kelly portfolio
alpha = 0.5           # fractional Kelly alpha. e.g. 0.5 for half-Kelly.

f_starQ5 = EV_rtrn/EV_sd^2  # optimal allocation (leverage) according to (quadratic approximation to) Kelly criterion
f_starQ5
leverageQ5 = alpha*f_starQ5 # actual position  
leverageQ5 

#with these new strategy, here is how the returns and wealth will evolve
RsQ5 <- leverageQ5*EV_mom_portfolio$monthly_returns
rsQ5 <- log(1 + RsQ5)
cumul_rsQ5 <- cumsum(rsQ5)
V_tQ5 <- exp(cumsum(rsQ5))

pQ5    = cumsum(EV_mom_portfolio$monthly_lrtrn)            # log price(t) = r(1) + .... + r(t)
PQ5     = exp(pQ5)

t1 = 'Asset price (black) and cumulative wealth (green)'
plot(EV_mom_portfolio$date,PQ5, type = 'l',main = t1,col = "black",ylim = c(0,max(c(V_tQ5,PQ5))))
lines(EV_mom_portfolio$date,V_tQ5, col = "green", main = t1)
lines(EV_mom_portfolio$date,rep(1,length(EV_mom_portfolio$year)), col = "black")

#Conclusions: leverage Q3 is 0.9 and leverage Q4 (momentum strategy) is 4.65 while our leverage (Q5) is now 0.58
#It seems that we have less wealth than before when we only targeted momentum (Q4)






#QUESTION 6 
#Create your own strategy
#We decided to implement a strategy based on picking the second-4th of logCAPE markets (by increasing order) and take a long position upon them. 
#By taking low values we can gain higher returns given the fact that stocks are probably undervalued.
#The resulting portfolio is a long-short one to increase even more diversification and gain higher exposure on excess returns even if market prices are falling 

logCAPE <- GlobalEquity[,4]

mydtQ6 <- data.frame(date=dats[1:n], year=year[1:n], month=month[1:n], momentum=momentum, volatility=avgvol, logCAPE=logCAPE, adjusted_lrtn=adjusted_lrtn)
mydtQ6 <- mydtQ6[complete.cases(mydtQ6), ]
head(mydtQ6) 

#First, remove the lowest momentum for the long strategy (https://www.investorschronicle.co.uk/2015/02/06/tips-and-ideas/market-tactics/the-cape-factor-WIUbSkdnHLsZBzxiMIQr2M/article.html)
mydtQ6_long <- mydtQ6  %>%
  group_by(date) %>%
  arrange(desc(momentum))  %>%
  slice(1:(2/3*n()))

#Then, we remove the highest momentum for the short strategy
mydtQ6_short <- mydtQ6  %>%
  group_by(date) %>%
  arrange(desc(momentum))  %>%
  slice(round((1/3*n())):round((3/3*n())))



#Sort the dataframe by highest and lowest logCAPE value
long_position <- mydtQ6_long  %>%
  group_by(date) %>%
  arrange(logCAPE)  %>%
  slice(round((1/4)*n()):round((2/4)*n()))

long_position <- as.data.frame(long_position)


short_position <- mydtQ6_short  %>%
  group_by(date) %>%
  arrange(logCAPE)  %>%
  slice(round((2/4)*n()):round((3/4)*n()))

short_position <- as.data.frame(short_position)

#Aggregate each portfolio 
long_prtf <- data.frame()
for(y in unique(year)){
  for(m in unique(month)){ 
    temp <- long_position[long_position[,'year'] == y & long_position[,'month'] == m, 'adjusted_lrtn']
    if(length(temp) > 0){ long_prtf <- rbind(long_prtf, data.frame(year = y, month = m, monthly_lrtrn= mean(temp)))} 
  }
}
head(long_prtf)

short_prtf <- data.frame()
for(y in unique(year)){
  for(m in unique(month)){ 
    temp <- short_position[short_position[,'year'] == y & short_position[,'month'] == m, 'adjusted_lrtn']
    if(length(temp) > 0){ short_prtf <- rbind(short_prtf, data.frame(year = y, month = m, monthly_lrtrn= mean(temp)))} 
  }
}
head(short_prtf)

#Create the long-short strategy/portfolio
long_short_portfolio <- data.frame(year=long_prtf$year, month=long_prtf$month ,monthly_returns = exp(long_prtf$monthly_lrtrn)-1-exp(short_prtf$monthly_lrtrn)+1, monthly_lrtrn=long_prtf$monthly_lrtrn - short_prtf$monthly_lrtrn)
#Add the date in the proper format
long_short_portfolio$date <- uniqueDats


#1)
#Here for the annual_logreturn we use mean()*12, as some years do not have 12 months
#storeQ6 <- data.frame()
#for(y in unique(long_short_portfolio$year)){
 # temp <- long_short_portfolio[long_short_portfolio[,'year'] == y, 'monthly_lrtrn']
  #if(length(temp) > 0){ storeQ6 <- rbind(storeQ6, data.frame(year = y, annual_lrtrn = mean(temp)*12, annual_returns =mean(exp(temp)-1)*12))} 
#}
#View(storeQ6)

#logrtrnQ6 <- storeQ6$annual_lrtrn
#clrQ6 <- cumsum(logrtrnQ6)
#clrQ6

logrtrnQ6 <- long_short_portfolio$monthly_lrtrn
clrQ6 <- cumsum(logrtrnQ6)
clrQ6

#cumulative wealth 
wealthQ6 <- exp(clrQ6)

#Plot of cumulative log returns and cumulative wealth for the long-short portfolio
t1 = 'Cumulative log returns'
#plot(storeQ6$year,clrQ6, type = 'l',main = t1, xlab='Year', col = "black")  
plot(long_short_portfolio$date,clrQ6, type = 'l',main = t1, xlab='Year', col = "black")  
t1 = 'Cumulative wealth'
#plot(storeQ6$year,wealthQ6,type = 'l', main = t1 ,col ="green",xlab='Year')
plot(long_short_portfolio$date,wealthQ6,type = 'l', main = t1 ,col ="green",xlab='Year')
#Conclusion: results are a bit lower (surprisingly?) than when we only use momentum as the guideline of the portfolio strategy
#However, volatility will probably be lower than our Sharpe Ratio will be improved in the end 



#Compute mean (for returns)
returnsQ6 <- long_short_portfolio$monthly_returns #OR mean(storeQ6$annual_returns)
long_short_rtrnQ6 <- mean(returnsQ6)*12
long_short_rtrnQ6

#Compute std
long_short_sdQ6 <- sd(returnsQ6)*sqrt(12)
long_short_sdQ6

#Compute Sharpe Ratio
long_short_SharpeRatioQ6 <- long_short_rtrnQ6/long_short_sdQ6
long_short_SharpeRatioQ6

EV_SharpeRatioQ4

reslQ6 = data.frame(rbind(c(long_short_rtrnQ6,long_short_sdQ6,long_short_SharpeRatioQ6)))
colnames(reslQ6) = c('return','std','Sharpe') 
rownames(reslQ6) = 'EV_portfolio'
reslQ6



#2)Compute empirical VaR and ES at 0.95
#Computations on aggregate returns of countries (i.e. EW_portfolio)
lossesEmpiricalQ6 <- -long_short_portfolio$monthly_lrtrn  #OR storeQ6$annual_returns
srtlEmpiricalQ6 <- sort(lossesEmpiricalQ6)

varEmpiricalQ6 <- srtlEmpiricalQ6[0.95*length(lossesEmpiricalQ6)]
eshfEmpiricalQ6 <- mean(srtlEmpiricalQ6[(0.95*length(lossesEmpiricalQ6)):length(lossesEmpiricalQ6)])



#3)
#initial parameter guess
#(using log returns would have given a better fit)
x <- long_short_portfolio$monthly_lrtrn

mean1 <- mean(x)*12
mean2 <- mean1
var1 <- var(x)*0.5*sqrt(12)
var2 <- var1*4
prob <- 0.8
dof1 <- 7
dof2 <- 7

# Step 3: optimize
# make named list of parameters
param0 <- makeNamedList(c(mean1,mean2,log(var1),log(var2),log(prob/(1-prob)),log(dof1),log(dof2)))
# use SANN (derivatives-free) method to improve starting values for parameters
init <- mle(minusLogPdfMt2, start = param0, method = 'SANN', control = list(maxit = 1000))
# make named list of parameters
param1 <- makeNamedList(init@coef)
# final stage ML estimation
finl <- mle(minusLogPdfMt2, start = param1, method = 'BFGS', control = list(reltol = 1e-8))

resl = disaggregateParam(finl@coef)
annu = c(resl$m1, resl$m2, sqrt(resl$v1), sqrt(resl$v2))
tble = data.frame(c(annu, resl$prob, resl$dof1, resl$dof2))
colnames(tble) = c(paste('MLE estimate of a mixture of two student-t'))
rownames(tble) = c('m1','m2','sqrt(v1)','sqrt(v2)','prob','dof1','dof2')
tble


#Compute the mixture VaR and ES at 0.95
ndraws = 100000 
x1 = sqrt(resl$v1)*rt(ndraws, df = resl$dof1) + resl$m1    # draw ndraws data points from t distribution with mean, variance, and dof from ML estimates
x2 = sqrt(resl$v2)*rt(ndraws, df = resl$dof2) + resl$m2    # draw ndraws data points from t distribution with mean, variance, and dof from ML estimates
u = runif(ndraws) < resl$prob                              # draw ndraws uniformly distributed numbers between 0 and 1 to determine if x comes from x1 or x1 based on the estimated prob from MLE
data_simul = x1*u + x2*(1-u)                               # generate simulated data from mix of the two t-distributions
losses = -data_simul                                       # convert x to losses
srtl = sort(losses)                                        # sort losses from smallest to largest (in order to get simulated VaR and ES)
varMixture = srtl[ndraws*0.95]                         # take sorted loss element number ndraws*conf[i] to represent VaR at conf[i] confidence
eshfMixture= mean(srtl[(ndraws*0.95):length(srtl)])    # take mean of sorted loss elements at and above ndraws*conf[i] to represent ES at conf[i] confidence

resl1 = cbind(varEmpiricalQ6, eshfEmpiricalQ6, varMixture, eshfMixture)
rownames(resl1) = c('0.95')
colnames(resl1) = c('Empirical VaR','Empirical ES','Mixture VaR', 'Mixture ES')
resl1



#4) Half-Kelly portfolio
alpha = 0.5           # fractional Kelly alpha. e.g. 0.5 for half-Kelly.

f_starQ6 = long_short_rtrnQ6/long_short_sdQ6^2  # optimal allocation (leverage) according to (quadratic approximation to) Kelly criterion
f_starQ6
leverageQ6 = alpha*f_starQ6 # actual position  
leverageQ6 

#with these new strategy, here is how the returns and wealth will evolve
RsQ6 <- leverageQ6*long_short_portfolio$monthly_returns
rsQ6 <- log(1 + RsQ6)
cumul_rsQ6 <- cumsum(rsQ6)
V_tQ6 <- exp(cumsum(rsQ6))

pQ6    = cumsum(long_short_portfolio$monthly_lrtrn)            # log price(t) = r(1) + .... + r(t)
PQ6     = exp(pQ6)

t1 = 'Asset price (black) and cumulative wealth (green)'
plot(long_short_portfolio$date,PQ6, type = 'l',main = t1,col = "black",ylim = c(0,max(c(V_tQ6,PQ6))))
lines(long_short_portfolio$date,V_tQ6, col = "green", main = t1)
lines(long_short_portfolio$date,rep(1,length(long_short_portfolio$year)), col = "black")

#Conclusions: leverage Q3 is 0.9 and leverage Q4 (momentum strategy) is 4.65 while our leverage (Q5) is now 0.58
#It seems that we have less wealth than before when we only targeted momentum (Q4)
leverageQ4





