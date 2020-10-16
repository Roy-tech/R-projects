# Crash and TaxAvoidance

#------------------------------------------------
# Step 1: Prepare Data to Compute Crash, Sigma, Dturn
#------------------------------------------------

rm(list=ls()) # To remove whole objects in the environment

library(tidyverse)
library(readxl)
library(lubridate)

??lubricate

file <-'D:/Crash Project for Econometrics/Crash_FHOL.xlsm'
Static <- read_excel(file,sheet='Static',col_names=T, skip=1, col_types='text', na='.') # skip means skipping first line
nstock <- length(Static$RIC)

?read_excel

# RI: Return index (price+dividend reinvestment)

RI <- read_excel(file, sheet='RI', col_names=F, col_types='numeric', skip=6, na='.') 

names(RI) <- c('date',Static$RIC)

RI$date <- as.Date(RI$date, origin = "1899-12-30") # reference Excel date

?as.Date
  
# from wide format to long format
# cols = 1+(1:nstock) because we are taking 2nd column to 121th, adding 1 to both indicates, it starts from second and there are 120 columns


RI_long <- pivot_longer(RI, 1+(1:nstock), names_to = 'stock', values_to= 'RI') %>% arrange(stock,date) 

# MV: Market capitalisation

MV <- read_excel(file,sheet='MV', col_names=F, col_types='numeric', skip=6, na='.') 

names(MV) <- c('date',Static$RIC)

MV$date <- as.Date(MV$date, origin = "1899-12-30") # reference Excel date

# from wide format to long format

MV_long <- pivot_longer(MV, 1+(1:nstock), names_to='stock', values_to='MV') %>% arrange(stock,date)

# Left join Ri_long and MV_long

RIMV_long <- left_join(RI_long, MV_long, by = c('stock','date')) 

# Calculate weekly return: Individual stock

ret <- RIMV_long %>% group_by(stock) %>% 
  mutate(ret = RI/ lag(RI)-1, ret = ifelse(ret > 100 | ret <= -1, NA, ret))


# Calculate weekly return: market-cap weighted market return

rm <- ret %>% group_by(date) %>% summarise(rm = weighted.mean(ret,MV,na.rm=T)) %>%
      mutate(lag1rm=lag(rm),lag2rm=lag(rm,2),lead1rm=lead(rm),lead2rm=lead(rm,2))


# Left join ret and rm

retrm <- left_join(ret,rm,by='date')

# monthly obs: dturn = VO/NOSH

# VO: volume (no. of shares) 

VO <- read_excel(file, sheet='VO', col_names=F, col_types='numeric', skip= 6, na='.')

names(VO) <- c('date',Static$RIC)

VO$date<-as.Date(VO$date, origin = "1899-12-30") # reference Excel date

# from wide format to long format

VO_long <- pivot_longer(VO,1+(1:nstock),names_to='stock',values_to='VO') %>% arrange(stock,date)

# NOSH: no. of share outstanding (no. of shares) 

NOSH <- read_excel(file, sheet='NOSH', col_names=F, col_types='numeric', skip=6, na='.') 

names(NOSH) <- c('date', Static$RIC)

NOSH$date <- as.Date(NOSH$date, origin = "1899-12-30") # reference Excel date

# from wide format to long format

NOSH_long <- pivot_longer(NOSH,1+(1:nstock),names_to='stock',values_to='NOSH') %>% arrange(stock,date)

# Left join VO and NOSH
dturn <- left_join(VO_long,NOSH_long,by=c('stock','date')) %>% 
  mutate(dturn=VO/NOSH)

#------------------------------------------------
# Step 2: Compute Crash, Sigma, Dturn, ret, size
#------------------------------------------------

data <- crossing(Static$RIC,1994:2020) %>% 
  mutate(crash=NA,sigma=NA,nweek=NA,dturn=NA,nmonth=NA,
         ret=NA,size=NA,MV=NA,ncskew=NA)

names(data)[1:2] <- c('stock','year')
nobs <- length(data$year)
for(i in 1:nobs){

  # Show progress on the screen
  if(i %% 100==0) {cat(paste0( i," out of ",nobs, "\n"))}
    
  
  # filter rows
  tmp <- retrm %>% filter( stock==data$stock[i] & year(date)==data$year[i] )
  nweekreg <-  length(which(is.finite(tmp$ret)))
  data$nweek[i] <- nweekreg
  data$sigma[i] <- sd(tmp$ret,na.rm=T)
  tmpdturn <- dturn %>% filter(stock==data$stock[i] & year(date)==data$year[i])
  data$nmonth[i] <-  length(which(is.finite(tmpdturn$dturn)))
  data$dturn[i] <- mean(tmpdturn$dturn,na.rm=T)
  data$ret[i] <- mean(tmp$ret,na.rm=T)
  data$size[i] <- median(log(tmp$MV),na.rm=T)
  data$MV[i] <- median(tmp$MV,na.rm=T)
  
  
  if (nweekreg<12){next}
  reg <- lm(ret~lag2rm+lag1rm+rm+lead1rm+lead2rm,data=tmp)
  w<-log(1+reg$residuals) 
  w1<-w[is.finite(w)]
  cutoff<- mean(w1)-3.2*sd(w1)
  if (any(w1 < cutoff ) ){data$crash[i]<-1}
  if (all(w1 > cutoff ) ){data$crash[i]<-0}
  n<-nweekreg
  data$ncskew[i] <- -(n*((n-1)^1.5) * sum(w1^3))/ ((n-1)*(n-2)*(sum(w1^2)^1.5))
  
}

#------------------------------------------------
# Step 3: Compute TaxAvoidance and other controls
#------------------------------------------------

# IncomeTax(WC01451)
WC01451 <- read_excel(file,sheet='WC01451',col_names=F,col_types='numeric',skip=6,na='.') 
names(WC01451) <- c('year',Static$RIC)
# from wide format to long format
WC01451_long <- pivot_longer(WC01451,1+(1:nstock),names_to='stock',values_to='WC01451') %>% arrange(stock,year)

# PretaxIncome (WC01401) 
WC01401 <- read_excel(file,sheet='WC01401',col_names=F,col_types='numeric',skip=6,na='.') 
names(WC01401) <- c('year',Static$RIC)
# from wide format to long format
WC01401_long <- pivot_longer(WC01401,1+(1:nstock),names_to='stock',values_to='WC01401') %>% arrange(stock,year)

# BookValue (WC03501)
WC03501 <- read_excel(file,sheet='WC03501',col_names=F,col_types='numeric',skip=6,na='.') 
names(WC03501) <- c('year',Static$RIC)
# from wide format to long format
WC03501_long <- pivot_longer(WC03501,1+(1:nstock),names_to='stock',values_to='WC03501') %>% arrange(stock,year)

# LTDebt (WC03251) 
WC03251 <- read_excel(file,sheet='WC03251',col_names=F,col_types='numeric',skip=6,na='.') 
names(WC03251) <- c('year',Static$RIC)
# from wide format to long format
WC03251_long <- pivot_longer(WC03251,1+(1:nstock),names_to='stock',values_to='WC03251') %>% arrange(stock,year)

# TotalAsset (WC02999) 
WC02999 <- read_excel(file,sheet='WC02999',col_names=F,col_types='numeric',skip=6,na='.') 
names(WC02999) <- c('year',Static$RIC)
# from wide format to long format
WC02999_long <- pivot_longer(WC02999,1+(1:nstock),names_to='stock',values_to='WC02999') %>% arrange(stock,year)

# NetIncome (WC01651) 
WC01651 <- read_excel(file,sheet='WC01651',col_names=F,col_types='numeric',skip=6,na='.') 
names(WC01651) <- c('year',Static$RIC)
# from wide format to long format
WC01651_long <- pivot_longer(WC01651,1+(1:nstock),names_to='stock',values_to='WC01651') %>% arrange(stock,year)

# Join Many Tables
FS <- WC01451_long %>%
  left_join(WC01401_long, by=c('stock','year')) %>%
  left_join(WC03501_long, by=c('stock','year')) %>%
  left_join(WC03251_long, by=c('stock','year')) %>%
  left_join(WC02999_long, by=c('stock','year')) %>%
  left_join(WC01651_long, by=c('stock','year')) %>%
  mutate(LRETR=ifelse(WC01401!=0, WC01451/WC01401, NA),
         LEV=ifelse(WC02999>0,WC03251/WC02999,NA),
         ROA=ifelse(WC02999>0,WC01651/WC02999,NA)) %>%
  select(stock,year,LRETR,WC03501,LEV,ROA)

# Left join data and FS
data1 <- left_join(data,FS, by=c('stock','year'))  %>% 
  mutate(MB=ifelse(WC03501>0,MV*10^3/WC03501,NA)) 
  

#------------------------------------------------
# Step 4: T1: Descriptive Stats of Crash (Ref: T1 in Kim,Lee,Zhang 2011JFE)
#------------------------------------------------
# create lagged variable
data2 <- data1 %>% group_by(stock) %>% 
  mutate(lagLRETR=lag(LRETR),
         lagdturn=lag(dturn),
         lagsigma=lag(sigma),
         lagret=lag(ret),
         lagsize=lag(size),
         lagLEV=lag(LEV),
         lagROA=lag(ROA),
         lagMB=lag(MB))

t1 <- data2 %>% group_by(year) %>% 
  summarise(nfirm=sum(crash>=0,na.rm=T),
            ncrash=sum(crash==1,na.rm=T),
            percentage=sum(ncrash/nfirm,na.rm=T))

#------------------------------------------------
# Step 5: T2A: Variable Stats (Ref: T2 in Kim,Lee,Zhang 2011JFE)
#------------------------------------------------
t2_v<-c('crash','ncskew','lagLRETR','lagdturn','lagsigma',
  'lagret','lagsize','lagLEV','lagROA','lagMB')
nrowt2<-length(t2_v)
t2_n<-rep(NA,nrowt2)
t2_mean<-rep(NA,nrowt2)
t2_std<-rep(NA,nrowt2)
t2_p5<-rep(NA,nrowt2)
t2_p25<-rep(NA,nrowt2)
t2_med<-rep(NA,nrowt2)
t2_p75<-rep(NA,nrowt2)
t2_p95<-rep(NA,nrowt2)
t2 <- data.frame(t2_v,t2_n,t2_mean,t2_std,t2_p5,
                 t2_p25,t2_med,t2_p75,t2_p95)

for(i in 1:nrowt2){
  var <- paste(t2$t2_v[i]) 
  t2$t2_n[i] <- sum(data2[[var]]>=0,na.rm=T)
  t2$t2_mean[i] <- mean(data2[[var]],na.rm=T)
  t2$t2_std[i] <- sd(data2[[var]],na.rm=T)
  t2$t2_p5[i] <- quantile(data2[[var]],0.05,na.rm=T)
  t2$t2_p25[i] <- quantile(data2[[var]],0.25,na.rm=T)
  t2$t2_med[i] <- quantile(data2[[var]],0.50,na.rm=T)
  t2$t2_p75[i] <- quantile(data2[[var]],0.75,na.rm=T)
  t2$t2_p95[i] <- quantile(data2[[var]],0.95,na.rm=T)
}

#------------------------------------------------
# Step 6: T2B: Correlation (Ref: T2 in Kim,Lee,Zhang 2011JFE)
#------------------------------------------------
t3<-cor(data2[,3:24], use="pairwise.complete.obs")

#------------------------------------------------
# Step 7: T3: Regression Analysis
#------------------------------------------------
# Linear Probability Model
m_LPM1a <- lm(crash ~ lagLRETR,data=data2)
summary(m_LPM1a)
m_LPM1b <- lm(ncskew ~ lagLRETR,data=data2)
summary(m_LPM1b)

m_LPM2a <- lm(crash ~ lagLRETR+
  lagdturn+lagsigma+lagret+lagsize+lagLEV+lagROA+lagMB,data=data2)
summary(m_LPM2a)
m_LPM2b <- lm(ncskew ~ lagLRETR+
  lagdturn+lagsigma+lagret+lagsize+lagLEV+lagROA+lagMB,data=data2)
summary(m_LPM2b)

m_LPM3a <- lm(crash ~ 0+lagLRETR+
  lagdturn+lagsigma+lagret+lagsize+lagLEV+lagROA+lagMB+
  factor(stock)+factor(year),data=data2)
summary(m_LPM3a)
m_LPM3b <- lm(ncskew ~ 0+lagLRETR+
                lagdturn+lagsigma+lagret+lagsize+lagLEV+lagROA+lagMB+
                factor(stock)+factor(year),data=data2)
summary(m_LPM3b)

# Logit Model
m_Logit1 <-glm(crash~lagLRETR,data=data2,family=binomial)
summary(m_Logit1)

m_Logit2 <- glm(crash ~ lagLRETR+
  lagdturn+lagsigma+lagret+lagsize+lagLEV+lagROA+lagMB
  ,data=data2,family=binomial)
summary(m_Logit2)

m_Logit3 <- glm(crash ~ 0+lagLRETR+factor(stock)+factor(year)+
  lagdturn+lagsigma+lagret+lagsize+lagLEV+lagROA+lagMB
  ,data=data2,family=binomial)
summary(m_Logit3)
