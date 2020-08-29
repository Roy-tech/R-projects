# Step: 1

XDD <- read.csv("C:/Project/XDD.csv", header = F, na.strings = ".")
XDD <- data.matrix(XDD, rownames.force = NA)
nstock <- ncol(XDD)
nday <- nrow(XDD)
eventdate <- c()
eventstock <- c()
for (s in 1:nstock) {
  for (t in 1:nday) {
    if (is.finite(XDD[t, s]) & XDD[t, s] >= 20120101 & XDD[t, s] <= 20171231) {
      eventstock <- c(eventstock, s)
      eventdate <- c(eventdate, XDD[t, s])
    }
  }
}
write.table(cbind(eventstock, eventdate), "C:/Project/Event.csv", row.names = F, sep = ",")

# Step: 2

ri <- read.csv("C:/Project/RI.csv", header = F, na.strings = ".")

reti <- matrix(NA, nday, nstock)
for (s in 1:nstock) {
  reti[2:nday, s] <- log(ri[2:nday, s]/ ri[1:(nday - 1), s])
}

rm <- read.csv("C:/Project/RMI.csv", header = F, na.strings = ".")

retm <- matrix(NA, nday, 1)
retm[2:nday] <- log(rm[2:nday, 1]/ rm[1:(nday - 1), 1])

# Step: 3

nest <- 200 
nwindow <- 10
eventparams <- matrix(NA, length(eventdate), 8)
eventr <- matrix(NA, length(eventdate), 2*nwindow + 1)
eventar <- matrix(NA, length(eventdate), 2*nwindow + 1)
eventcar <- matrix(NA, length(eventdate), 2*nwindow + 1)

date <- read.csv("C:/Project/Date.csv", header = F, na.strings = ".")

for (e in 1:length(eventdate)) {
  t <- which(date == eventdate[e])
  i <- eventstock[e]
  y <- reti[(t - nwindow - nest):(t - nwindow - 1), i]
  x <- retm[(t - nwindow - nest):(t - nwindow - 1)]
  index <- which(y > - 10 & y < 10)
  if (length(index) >= 60) {
    mm <- lm(y[index]~x[index])
    eventparams[e, 1] <- mm$coefficients[1]
    eventparams[e, 2] <- mm$coefficients[2]
    eventparams[e, 3] <- length(index)
    eventparams[e, 4] <- var(mm$residuals)
    eventparams[e, 5] <- mean(y[index])
    eventparams[e, 6] <- var(y[index])
    eventparams[e, 7] <- mean(x[index])
    eventparams[e, 8] <- var(x[index])
    eventr[e,] <- reti[(t - nwindow):(t + nwindow), i]
    eventar[e,] <- reti[(t - nwindow):(t + nwindow), i] - eventparams[e, 1] - eventparams[e, 2]*retm[(t - nwindow):(t + nwindow)]
   
    eventcar[ e, 1] <- eventar[e, 1]
    
    for (a in 2:(2*nwindow + 1)) {
      eventcar[e, a] <- eventcar[e, (a - 1)] + eventar[e, a]
    }
    
  }
  
}

z <- c("Stock#", "Eventdate", "Alpha", "Beta", "Nobs", "Var_error", "Mean_Reti", "Var_Reti", " Mean_Retm", "Var_Retm")
write.table(cbind(eventstock, eventdate, eventparams), "C:/Project/Out_eventparams.csv", row.names = F, col.names = z, sep = ",")

# Step 4:

CAAR <- c()
for (a in 1:21) {
  CAAR <- c(CAAR, mean(eventcar[, a]*100, na.rm = T))
}

EventWindow <- (- 10:10)
plot(EventWindow, CAAR, type = "o", col = "navy blue")

# Step 5: Individual AR

t_ARi <- matrix(NA, length(eventdate), (2*nwindow + 1))
for (e in 1:length(eventdate)) {
  for (a in 1:(2*nwindow + 1)) {
    var_ARi <- eventparams[e, 4] + (1/ eventparams[e, 3])*(1 + ((eventr[e, a] - eventparams[e, 7])^2)/ eventparams[e, 8])
    t_ARi[e, a] <- eventar[e, a]/ sqrt(var_ARi)
  }
}

z <- c("Stock#", "Eventdate", -10:10)
write.table(cbind(eventstock, eventdate, eventar), "C:/Project/Out_ARi.csv", row.names = F, col.names = z, sep = ",")
write.table(cbind(eventstock, eventdate, t_ARi), "C:/Project/Out_t_ARi.csv", row.names = F, col.names = z, sep = ",")

# Step 6: individual CAR, i.e.(-10,-2), (-1,+1), (+2,+10)

CARi <- matrix(NA, length(eventdate), 3)
t_CARi <- matrix(NA, length(eventdate), 3)
for (e in 1:length(eventdate)) {
  CARi[e, 1] <- sum(eventar[e, 1:9])
  CARi[e, 2] <- sum(eventar[e, 10:12])
  CARi[e, 3] <- sum(eventar[e, 13:21])
  t_CARi[e, 1] <- CARi[e, 1]/ sqrt(9*eventparams[e, 4])
  t_CARi[e, 2] <- CARi[e, 2]/ sqrt(3*eventparams[e, 4])
  t_CARi[e, 3] <- CARi[e, 3]/ sqrt(9*eventparams[e, 4])
}

z <- c("Stock#", "Eventdate", "CAR(-10, -2)", "CAR(-1, +1)", "CAR(+2, +10)")
write.table(cbind(eventstock, eventdate, CARi), "C:/Project/Out_CARi.csv", row.names = F, col.names = z, sep = ",")
write.table(cbind(eventstock, eventdate, t_CARi), "C:/Project/Out_t_CARi.csv", row.names = F, col.names = z, sep = ",")

# Step 7: Cross-sectional average of AR (AAR)

AARi <- matrix(NA, 1, (2*nwindow + 1))
t_AARi <- matrix(NA, 1, (2*nwindow + 1))
for (d in 1:21) {
  AARi[1, d] <- mean(eventar[, d], na.rm = T)
  nevent <- length(which(is.finite(eventar[, 11])))
  t_AARi[1, d] <- AARi[d]/ sqrt(sum(eventparams[, 4], na.rm = T)/ nevent^2)
}

Z <- (- 10:10)
write.table(rbind(AARi, t_AARi), "C:/Project/Out_AARi.csv", row.names = F, col.names = Z, sep = ",")

# Step 8: Cross-sectional average of CAR (CAAR) i.e. (-10,-2), (-1,+1),(+2,+10)

CAARi <- matrix(NA, 1, 3)
t_CAARi <- matrix(NA, 1, 3)
for (j in 1:3) {
  CAARi[1, j] <- mean(CARi[, j], na.rm = T)
  nevent <- length(which(is.finite(eventar[, 11])))
  t_CAARi[1] <- CAARi[1]/ sqrt(sum(9*eventparams[, 3], na.rm = T)/ nevent^2)
  t_CAARi[2] <- CAARi[2]/ sqrt(sum(3*eventparams[, 3], na.rm = T)/ nevent^2)
  t_CAARi[3] <- CAARi[3]/ sqrt(sum(9*eventparams[, 3], na.rm = T)/ nevent^2)
}

z <- c("CAR(-10,-2)", "CAR(-1,+1)", "CAR(+2,+10)")
write.table(rbind(CAARi, t_CAARi), "C:/Project/Out_CAARi.csv", row.names = F, col.names = z, sep = ",")

# Step 9:

vol <- read.csv("C:/Project/VO.csv", header = F, na.strings = ".")
dol <- read.csv("C:/Project/P.csv", header = F, na.strings = ".")
dol <- data.matrix(dol, rownames.force = NA)
vol <- data.matrix(vol, rownames.force = NA)

dol.vol <- dol*vol

lq <- log(1 + (abs(reti)/ dol.vol))*10000

lq[!is.finite(lq)] <- NA

mvol <- read.csv("C:/Project/MV.csv", header = F, na.strings = ".")
mvol <- data.matrix(mvol, rownames.force = NA)

mktlq <- lq*mvol

Lm <- matrix(NA, nday, 1)
for (t in 1:nday) {
    Lm[t] <- sum(mktlq[t,], na.rm = T)/ sum(mvol[t,], na.rm = T)
  }

# Step 10:

eventparamls <- matrix(NA, length(eventdate), 8)
eventl <- matrix(NA, length(eventdate), 2*nwindow + 1)
evental <- matrix(NA, length(eventdate), 2*nwindow + 1)
eventcal <- matrix(NA, length(eventdate), 2*nwindow + 1)

for (e in 1:length(eventdate)) {
  t <- which(date == eventdate[e])
  i <- eventstock[e]
  l <- lq[(t - nwindow - nest):(t - nwindow - 1), i]
  m <- Lm[(t - nwindow - nest):(t - nwindow - 1)]
  index <- which(l > -10 & l < 10)
  if (length(index) >= 60) {
    reglq <- lm(l[index] ~ m[index])
    eventparamls[e, 1] <- reglq$coefficient[1]
    eventparamls[e, 2] <- reglq$coefficient[2]
    eventparamls[e, 3] <- length(index)
    eventparamls[e, 4] <- var(reglq$residuals)
    eventparamls[e, 5] <- mean(l[index])
    eventparamls[e, 6] <- var(l[index])
    eventparamls[e, 7] <- mean(m[index])
    eventparamls[e, 8] <- var(m[index])
    eventl[e,] <- lq[(t - nwindow):(t + nwindow), i]
    evental[e,] <- lq[(t - nwindow):(t + nwindow), i] - eventparamls[e, 1] - eventparamls[e, 2]*Lm[(t - nwindow):(t + nwindow)]
    eventcal[e, 1] <- evental[e, 1]
    
    for (a in 2:(2*nwindow + 1)) {
      eventcal[e, a] <- eventcal[e, (a - 1)] + evental[e, a]
    }
  }
}

z <- c('Stock#', 'Eventdate', 'Alpha', 'Beta', 'Nobs', 'Var_error', "Mean_LQ", "Var_LQ", "Mean_LQM", "Var_LQM")
write.table(cbind(eventstock, eventdate, eventparamls), "C:/Project/Out_eventparmls.csv", row.names = F, col.names = z, sep = ",")

# Step 11:

CAAL <- c()
for (a in 1:(2*nwindow + 1)) {
  CAAL <- c(CAAL, mean(eventcal[,a]*100, na.rm = T))
}
EventWindow <- c(-nwindow:nwindow)
plot(EventWindow, CAAL, type = 'o', col = 'blue')

# Step 12: Individual AL

t_ALq <- matrix(NA, length(eventdate), 21)
for (e in 1:length(eventdate)) {
  for (a in 1:(2*nwindow + 1)) {
    var_ALq <- eventparamls[e, 4] + (1/ eventparamls[e, 3])*(1 + ((eventl[e, a] - eventparamls[e, 7])^2)/ eventparamls[e, 8])
    t_ALq[e, a] <- evental[e, a]/ sqrt(var_ALq)
  }
}

z <- c("Stock#", "Eventdate", - 10: 10)
write.table(cbind(eventstock, eventdate, evental), "C:/Project/Out_ALq.csv", row.names = F, col.names = z, sep = ",")
write.table(cbind(eventstock, eventdate, t_ALq), "C:/Project/Out_t_ALq.csv", row.names = F, col.names = z, sep = ",")

# Step 13: individual CAL, i.e.(-10,-2), (-1,+1),(+2,+10)

CALq <- matrix(NA, length(eventdate), 3)
t_CALq <- matrix(NA, length(eventdate), 3)
for (e in 1:length(eventdate)) {
  CALq[e, 1] <- sum(evental[e, 1:9])
  CALq[e, 2] <- sum(evental[e, 10:12])
  CALq[e, 3] <- sum(evental[e, 13:21])
  t_CALq[e, 1] <- CALq[e, 1]/ sqrt(9*eventparamls[e, 4])
  t_CALq[e, 2] <- CALq[e, 2]/ sqrt(3*eventparamls[e, 4])
  t_CALq[e, 3] <- CALq[e, 3]/ sqrt(9*eventparamls[e, 4])
}

Z <- c("Stock#", "Eventdate", "(-10, -2)", "(-1, +1)", "(+2, +10)")
write.table(cbind(eventstock, eventdate, CALq), "C:/Project/Out_CALq.csv", row.names = F, col.names = Z, sep = ",")

# Step 14: Cross-sectional average of AL (AAL)

AALq <- matrix(NA, 1, (2*nwindow + 1))
t_AALq <- matrix(NA, 1, (2*nwindow + 1))
for (a in 1:(2*nwindow + 1)) {
  AALq[a] <- mean(evental[, a], na.rm = T)
  neventl <- length(which(is.finite(evental[,nwindow + 1])))
  t_AALq[a] <- AALq[a]/ sqrt(sum(eventparamls[, 4], na.rm = T)/ neventl^2)
}

k <- (- 10:10)
write.table(rbind(AALq, t_AALq), "C:/Project/Out_AALq.csv", row.names = F, col.names = k, sep = ",")

# Step 15: Cross-sectional average of CAR (CAAL) i.e. (-10, -2), (-1, +1), (+2, +10)

CAALq <- matrix(NA, 1, 3)
t_CAALq <- matrix(NA, 1, 3)
for (n in 1:3) {
  CAALq[n] <- mean(CALq[,n], na.rm = T)
  neventl <- length(which(is.finite(evental[, nwindow + 1])))
  t_CAALq[1] <- CAALq[1]/ sqrt(sum(9*eventparamls[, 4], na.rm = T)/ neventl^2)
  t_CAALq[2] <- CAALq[2]/ sqrt(sum(3*eventparamls[, 4], na.rm = T)/ neventl^2)
  t_CAALq[3] <- CAALq[3]/ sqrt(sum(9*eventparamls[, 4], na.rm = T)/ neventl^2)
}

o <- c("CAAL(-10, -2)", "CAAL(-1, +1)", "CAAL(+2, +10)")
write.table(rbind(CAALq, t_CAALq), "C:/Project/Out_CAALq.csv", row.names = F, col.names = o, sep = ",")

# Step 16: Cross section regression(-10, -2), (-1, +1), (+2, +10)

Return_6m <- matrix(NA, length(eventdate), 1)
Market_value <- matrix(NA, length(eventdate), 1)
Volatility <- matrix(NA, length(eventdate), 1)
Liquidity <- matrix(NA, length(eventdate), 1)

for (e in 1:length(eventdate)) {
  t <- which(date == eventdate[e])
  i <- eventstock[e]
  Return_6m[e,] <- log(ri[(t - nwindow - 125), i]/ ri[(t - nwindow - 1), i])
  Liquidity[e] <- mean(lq[(t - nwindow - nest):(t - nwindow - 1), i], na.rm = T)
  Market_value[e] <- mean(mvol[(t - nwindow - nest):(t - nwindow - 1), i])
  Volatility[e] <- sqrt(var(reti[(t - nwindow - nest):(t - nwindow - 1), i]))
}

# Step 17: Cross regression, cross_CAR1 = (-10,-2), cross_CAR2 = (-1, +1), cross_CAR3 = (+2, +10)

 cross_CAR1 <- lm(CARi[, 1] ~ Market_value + Volatility + Liquidity + Return_6m)
 cross_CAR2 <- lm(CARi[, 2] ~ Market_value + Volatility + Liquidity + Return_6m)
 cross_CAR3 <- lm(CARi[, 3] ~ Market_value + Volatility + Liquidity + Return_6m)

summary(cross_CAR1)
summary(cross_CAR2)
summary(cross_CAR3)

# Step 18: Cross regression, cross_CAL1 = (-10,-2), cross_CAL2 = (-1, +1), cross_CAL3 = (+2, +10)

cross_CAL1 <- lm(CALq[, 1] ~ Market_value + Volatility + Liquidity + Return_6m)
cross_CAL2 <- lm(CALq[, 2] ~ Market_value + Volatility + Liquidity + Return_6m)
cross_CAL3 <- lm(CALq[, 3] ~ Market_value + Volatility + Liquidity + Return_6m)

summary(cross_CAL1)
summary(cross_CAL2)
summary(cross_CAL3)


# Step 19: We have used GQ test on all our models to check for heteroscedasticity. We have not noticed any heteroscedasticity problem except for cross_CAR2 model. We have used "lmtest" package to do the GQ test below.


gqtest(mm)
gqtest(reglq)
gqtest(cross_CAR1)
gqtest(cross_CAR2)
gqtest(cross_CAR3)
gqtest(cross_CAL1)
gqtest(cross_CAL2)
gqtest(cross_CAL3)

# Step 20: White correction test to solve heteroscedasticity problesm for cross_CAR2 regression model as we do not know the exact source of heteroscedastiy problem. We have loaded "sandwich" package to execute the heteroscedasticity correction test.

coeftest(cross_CAR2, vcov = vcovHC)



