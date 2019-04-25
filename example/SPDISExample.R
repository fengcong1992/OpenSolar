#------------------------------------------------------  NOTE  ---------------------------------------------------------
# 1 This code is to show a SPDIS used case: temporal reconciliation
# 2 Reference: Yang, Dazhi, et al. "Reconciling solar forecasts: Geographical hierarchy." Solar Energy 146 (2017): 276-286.
# 3 Coder: Cong Feng, Dazhi Yang; Data: Texas location (32.05, -94.15); Date: 2019/04/24       @ DOES Lab, UTD.
#-----------------------------------------------------------------------------------------------------------------------
# clear R workspace and console
rm(list=ls(all=TRUE))
cat("\014")
libs <- c("hts", "forecast", "doSNOW", "data.table")
invisible(lapply(libs, library, character.only = TRUE))

setwd("your working direcotry") #working directory

load('SPDISExampleData.RData')
level1 <- max(as.numeric(substr(colnames(data), 1, 2))) #total no. of level1 series
level2 <- rle(as.numeric(substr(colnames(data), 1, 2)))$length
nodes <- list(level1, level2)
temp <- ts(data)
xx <- hts(temp, nodes, characters = c(2,4))

#----------------------------- forecast with ETS -------------------------------
L01 <- aggts(xx, levels = c(0, 1)) #extract L0 and L1 time series, a total of 6 in this case
train.days <- 7 #ETS training data length

cl <- makeCluster(3) #no. of threads
registerDoSNOW(cl)
yyy <- foreach(i = 1:ncol(L01), .combine = cbind, .packages = c('forecast')) %dopar% {
  x <- L01[,i]
  fcst <- NULL
  for(j in 1:(365-train.days))
  {
    train <- ts(x[(24*j-23):(24*(train.days+j-1))], frequency=24)
    fit<-ets(train, model="ZZZ", ic = "aic") #ZZZ include all 30 ETS methods, aic is used as the model selection criterion
    ets <- forecast(fit,h=24,method ='ets')$mean #only point forecast for now
    fcst <- append(fcst, ets)
    print(j)
  }
  round(fcst,3)
}
stopCluster(cl)

yyy <- ifelse(yyy<0,0,yyy) #keeps the forecasts positive. We are working on another paper which uses Box-Cox tramsform to achieve this. 
#write.csv(yyy, "L01ETS1.csv", row.names = FALSE)

Fcst.L01 <- yyy
var.cal.percentage <- 0.5 #percetage of data used to calculate the MinT covariance 

Fcst.L2 <- Fcst.L2[-c(1:(7*24)),] #truncate first 7 days data to match ets forecasts

Fcst.L012 <- cbind(Fcst.L01, Fcst.L2)
daytime <- which(Fcst.L012[,1]>10)
Fcst.L012 <- Fcst.L012[daytime,]
S <- smatrix(xx)

#################################################################################
# hierarchical reconciliation
#################################################################################

#computer base forecast error covariance and WLS weights
n.train <- 1:round(var.cal.percentage*nrow(Fcst.L012),0) #index for training data 
n.test <- (round(var.cal.percentage*nrow(Fcst.L012),0)+1):nrow(Fcst.L012) #index for test data
actual.train <- aggts(xx, levels = c(0,1,2))[-c(1:(7*24)),][daytime,][n.train,]
Fcst.train <- Fcst.L012[n.train,]
res = as.matrix(actual.train-Fcst.train) #1-step-ahead forecast error
diag.W1 <- 1/diag(cov(res)) #weights for WLS

#update the forecast results for true out-of-sample validation
Fcst.L012 <- Fcst.L012[n.test,]
actual <- aggts(xx, levels = 1)[-c(1:(7*24)),][daytime,][n.test,]

#bottom up approach
Ptilde.bu <- t(S%*%t(Fcst.L012[,(1+level1+1):ncol(Fcst.L012)]))[,1:level1+1] #level1 reconciled forecasts

#OLS
Ptilde.ols <- combinef(Fcst.L012, xx$nodes, keep = c("gts")) #The optimal reconcilation using function
Ptilde.ols.L1 <- aggts(Ptilde.ols, levels = 1)
Ptilde.ols.L2 <- aggts(Ptilde.ols, levels = 2)

#WLS
Ptilde.wls <- combinef(Fcst.L012, xx$nodes, weights = diag.W1, keep = c("gts"))
Ptilde.wls.L1 <- aggts(Ptilde.wls, levels = 1)
Ptilde.wls.L2 <- aggts(Ptilde.wls, levels = 2)

#HLS
Lambda.inv <- diag(1/as.vector(S%*%matrix(1, ncol = 1, nrow = ncol(S))))
beta.hls <- solve(t(S)%*%Lambda.inv%*%S)%*%t(S)%*%Lambda.inv
Ptilde.hls <- t(S%*%(beta.hls%*%t(Fcst.L012)))
Ptilde.hls.L1 <-Ptilde.hls[,(1:level1)+1]
Ptilde.hls.L2 <- Ptilde.hls[,-(1:(level1+1))]

#MinT
Ptilde.mint <- MinT(Fcst.L012, xx$nodes, residual = res, covariance = "shr", keep = "gts", algorithms = "lu")
Ptilde.mint.L1 <- aggts(Ptilde.mint, levels = 1)
Ptilde.mint.L2 <- aggts(Ptilde.mint, levels = 2)


#------------------------------------ L1 error computation -------------------------------------

RMSE <- function(actual, predict){
  apply((actual-predict)^2, 2, function(x) sqrt(mean(x)))/apply(actual, 2, mean)*100
}
MBE <- function(actual, predict){
  colMeans(actual-predict)/colMeans(actual)*100
}

#L1 error
error.L1 <- list(ETS = Fcst.L012[,(1:level1)+1], BU = Ptilde.bu, OLS = Ptilde.ols.L1, WLS = Ptilde.wls.L1, HLS = Ptilde.hls.L1, MinT = Ptilde.mint.L1)

rmse.L1 <- lapply(error.L1, function(x) round(RMSE(actual, x),2))
mbe.L1 <- lapply(error.L1, function(x) round(MBE(actual, x),2))
rmse.L1 <- data.frame(matrix(unlist(rmse.L1), nrow = 5, byrow = FALSE))
names(rmse.L1) <- c("ETS", "BU", "OLS", "WLS", "HLS", "MinT")
mbe.L1 <- data.frame(matrix(unlist(mbe.L1), nrow = 5, byrow = FALSE))
names(mbe.L1) <- c("ETS", "BU", "OLS", "WLS", "HLS", "MinT")

print(rmse.L1)
print(mbe.L1)


#------------------------------------ L1 error computation -------------------------------------
#L2 error
error.L2 <- list(Three.Tier = Fcst.L012[,-(1:(level1+1))], OLS = Ptilde.ols.L2, WLS = Ptilde.wls.L2, HLS = Ptilde.hls.L2, MinT = Ptilde.mint.L2)
rmse.L2 <- lapply(error.L2, function(x) round(RMSE(aggts(xx, levels = c(2))[-c(1:(7*24)),][daytime,][n.test,], x),2))
rmse.L2 <- unlist(rmse.L2)

#DM test
meas <- aggts(xx, levels = c(2))[-c(1:(7*24)),][daytime,][n.test,]
pred <- list(Three.Tier = Fcst.L012[,-(1:(level1+1))], OLS = Ptilde.ols.L2, WLS = Ptilde.wls.L2, HLS = Ptilde.hls.L2, MinT = Ptilde.mint.L2)
DM.mat <- matrix(0, 5, 5)
for(i in 1:4)
{
  for(j in (i+1):5)
  {
    DM.stat<- NULL
    for(k in 1:sum(level2))
    {
      e1 <- meas[,k] - pred[[i]][,k]  
      e2 <- meas[,k] - pred[[j]][,k] 
      DM.stat[k] <- dm.test(e1, e2, alternative = "two.sided", h = 1, power = 2)$statistic
    }
    #m1 is better or worse than m2
    better <- which(DM.stat < qnorm(0.025)); worse <- which(DM.stat > qnorm(0.975));
    DM.mat[j,i] <- length(better)/sum(level2)
    DM.mat[i,j] <- length(worse)/sum(level2)
  }
}

DM.mat <- round(DM.mat*100, 2)
rownames(DM.mat) = colnames(DM.mat) <- c("3TIER", "OLS", "WLS", "HLS", "MinT")

print(DM.mat)

