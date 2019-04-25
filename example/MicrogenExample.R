#------------------------------------------------------  NOTE  ---------------------------------------------------------
# 1 This code is to show a SPDIS used case: temporal reconciliation
# 2 Reference: Yang, Dazhi, et al. "Reconciling solar forecasts: Temporal hierarchy." Solar Energy 158 (2017): 332-346.
# 3 Coder: Cong Feng, Dazhi Yang; Data: Texas location (32.05, -94.15); Date: 2019/04/24       @ DOES Lab, UTD.
#-----------------------------------------------------------------------------------------------------------------------
# clear R workspace and console
rm(list=ls(all=TRUE))
cat("\014")

# load packages 
libs <- c('OpenSolar', 'lubridate', "thief", "ggplot2", "xtable", "stringr", 'metafolio')
invisible(lapply(libs, library, character.only = TRUE))


#------------------------- global variables -------------------------------
root_data <- '/Users/cfeng/Desktop/solar_data/OpenSolar/data'
setwd(root_data)
load('MicrogenExampleData.RData')
Tm.5 <- seq(as.POSIXct("2006-01-01 00:00:00"), as.POSIXct("2006-12-31 23:55:00"), by = 60*5)
Tm.60 <- seq(as.POSIXct("2006-01-01 00:00:00"), as.POSIXct("2006-12-31 23:55:00"), by = 60*60) + 1800
train.days <- 14
m = 24
forecast.horizon = 2 # h on the most aggregated series
var.cal.percentage <- 0.5 #percetage of data used to calculate the MinT covariance 
plot.size = 8
line.size = 0.4
point.size = 0.3
text.size = 2.8
#--------------------------------------------------------------------------

#SPDIS.download(root_data, 'California', ifunzip = T, actualonly = F)
#data1 <- data.frame(read.table(file.path(root_data,'California/Actual_32.55_-117.05_2006_DPV_13MW_5_Min.csv'),header = TRUE,sep = ","))
#data2 <- data.frame(read.table(file.path(root_data,'California/DA_32.55_-117.05_2006_DPV_13MW_60_Min.csv'),header = TRUE,sep = ","))

#meas <- data1[12*(seq(1, 8760)-1)+1, 'Power.MW.']/max(data1$Power.MW.)
#pred <- data2$Power.MW./max(data2$Power.MW.)


# load data
load(file.path(root_data, 'SPDISExampleData.RData')) # Texas location (32.05, -94.15)
Y <- ts(meas, freq = m)
#NWP <- ts(pred, freq = m)

Y.tilde.struc = Y.tilde.ols = Y.tilde.mse <- ts(rep(NA, length(Y)), freq = m)
for(i in 1:(365-train.days-forecast.horizon+1))
{
  y <- window(Y, start = c(i,1), end = c(i+train.days-1,24))
  yk <- tsaggregates(y, m = m, align = "end")
  frc <- thief:::th.forecast(aggy = yk, h = m * forecast.horizon, usemodel = "ets", forecastfunction = NULL)
  #replace the most disaggregated level forecasts with NWP results from 3Tier
  #frc$forecast[[1]] <- window(NWP, start = c(i+train.days, 1), end = c(i+train.days+forecast.horizon-1,24))
  #frc$residuals[[1]] <- window(NWP, start = c(i,1), end = c(i+train.days-1,24)) - y
  #frc$mse[1] <- mean(frc$residuals[[1]]^2)
  
  #Lambda_S, reconcile using the hierarchical structure
  tmp_S <- reconcilethief(frc$forecast, comb = "struc", returnall = FALSE)
  record <- ((forecast.horizon-1)*m +1):(forecast.horizon * m)
  Y.tilde.struc[record + (i+train.days-1)*m]<- tmp_S[record]
  
  #Lambda_O, reconcile using the ordinary least squares
  tmp_O <- reconcilethief(frc$forecast, comb = "ols", returnall = FALSE)
  Y.tilde.ols[record + (i+train.days-1)*m] <- tmp_O[record]
  
  #Lambda_V, reconcile using the variance scaling
  tmp_V <- reconcilethief(frc$forecast, comb = "mse", mse = frc$mse, returnall = FALSE)
  Y.tilde.mse[record + (i+train.days-1)*m] <- tmp_V[record]
}


data.out <- data.frame(meas = rep(Y,3), pred = do.call(c, list(Y.tilde.ols, Y.tilde.mse, Y.tilde.struc)), group = rep(c("OLS", "WLS", "HLS"), each = length(Y)))


#------------------------------- Compile Results ----------------------------------
#make persistence forecast
persistence <- c(rep(NA, 15*24), data.out[1:8760,]$meas[1:(8760-(15*24))])
data.out <- rbind(data.frame(meas = data.out[1:8760,]$meas, pred = persistence, group = "PERS"), data.out)

measurements<- tsaggregates(data.out$meas, m = m, align = "end")
forecasts <- tsaggregates(data.out$pred, m = m, align = "end")
k <- c(1,2,3,4,6,8,12,24)
data.plot = rmse = annotate.data <- NULL
RMSE <- function(actual, predict){ sqrt(mean((actual-predict)^2))/mean(actual)*100 }
for(i in c(1, 3, 5, 8)) # 1, 3, 6, 24 hourly 
{
  tmp <- data.frame(x = measurements[[i]]/k[i], y = forecasts[[i]]/k[i], group = rep(paste(c(" PERS", "OLS", "WLS", "HLS"), names(measurements)[i]), each = 8760/k[i]))
  select <- which(tmp$x > 0 & !is.na(tmp$y) & tmp$y>0) #remove night time and first two weeks of data (they were used to generate first forecasts)
  tmp$group <- factor(tmp$group, levels = paste(c(" PERS", "OLS", "WLS", "HLS"), names(measurements)[i]))
  tmp <- tmp[select,]
  for(j in 1:length(levels(tmp$group)))
  {
    select <- which(tmp$group == levels(tmp$group)[j])
    rmse <- append(rmse, RMSE(tmp$x[select], tmp$y[select]))
    annotate.data <- rbind(annotate.data, data.frame(rmse = paste("nRMSE = ", as.character(round(RMSE(tmp$x[select], tmp$y[select]),1)), "%", sep = ""), group = levels(tmp$group)[j]))
  }
  data.plot <- rbind(data.plot, tmp)
}

annotate.data$skill <- paste("Fcast. Skill = ",as.vector(t(apply(matrix(rmse, nrow=4, byrow = T), 2, function(x) round(1- x/matrix(rmse, nrow=4, byrow = T)[,1], 3)))))

#---------------- A figure similar to Fig. 6 in the reference paper ------------------
#remove persistence during plotting
annotate.data <- annotate.data[!str_detect(annotate.data$group, "PERS"),] 
data.plot <- data.plot[!str_detect(data.plot$group, "PERS"),]
p <- ggplot(data = data.plot, aes(x = x, y = y)) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "red", size = line.size) +
  stat_binhex(bins = 40) +
  #geom_point(size = point.size, alpha = 0.6) +
  scale_color_manual(values = c("black", gg_color_hue(2)[2])) +
  facet_wrap(~group, ncol=3) +
  xlab("Measured normalized power output [dimensionless]") +
  ylab("Forecast normalized power output [dimensionless]") +
  scale_fill_gradientn(colours = c("lightblue","yellow","red"), name = "Count") + 
  geom_text(aes(x = 0.4, 0.23, label = rmse), size =2.4, data = annotate.data, hjust = 0, family = "Times") +
  geom_text(aes(x = 0.4, 0.15, label = skill), size =2.4, data = annotate.data, hjust = 0, family = "Times") +
  theme_grey() +
  theme(plot.margin = unit(c(1,1,0,0), "lines"), legend.position = "none", text = element_text(family = "Times"), axis.title = element_text(size = plot.size), axis.text = element_text(size = plot.size), strip.text = element_text(size = plot.size))

print(p)


#----------------------- The table similar to Table 1 in the reference paper ------------------------
TeX.rmse  <- NULL
for(i in 1:length(k)) # 1, 3, 6, 24 hourly 
{
  tmp <- data.frame(x = measurements[[i]]/k[i], y = forecasts[[i]]/k[i], group = rep(paste(c(" PERS", "OLS", "WLS", "HLS"), names(measurements)[i]), each = 8760/k[i]))
  select <- which(tmp$x > 0 & !is.na(tmp$y) & tmp$y>0) #remove night time and first two weeks of data (they were used to generate first forecasts)
  tmp <- tmp[select,]
  tmp$group <- factor(tmp$group, levels = paste(c(" PERS", "OLS", "WLS", "HLS"), names(measurements)[i]))
  rmse=NULL
  for(j in 1:length(levels(tmp$group)))
  {
    select <- which(tmp$group == levels(tmp$group)[j])
    rmse <- append(rmse, round(RMSE(tmp$x[select], tmp$y[select]),1))
  }
  TeX.rmse <- rbind(TeX.rmse, rmse)
}

TeX.fs <- apply(TeX.rmse, 2, function(x) round(1- x/TeX.rmse[,1], 3))

colnames(TeX.rmse) = colnames(TeX.fs) <- c("Persistence", "OLS", "WLS", "HLS")
rownames(TeX.rmse) = rownames(TeX.fs) <- c("$k = 1$", "$k = 2$", "$k = 3$", "$k = 4$","$k = 6$", "$k = 8$","$k = 12$", "$k = 24$")
print(xtable(TeX.rmse, digits = 1), include.rownames=TRUE, sanitize.text.function=function(x){x})
print(xtable(TeX.fs, digits = 3), include.rownames=TRUE, sanitize.text.function=function(x){x})

