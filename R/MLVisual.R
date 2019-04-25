#--------------------------------  NOTE  ----------------------------------------
# 1 This code is to visualize the MLForecast results;
# 3 Coder: Cong Feng        Date: 2019/04/23       @ DOES Lab, UTD.
#--------------------------------------------------------------------------------
MLVisual <- function(data_input1, data_input2, p_stt, p_len){
  library(ggplot2)
  gg_color_hue <- function(n) {
    hues = seq(15, 375, length = n + 1)
    hcl(h = hues, l = 65, c = 100)[1:n]
  }
  
  
  #----------------------- Plot 1 -------------------------
  
  # prepare data for time series plot
  p_stt <- 100
  p_len <- 400
  
  #data_input1 <- data_denormforec[p_stt:(p_stt+p_len-1),]
  data_input1 <- data_input1[p_stt:(p_stt+p_len-1),]
  
  data_plot1 <- NULL
  for (n_col in 1:length(data_input1)) {
    data_col <- data.frame(seq(1, nrow(data_input1)), data_input1[,n_col], colnames(data_input1)[n_col]) 
    colnames(data_col) <- c('TimeStamp', 'Value', 'Group')
    data_plot1 <- rbind(data_plot1, data_col)
  }
  
  
  group_label <- c(colnames(data_input1)[ncol(data_input1)], colnames(data_input1)[-ncol(data_input1)])
  data_plot1$Group <- factor(data_plot1$Group, levels = group_label)
  cols <- c('black', gg_color_hue(ncol(data_input1)-1))
  
  
  plot1 <- ggplot(data_plot1,aes(x=TimeStamp,y=Value,color=Group,
                                 linetype = Group))+ 
    geom_line() +
    theme(legend.direction = "vertical",legend.position=c(.1,.5),legend.key = element_blank(),
          legend.background = element_rect(color = "black", size = 0.5, linetype = "solid"),
          axis.text.x = element_text(angle=0)) +
    guides(color= guide_legend("Method"), linetype=guide_legend("Method")) 
  
  
  #----------------------- Plot 2 -------------------------
  #data_input2 <- eval_forecasts
  list_metric <- c('MAPE', 'nMAE', 'nRMSE')
  list_model <- rownames(data_input2)
  list_model <- setdiff(list_model, c('SVR1', 'SVR3'))
  
  data_plot2 <- NULL
  for (n_metric in 1:length(list_metric)) {
    data_metric <- data_input2[list_model,n_metric]
    data_metric2 <- data.frame(data_metric, list_model, list_metric[n_metric])
    colnames(data_metric2) <- c('Value', 'Model', 'Metric')
    data_plot2 <- rbind(data_plot2, data_metric2)
  }
  data_plot2$Value <- round(data_plot2$Value*100, digits = 2)
  
  
  plot2 <- ggplot(data_plot2,aes(x=Model,y=Value, fill=factor(Model)))+ geom_bar(stat="identity",position='dodge',width=.8) + 
    facet_grid(Metric ~. ,scales="free") +
    labs(x = "Model",y = 'Forecasting Accuracy'~'['*'%'*']') + 
    theme(legend.position='none') 
  
  return(list(plot1, plot2))
}