#--------------------------------  NOTE  ----------------------------------------
# 1 This code is to perform solar forecasting by machine learning models;
# 3 Coder: Cong Feng        Date: 2019/02/15       @ DOES Lab, UTD.
#--------------------------------------------------------------------------------
MLForecast <- function(data_inputmain, n_step, p_ratio){
  format_data <-function(data_input,time_horizon,forc_variable){
    cat('The data is arranged for forecasting horizon:',time_horizon,'\n')
    cat('The forecast target variable is:',forc_variable,'\n')

    data_format1 <- data.frame(data_input[1:(nrow(data_input)-time_horizon),],
                               data_input[(1+time_horizon):nrow(data_input),forc_variable])
    data_add <- cbind(data_input[(nrow(data_input)-time_horizon+1):nrow(data_input),],
                      data_input[(nrow(data_input)-time_horizon+1):nrow(data_input),forc_variable])

    colnames(data_format1) <- c(colnames(data_input),'target')
    colnames(data_add) <- c(colnames(data_input),'target')

    data_format <- data.frame(rbind(data_format1,data_add))
    return(data_format)
  }


  data_normalization1 <- function(read_in_data){
    print('Normalize training data...')
    # initialize the data matrix for returning
    data_after_normliz <- data.frame(matrix(0,nrow(read_in_data),ncol(read_in_data)))
    colnames(data_after_normliz) <- colnames(read_in_data)
    min_max <- data.frame(matrix(0,2,ncol(read_in_data)))
    rownames(min_max) <- c('min','max')
    colnames(min_max) <- colnames(read_in_data)
    # normalization
    for (i in 1:ncol(read_in_data)) { # question???: should normalize hour column?
      data_after_normliz[,i] <- (read_in_data[,i] - min(read_in_data[,i]))/(max(read_in_data[,i])-min(read_in_data[,i]))

      #for (j in 1:nrow(read_in_data)) {
      # print(j)
      #  data_after_normliz[j,i] <- (read_in_data[j,i] - min(read_in_data[,i]))/(max(read_in_data[,i])-min(read_in_data[,i]))
      #}
      min_max[,i] <- c(min(read_in_data[,i]), max(read_in_data[,i]))
    }
    # return normalized data
    data_output <- list(data_after_normliz, min_max)
    return(data_output)
  }

  data_denormalization <- function(read_in_data, min_max){
    print('Denormalize data...')
    # initialize the data matrix for returning
    data_after_denormliz <- data.frame(matrix(0,nrow(read_in_data),ncol(read_in_data)))
    colnames(data_after_denormliz) <- colnames(read_in_data)
    # normalization
    for (i in 1:ncol(read_in_data)) { # question???: should normalize hour column?
      for (j in 1:nrow(read_in_data)) {
        data_after_denormliz[j,i] <- read_in_data[j,i]*(min_max[2,i]-min_max[1,i])+min_max[1,i]
      }
    }
    data_output <- data_after_denormliz

    return(data_output)
  }


  data_split <- function(data_input, split_ratio){
    data1 <- data_input[1:ceiling(nrow(data_input)*split_ratio),]
    data2 <- data_input[(ceiling(nrow(data_input)*split_ratio)+1):nrow(data_input),]
    return(list(data1, data2))
  }


  ann_train<-function(training_data,learning_function,act_function,version_ctrl){
    library(caret)
    library(RSNNS)
    library(nnet)
    #x is the model inputs and y is the model target
    x<-training_data[,1:(ncol(training_data)-1)]
    y<-training_data[,(ncol(training_data))]
    #Train model directly (or used commented out caret module which took too long on the full dataset)
    # 1st Version
    if (version_ctrl == 'type1') {
      model_ann <-mlp(x, y, size = c(30), maxit = 1000,
                      initFunc = "Randomize_Weights", initFuncParams = c(-0.3, 0.3),
                      learnFunc = learning_function, learnFuncParams = c(0.2,0),
                      updateFunc = "Topological_Order", updateFuncParams = c(0),
                      hiddenActFunc = act_function, shufflePatterns = TRUE, linOut = FALSE,
                      inputsTest = NULL, targetsTest = NULL, pruneFunc = NULL,
                      pruneFuncParams = NULL)
    }
    if (version_ctrl == 'type2') {
      model_ann <- caret::train(x,y,
                                method = "nnet",
                                preProcess = "range", #scales the data to be within [0,1]
                                tuneLength = 5,
                                trace = FALSE,
                                maxit = 100)
    }
    if (version_ctrl == 'type3') {
      model_ann <- rbf(x, y, size=5, maxit=1000,
                       initFuncParams=c(0, 1, 0, 0.01, 0.001),
                       learnFuncParams=c(1e-8, 0, 1e-8, 0.1, 0.8), linOut=FALSE)
    }
    return(model_ann)
  }


  svm_train<-function(training_data,kernel_type,version_ctrl){
    library(caret)
    library(e1071)
    #x is the model inputs and y is the model target
    x <- data.frame(training_data[,1:(ncol(training_data)-1)])
    y <- training_data[,(ncol(training_data))]
    # 1st Version
    if (version_ctrl == 'type1') {
      model <-svm(x,y,type="eps-regression",kernal=kernel_type,gamma = 0.001,degree = 5,epsilon = 0.001)
    }
    # 2nd Version
    if (version_ctrl == 'type2') {
      #tune_result <- tune(svm, train.x=x, train.y=y,kernel=kernel_type, ranges=list(cost=seq(0.01,100,0.01), gamma=seq(0.001,10,0.001)))
      #svm_tuned_model <- tune_result$best.model
    }
    # 3rd Version
    if (version_ctrl == 'type3') {
      cvCtrl <- trainControl(method = "repeatedcv", repeats = 5)
      #gridval=svmGrid(x,y,2)
      model<-caret::train(x,y,method=kernel_type,tuneLength = 1,preProc = c("center","scale"),metric="RMSE",trControl=cvCtrl)	# 9 values of the cost function
    }
    return(model)
  }


  gbm_train<-function(training_data,distri_method,version_ctrl){
    library(gbm)
    library(caret)
    #x is the model inputs and y is the model target
    x<- data.frame(training_data[,1:(ncol(training_data)-1)])
    y<-training_data[,(ncol(training_data))]
    # 1st Version
    if (version_ctrl == 'type1') {
      model_gbm <- gbm.fit(x,y,distribution = distri_method,n.trees = 10000,
                           interaction.depth = 1,n.minobsinnode = 10,shrinkage = 0.001,bag.fraction = 0.5,verbose = FALSE)
      # check performance using an out-of-bag estimator
      # OOB underestimates the optimal number of iterations
      best.iter <- gbm.perf(model_gbm,method="OOB",plot.it = FALSE)

    }
    # 2nd version
    if (version_ctrl == 'type2') {
      #use the caret module to train the gbm algorithm using a grid of parameters taken from this source: http://stackoverflow.com/questions/8722247/r-caret-and-gbm-cant-find-ntrees-input
      gbmGrid <- expand.grid(interaction.depth = (1:5) * 2, n.trees = (1:5)*50, shrinkage = .1,n.minobsinnode=10)
      bootControl <- trainControl(number = 1)
      #model<-gbm.fit(x,y,distribution = "gaussian",n.trees=100)
      model_gbm <- caret::train(x, y, method = "gbm", tuneLength = 5, trControl = bootControl, tuneGrid = gbmGrid)
    }
    if (version_ctrl == 'type1') {
      return(list(model_gbm,best.iter))
    }
    if (version_ctrl == 'type2') {
      return(model_gbm)
    }
  }


  rf_train<-function(training_data,version_ctrl){
    library(randomForest)
    #x is the model inputs and y is the model target
    x <- data.frame(training_data[,1:(ncol(training_data)-1)])
    y <- training_data[,(ncol(training_data))]
    #Train model directly (or used commented out caret module which took too long on the full dataset)
    # 1st Version
    if (version_ctrl == 'type1') {
      model_rf <- randomForest(x,y, mtry=5,ntree = 1000, importance=TRUE, na.action=na.omit)

    }
    if (version_ctrl == 'type2') {
      model_rf <- tuneRF(x, y, mtryStart=1, ntreeTry=1000, stepFactor=2, improve=0.05,
                         trace=TRUE, plot=FALSE, doBest=TRUE)
    }
    if (version_ctrl == 'type3') {
      # train model
      control <- trainControl(method="repeatedcv", number=10, repeats=3)
      tunegrid <- expand.grid(.mtry=c(1:15))
      model_rf <- randomForest::train(x,y, method = 'rf',metric='RMSE', tuneGrid=tunegrid, trControl=control)
      summary(model_rf)
      plot(model_rf)
    }
    return(model_rf)
  }


  ann_predict<-function(model,input_data){
    # Use predict to estimate the output of the test data
    #x_test is the feature data of the testing set, y_test is the output of the testing set, and y_predict is the ML output
    x_test <- data.frame(input_data[1:nrow(input_data),1:(ncol(input_data)-1)])
    y_predict<-predict(model,x_test)
    return(y_predict)
  }


  svm_predict<-function(model,input_data, type){
    # Use predict to estimate the output of the test data
    x_test <- data.frame(input_data[,1:(ncol(input_data)-1)])
    colnames(x_test) <- colnames(input_data)[1:(ncol(input_data)-1)]
    if(type == 'type1'){
      y_predict <- predict(model,x_test)
    }
    if(type == 'type3'){
      y_predict <- predict(model, newdata = x_test)
    }
    return(y_predict)
  }


  gbm_predict<-function(model,input_data,type_of_model){
    # Use predict to estimate the output of the test data
    x_test<- data.frame(input_data[1:nrow(input_data),1:(ncol(input_data)-1)])
    # two types of the model need different command to do the prediction
    if (type_of_model == 'type1') {
      y_predict<-predict(model[1],x_test,model[2])
    }
    if (type_of_model == 'type2') {
      y_predict<-predict(model,x_test)
    }
    return(y_predict)
  }


  rf_predict<-function(model,input_data){
    # Use predict to estimate the output of the test data
    #x_test is the feature data of the testing set, y_test is the output of the testing set, and y_predict is the ML output
    x_test <- data.frame(input_data[,1:(ncol(input_data)-1)])
    y_predict<-predict(model,x_test)
    return(y_predict)
  }


  SAML_benchmarks <- function(training_data, testing_data){
    # training
    # svm models
    model1st_svm_radial <- svm_train(training_data,"svmRadial",'type3')
    model1st_svm_li <- svm_train(training_data,"svmLinear",'type3')
    model1st_svm_poly <- svm_train(training_data,"svmPoly",'type3')
    # ann models
    model1st_ann_1 <- ann_train(training_data,'Std_Backpropagation','Act_Logistic','type1')
    model1st_ann_2 <- ann_train(training_data,'BackpropChunk','Act_Logistic','type1')
    model1st_ann_4 <- ann_train(training_data,'BackpropMomentum','Act_Logistic','type1')
    # gbm models
    model1st_gbm_1 <- gbm_train(training_data,'gaussian','type1')
    model1st_gbm_3 <- gbm_train(training_data,'laplace','type1')
    model1st_gbm_4 <- gbm_train(training_data,'None','type2')
    # random forest models
    model1st_rf1 <- rf_train(training_data,'type1')

    # forecasting
    # svm models
    data_2nd_train_svm_radial <- data.frame(svm_predict(model1st_svm_radial,testing_data,'type3'))
    data_2nd_train_svm_linear <- data.frame(svm_predict(model1st_svm_li,testing_data,'type3'))
    data_2nd_train_svm_poly <- data.frame(svm_predict(model1st_svm_poly,testing_data,'type3'))
    # ann models
    data_2nd_train_ann_1 <- data.frame(ann_predict(model1st_ann_1,testing_data))
    data_2nd_train_ann_2 <- data.frame(ann_predict(model1st_ann_2,testing_data))
    data_2nd_train_ann_4 <- data.frame(ann_predict(model1st_ann_4,testing_data))
    # gbm models
    data_2nd_train_gbm_1 <- data.frame(gbm_predict(model1st_gbm_1,testing_data,'type1'))
    data_2nd_train_gbm_3 <- data.frame(gbm_predict(model1st_gbm_3,testing_data,'type1'))
    data_2nd_train_gbm_4 <- data.frame(gbm_predict(model1st_gbm_4,testing_data,'type2'))
    # random forest models
    data_2nd_train_rf1 <- data.frame(rf_predict(model1st_rf1,testing_data))

    # compile data for 2nd-layer model training
    data_output <- data.frame(cbind(data_2nd_train_svm_radial,data_2nd_train_svm_linear,data_2nd_train_svm_poly,
                                    data_2nd_train_ann_1,data_2nd_train_ann_2,data_2nd_train_ann_4,data_2nd_train_gbm_1,
                                    data_2nd_train_gbm_3,data_2nd_train_gbm_4,data_2nd_train_rf1),testing_data[,ncol(testing_data)])

    colnames(data_output) <- c('SVR1','SVR2','SVR3','ANN1','ANN2','ANN3','GBM1','GBM2','GBM3','RF','ACTUAL')
    return(data_output)
  }


  rep.col<-function(x,n){
    matrix(rep(x,each=n), ncol=n, byrow=TRUE)
  }


  EvaMetrics <- function(anly) {
    library("stats")
    library("fBasics")
    #do the forecast error calculation and normalization
    cap <- max(anly[,2])
    fe1 <- as.matrix((anly[,1]-anly[,2])/anly[,2]) # different normalization standards
    fe2 <- as.matrix((anly[,1]-anly[,2])/cap)
    # remove the NAs
    fe1 <- fe1[!is.infinite(fe1)]
    fe1 <- fe1[!is.na(fe1)]
    fe2 <- fe2[!is.na(fe2)]

    #-------------------- error evaluation metrics ------------------
    mape <- mean(abs(fe1))     # mean absolute percentage error
    mae <- mean(abs(fe2))*cap  # mean absolute error
    nmae <- mean(abs(fe2))     # normalized mean absolute error
    mse <- mean((fe2*cap)^2)   # mean square error
    rmse <- sqrt(mse)          # root mean square error
    nrmse <- rmse/cap          # normalized root mean square error
    maxae <- max(anly[,1]-anly[,2])          # maximum absolute error
    mbe <- mean(anly[,1]-anly[,2])   # mean bias error
    rmqe <- (mean((fe2*cap)^4))^(1/4) # Root mean quartic error
    nrmqe <- (mean((fe2*cap)^4))^(1/4)/cap # normalized root mean quartic error
    sum_error <- sum(abs(fe2))*cap

    result <- data.frame(mape,mae,nmae,mse,rmse,nrmse,maxae,mbe,rmqe,nrmqe,sum_error)
    colnames(result) <- c('MAPE','MAE','nMAE','MSE','RMSE','nRMSE','MAXAE','MBE','RMQE','nRMQE','Sum_ERROR')
    return(result)
  }

  # format the data into [X, y]
  data_fmt <- format_data(data_inputmain[,3:ncol(data_inputmain)], n_step, colnames(data_inputmain)[ncol(data_inputmain)])
  # normalization by (x-min(x))/(max(x)-min(x))
  list_norm <- data_normalization1(data_fmt)
  data_norm <- list_norm[[1]]
  max_min <- list_norm[[2]]
  # data split
  list_split <- data_split(data_norm, p_ratio)
  data_train <- list_split[[1]]
  data_test <- list_split[[2]]
  # train and forecast
  data_forecasts <- SAML_benchmarks(data_train, data_test)
  # denormalize the forecasts
  data_denormforec <- data_denormalization(data_forecasts, rep.col(max_min[, ncol(max_min)], ncol(data_forecasts)))
  # evaluation results
  eval_forecasts <- NULL
  for (i in 1:(ncol(data_denormforec)-1)) {
    eval_sing <- EvaMetrics(data.frame(data_denormforec[, i], data_denormforec[, ncol(data_denormforec)]))
    eval_forecasts <- rbind(eval_forecasts, eval_sing)
  }
  eval_forecasts <- data.frame(eval_forecasts)
  rownames(eval_forecasts) <- colnames(data_forecasts)[1:(ncol(data_forecasts)-1)]
  return(list(data_forecasts, eval_forecasts))
}



