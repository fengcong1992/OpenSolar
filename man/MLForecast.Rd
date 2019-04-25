\name{MLForecast}
\alias{MLForecast}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Perform short-term solar forecasting using the state-of-the-art machine learning algorithms
}
\description{
Perform short-term solar forecasting using the state-of-the-art machine learning algorithms, including 3 ANNs, 3 SVR models, 3 GBMs, and an RF
}
\usage{
MLForecast(data_inputmain, n_step, p_ratio)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data_inputmain}{
  input data
  }
  \item{n_step}{
  forecasting look-ahead step
  }
  \item{p_ratio}{
  training/testing data partition ratio
  }
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
A data frame containing forecasting and actual time series and a table with evaluation metrics
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
C. Feng, D. Yang, B.-M. Hodge, and J. Zhang
}
