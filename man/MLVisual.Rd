\name{MLVisual}
\alias{MLVisual}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Visualize results from \code{\link{MLForecast}}
}
\description{
Visualize results from \code{\link{MLForecast}} by a time series plot and a metric barplot
}
\usage{
MLVisual(data_input1, data_input2, p_stt, p_len)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data_input1}{
  forecasting and actual time series dataframe
  }
  \item{data_input2}{
  evaluation metric table
  }
  \item{p_stt}{
  start point of the time series plot
  }
  \item{p_len}{
  length of the time series plot
  }
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
A time series plot and a metric barplot
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
C. Feng, D. Yang, B.-M. Hodge, and J. Zhang
}
