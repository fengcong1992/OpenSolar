\name{Dataport.get}
\alias{Dataport.get}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Download and process Dataport data
}
\description{
Download and process Dataport data of houses that have PV installations
}
\usage{
Dataport.get(usrname, pswd, hsid, timeres, qc)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{usrname}{
  username
  }
  \item{pswd}{
  password
  }
  \item{hsid}{
  house ID, which can be accessed through \code{\link{Dataport.meta}}
  }
  \item{timeres}{
  time series time resolution, could be 'minitues', '15min', or 'hours'
  }
  \item{qc}{
  boolean, indicating whether quality control should be applied
  }
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
A data frame containing measurement time series of a house
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
C. Feng, D. Yang, B.-M. Hodge, and J. Zhang
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{Dataport.meta}}
}
\examples{

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
