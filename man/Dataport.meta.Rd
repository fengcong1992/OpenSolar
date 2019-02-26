\name{Dataport.meta}
\alias{Dataport.meta}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Get meta data of Dataport houses with PV installation
}
\description{
Get meta data of Dataport houses with PV installation
}
\usage{
Dataport.meta(usrname, pswd, qc, ifdownload, root_save)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{usrname}{
  username
  }
  \item{pswd}{
  password
  }
  \item{qc}{
  boolean, indicating whether quality control should be applied
  }
  \item{ifdownload}{
  boolean, indicating whether the data should be downloaded
  }
  \item{root_save}{
  saving directory
  }
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
A data frame containing state, city, and PV capacity, etc.
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
C. Feng, N. Dawkrajai, D. Yang, B.-M. Hodge, and J. Zhang
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{Dataport.get}}
}
\examples{

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
