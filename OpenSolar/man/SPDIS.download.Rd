\name{SPDIS.download}
\alias{SPDIS.download}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Download SPDIS data
}
\description{
Download and/or unziip SPDIS data into a specified directory by state.
}
\usage{
SPDIS.download(root_data, list_st_download, ifunzip, actualonly)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{root_data}{
  the directory for the downloads
  }
  \item{list_st_download}{
  a list of states of data to download
  }
  \item{ifunzip}{
  boolean, indicating whether the files should be unzipped
  }
  \item{actualonly}{
  boolean, indicating whether only the actual data is needed
  }
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
A zip file or a number of csv files, saved into the specified direcotry
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
\code{\link{SPDIS.read}}
}
\examples{
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
