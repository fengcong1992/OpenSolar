\name{SRRL.download}
\alias{SRRL.download}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Download SRRL data
}
\description{
Download SRRL sky images and/or measurement time series
}
\usage{
SRRL.download(root_data, date_start, date_end, skyimg, tmseries, ifunzip, ifunique)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{root_data}{
  the directory for the downloads
  }
  \item{date_start, date_end}{
  data between these two dates will be downloaded
  }
  \item{skyimg, tmseries}{
  booleans, indicating which kind of data to download
  }
  \item{ifunzip}{
  boolean, indicating whether the files should be unzipped
  }
  \item{ifunique}{
  boolean, indicating whether redundant images are removed from the disk
  }
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
Sky images or a csv file with measurement time series, saved into the specified direcotry
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
\code{\link{SRRL.read}}
}
\examples{
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
