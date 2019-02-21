\name{SPDIS.read}
\alias{SPDIS.read}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Read and process the SPDIS data
}
\description{
Format, clean, and process SPDIS data
}
\usage{
SPDIS.read(root_data, name_st, list_files, readall)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{root_data}{
  the directory for the downloads
  }
  \item{name_st}{
  the name of the state of data to process
  }
  \item{list_files}{
  character strings, indicating the list of files to process if \code{\link{readall=F}}
  }
  \item{readall}{
  boolean, indicating if all the files will be processed
  }
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
A list of time series data frame and meta data
%% ...
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
\code{\link{SPDIS.download}}
}
\examples{
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
