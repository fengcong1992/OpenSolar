\name{SRRL.read}
\alias{SRRL.read}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Process SRRL sky images
}
\description{
Process SRRL sky images and extract useful features
}
\usage{
SRRL.read(timestamp, root_data, returnRGB, processraw, processadv)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{timestamp}{
  a character, indicating the time stamp of the processed image
  }
  \item{root_data}{
  the directory of the saved image{s}
  }
  \item{returnRGB}{
  boolean, indicating weather return image pixel RGB values
  }
  \item{processraw}{
  boolean, indicating weather return mean, standard deviation, and the second-order entropy of raw sky image pixel normalized red blue     ratios
  }
  \item{processadv}{
  boolean, indicating weather return thin cloud coverage, and opaque cloud coverage
  }
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
A list of sky image feature data frame and sky image RGB tables
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
\code{\link{SRRL.download}}
}
\examples{

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
