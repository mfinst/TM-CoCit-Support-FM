\name{arrangeColors}
\alias{arrangeColors}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Arrange Colors
}
\description{
For Given N Colors, to the biggest groups in the groupSizes Table the colors are selected in order the remaining groups are getting the defaultColors

}
\usage{
arrangeColors(colorVector, groupSizes, defaultColor = "lightgrey")
}

\arguments{
  \item{colorVector}{
The Colors as a string vector like c("red", "blue")
sort(table(mapAuthorToDOI(wckarate)[,1]),decreasing = TRUE))
}
  \item{groupSizes}{
The Result of a sorted Table of wckrate
sort(table(mapAuthorToDOI(wckarate)[,1]),decreasing = TRUE)
}
  \item{defaultColor}{
The defaultColors for the remaining Groups
Default is "lightgrey"
}
}
\details{

}
\value{
 A vector with elements same as the groups existing
}
\references{

}
\author{
MFinst
}
\note{

}



\seealso{

}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (colorVector, groupSizes, defaultColor = "lightgrey")
{
    N = length(colorVector)
    result = replicate(length(groupSizes), defaultColor)
    for (i in 1:N) {
        result[as.numeric(names(groupSizes)[i])] = colorVector[i]
    }
    return(result)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ external }% use one of  RShowDoc("KEYWORDS")
