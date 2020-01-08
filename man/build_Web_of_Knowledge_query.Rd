\name{build_Web_of_Knowledge_query}
\alias{build_Web_of_Knowledge_query}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Search Query Builder for WoK
}
\description{
Builds a String for the Web of Knowledge Advanced Search that contains all DOIs given as Input
}
\usage{
build_Web_of_Knowledge_query(DOIvector)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{DOIvector}{
%%     ~~Describe \code{DOIvector} here~~
}
}
\details{
Vektor with DOIs as Strings
}
\value{
String in the format of CO=(X1 OR X2 OR ... OR Xn)
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
MFinst
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (DOIvector)
{
    result = "DO=("
    for (i in 1:length(DOIvector)) {
        if (DOIvector[i] != "") {
            result = paste(result, DOIvector[i], " OR ", sep = "")
        }
    }
    result = substr(result, 1, nchar(result) - 3)
    return(paste(result, ")", sep = ""))
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ external }% use one of  RShowDoc("KEYWORDS")
