\name{markAuthors}
\alias{markAuthors}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Mark Authors which had Abstracts
}
\description{
Maps the * to all vertices with abstracts on the dd Datastructure. After that rerun the whole neighborship process.
To Use this Function you need your input dataframe to look like this:
A, B, A_DOI, B_DOI, ANZ, ANZC1, ANZC2, CCV
Author A, Author B
Doi for A and B,
Count of both,
Counted occurences for A and B,
Co-Citaion Value

Always save your dataframe a Strings, not factors as this could lead to errors.
}
\usage{
markAuthors(ddResult, mappedValues)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{ddResult}{
Basically the result of the CoCit Calculation as a table. See description on informations how to format your dataframe to accept the data.
}
  \item{mappedValues}{
The mapped values calculated with mapAuthorToDOI(wckarate)
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
Same as the Input Structure but with marked authornames for each Abstract.
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
function (ddResult, mappedValues)
{
    ddResult = type.convert(x = ddResult, as.is = TRUE)
    ddResult$A_DOI = trimws(ddResult$A_DOI)
    ddResult$B_DOI = trimws(ddResult$B_DOI)
    mappedValues = as.data.frame(mappedValues, stringsAsFactors = FALSE)
    colnames(mappedValues)[1] = "GROUPS"
    colnames(mappedValues)[2] = "AUTHOR"
    colnames(mappedValues)[3] = "DOI"
    colnames(mappedValues)[4] = "ABSTRACT"
    mappedValues$AUTHOR = trimws(mappedValues$AUTHOR)
    mappedValues$DOI = trimws(mappedValues$DOI)
    for (i in 1:length(ddResult$A)) {
        row = ddResult[i, ]
        authorA = row$A
        authorADOI = row$A_DOI
        authorB = row$B
        authorBDOI = row$B_DOI
        abs1 = as.character(mappedValues$ABSTRACT[as.character(mappedValues$DOI) ==
            authorADOI])
        abs2 = as.character(mappedValues$ABSTRACT[as.character(mappedValues$DOI) ==
            authorBDOI])
        if (length(abs1) > 1) {
            abs1 = abs1[1]
        }
        if (length(abs2) > 1) {
            abs2 = abs2[1]
        }
        if (abs1 != "") {
            authorA = paste("*", authorA, sep = "")
            print(paste("-A-", authorA, sep = " "))
            ddResult[i, ]$A = as.character(authorA)
        }
        if (abs2 != "") {
            authorB = paste("*", authorB, sep = "")
            print(paste("-B-", authorB, sep = " "))
            ddResult[i, ]$B = as.character(authorB)
        }
    }
    return(ddResult)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ external }% use one of  RShowDoc("KEYWORDS")
