\name{mapAuthorToDOI}
\alias{mapAuthorToDOI}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Map Author to Digital Object Identifier (DOI)
}
\description{
Maps the groups to the corresponding DOI values
}
\usage{
mapAuthorToDOI(groups, markRelevant = FALSE, marking.symbol = "*")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{groups}{
Result based on the igraph calculation with walktrap.community(g)
}
  \item{markRelevant}{
If TRUE, all authors who have an abstract will get a * behind the citations name
}
  \item{marking.symbol}{
This can be used to change the Symbol which gets appended to the citationname when markRevelant is TRUE
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
A multidimensional List:
[1] group
[2] citation
[3] DOI or "" if no DOI was found
[4] Abstract if there
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
function (groups, markRelevant = FALSE, marking.symbol = "*")
{
    result = vector(mode = "character", length = 0)
    for (i in 1:length(groups)) {
        authors = groups[[i]]
        group = vector(mode = "character", length = 0)
        for (y in 1:length(authors)) {
            DOI = ""
            if (!is.na(dd$B_DOI[dd$B == authors[y]][1])) {
                DOI = dd$B_DOI[dd$B == authors[y]][1]
            }
            else if (!is.na(dd$A_DOI[dd$A == authors[y]][1])) {
                DOI = dd$A_DOI[dd$A == authors[y]][1]
            }
            abstract = WoK_extracted$AB[WoK_extracted$DI == trimws(as.character(DOI[[1]]))]
            author = authors[y]
            if (length(abstract) == 0) {
                abstract = ""
            }
            else if (is.na(trimws(as.character(abstract)))) {
                abstract = ""
            }
            else if (markRelevant) {
                author = paste(marking.symbol, author, sep = "")
            }
            row = c(i, author, as.character(DOI[[1]]), as.character(abstract))
            group = rbind(group, row)
        }
        result = rbind(result, group)
    }
    return(result)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ external }% use one of  RShowDoc("KEYWORDS")
