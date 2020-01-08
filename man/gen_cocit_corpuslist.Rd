\name{gen_cocit_corpuslist}
\alias{gen_cocit_corpuslist}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Generate Co-Citations Corpuslist
}
\description{
Generates a List of Corpus based on a group on the abstracts that were found.
}
\usage{
gen_cocit_corpuslist(groupsWithAbstracts, mode = "corpus")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{groupsWithAbstracts}{
A List containing Lists with abstracts and authornames
}
  \item{mode}{
The actual value in the result, possible values are corpus and abstract:
       "corpus" The Corpus of all abstracts
       "abstract" Just a the list of abstracts that are found
       Default is "corpus"
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
A list of lists containing the specified structure by the mode variable
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
function (groupsWithAbstracts, mode = "corpus")
{
    groupCount = length(unique(groupsWithAbstracts[, 1]))
    abstractCount = length(groupsWithAbstracts[, 1])
    result = list()
    currentGroup = 1
    currentGroupList = vector(mode = "character")
    for (i in 1:abstractCount) {
        if (groupsWithAbstracts[, 1][i] != currentGroup) {
            if (mode == "corpus") {
                result[[currentGroup]] = corpus_gen(currentGroupList,
                  lang = "english")
            }
            else if (mode == "abstract" || mode == "author") {
                result[[currentGroup]] = currentGroupList
            }
            currentGroup = groupsWithAbstracts[, 1][i]
            currentGroupList = vector(mode = "character")
        }
        if (!(mode == "author")) {
            if (groupsWithAbstracts[, 4][i] != "") {
                currentGroupList = append(currentGroupList, c(groupsWithAbstracts[,
                  2][i], groupsWithAbstracts[, 4][i]))
            }
        }
        else {
            currentGroupList = append(currentGroupList, groupsWithAbstracts[,
                2][i])
        }
    }
    if (mode == "corpus") {
        result[[groupCount]] = corpus_gen(currentGroupList, lang = "english")
    }
    else if (mode == "abstract" || mode == "author") {
        result[[groupCount]] = currentGroupList
    }
    return(result)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
