\name{build_structure_for_list}
\alias{build_structure_for_list}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Build Data Structure for a List of Corpus Objects
}
\description{
Builds a structure for a list of Corpus Objects. The structure is based on the structKey given with the default being a Term Frequency Matrix
}
\usage{
build_structure_for_list(structKey = "tf", corpusList = NULL, tokenizer)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{structKey}{
 key of structure to be generated
 Possible Values are:
  "tf" for Term frequency Matrix
  "dtm" for DocumentTermMatrix (tm Package Object)
  "m" for a Matrix of terms
  "v" for a sorted Vector of terms
  "tfidf" for a term frequency inverse document frequency Matrix
}
  \item{corpusList}{
a List of VCorpus which generate from tm package: c(VCorpus1, ..., VCorpusN)
}
  \item{tokenizer}{
tokenizer based on the RWeka Package
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
List of given data structure
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
tm Package for VCorpus
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (structKey = "tf", corpusList = NULL, tokenizer)
{
    if (is.null(corpusList)) {
        print("corpus is NULL!")
        return(NULL)
    }
    structureCount = length(corpusList)
    result_structList = list()
    for (i in 1:structureCount) {
        struct = matrix_gen(corpus = corpusList[[i]], struct = structKey,
            dtmTokenizer = tokenizer)
        result_structList[[i]] = struct
        print(paste(i, " struct calculated"))
    }
    return(result_structList)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ external }% use one of  RShowDoc("KEYWORDS")
