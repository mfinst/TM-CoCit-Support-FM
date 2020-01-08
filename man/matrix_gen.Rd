\name{matrix_gen}
\alias{matrix_gen}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Matrix Generator
}
\description{
Function to generate a dtm and an tfidf from a corpus
}
\usage{
matrix_gen(corpus = NULL, structKey = "m", dtmTokenizer = NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{corpus}{
%%     ~~Describe \code{corpus} here~~
}
  \item{structKey}{
 key of structure to be generated
 Possible Values are:
  "tf" for Term frequency Matrix
  "dtm" for DocumentTermMatrix (tm Package Object)
  "m" for a Matrix of terms
  "v" for a sorted Vector of terms
  "tfidf" for a term frequency inverse document frequency Matrix
}
  \item{dtmTokenizer}{
Used if word combinations matter. You can use the already initilized tokenizer in the package or init your own with the RWeka Package.
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
Structure specified by the structKey
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
function (corpus = NULL, structKey = "m", dtmTokenizer = NULL)
{
    if (!is.null(dtmTokenizer)) {
        dtm = DocumentTermMatrix(corpus, control = list(tokenize = phraseTokenizer))
    }
    else {
        dtm = DocumentTermMatrix(corpus)
    }
    if (structKey == "dtm") {
        return(dtm)
    }
    m = as.matrix(dtm)
    if (structKey == "m") {
        return(m)
    }
    v = sort(colSums(m), decreasing = TRUE)
    if (structKey == "v") {
        return(v)
    }
    d = data.frame(word = names(v), freq = v)
    if (structKey == "d" || structKey == "tf") {
        return(d)
    }
    dtm.tfidf = weightTfIdf(dtm)
    dtm.tfidf = removeSparseTerms(dtm.tfidf, 0.999)
    tfidf.matrix = as.matrix(dtm.tfidf)
    if (structKey == "tfidf") {
        return(dtm.tfidf)
    }
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ external }% use one of  RShowDoc("KEYWORDS")
