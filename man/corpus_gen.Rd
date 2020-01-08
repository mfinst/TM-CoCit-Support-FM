\name{corpus_gen}
\alias{corpus_gen}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Corpus Generator
}
\description{
Prepares a certain data vector to build a corpus from. It also filters stopwords, performes stemming, strips whitespace and removes punctuation.
You could also use the tm Package to build the Corpus from scratch but this function makes it easy for repeated generations of VCorpus objects.
}
\usage{
corpus_gen(data.vector, lang, furtherStops = NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data.vector}{
A vector which contains a String for each Document: c("DocA", "DocB", ..., "DocN")
}
  \item{lang}{
Language as a String in which the Documents are. Default is "english". This param also has influence in which stopwords are filtered in the generation step.
}
  \item{furtherStops}{
a Vector of words which should also filtered from the corpus beside the normal stopwords
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
VCorpus Object
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
function (data.vector, lang, furtherStops = NULL)
{
    corpus = VCorpus(VectorSource(as.vector(data.vector)), readerControl = list(language = lang))
    corpus = tm_map(corpus, content_transformer(tolower))
    corpus = tm_map(corpus, stripWhitespace)
    corpus = tm_map(corpus, removePunctuation)
    corpus = tm_map(corpus, stemDocument, lang)
    corpus = tm_map(corpus, removeWords, stopwords(lang))
    if (!is.null(furtherStops)) {
        corpus = tm_map(corpus, removeWords, furtherStops)
    }
    return(corpus)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ external }% use one of  RShowDoc("KEYWORDS")
