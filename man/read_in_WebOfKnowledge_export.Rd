\name{read_in_WebOfKnowledge_exportn}
\alias{read_in_WebOfKnowledge_export}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Matrix Generator
}
\description{
Function to read in an exported document from the Web of Knowledge
}
\usage{
read_in_WebOfKnowledge_export(fileName = "savedrecs.txt", seperator = "\t", generate_PaperNo_Row = TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{fileName}{
  Path of the file to read in. The Default requires a file in the working directory with the name savedrecs.txt.
}
  \item{seperator}{
  A String representing the seperator of the csv/txt file. Default Web of Knowledge exports are tab seperated.
}
  \item{generate_PaperNo_Row}{
generates a new row of the document that represents an index for every row.
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
A loaded in Document with or without a running index for all rows.
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
  function(fileName = "savedrecs.txt",
           seperator = "\t",
           generate_PaperNo_Row = TRUE) {
    WoK_extracted = read.csv(
      fileName,
      header = TRUE,
      sep = seperator,
      encoding = "utf-8",
      na.strings = "",
      skipNul = TRUE,
      row.names = NULL,
      nrows = 0,
      quote = "",
      fill = FALSE
    )
    if (generate_PaperNo_Row) {
       WoK_extracted$Papern.No = seq.int(nrow(WoK_extracted))
    }
    return(WoK_extracted)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ external }% use one of  RShowDoc("KEYWORDS")
