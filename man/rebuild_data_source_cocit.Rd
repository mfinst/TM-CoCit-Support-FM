\name{rebuild_data_source_cocit}
\alias{rebuild_data_source_cocit}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Rebuild CoCit from papern
}
\description{
This function builds a cocitations Table with DOI Information based on a paperTable. The Papers are normally extracted from the Web of Knowledge in csv Format. Make sure to add a row with Paper numbers as these are required to run the function.
}
\usage{
rebuild_data_source_cocit(paperTable, ignoreCRs)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{paperTable}{
An imported csv table from web of knowledge with papern.No for each Row. File can be read like this: read.csv("file", sep = ";", header = TRUE, skip = 1)
Make sure to add a column with the Name Papern.No. This row will be used to assign the citations to a paper in the generation
}
  \item{ignoreCRs}{old}
}
\details{
Attention, this function can run for more than an hour based on the number ob papers given. It can run around 1 hour for around 9000 inidivual papers.
}
\value{
A Dataframe with the following columns:
PNo, autor, jahr, journal, version, seite, CR, DOI
PNo: Paper Number
autor: Author name
jahr: Year of publishing
journal: Journalname
version: Version name of the Journal
seite: Page on which the citation was released
CR: Citationnumber, which is initilizied with 0
DOI: Digital Object Identifier

If a Value is not found its just replaced with an empty string, expect the year, which gets replaced with a String "None"
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
function (paperTable, ignoreCRs = FALSE)
{
    start_time = Sys.time()
    PNos = paperTable$Papern.No
    CRs = paperTable$CR
    papersLength = length(PNos)
    CRPlaceholder = "0"
    allCRs = as.data.frame(x = c(), autor = c(), jahr = c(),
        journal = c(), version = c(), seite = c(), CR = c(),
        DOI = c(), stringsAsFactors = FALSE)
    allCRs = rbind(allCRs, c("x", "autor", "jahr", "journal",
        "version", "seite", "CR", "DOI"))
    allCRs = type.convert(x = allCRs, as.is = TRUE)
    allIgnoredCits = vector(mode = "character", length = 0)
    names(allCRs)[1] = "x"
    names(allCRs)[2] = "autor"
    names(allCRs)[3] = "jahr"
    names(allCRs)[4] = "journal"
    names(allCRs)[5] = "version"
    names(allCRs)[6] = "seite"
    names(allCRs)[7] = "CR"
    names(allCRs)[8] = "DOI"
    for (i in 1:papersLength) {
        print(paste(i, "of", papersLength))
        cocits = strsplit(x = as.character(CRs[i]), split = ";")
        rows = vector(mode = "character", length = 0)
        if (length(cocits[[1]] > 0)) {
            for (y in 1:length(cocits[[1]])) {
                citationInfoList = strsplit(cocits[[1]][y], ",")
                DOI = grep(x = citationInfoList[[1]], pattern = "DOI ",
                  value = TRUE)
                if (length(DOI) == 0) {
                  DOI = ""
                }
                else {
                  if (length(DOI) > 1) {
                    DOI = DOI[1]
                    DOI = gsub("\\\\[", "", x = DOI)
                  }
                  DOI = gsub("DOI ", "", x = DOI)
                }
                author = trimws(citationInfoList[[1]][1])
                year = trimws(citationInfoList[[1]][2])
                if (is.na(as.numeric(year))) {
                  year = "None"
                }
                if (length(citationInfoList[[1]]) >= 3) {
                  journal = trimws(citationInfoList[[1]][3])
                }
                else {
                  journal = ""
                }
                if (length(citationInfoList[[1]]) >= 4) {
                  version = trimws(citationInfoList[[1]][4])
                  if (!startsWith(version, "V")) {
                    version = ""
                  }
                }
                else {
                  version = ""
                }
                if (length(citationInfoList[[1]]) >= 5) {
                  seite = trimws(citationInfoList[[1]][5])
                  if (!startsWith(seite, "P")) {
                    seite = ""
                  }
                }
                else {
                  seite = ""
                }
                allCRs = rbind(allCRs, c(as.character(PNos[i]),
                  as.character(author), as.character(year), as.character(journal),
                  as.character(version), as.character(seite),
                  as.character(CRPlaceholder), as.character(DOI)))
            }
        }
    }
    end_time = Sys.time()
    final_time = end_time - start_time
    print(final_time)
    return(allCRs)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ external }% use one of  RShowDoc("KEYWORDS")
