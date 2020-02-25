\name{assign_citation_numbers}
\alias{assign_citation_numbers}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Assign Citation Numbers
}
\description{
This function assigns Citationnumbers to the extracted Citations from the rebuild_data_source_cocit function
}
\usage{
assign_citation_numbers(cits)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{cits}{
A Dataframe with all Citations with the following dimensions: x, autor, jahr, journal, version, seite, CR, DOI
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
The same dataframe as the input but with assigned Citationnumbers.
Assigned means, for every unique Citation there is an unique Citationnumber choosen.
}
\references{

}
\author{
MFinst
}
\note{

}


\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function(cits) {
  start_time = Sys.time()
  cits = type.convert(cits, as.is = TRUE)
  cr_number = 0
  cr_nummer_eintrag = "CR"
  cits_length = length(cits$x)
  for (i in 2:cits_length) {
    checkCit = FALSE
    canCheck = TRUE
    citation_infos = cits[i, ]
    cr = citation_infos$CR
    autor = citation_infos$autor
    if (!grepl("?", autor)) {
      canCheck = FALSE
    }
    if (cr == "CR") {
      canCheck = FALSE
    }
    if (canCheck) {
      jahr = citation_infos$jahr
      doi = citation_infos$DOI
      if (cr == "0") {
        cr_number = cr_number + 1
        cr_nummer_eintrag = paste("CR", cr_number, sep = "")
        checkCit = TRUE
      }
      if (checkCit) {
        autor_jahr_matches = cits$autor[(cits$autor == autor &
                                           cits$jahr == jahr)]
        if (length(autor_jahr_matches) > 0) {
          if (length(autor_jahr_matches) == 1) {
            cits$CR[(cits$autor == autor &
                       cits$jahr == jahr)] = cr_nummer_eintrag
          } else {
            autor_jahr_doi_matches = cits$autor[(cits$autor == autor &
                                                   cits$jahr == jahr
                                                 & cits$DOI == doi)]
            if (length(autor_jahr_doi_matches) > 0 &&
                trimws(doi) != "") {
              cits$CR[(cits$autor == autor &
                         cits$jahr == jahr & cits$DOI == doi)] = cr_nummer_eintrag
            } else {
              journal = citation_infos$journal # Journal des Eintrags
              if (!grepl("?", journal, fixed = TRUE) &
                  !grepl("]", journal, fixed = TRUE) &
                  !grepl("[", journal, fixed = TRUE)) {
                journals_for_autor = cits$journal[(cits$autor == autor &
                                                     cits$jahr == jahr)]
                unique_journals_for_autor = unique(journals_for_autor)
                similarJournals = "" # Initialisierung des Regex
                if (length(unique_journals_for_autor) > 1) {
                  for (j in 1:length(unique_journals_for_autor)) {
                    single_journal = unique_journals_for_autor[j]
                    isSimilar = grepl(single_journal, journal, fixed = TRUE)
                    if (isSimilar) {
                      if (similarJournals != "") {
                        similarJournals = paste(similarJournals,
                                                single_journal ,
                                                sep = "|")
                      } else {
                        similarJournals = single_journal
                      }
                    }
                  }
                  similarJournals <-
                    gsub(
                      pattern = "\\\\+",
                      replacement = "\\\\\\\\+",
                      x = similarJournals
                    )
                  similarJournals <-
                    gsub(
                      pattern = "\\\\.",
                      replacement = "\\\\\\\\.",
                      x = similarJournals
                    )
                  similarJournals <-
                    gsub(
                      pattern = "\\\\-",
                      replacement = "\\\\\\\\-",
                      x = similarJournals
                    )
                  similarJournals <-
                    gsub(
                      pattern = "\\\\)",
                      replacement = "\\\\\\\\)",
                      x = similarJournals
                    )
                  similarJournals <-
                    gsub(
                      pattern = "\\\\(",
                      replacement = "\\\\\\\\(",
                      x = similarJournals
                    )
                  similarJournals <-
                    gsub(
                      pattern = "\\\\*",
                      replacement = "\\\\\\\\*",
                      x = similarJournals
                    )
                  similarJournals <-
                    gsub(
                      pattern = "\\\\!",
                      replacement = "\\\\\\\\!",
                      x = similarJournals
                    )
                  cits$CR[(
                    cits$autor == autor & cits$jahr == jahr
                    & grepl(similarJournals, cits$journal)
                  )] =
                    cr_nummer_eintrag
                } else {
                  journal_matches = cits$CR[(
                    grepl(journal, cits$journal, fixed = TRUE)
                    &
                      cits$autor == autor & cits$jahr == jahr
                  )]
                  if (length(journal_matches) > 0) {
                    cits$CR[(
                      grepl(journal, cits$journal, fixed = TRUE)
                      &
                        cits$autor == autor & cits$jahr == jahr
                    )] =
                      cr_nummer_eintrag
                  } else {
                    cits$CR[i] = cr_nummer_eintrag
                  }
                }
              } else {
                cits$CR[i] = cr_nummer_eintrag
              }
            }
          }
        }
      }
    }
    if ((i \%\% 20) == 0) {
      print(paste(i, "of", cits_length, sep = " "))
    }
  }
  end_time = Sys.time()
  final_time = end_time - start_time
  print(final_time)
  return(cits)
}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ external }% use one of  RShowDoc("KEYWORDS")
