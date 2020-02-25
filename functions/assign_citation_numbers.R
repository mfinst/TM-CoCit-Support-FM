##
# Assign Citation Numbers
##
# This function assigns Citationnumbers to the extracted Citations from rebuild_data_source_cocit
##
# cits: A Dataframe with all Citations with the following dimensions:
#       x, autor, jahr, journal, version, seite, CR, DOI
# @return: The same dataframe as the input but with assigned Citationnumbers
##
assign_citation_numbers <-
  function (cits)
  {
    start_time = Sys.time()
    cits = type.convert(cits, as.is = TRUE)
    cr_number = 0
    cr_nummer_eintrag = "CR"
    cits_length = length(cits$x)
    for (i in 2:cits_length) {
      checkCit = FALSE
      canCheck = TRUE
      citation_infos = cits[i,]
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
          autor_jahr_matches = cits$autor[(cits$autor ==
                                             autor &
                                             cits$jahr == jahr)]
          if (length(autor_jahr_matches) > 0) {
            if (length(autor_jahr_matches) == 1) {
              cits$CR[(cits$autor == autor & cits$jahr ==
                         jahr)] = cr_nummer_eintrag
            }
            else {
              autor_jahr_doi_matches = cits$autor[(cits$autor ==
                                                     autor &
                                                     cits$jahr == jahr & cits$DOI ==
                                                     doi)]
              if (length(autor_jahr_doi_matches) > 0 &&
                  trimws(doi) != "") {
                cits$CR[(cits$autor == autor & cits$jahr ==
                           jahr &
                           cits$DOI == doi)] = cr_nummer_eintrag
              }
              else {
                journal = citation_infos$journal
                if (!grepl("?", journal, fixed = TRUE) &
                    !grepl("]", journal, fixed = TRUE) &
                    !grepl("[", journal, fixed = TRUE)) {
                  journals_for_autor = cits$journal[(cits$autor ==
                                                       autor &
                                                       cits$jahr == jahr)]
                  unique_journals_for_autor = unique(journals_for_autor)
                  similarJournals = ""
                  if (length(unique_journals_for_autor) >
                      1) {
                    for (j in 1:length(unique_journals_for_autor)) {
                      single_journal = unique_journals_for_autor[j]
                      if (is.na(single_journal) || is.na(journal)) {
                        isSimilar = FALSE
                      } else {
                        isSimilar = grepl(single_journal,
                                          journal, fixed = TRUE)
                      }
                      if (isSimilar) {
                        if (similarJournals != "") {
                          similarJournals = paste(similarJournals,
                                                  single_journal,
                                                  sep = "|")
                        }
                        else {
                          similarJournals = single_journal
                        }
                      }
                    }
                    similarJournals <- gsub(
                      pattern = "\\+",
                      replacement = "\\\\+",
                      x = similarJournals
                    )
                    similarJournals <- gsub(
                      pattern = "\\.",
                      replacement = "\\\\.",
                      x = similarJournals
                    )
                    similarJournals <- gsub(
                      pattern = "\\-",
                      replacement = "\\\\-",
                      x = similarJournals
                    )
                    similarJournals <- gsub(
                      pattern = "\\)",
                      replacement = "\\\\)",
                      x = similarJournals
                    )
                    similarJournals <- gsub(
                      pattern = "\\(",
                      replacement = "\\\\(",
                      x = similarJournals
                    )
                    similarJournals <- gsub(
                      pattern = "\\*",
                      replacement = "\\\\*",
                      x = similarJournals
                    )
                    similarJournals <- gsub(
                      pattern = "\\!",
                      replacement = "\\\\!",
                      x = similarJournals
                    )
                    cits$CR[(
                      cits$autor == autor & cits$jahr ==
                        jahr &
                        grepl(similarJournals, cits$journal)
                    )] = cr_nummer_eintrag
                  }
                  else {
                    journal_matches = cits$CR[(
                      grepl(journal,
                            cits$journal, fixed = TRUE) &
                        cits$autor ==
                        autor &
                        cits$jahr == jahr
                    )]
                    if (length(journal_matches) > 0) {
                      cits$CR[(
                        grepl(journal, cits$journal,
                              fixed = TRUE) & cits$autor ==
                          autor &
                          cits$jahr == jahr
                      )] = cr_nummer_eintrag
                    }
                    else {
                      cits$CR[i] = cr_nummer_eintrag
                    }
                  }
                }
                else {
                  cits$CR[i] = cr_nummer_eintrag
                }
              }
            }
          }
        }
      }
      if ((i %% 20) == 0) {
        print(paste(i, "of", cits_length, sep = " "))
      }
    }
    end_time = Sys.time()
    final_time = end_time - start_time
    print(final_time)
    return(cits)
  }