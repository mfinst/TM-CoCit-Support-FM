assign_citation_numbers <-
  function(cits) {
    start_time = Sys.time() # Zeit Benchmark
    
    cits = type.convert(cits, as.is = TRUE) # Konvertieren der Eingabe in Strings
    
    # Initialisierung
    cr_number = 0 # Zahlenwert der Nummer
    cr_nummer_eintrag = "CR" # String Anteil der Zitationsnummer
    cits_length = length(cits$x) # Anzahl der Zitationen
    # cits_length = 150 # Fixed Anzahl f?r Debugging
    
    # Main Loop
    for (i in 2:cits_length) {
      checkCit = FALSE # Flag ob Zitation ?berpr?ft werden muss
      canCheck = TRUE # Flag ob die Zitation ?berpr?ft werden kann
      citation_infos = cits[i, ] # Informationen ?ber die aktuelle Zitation
      
      cr = citation_infos$CR # hole Zitationsnummer aus dem Datensatz
      autor = citation_infos$autor # Autorname
      
      # Pr?fe ob Fragezeichen im Autornamen vorhanden sind
      if (!grepl("?", autor)) {
        # Zitationen mit Fragezeichen im Autorennamen werden als Broken gewertet
        # und werden im weiteren nicht ber?cksichtigt
        canCheck = FALSE
      }
      
      # Pr?fe ob eine ?berfl?ssige Spalte vorliegt
      if (cr == "CR") {
        canCheck = FALSE
      }
      
      # Pr?fe Zitation sobald Zitation Pr?fbar ist
      if (canCheck) {
        jahr = citation_infos$jahr # Jahr der Zitation
        doi = citation_infos$DOI # DOI der Zitation
        
        # Pr?fe ob die Zitationsnummer den generischen Eintrag hat
        if (cr == "0") {
          cr_number = cr_number + 1 # Inkrementiere Zitationsnummer
          cr_nummer_eintrag = paste("CR", cr_number, sep = "") # Erstelle Zitationsnummer
          checkCit = TRUE
        }
        
        # Pr?fen der Zitation auf weitere gleiche Eintr?ge f?r gleiche Zitationsnummer
        if (checkCit) {
          # Holt alle Zeilen mit der gleichen Autor Jahr Kombination
          autor_jahr_matches = cits$autor[(cits$autor == autor &
                                             cits$jahr == jahr)]
          
          # Pr?fe ob ?berhaupt eine Kombination vorliegt
          if (length(autor_jahr_matches) > 0) {
            # Einzelne Eintr?ge k?nnen sofort zugewiesen werden!
            if (length(autor_jahr_matches) == 1) {
              cits$CR[(cits$autor == autor &
                         cits$jahr == jahr)] = cr_nummer_eintrag  # Weise Nummer zu
            } else {
              # Es liegen mehrere gleiche Autor Jahr Kombinationen vor!
              # Alle Eintr?ge der Autor Jahr Kombination mit gleicher DOI
              autor_jahr_doi_matches = cits$autor[(cits$autor == autor &
                                                     cits$jahr == jahr & cits$DOI == doi)]
              
              # wenn eine DOI vorhanden ist, dann erhalten alle Eintr?ge die gleiche DOI
              if (length(autor_jahr_doi_matches) > 0 &&
                  trimws(doi) != "") {
                cits$CR[(cits$autor == autor &
                           cits$jahr == jahr & cits$DOI == doi)] = cr_nummer_eintrag
              } else {
                # Es ist keine DOI vorhanden!
                journal = citation_infos$journal # Journal des Eintrags
                
                # Pr?fe ob der Journalname Fragezeichen oder Klammern enth?lt, da so sonst ein Broken Eintrag vorliegt
                if (!grepl("?", journal, fixed = TRUE) &
                    !grepl("]", journal, fixed = TRUE) &
                    !grepl("[", journal, fixed = TRUE)) {
                  # Journaleintrag ist OK!
                  
                  # hole alle Journals der aktuellen Autor Jahr Kombination
                  journals_for_autor = cits$journal[(cits$autor == autor &
                                                       cits$jahr == jahr)]
                  
                  # eliminiere doppelte Namen
                  unique_journals_for_autor = unique(journals_for_autor)
                  similarJournals = "" # Initialisierung des Regex
                  
                  if (length(unique_journals_for_autor) > 1) {
                    # check ob mehrere Journals existieren
                    
                    # f?r jedes unterschiedliche Journal
                    for (j in 1:length(unique_journals_for_autor)) {
                      single_journal = unique_journals_for_autor[j] # einzelnes Journal
                      
                      if (is.na(single_journal) || is.na(journal)) {
                        isSimilar = FALSE
                      } else {
                        isSimilar = grepl(single_journal,
                                          journal, fixed = TRUE)
                      }
                      
                      if (isSimilar) {
                        # Journal ist ?hnlich
                        # Baue Regex String
                        if (similarJournals != "") {
                          # bedenke erstes Element beim Bauen des Regex Strings
                          similarJournals = paste(similarJournals,
                                                  single_journal ,
                                                  sep = "|")
                        } else {
                          similarJournals = single_journal
                        }
                      }
                    }
                    # nun kann die Zitation mit den gleichen Journals die gleiche CR Nummer erhalten
                    #
                    # Spezielle Character m?ssen escaped werden, da der Regex diese Auswerten w?rde
                    # Das w?rde zu Fehlern bei der Ausf?hrung f?hren
                    similarJournals <-
                      gsub(
                        pattern = "\\+",
                        replacement = "\\\\+",
                        x = similarJournals
                      )
                    similarJournals <-
                      gsub(
                        pattern = "\\.",
                        replacement = "\\\\.",
                        x = similarJournals
                      )
                    similarJournals <-
                      gsub(
                        pattern = "\\-",
                        replacement = "\\\\-",
                        x = similarJournals
                      )
                    similarJournals <-
                      gsub(
                        pattern = "\\)",
                        replacement = "\\\\)",
                        x = similarJournals
                      )
                    similarJournals <-
                      gsub(
                        pattern = "\\(",
                        replacement = "\\\\(",
                        x = similarJournals
                      )
                    similarJournals <-
                      gsub(
                        pattern = "\\*",
                        replacement = "\\\\*",
                        x = similarJournals
                      )
                    similarJournals <-
                      gsub(
                        pattern = "\\!",
                        replacement = "\\\\!",
                        x = similarJournals
                      )
                    
                    # Weise allen Autor Jahr Kombinationen mit ?hnlichem Journal die gleiche Numemr zu
                    cits$CR[(
                      cits$autor == autor &
                        cits$jahr == jahr &
                        grepl(similarJournals, cits$journal)
                    )] = cr_nummer_eintrag
                    
                  } else {
                    # Es gibt nur einen Journalnamen f?r die Autor Jahr Kombination!
                    journal_matches = cits$CR[(
                      grepl(journal, cits$journal, fixed = TRUE) &
                        cits$autor == autor & cits$jahr == jahr
                    )]
                    
                    if (length(journal_matches) > 0) {
                      cits$CR[(
                        grepl(journal, cits$journal, fixed = TRUE) &
                          cits$autor == autor & cits$jahr == jahr
                      )] = cr_nummer_eintrag
                    } else {
                      # Fallback Eintrag
                      cits$CR[i] = cr_nummer_eintrag
                    }
                  }
                  
                } else {
                  # Unbedeutender Eintrag, da Fragezeichen im autor sind, deshalb unterschiedliche CR Nummer
                  cits$CR[i] = cr_nummer_eintrag
                }
                #journal check end
              }
            }
          }
        }
      }
      # Loop Progress Notifier
      # Alle 20 Zitationen wird der Progress ausgegeben
      if ((i %% 20) == 0) {
        print(paste(i, "of", cits_length, sep = " "))
      }
      # Loop end
    }
    # Time Benchmark
    end_time = Sys.time()
    final_time = end_time - start_time
    print(final_time)
    
    cits = cits[-1,]
    colnames(cits)[1] = 'PNo'
    colnames(cits)[2] = 'Autor'
    colnames(cits)[3] = 'Jahr'
    colnames(cits)[7] = 'CRNo'
    
    return(cits)
  }
