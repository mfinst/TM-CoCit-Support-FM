rebuild_data_source_cocit <-
function(paperTable) {
  start_time = Sys.time() # Zeit Benchmark
  
  # Initialisierung
  PNos = paperTable$Papern.No # Ein vektor für alle Paper.No
  papersLength = length(PNos) # Anzahl der Paper
  #papersLength = 50 # Fixed Iterationen für Debugging
  CRPlaceholder = "0"
  
  # Zitations Tracker
  allCRs = as.data.frame( x = c(), autor = c(), jahr = c(), journal = c(), version = c(), seite = c(),  CR = c(), DOI = c(), stringsAsFactors = FALSE)
  allCRs = rbind(allCRs, c("x","autor","jahr","journal","version","seite","CR","DOI")) # Hinzufügen einer pseudo Spalte, da sonst die Namen verschwienden
  allCRs = type.convert(x = allCRs, as.is = TRUE)
  allIgnoredCits = vector(mode="character", length=0)
  names(allCRs)[1] = "x"
  names(allCRs)[2] = "autor"
  names(allCRs)[3] = "jahr"
  names(allCRs)[4] = "journal"
  names(allCRs)[5] = "version"
  names(allCRs)[6] = "seite"
  names(allCRs)[7] = "CR"
  names(allCRs)[8] = "DOI"
  
  # Main Loop
  for(i in 1:papersLength) {
    print(paste(i, "of", papersLength)) # Anzeige welche Publikation grade extrahiert wird
    
    # Zerlegen aller Zitationen in eine Liste von Strings
    cocits = strsplit(x = as.character(CRs[i]), split = ";")
    rows = vector(mode="character", length=0)
    
    # Überspringe Paper ohne Zitationen
    if (length(cocits[[1]] > 0)){
      
      # Iteriere über die Liste der Zitationen
      for(y in 1:length(cocits[[1]])) {
        
        # Zerlege Zitations String in seine Bestandteile
        citationInfoList = strsplit(cocits[[1]][y], ",")
        
        # Bestimme DOI
        DOI = grep(x = citationInfoList[[1]], pattern = "DOI ", value = TRUE) # finde alle DOIs
        if (length(DOI) == 0 ) {
          # DOI ist Leer
          DOI = ''
        } 
        else {
          # DOI ist nicht leer
          # Prüfe ob mehrere DOIs gefunden wurden
          if (length(DOI) > 1) {
            DOI = DOI[1] # selektiere das erste DOI
            DOI = gsub("\\[", "", x = DOI)
          }
          DOI = gsub("DOI ", "", x = DOI) # entferne den String DOI
        }
        
        #CR = CRInt # Lege Zitationsnummer fest
        
        author = trimws(citationInfoList[[1]][1]) # Autor aus erster Stelle der Zitation
        year = trimws(citationInfoList[[1]][2]) # Jahreszahl aus zweiter Stelle der Zitation
        
        # Prüfe: Ist es eine Jahreszahl?
        if (is.na(as.numeric(year))) {
          year = "None" # Wenn keine Jahreszahl gefunden wurde
        } 
        
        # Prüfe: Journal an der dritten Stelle steht
        if (length(citationInfoList[[1]]) >= 3) {
          journal = trimws(citationInfoList[[1]][3])
        } else {
          journal = ""
        }
        
        # Prüfe: Version an vierter Stelle steht
        if (length(citationInfoList[[1]]) >= 4) {
          version = trimws(citationInfoList[[1]][4])
          if (!startsWith(version, "V")) {
            version = ""
          }
        } else {
          version = ""
        }
        
        # Prüfe: Seitenanzahl an fünfter Stelle steht
        if (length(citationInfoList[[1]]) >= 5) {
          seite = trimws(citationInfoList[[1]][5])
          if (!startsWith(seite, "P")) {
            seite = ""
          }
        } else {
          seite = ""
        }
        
        # Füge Zitation dem Ergebnis hinzu
        allCRs = rbind(allCRs, c(
          as.character(PNos[i]),
          as.character(author),
          as.character(year),
          as.character(journal),
          as.character(version),
          as.character(seite),
          as.character(CRPlaceholder), 
          as.character(DOI)
        ))
      }
    }
  }
  # Laufzeit Reminder
  end_time = Sys.time()
  final_time = end_time - start_time
  print(final_time) # Gibt die akkumulierte Laufzeit zurück
  
  return(allCRs)
}
