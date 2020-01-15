##
# Mark Authors with abstracts
##
# Maps the * to all vertices with abstracts on the dd Datastructure
# after that rerun the whole neighborship process
##
# ddResult: Basically the result of the CoCit Calculation as a table
# mappedValues: the mapped values calculated with mapAuthorToDOI(wckarate)
# return: dd structure with new author Names
##
markAuthors <- function(ddResult, mappedValues) {
  # Initialisierung
  # Type Konvertierung wegen potenzieller Faktormengen
  ddResult = type.convert(x = ddResult, as.is = TRUE)

  # Whitespace Entfernung an Anfang und Ende der DOIs
  ddResult$A_DOI = trimws(ddResult$A_DOI)
  ddResult$B_DOI = trimws(ddResult$B_DOI)

  # Erstelle Dataframe für bessere Lesbarkeit der Funktion
  mappedValues = as.data.frame(mappedValues, stringsAsFactors = FALSE)
  colnames(mappedValues)[1] = "GROUPS"
  colnames(mappedValues)[2] = "AUTHOR"
  colnames(mappedValues)[3] = "DOI"
  colnames(mappedValues)[4] = "ABSTRACT"
  # Entferne trailing Whitespace für Autor und DOIs
  mappedValues$AUTHOR = trimws(mappedValues$AUTHOR)
  mappedValues$DOI = trimws(mappedValues$DOI)

  # Main Loop; über alle Autoren der cocit Tabelle
  for(i in 1:length(ddResult$A)) {

    row = ddResult[i,] # holt die aktuelle Zeile

    # Zuweisung der Zeilen Informationen
    authorA = row$A
    authorADOI = row$A_DOI
    authorB = row$B
    authorBDOI = row$B_DOI

    # prüfe ob die DOI in den exportierten Daten exisitiert
    abs1 = as.character(mappedValues$ABSTRACT[as.character(mappedValues$DOI)==authorADOI])
    abs2 = as.character(mappedValues$ABSTRACT[as.character(mappedValues$DOI)==authorBDOI])

    # Wenn ein Abstract mehrfach existiert, selektiere das erste
    if(length(abs1)>1) {
      abs1 = abs1[1]
    }
    if(length(abs2)>1) {
      abs2 = abs2[1]
    }

    # Wenn ein Abstract gefunden wurde
    # Markiere Autor
    if (length(abs1 != 0)) {
      if (abs1 != "") {
        authorA = paste("*", authorA, sep = "")
        print(paste('-A-', authorA, sep = ' '))
        ddResult[i,]$A = as.character(authorA)
      }
    }
    if (length(abs2) != 0) {
      if (abs2 != "") {
        authorB = paste("*", authorB, sep = "")
        print(paste('-B-', authorB, sep = ' '))
        ddResult[i,]$B = as.character(authorB)
      }
    }

  }
  return(ddResult)
}
