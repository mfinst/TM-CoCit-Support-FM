gen_cocit_corpuslist <-
function(groupsWithAbstracts, mode = "corpus") {
  # Initialisierung
  groupCount = length(unique(groupsWithAbstracts[,1])) # Anzahl aller Gruppen
  abstractCount = length(groupsWithAbstracts[,1]) # Anzahl aller Eintr�ge
  result = list() # Ergebnis Liste
  currentGroup = 1 # Aktuelle Gruppe; Da die Liste sortiert eingegeben wird, steht Gruppe 1 immer am Anfang
  currentGroupList = vector(mode="character") # zwischen Ergebnis Vektor
  
  # Main Loop: �ber alle Eintr�ge
  for (i in 1:abstractCount) {
    
    # Wenn die Gruppennummer abweicht wird die Struktur erzeugt
    if (groupsWithAbstracts[,1][i] != currentGroup) {
      # Erstellt den Corpus
      if (mode == "corpus") {
        # nutzt die corpus_gen Methode um die VCorpus Objekte zu erstellen
        result[[currentGroup]] = corpus_gen(currentGroupList, lang = "english")
      } else if (mode == "abstract" || mode == "author") {
        # Modus f�r das zuweisen der Texte
        result[[currentGroup]] = currentGroupList
      }
      
      # Ergebnis hinzuf�gen
      currentGroup = groupsWithAbstracts[,1][i]
      currentGroupList = vector(mode="character")
    }
    if (!(mode == "author")) {
      if (groupsWithAbstracts[,4][i] != "") {
        # abstract found
        currentGroupList = append(currentGroupList,c(groupsWithAbstracts[,2][i],groupsWithAbstracts[,4][i]))
      }
    } 
    else {
      currentGroupList = append(currentGroupList,groupsWithAbstracts[,2][i])
    }
    
  }
  
  # generiert das letzte Objekt
  if ( mode == "corpus") {
    result[[groupCount]] = corpus_gen(currentGroupList, lang = "english")
  } else if (mode == "abstract" || mode == "author") {
    result[[groupCount]] = currentGroupList
  }
  
  return(result)
}
