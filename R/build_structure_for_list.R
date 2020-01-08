build_structure_for_list <-
function(structKey = 'tf', corpusList = NULL, tokenizer) {
  # Prüfe ob die Corpus Liste vorhanden ist
  if (is.null(corpusList)) {
    print('corpus is NULL!')
    return(NULL)
  }
  
  structureCount = length(corpusList)
  result_structList = list() # Ergebnis Liste
  
  for (i in 1:structureCount) {
    
    # generiert die geforderte Struktur mit der matrix_gen Funktion
    struct = matrix_gen(corpus = corpusList[[i]], struct = structKey, dtmTokenizer = tokenizer)
    result_structList[[i]] = struct # fügt Struktur dem Ergebnis hinzu
    
    print(paste(i, ' struct calculated'))
  }
  return(result_structList)
}
