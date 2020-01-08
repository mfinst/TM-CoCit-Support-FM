corpus_gen <-
function(data.vector, lang, furtherStops = NULL) {
  # erstellt den Corpus aus einer Vektorquelle
  corpus = VCorpus(VectorSource(as.vector(data.vector)), readerControl = list(language=lang))
  
  # Verarbeitungsschritte mit tm_map
  corpus = tm_map(corpus, content_transformer(tolower))
  corpus = tm_map(corpus, stripWhitespace)
  corpus = tm_map(corpus, removePunctuation) 
  corpus = tm_map(corpus, stemDocument, lang)
  corpus = tm_map(corpus, removeWords, stopwords(lang))
  
  # Wenn weitere Filterwörter vorhanden sind
  if(!is.null(furtherStops)) {
    corpus = tm_map(corpus, removeWords, furtherStops)
  }
  return(corpus)
}
