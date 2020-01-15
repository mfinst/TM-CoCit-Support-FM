corpus_gen <-
function(data.vector, lang, furtherStops = NULL) {
  # erstellt den Corpus aus einer Vektorquelle
  corpus = tm::VCorpus(tm::VectorSource(as.vector(data.vector)), readerControl = list(language=lang))

  # Verarbeitungsschritte mit tm_map
  corpus = tm::tm_map(corpus, tm::content_transformer(base::tolower))
  corpus = tm::tm_map(corpus, base::stripWhitespace)
  corpus = tm::tm_map(corpus, tm::removePunctuation)
  corpus = tm::tm_map(corpus, tm::stemDocument, lang)
  corpus = tm::tm_map(corpus, tm::removeWords, stopwords(lang))

  # Wenn weitere Filterw?rter vorhanden sind
  if(!is.null(furtherStops)) {
    corpus = tm::tm_map(corpus, tm::removeWords, furtherStops)
  }
  return(corpus)
}
