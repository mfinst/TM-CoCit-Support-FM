##
# corpus_gen
##
# prepares a certain data vector to build a corpus from
# it also filters stopwords, performes stemming, strips whitespace and removes punctuation
##
# @data.vector = documents represented as a vector
# @lang = string that represents the language
# @return: CorpusObject
##
corpus_gen <- function(data.vector, lang, furtherStops = NULL) {
  # erstellt den Corpus aus einer Vektorquelle
  corpus = tm::VCorpus(VectorSource(as.vector(data.vector)), readerControl = list(language=lang))

  # Verarbeitungsschritte mit tm_map
  corpus = tm_map(corpus, content_transformer(tolower))
  corpus = tm_map(corpus, stripWhitespace)
  corpus = tm_map(corpus, tm::removePunctuation)
  corpus = tm_map(corpus, tm::stemDocument, lang)
  corpus = tm_map(corpus, tm::removeWords, stopwords(lang))

  # Wenn weitere Filterwörter vorhanden sind
  if(!is.null(furtherStops)) {
    corpus = tm_map(corpus, removeWords, furtherStops)
  }
  return(corpus)
}
