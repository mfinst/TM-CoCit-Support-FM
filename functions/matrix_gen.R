##
# matrix_gen
##
# Function to generate a dtm and an tfidf from a corpus
##
# corpus: Corpus Object that resemples the texts
# structKey: 'm', 'v', 'tfidf, 'dtm'; tells the function which structure shall be returned
#              m = Matrix with the words, 
#              v = sorted colums vector
#              d or tf = frequency matrix with percentages, 
#              tfidf = term frequency inverse document frequency
#              dtm = document term matrix
# tokenizer: used if word combinations matter
# @return: Structure specified by the structKey
matrix_gen <- function(corpus = NULL, structKey = 'm', dtmTokenizer = NULL) {
  
  # Die DocumentTermMatrix wird zuerst erstellt
  if (!is.null(dtmTokenizer)) {
    dtm = DocumentTermMatrix(corpus, control = list(tokenize = phraseTokenizer))
  } else {
    dtm = DocumentTermMatrix(corpus)
  }
  if(structKey == 'dtm'){
    return(dtm)
  }
  
  # Matrix Berechnung
  m = as.matrix(dtm)
  if(structKey == 'm') {
    return(m)
  }
  v = sort(colSums(m), decreasing=TRUE)
  if(structKey == 'v') {
    return(v)
  }
  
  # TF Matrix Berechnung
  d = data.frame(word = names(v), freq = v)
  if(structKey == 'd' || structKey == 'tf') {
    return(d)
  }
  
  # TF-IDF Berechnung
  dtm.tfidf = weightTfIdf(dtm)
  dtm.tfidf = removeSparseTerms(dtm.tfidf, 0.999)
  tfidf.matrix = as.matrix(dtm.tfidf)
  if(structKey == 'tfidf') {
    return(dtm.tfidf)
  }
}