function(clustering.results, dataset, axis.name) {
  result = c()
  for(i in 1:clustering.results[["iter"]]) {
    vector = clustering.results[["cluster"]]
    vector = as.data.frame(vector)
    vector = dataset[axis.name][vector == as.double(i)]
    # print(vector)
    corpus = corpus_gen(vector, "english")
    # print(corpus)
    dtmKmeans = DocumentTermMatrix(corpusKmeans)
    dataKmeans1 = data.frame(word = names(sort(colSums(as.matrix(dtmKmeans)),decreasing=TRUE)),freq=sort(colSums(as.matrix(dtmKmeans)),decreasing=TRUE))
    # print(dataKmeans1)
    result[[i]] = dataKmeans1
  }
  return(result)
}
