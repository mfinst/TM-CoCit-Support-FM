function(kmeansObject, vcorpusObject) {
  clusterCount = length(table(clusterAll.kmeans$cluster))
  result = list()
  # extract
  # ToDo: Improve Failsafeness
  for (i in 1:clusterCount) {
    extracted = vcorpusObject[clusterAll.kmeans$cluster == i]
    print(extracted)
    result[[i]] = extracted
  }
  return(result)
}
