function(ldaObject, vcorpusObject) {
  # get count of documents
  topicCount = length(table(topics(ldaObject)))
  result = list()
  # extract
  for (i in 1:topicCount) {
    extracted <- vcorpusObject[
      as.integer(
        names(topics(ldaObject)[topics(ldaObject) == as.character(i)])
      )
      ]
    print(extracted)
    result[[i]] = extracted
  }
  return(result)
}
