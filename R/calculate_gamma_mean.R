function(lda_object) {
  # determine Array Length
  length_array = count(as.data.frame(lda_object@gamma))$n
  # init array
  mean_array = vector( mode = "double", length = length_array)
  for (i in 1: length_array) {
    mean_array[i] = max(as.data.frame(lda_object@gamma)[i,])
    # debugging
    # print(max(as.data.frame(lda_object@gamma)[i,]))
  }
  # debugging
  # print(mean_array)
  return(mean_array)
}
