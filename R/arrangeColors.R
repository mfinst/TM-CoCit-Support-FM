arrangeColors <-
function( colorVector, groupSizes, defaultColor= "lightgrey") {
  N = length(colorVector)
    result = replicate(length(groupSizes), defaultColor)
  for (i in 1:N) {
    result[as.numeric(names(groupSizes)[i])] = colorVector[i]
  }
  return(result)
}
