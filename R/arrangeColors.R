arrangeColors <-
function( colorVector, groupSizes, defaultColor= "lightgrey") {
  N = length(colorVector) # Anzahl der Farben
    result = replicate(length(groupSizes), defaultColor) # Ergebnis VEktor
  for (i in 1:N) {
    # An die Stellen wo die größten Cluster in der Liste stehen werden die Farben gesetzt
    result[as.numeric(names(groupSizes)[i])] = colorVector[i]
  }
  return(result)
}
