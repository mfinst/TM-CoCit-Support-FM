##
# Arrange Colors
##
# For Given N Colors, to the biggest groups in the groupSizes Table the colors are selected in order
# the remaining groups are getting the defaultColors
##
# colorVector: The Colors as a string vector like c("red", "blue")
# groupSizes: The Result of a sorted Table of wckrate
#             sort(table(mapAuthorToDOI(wckarate)[,1]),decreasing = TRUE)
# defaultColors: The defaultColors for the remaining Groups
#                Default is "lightgrey"
# @Return: A vector with elements same as the groups existing
##
arrangeColors <- function( colorVector, groupSizes, defaultColor= "lightgrey") {
  N = length(colorVector) # Anzahl der Farben
    result = replicate(length(groupSizes), defaultColor) # Ergebnis VEktor
  for (i in 1:N) {
    # An die Stellen wo die größten Cluster in der Liste stehen werden die Farben gesetzt
    result[as.numeric(names(groupSizes)[i])] = colorVector[i]
  }
  return(result)
}
