##
# QUERY Builder WoK
##
# DOIvektor: Vektor mit DOIs
# @Return: String in the format of CO=(X1 OR X2 OR ... OR Xn) 
#          Used for Advanced Searchrequests at the Web of Knowledge
##
build_Web_of_Knowledge_query <- function(DOIvector) {
  result = "DO=("
  for(i in 1:length(DOIvector)) {
    if (DOIvector[i] != "") {
      result = paste(result, DOIvector[i], " OR ", sep= "")
    }
  }
  
  # entfernt den letzten " OR" Eintrag
  result = substr(result,1,nchar(result)-3)
  return(paste(result, ")", sep = ""))
}