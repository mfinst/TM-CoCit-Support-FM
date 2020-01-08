##
# Map Author to DOI
##
# Maps the groups to the corresponding DOI values
##
# groups: Ergebnis des walktrap.community(g) 
# markRelevant = if TRUE, all authors who have an abstract will get a * before their name
# @return: 2 Dimensionale Liste [1] group [2] autor/jahr [3] DOI ODER "" wenn keine DOI vorhanden  [4] Abstract if there
##
mapAuthorToDOI <- function(groups, markRelevant = FALSE, marking.symbol = '*') {
  result = vector(mode="character", length = 0)
  for(i in 1:length(groups)) {
    authors = groups[[i]]
    group = vector(mode="character", length = 0)
    # Map Autor to DOI
    for (y in 1:length(authors)) {
      DOI = ''
      if (!is.na(dd$B_DOI[dd$B == authors[y]][1])) {
        DOI = dd$B_DOI[dd$B == authors[y]][1]
      } else if (!is.na(dd$A_DOI[dd$A == authors[y]][1])) {
        DOI = dd$A_DOI[dd$A == authors[y]][1]
      }
      # get Abstract
      abstract = WoK_extracted$AB[WoK_extracted$DI == trimws(as.character(DOI[[1]]))]
      author = authors[y]
      if ( length(abstract) == 0) {
        abstract = ""
      } else if (is.na(trimws(as.character(abstract)))) {
        abstract = ""
      } else if (markRelevant) {
        # author is relevant
        author = paste(marking.symbol,author,sep = '')
      }
      # append DOI for autor
      row = c(i,author,as.character(DOI[[1]]), as.character(abstract))
      group = rbind(group, row)
    }
    result = rbind(result, group)
  }
  return(result)
}