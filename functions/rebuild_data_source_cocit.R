##
# Rebuild CoCit from papern 
##
# This function rebuilds the cocitations Table with DOI Information based on a paperTable
##
# paperTable: An imported csv table from web of knowledge with papern.No for each Row
#             File can be read like this: read.csv("file", sep = ";", header = TRUE, skip = 1)
# @return: A Table with the following information per row: PNo, CRNo, Author, Year, DOI
##
function(paperTable, ignoreCRs = FALSE) {
  start_time = Sys.time()
  # init stuff
  CRInt = 1
  # vectorize
  #authors = paperTable$AU
  PNos = paperTable$Papern.No
  CRs = paperTable$CR
  # init empty for rbind
  result = vector(mode="character", length=0)
  # iterate over every paper
  papersLength = length(PNos)
  #papersLength = 50
  # global Citation tracker
  allCRs = as.data.frame( x = c(), autor = c(), jahr = c(), journal = c(), version = c(), seite = c(),  CR = c(), DOI = c(), stringsAsFactors = FALSE)
  allCRs = rbind(allCRs, c("x","autor","jahr","journal","version","seite","CR","DOI"))
  allCRs = type.convert(x = allCRs, as.is = TRUE)
  allIgnoredCits = vector(mode="character", length=0)
  names(allCRs)[1] = "x"
  names(allCRs)[2] = "autor"
  names(allCRs)[3] = "jahr"
  names(allCRs)[4] = "journal"
  names(allCRs)[5] = "version"
  names(allCRs)[6] = "seite"
  names(allCRs)[7] = "CR"
  names(allCRs)[8] = "DOI"
  includeInt = 0
  errorCounter = 0
  errorCits = vector(mode="character", length = 0)
  # for testing; only the first two papers
  #for(i in 1:50) {
    # for productive
  for(i in 1:papersLength) {
    # Progress Print
    print(paste(i, "of", papersLength))
    cocit = strsplit(x = as.character(CRs[i]), split = ";")
    rows = vector(mode="character", length=0)
    # skip empty rows
    if (length(cocit[[1]] > 0)){
      # generate row
      for(y in 1:length(cocit[[1]])) {
        # extract infos
        # split seperate Lines
        # retriev infos
        infos = strsplit(cocit[[1]][y], ",")
        # find DOI
        possibleDOI = grep(x = infos[[1]], pattern = "DOI ", value = TRUE)
        if (length(possibleDOI) == 0 ) {
          # DOI is empty
          possibleDOI = ''
        } 
        else {
          # detect if more than one is found
          if (length(possibleDOI) > 1) {
            possibleDOI = possibleDOI[1]
            possibleDOI = gsub("\\[", "", x = possibleDOI)
          }
          possibleDOI = gsub("DOI ", "", x = possibleDOI)
          # trimws(possibleDOI)
        }
        CR = CRInt
        author = trimws(infos[[1]][1])
        year = trimws(infos[[1]][2])
        # nchar(year) != 4
        if (is.na(as.numeric(year))) {
          # string to long, to short or not a number
          year = "None"
        } 
        if (length(infos[[1]]) >= 3) {
          journal = trimws(infos[[1]][3])
        } else {
          journal = ""
        }
        if (length(infos[[1]]) >= 4) {
          version = trimws(infos[[1]][4])
          if (!startsWith(version, "V")) {
            version = ""
          }
        } else {
          version = ""
        }
        if (length(infos[[1]]) >= 5) {
          seite = trimws(infos[[1]][5])
          if (!startsWith(seite, "P")) {
            seite = ""
          }
        } else {
          seite = ""
        }
        if (!ignoreCRs) {
          #authYearCombo = paste(author, year, sep = " ")
          # check if zitation ist bereits aufgetreten!
          
          foundCR = allCRs$CR[(allCRs$author == author & allCRs$year == year)]
          foundJournal = allCRs$journal[(allCRs$author == author & allCRs$year == year)]
          foundVersion = allCRs$version[(allCRs$author == author & allCRs$year == year)]
          foundSeite = allCRs$seite[(allCRs$author == author & allCRs$year == year)]
          foundDOI = allCRs$DOI[(allCRs$author == author & allCRs$year == year)]
          newCitation = FALSE
          if (length(foundCR) < 1 || author =="[Anonymous]") {
            newCitation = TRUE
          } else if (foundDOI == possibleDOI) {
            newCitation = FALSE
            #Journal Check
          } else if (grepl(journal, foundJournal, fixed = TRUE) || grepl(foundJournal, journal, fixed = TRUE)) {
            newCitation = FALSE
          } else {
            newCitation = FALSE
          }
          
          if (newCitation) {
            # make row for Citation table
            allCRs = rbind(allCRs, c(
              as.character(PNos[i]),
              as.character(author),
              as.character(year),
              as.character(journal),
              as.character(version),
              as.character(seite),
              as.character(CR),
              as.character(possibleDOI)
            ))
            includeInt = includeInt + 1
            CR = CRInt
            CRInt = CRInt + 1
          } else {
            CR = foundCR[1]
          }
        } else {
          # all CRs 0 for debugging cases
          allCRs = rbind(allCRs, c(
            as.character(PNos[i]),
            as.character(author),
            as.character(year),
            as.character(journal),
            as.character(version),
            as.character(seite),
            as.character(CR),
            as.character(possibleDOI)
          ))
          includeInt = 0
          CR = 0
          CRInt = 0
        }
        
        newRow = c(
          as.character(PNos[i]),
          paste('CR', CR, sep = ""),
          author,
          year,
          possibleDOI)
        
        if (!length(newRow) == 5) {
          print(paste("differing Length", length(newRow)))
          print(newRow)
        }
        rows = rbind(rows, newRow)
      }
      # add row to result
      result = rbind(result, rows)
    }
  }
  # runtime evaluation
  end_time = Sys.time()
  final_time = end_time - start_time
  print(final_time)
  # build result frame!
  result = data.frame(result, row.names = NULL)
  names(result)[1] = "PNo"
  names(result)[2] = "CRNo"
  names(result)[3] = "Autor"
  names(result)[4] = "Jahr"
  names(result)[5] = "DOI"
  # errors
  #print(allCRs)
  # run again!
  #finalIgnoredCRsDF = data.frame(allCRs, row.names = NULL, stringsAsFactors = FALSE)
  if (ignoreCRs) {
    allCRs = allCRs[-1,]
    colnames(allCRs)[1] = "PNo"
    return(allCRs)
  }
  if ( errorCounter > 0) {
    print(paste(errorCounter, 'Errors detected'))
    return(list(result, errorCits))
  }
  return(result)
}
