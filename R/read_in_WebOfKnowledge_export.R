read_in_WebOfKnowledge_export <-
  function(fileName = "savedrecs.txt",
           seperator = "\t",
           generate_PaperNo_Row = TRUE) {
    WoK_extracted = read.csv(
      fileName,
      header = TRUE,
      sep = seperator,
      encoding = "utf-8",
      na.strings = "",
      skipNul = TRUE,
      row.names = NULL,
      nrows = 0,
      quote = "",
      fill = FALSE
    )
    if (generate_PaperNo_Row) {
       WoK_extracted$Papern.No = seq.int(nrow(WoK_extracted))
    }
    return(WoK_extracted)
  }
