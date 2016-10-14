#simuldata <- read.xlsx(file="1997-2015 Informs Data.xlsx", sheetIndex=1,stringsAsFactors = FALSE)
rm(list=ls()[!ls() %in% c('simuldata')])
output <- matrix(ncol = 3)
simuldata$Authors <- gsub("\\[ Full.*|\\[ Abstract.*","",simuldata$Authors)
simuldata$Authors <- gsub("\\),","\\)",simuldata$Authors)

for (i in c(1:102,104:1543, 1545:6558, 6560:nrow(simuldata))){
#for (i in 301:301){
    test <- simuldata[i,]
    
    repeat{
      test2 <- gsub("[0-9A-Za-z]","",test[7])
      test2 <- gsub("\\.|\\,|\\-|í|á|ú|ã|\\&|\\/|!","",test2)
      test2 <- gsub(" ", "", test2)
      if(grepl("\\(\\(",test2)||test$Authors=="NA"){
        test$Authors <- ""
      } else {
      temp_author <- str_match(test$Authors, ".*?, and.*\\(")
      if (is.na(temp_author) || (grepl("\\(",temp_author) && !(substring(temp_author,1,5)==", and"))){
        temp_author <- str_match(test$Authors, ".*?,")
      }
      if (is.na(temp_author) || (grepl("\\(",temp_author) && !(substring(temp_author,1,5)==", and"))){
        temp_author <- str_match(test$Authors, ".*?\\(")
      }
      temp_author <- temp_author[!is.na(temp_author)]
      temp_author <- trimws(gsub("\\(","",temp_author))
      temp_aff <- str_match(test$Authors,".*?\\(.*?\\)")
      temp_aff <- temp_aff[!is.na(temp_aff)]
      temp_aff <- paste("(",strsplit(temp_aff,"\\(")[[1]][[2]],sep="")
    
      # Write out author, affiliation, and paper name to new record
      temp_record <- c(i,temp_author,trimws(gsub("\\(|\\)","",temp_aff)))
      output <- rbind(output,temp_record)
    
      # Remove added author and affiliation (if necessary)
      test$Authors <- trimws(gsub(temp_author,"",test$Authors))
      if (substr(test$Authors,1,1) == "("){
      test$Authors <- trimws(sub(trimws(temp_aff),"",test$Authors,fixed=TRUE))
      }
      }
    if (test$Authors == ""){break}
      }
}


rownames(output) <- NULL # Re-number each record
colnames(output) <- c("PaperID", "Author", "Affiliation") # Name the columns of the output table
output <- output[2:nrow(output),] # Remove the first row of NAs that was created with the initial instanciation of the output matrix

# Re-joining suffixes separated by commas
for (i in 1:nrow(output)){
  if (output[i,2] %in% c("Jr.", "Jr.,")){
    output[i-1,2] <- paste(output[i-1,2]," Jr.", sep="")
    output <- output[-i,]
  }
  if (i >= nrow(output)){break}
}

# Splitting out records with "and" in them
for (i in 1:nrow(output)){
  if (grepl(" and ",output[i,2])){
    output <- rbind(output,c(output[i,1],gsub(" and ", "", str_extract(output[i,2]," and .*")),output[i,3]))
    output[i,2] <- gsub(" and .*", "", output[i,2])
  }
  if (i >= nrow(output)){break}
}

# Removing commas at the ends of records and "ands" at the 
for (i in 1:nrow(output)){
  output[i,2] <- gsub(",$|^and","",output[i,2])
}

write.xlsx(output,file="Authors, Papers, and Affiliations.xlsx")
