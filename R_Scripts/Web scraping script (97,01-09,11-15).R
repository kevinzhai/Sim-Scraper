# Scraping data for 1997

output = matrix(ncol=7)

# Read in data for 1997
link = "http://informs-sim.org/wsc97papers/prog97sim.html"
data = read_html(link)

temp_year <- 1997 # current year for which we are looping
temp_area <- character() # highest level categorization, "area"
temp_session <- character() # second highest categorization, "session"
temp_chair <- character() # chair of each session
temp_details <- character(3) # vector containing title, authors, and abstract for each paper
temp_record <- character(7) # combined area, session, chair, and paper details
area_index <- integer() # vector containing indices of area nodes
session_index <- integer() # vector containing indices of session nodes
paper_index <- integer() # vector containing indices of paper nodes
session_name_index <- character() # vector containing names of sessions; used to identify sessions whose data is not stored in a "p" element

# Pull in all nodes corresponding to area names, sessions, and papers
# Note that some session names are not stored in a "center" or "p" node, but in a "b" instead; we will handle those later
titles <- data %>% html_nodes("center, p")

# Identify the nodes which contain information about the area (contain "h3" elements)
for (i in 15:length(titles)) {
  areas <- titles[[i]] %>% html_nodes("h3")
  
  if (length(areas) != 0)
  {area_index <- c(area_index,i)}
}

# Identify the nodes which contain information about the session (contain "b" elements)
# Also, create a list of all session names, regardless of the level at which they are stored
for (i in 15:length(titles)) {
  sessions <- titles[[i]] %>% html_nodes("b")
  session_name <- sessions %>% html_text()
  
  if (length(sessions) != 0)
  {session_index <- c(session_index,i)
  session_name_index <- c(session_name_index, session_name)}
}

# The first session within each area has its information stored differently from other sessions, because why the hell not
# (In a "b" element at the highest level, rather than as an element within a "p" element)
# These need to be identified by comparing the list of "b" elements under "p" elements to the whole list of "b" elements at all levels)
# Note that the chairs of these special sessions are not stored with the session names, unlike all of the others sessions, because life would just be too easy otherwise
# I filled in these session chairs with "NA" for the time being

special_sessions <- data %>% html_nodes("b") %>% html_text()
special_sessions <- special_sessions[!(special_sessions %in% session_name_index)]
special_sessions <- special_sessions[6:length(special_sessions)] # first few "b" elements are from the top of the page and can be discarded

# Some of the areas do not have sessions; in order to keep the special sessions aligned with the areas, null values have to be manually inserted
special_sessions[c(9,11:13)] <- "NA"
special_sessions[10] <- "Simulating Self-Similar Network Traffic"

# Identify the nodes which contain information about the papers (contain "i" elements)
for (i in 15:length(titles)) {
  papers <- titles[[i]] %>% html_nodes("a")
  
  if (length(papers) != 0)
  {paper_index <- c(paper_index,i)}
}

# For each paper, pull the appropriate fields
# Area and session are obtained by comparing the indices of the paper nodes to those of the areas and sessions nodes
for (i in 1:length(paper_index)) {
  temp_details[1] <- titles[[paper_index[[i]]]]  %>% html_nodes("a") %>% html_text() # Pull title from "a" subnode
  temp_details[1] <- gsub("\r\n", " ", temp_details[1]) # Remove formatting
  temp_details[2] <- titles[[paper_index[[i]]]]  %>% html_text() # Pull all of the text for the "p" node associated with the paper
  temp_details[2] <- gsub("\r\n", " ", temp_details[2]) # Remove formatting
  
  # If the title contains "C++", grep goes off the rails because it interprets the "+"s as operators rather than characters
  # There are obviously more elegant ways to address this, but my head hurts after 6 hours of this nonsense
  if (grepl("[[:punct:]]",temp_details[1])){
    temp_details[2] <- trimws(gsub(temp_details[1], " ", temp_details[2],fixed=TRUE))
  }
  else {
    temp_details[2] <- trimws(gsub(temp_details[1], " ", temp_details[2]))
  }
  
  temp_details[3] <- "NA"
  
  # Identify the area by pulling the largest index associated with an area that is less than the index associated with the current paper
  temp_area <- titles[[area_index[[max(which(area_index < paper_index[[i]]))]]]] %>% html_text
  
  # If the session is the first for a given area, use the special sessions table to identify the name
  if(session_index[[max(which(session_index < paper_index[[i]]))]] %in% area_index){
    temp_session <- special_sessions[max(which(area_index < paper_index[[i]]))]
    temp_chair = "NA"
  }
  
  # Otherwise, find the largest the largest index associated with an session that is less than the index associated with the current paper
  else {
    temp_session <- titles[[session_index[[max(which(session_index < paper_index[[i]]))]]]] %>% html_text
    temp_session <- gsub("\r\n", " ", temp_session) # Remove formatting
    temp_chair <- gsub("Chair:  |Chair: ", "", str_extract(temp_session,"Chair:.*")) # Extract the name of the chair from the session info
    temp_session <- trimws(gsub("Chair:.*", " ", temp_session)) # Remove the chair info from the session name
  }
  
  # Create the new record and add it to the list
  temp_record <- c(temp_year, temp_area, temp_session, temp_chair, temp_details) # Concatenate year, area, session, chair, paper title, authors, and abstract to make a new record
  output <- rbind(output,temp_record) # Add the new record to the existing list
}

# Post-scraping output matrix processing
output <- output[!(output[,6] == "Return to" |output[,6] == ""),] # Remove the records that were created as a result of pulling "b" elements with "return to top" or "" inside them


for (year in 2001:2009)
{
  
  temp_year <- as.character(year) # current year for which we are looping
  temp_area <- character() # highest level categorization, "area"
  temp_session <- character() # second highest categorization, "session"
  temp_chair <- character() # chair of each session
  temp_details <- character(3) # vector containing title, authors, and abstract for each paper
  temp_record <- character(7) # combined area, session, chair, and paper details
  area_index <- integer() # vector containing indices of area nodes
  paper_name <- character() # name of each paper
  paper_index <- integer() # vector containing indices of paper nodes
  paper_name_index <- character() # vector containing names of all papers
  session_index <- integer() # vector containing indices of session nodes
  session_name_index <- character() # vector containing names of sessions; used to identify sessions whose data is not stored in a "p" element
  area_name <- character() # name of current area
  area_name_index <- character() # vector containing names of all areas
  sessions2 <- character() # vector containing special session names that aren't stored in the normal format
  
  # Generate appropriate link based on year
  
  if (year >= 2007 && year <= 2009){
    link = paste("http://www.informs-sim.org/wsc",substring(temp_year,3,4),"papers/prog",substring(temp_year,3,4),"soc.html", sep="")
  }
  
  if (year >= 2004 && year <= 2006){
    link = paste("http://www.informs-sim.org/wsc",substring(temp_year,3,4),"papers/prog",substring(temp_year,3,4),".html", sep="")
  }
  
  if (year >= 2001 && year <= 2003){
    link = paste("http://www.informs-sim.org/wsc",substring(temp_year,3,4),"papers/prog",substring(temp_year,3,4),".htm", sep="")
  }
  
  data = read_html(link) # read in HTML data
  
  # Pull in all nodes corresponding to area names, sessions, and papers
  # Note that some session names are not stored in a "center" or "p" node, but in a "b" instead; we will handle those later
  titles <- data %>% html_nodes("div, font, p")
  
  # Identify the nodes which contain information about the area (contain "h3" elements)
    for (i in 1:length(titles)) {
    areas <- titles[[i]] %>% html_nodes("h3")
    area_name <- areas %>% html_text()
    
    # If the node contains the name of an area, add it to the vector containing all area names
    if (length(areas) != 0)
    {area_index <- c(area_index,i)
    area_name_index <- c(area_name_index,area_name)}
  }
  
  # Identify the nodes which contain information about each session
  for (i in 1:length(titles)) {
    
    sessions <- titles[[i]] %>% html_nodes("p")
    session_name <- sessions %>% html_text()
    # Remove the data that corresponds to link breaks or "return to top"
    session_name <- session_name[!(session_name %in% c("\r\n\r\n","[ Return \r\nto Top]","[ Return \r\nto Top  ]", "[ Return \r\nto Top | Program Information \r\nPage | WSC Home Page ]","\r\n","[ Return \r\nto Top ]","","[ Return to Top ]","[ Return to Top  ]",""))]
    
    # If the node contains the name of an area, add it to the vector containing all area names
    if (length(sessions) != 0 && length(session_name) != 0)
    {session_index <- c(session_index,i)
    session_name_index <- c(session_name_index, session_name)}
  }
  
  # If the previous loop was unable to retrieve any session names, set the minimum index to the length of the entire set of nodes
  if (length(session_index)==0){
    session_index <- length(titles)
  }
  
  # For the nodes before the first pulled session, use the time stamp to identify any additional session that were stored different
  for (i in (min(session_index)-1):1){
    sessions2 <- titles[[i]] %>% html_text()
    if (length(sessions2) != 0 && grepl(".*[0-9]{1}:[0-9]{2}.*|.*[0-9]{1}:[0-9]{2}.*",sessions2))
    {session_index <- c(i,session_index)
    session_name_index <- c(sessions2,session_name_index)}
  }
  
  # Do the same process as above for 2009, but with slight modifications due to its format
  if (temp_year == 2009){
    session_index <- integer()
    session_name_index <- character()
    for (i in 1:length(titles)){
      sessions2 <- titles[[i]] %>% html_text()
      if (length(sessions2) != 0 && grepl(".*[0-9]{1}:[0-9]{2}.*|.*[0-9]{1}:[0-9]{2}.*",sessions2)){
        session_index <- c(session_index,i)
        session_name_index <- c(session_name_index,sessions2[1])
      }
    }
  }
  
  # Identify the nodes which contain information about the papers (contain "i" elements)
  for (i in 1:length(titles)) {
    papers <- titles[[i]] %>% html_nodes("i , i a")
    paper_name <- papers %>% html_text()
    # Remove records that don't pertain to page titles
    paper_name <- paper_name[!(paper_name %in% c("Exploring New Frontiers","Full Paper as a pdf file","Return \r\nto Top"   ,""))]
    
    
    # If the node contains the name of a paper, add it to the vector containing all paper names
    if (length(papers) != 0 && length(paper_name != 0))
    {paper_index <- c(paper_index,i)
    paper_name_index <- c(paper_name_index, paper_name[1])}
  }
  
  # For each paper, pull the appropriate fields
  # Area and session are obtained by comparing the indices of the paper nodes to those of the areas and sessions nodes
  for (i in 1:length(paper_name_index)) {
    temp_details[1] <- paper_name_index[[i]] # Pull title from "a" subnode
    temp_details[1] <- gsub(" \r\n ", "", temp_details[1]) # Remove formatting
    temp_details[2] <- titles[[paper_index[[i]]]]  %>% html_text() # Pull all of the text for the "p" node associated with the paper
    temp_details[2] <- str_replace_all(temp_details[2],"ó", "o") # Replace the special character that appears once in all 10 years
    temp_details[2] <- gsub(" \r\n ", "", temp_details[2]) # Remove formatting
    
    # If the title contains punctuation, treat it as a fixed string of characters when extracting the author info
    if (grepl("[[:punct:]]",temp_details[1])){
      temp_details[2] <- trimws(gsub(temp_details[1], " ", temp_details[2],fixed=TRUE))
    }
    else {
      temp_details[2] <- trimws(gsub(temp_details[1], " ", temp_details[2]))
    }
    
    # If the author string contains a "full paper as pdf" reference, drop it
    if (grepl("pdf",temp_details[2],ignore.case=TRUE)){
      temp_details[2] <- substr(temp_details[2],1,nchar(temp_details[2])-28)
    }
    
    temp_details[3] <- "NA"
    
    # Identify the area by pulling the largest index associated with an area that is less than the index associated with the current paper
    temp_area <- area_name_index[[max(which(area_index < paper_index[[i]]))]]
    temp_area <- gsub("\r\n","",temp_area) # Remove formatting
    
    # Find the largest the largest index associated with an session that is less than the index associated with the current paper
    temp_session <- session_name_index[[max(which(session_index < paper_index[[i]]))]]
    temp_session <- gsub("\r\n", "", temp_session) # Remove formatting
    temp_chair <- str_extract(temp_session,"Chair:.*") # Extract the name of the chair from the session info
    temp_session <- trimws(gsub("Chair:.*", " ", temp_session)) # Remove the chair info from the session name
    temp_chair <- temp_chair <- gsub("Chair: ","",temp_chair)
    temp_session <- gsub(".*PM|.*P.M. ","",temp_session)
    temp_session <- gsub(".*AM|.*A.M. ","",temp_session)
    
    # Create the new record and add it to the list
    temp_record <- c(temp_year, temp_area, temp_session, temp_chair, temp_details) # Concatenate year, area, session, chair, paper title, authors, and abstract to make a new record
    output <- rbind(output,temp_record) # Add the new record to the existing list
  }
}

# Scraping data for 2011 - 2015 from informs-sim.org
# The webpages for 2011 and 2012 are structured slightly differently at the highest level (area/section)
# However, their structure is identical from the session level down to the slots/papers

# Loop through each year from 2011 to 2015
for (year in 2011:2015)
{
  temp_year <- as.character(year) # current year for which we are looping
  temp_area <- character() # highest level categorization, "area"
  temp_session <- character() # second highest categorization, "session"
  temp_chair <- character() # chair of each session
  temp_details <- character(3) # vector containing title, authors, and abstract for each paper
  temp_record <- character(7) # combined area, session, chair, and paper details
  temp_length <- character(1) # used to identify missing nodes/elements
  
  # The URL for 2011 is different from 2012 - 2015, so generate the URLs according to their respective patterns
  # Also, parse the HTML data for the webpage corresponding to the current year
  if (year > 2011)
  {link = paste("http://www.informs-sim.org/wsc",substring(as.character(year),3,4),"papers/by_area.html", sep="")}
  if (year == 2011)
  {link = paste("http://www.informs-sim.org/wsc",substring(as.character(year),3,4),"papers/prog",substring(as.character(year),3,4),".html", sep="")}
  data = read_html(link)

  # For the 2011 and 2012 webpages, the names of the areas are stored in "area" nodes that are distinct from the contents within each area
  # The contents of each area are stored in "section" nodes that have a 1-to-1 correspondence with the "area" nodes
  # Both are necessary to obtain all of the information needed for each record
  
  if (year == 2011 || year == 2012)
  {
    # Extract the master table in which all of the information is stored
    table <- data %>% html_nodes("table")
    
    # Extract the areas using the appropriate CSS selector, and remove the "Return To Top" elements also extracted by the same selector
    areas <- table %>% html_nodes(".centered") %>% html_text()
    areas <- areas[areas != "Return to Top"]
  
    # Extract the section associated with each area
    # The sessions are grouped under "section" nodes that are distinct from the area titles themselves, unlike the pages for 2013 - 2015
    # However, the HTML is structured so that there is a 1-to-1 correspondence between the section nodes and the area nodes
    # The number of section nodes pulled by this statement should equal the number of areas extracted in the previous step!
    sections <- table %>% html_nodes(".section-entry")
  }
  
  # From 2013 onwards, all of the session and paper data is stored within child nodes of the area nodes, so the area nodes contain everything we need
  else
  {
    areas <- data %>% html_nodes(".area-section")
  }
  
  # For each section/area, drill down to the level of the sessions and papers within them
  for (i in 1:length(areas))
  {
    
    # For 2011 and 2012, use the area nodes to obtain the area name and the section nodes to obtain the session information
    if (year == 2011 || year == 2012)
    {
      temp_area <- areas[[i]] # Save the name of the area corresponding to the current section, to be outputted later
      sessions <- sections[[i]] %>% html_nodes(".session-entry") # For the current section, pull all of the associated sessions
    }
    
    # For 2013 onwards, both the area name and session info can be obtained from the area nodes
    else
    {
      temp_area <- areas[[i]] %>% html_nodes(".section-title") %>% html_text() # Save the area currently being looped through, to be outputted at the end
      sessions <- areas[[i]] %>% html_nodes(".session-entry") # For the current area, pull all of the associated sessions 
    }
    
    # At this point we have a list of sessions within the current area
    # Sessions and lower-level child nodes are structured identically from 2011 - 2015, so no more year-based conditional logic is required
    
    # For each session in the current area, drill down to the level of the papers associated with the session
    for (j in 1:length(sessions)){
      
      temp_session <- sessions[[j]] %>% html_nodes(".session-title") %>% html_text() # Save the session currently being looped through, to be outputted at the end
      temp_chair <- sessions[[j]] %>% html_nodes(".session-chair") %>% html_text() # Save the chair of the session currently being looped through, to be outputted at the end
      if (length(temp_chair) == 0 || temp_chair == "") {temp_chair = "NA"} # If the "session chair" element is missing or blank, fill in with "NA"
      temp_chair <- gsub("Chair: ","",temp_chair) # Remove the string "Chair: " at the beginning of the text associated with each session chair
      
      slots <- sessions[[j]] %>% html_nodes(".slot-entry") # For the current session, pull all of the associated papers
      
      # For each paper in the current area and session, pull the title, authors, and abstract
      for (k in 1:length(slots)){
        
        # If there are no papers associated with the session, return "NA" for all 3 elements of interest
        if (length(slots) == 0)
        {temp_details <- c("NA", "NA", "NA")
        temp_record <- c(temp_year,temp_area,temp_session, temp_chair, temp_details) # combine all of the extracted elements to form new record
        output <- rbind(output,temp_record) # append new record to existing table
        break}
        
        # Check if the title is specified for the current paper
        temp_length <- slots[[k]] %>% html_nodes(".slot-title") %>% length()
        
        # If not, record "NA"; otherwise, extract the title and save it
        if (temp_length == 0) 
        {temp_details[1] = "NA"} 
        else{
          temp_details[1] <- slots[[k]] %>% html_nodes(".slot-title") %>% html_text()}
        
        # Check if authors are specified for the current paper
        temp_length <- slots[[k]] %>% html_nodes(".slot-authors") %>% length()
        
        # If not, record "NA"; otherwise, extract the authors and save them (in a single field)
        if (temp_length == 0)
        {temp_details[2] = "NA"} 
        else
        {temp_details[2] <- slots[[k]] %>% html_nodes(".slot-authors") %>% html_text()}
        
        # Check if an abstract is given for the current paper
        temp_length <- slots[[k]] %>% html_nodes(".slot-abstract") %>% length()
        
        # If not, record "NA"; otherwise, extract the abstract and save it (in a single field)
        if (temp_length == 0)
        {temp_details[3] = "NA"} 
        else
        {temp_details[3] <- slots[[k]] %>% html_nodes(".slot-abstract") %>% html_nodes("blockquote") %>% html_text()}
        
        
        # Create the new record and add it to the existing list of records
        temp_record <- c(temp_year, temp_area, temp_session, temp_chair, temp_details) # Concatenate year, area, session, chair, paper title, authors, and abstract to make a new record
        output <- rbind(output,temp_record) # Add the new record to the existing list
      }
    }
  }
}

# Post-scraping output matrix processing
rownames(output) <- NULL # Re-number each record
colnames(output) <- c("Year", "Area", "Session", "Chair", "Title", "Authors", "Abstract") # Name the columns of the output table
output <- output[2:nrow(output),] # Remove the first row of NAs that was created with the initial instanciation of the output matrix