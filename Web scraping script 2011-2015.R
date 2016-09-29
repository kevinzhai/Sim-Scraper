# Scraping data for 2011 - 2015 from informs-sim.org
# The webpages for 2011 and 2012 are structured slightly differently at the highest level (area/section)
# However, their structure is identical from the session level down to the slots/papers

library(rvest)

# Create matrix to store output data
output <- matrix(ncol=7)

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


# Scraping data for 2010 (in progress)
#year = 2010
#link = paste("http://www.informs-sim.org/wsc",substring(as.character(year),3,4),"papers/prog",substring(as.character(year),3,4),".html", sep="")
#data = read_html(link)

#titles = data %>% html_nodes(".centered") %>% html_text()