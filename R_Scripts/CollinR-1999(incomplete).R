library("rvest")
library("stringr")
library("tidyr")
library("selectr")
library("RSelenium")

url <- "http://www.informs-sim.org/wsc99papers/prog99sim.html"
webpage <- read_html(url)
winsim_table <- html_nodes(webpage, "table")

areas <- html_nodes(winsim_table, xpath ="//a[@name]") %>% html_text()
areas <- gsub("\r\n\r\nReturn to top \r\n","",areas)
areas <- areas[2:17]

titles <- html_nodes(webpage, xpath = "//body//dir//p//text()[1]") %>% html_text()
titles <- titles[titles != "Abstract"]
titles <- titles[titles != "Ø"]
titles <- titles[titles != "\r\n"]
titles <- gsub("\r\n"," ",titles)
titles <- titles[titles != "Full Paper (pdf format)"]
titles <- titles[titles != "k-p"]
titles <- titles[titles != " "]
titles <- titles[titles != "  "]

authors <- html_nodes(webpage, xpath = "//p//text()[2]") %>% html_text()
authors12 <- titles[c(grep("[(]", titles))]
authors103 <- authors12[3]
authors12 <- authors12[1]
authors <- authors[c(grep("[(]", authors))]
sequence = seq(1,length(authors),1)
authors = authors[sequence[!sequence %in% grep("Chair:",authors)]]
authors14 <- html_nodes(webpage,xpath = "/html/body/dir[12]/p/text()") %>% html_text()
authors14 <- authors14[3]
titles <- titles[titles != " Williard C. Hewitt, Jr. and Eric E. Miller (TransSolutions Corporation)"]
titles <- titles[titles !=" Harriet Black Nembhard, Ming-Shu Kao, and Gino Lim (University of Wisconsin-Madison)"]
##  James D. Arthur and Dell Lunceford are (to my very puny knowledge) unscrapeable ##
authors150 <- "Dell Lunceford (U.S. Army Model and Simulation Office)"
authors22 <- "James D. Arthur (Virginia Polytechnic Institute and State University) and Robert G. Sargent (Syracuse University)"
authors82 <- html_nodes(webpage, xpath = "/html/body/dir[49]/p[3]/text()[2]") %>% html_text()
authors93 <- html_nodes(webpage, xpath = "/html/body/dir[53]/p[3]/text()[3]") %>% html_text()
authors206 <- html_nodes(webpage, xpath = "/html/body/dir[94]/p[1]/text()[3]") %>% html_text()
authors = c(authors[1:11],authors12,authors[12:length(authors)])
authors = c(authors[1:13],authors14,authors[14:length(authors)])
authors = c(authors[1:21],authors22,authors[22:length(authors)])
authors = c(authors[1:81],authors82,authors[82:length(authors)])
authors = c(authors[1:92],authors93,authors[93:length(authors)])
authors = c(authors[1:102],authors103,authors[103:length(authors)])
authors = c(authors[1:149],authors150,authors[150:length(authors)])
authors = c(authors[1:205],authors206,authors[206:length(authors)])

chairs <- html_nodes(webpage, xpath = "//body//text()") %>% html_text()
chairs <- chairs[c(grep("Chair:", chairs))]

body <- html_nodes(webpage, xpath = "//html//text()") %>% html_text()
body <- body[51:length(body)]

areaind = c()
for (i in 1:length(areas)){
	areaind[i] = min(grep(areas[i],body))
}
titleind = c()
for (i in 1:length(titles)){
	titleind[i] = min(grep(substr(titles[i],1,30),body))
}
titleind <- gsub("Inf",1497,titleind)

areaslength = c()
for(i in 1:length(areaind)-1){
	areaslength[i] = findInterval(areaind[i]:areaind[i+1],titleind)