library("rvest")
library("stringr")
library("tidyr")
library("selectr")
library("RSelenium")
checkForServer()
startServer()
drv <- remoteDriver$new()
drv$open()

url <- "http://www.informs-sim.org/wsc00papers/prog00.htm"
webpage <- read_html(url)
winsim_table <- html_nodes(webpage, "table")

areas <- html_nodes(winsim_table, xpath ="//a[@name]") %>% html_text()

titles <- html_nodes(winsim_table, xpath ="//i//a[@href]") %>% html_text()

authors <- html_nodes(webpage, xpath = "//p//text()[2]") %>% html_text()
authors <- authors[c(grep("[(]", authors))]
authors <- c(authors[1:89], (html_nodes(webpage, xpath = "/html/body/ul[53]/p[2]/text()[1]") %>% html_text()),
             authors[90:length(authors)])

chairs <- html_nodes(webpage, xpath = "//body//text()") %>% html_text()
chairs <- chairs[c(grep("Chair", chairs))]

sessions <- html_nodes(webpage, xpath = "//b//text()[2]") %>% html_text()
sessions <- sessions[sessions != "Return \r\nto Top"]




