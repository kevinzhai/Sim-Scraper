setwd("C:/Users/Collin/Documents/ACA")
library(rvest)
library(plyr)
url = c()
r = c(as.character(97:99),"00","01","02","03","04","05","06","07","08","09",as.character(10:15))
for (y in 1:length(r)){
if (y <= 3){
url[y] <- paste("http://www.informs-sim.org/wsc",r[y],"papers/prog",r[y],"sim.html", sep = "")
}
else if (y >= 16){
url[y] <- paste("http://www.informs-sim.org/wsc",r[y],"papers/by_area.html", sep = "")
}
else{
url[y] <- paste("http://www.informs-sim.org/wsc",r[y],"papers/prog",r[y],".html", sep = "")
}
}
######### 1997 Data ##################
webpage <- html(url[1])
## Accesssing Session Names
sessionNodes = html_nodes(webpage,"b")
sessions = html_text(sessionNodes)
sessions_1 = sessions[5:length(sessions)]
## Removing "Return to top " lines
sessions_2 = sessions_1[which(sessions_1 != "Return to top ")]
## Accessing Titles and removing "top" lines
titleNodes <- html_nodes(webpage,"a")
titles97 <- html_text(titleNodes)
ind = which(titles97 != "" & titles97 != "top")
titles97_1 = titles97[ind]
## Accessing Area Names and Modifying the list for search purposes
areaNodes = html_nodes(webpage,"td")
areas = html_text(areaNodes)
areas[12] = "STATE-OF-THE-ART TUTORIALS"
areas[13] = "INFORMS college on Simulation Ph.D. Colloquium"
areas[14] = "poster session"
## Finding Areas names in the Title and Session vectors and removing them
n=1
m=1
index1 = c()
index2 = c()
for (i in 1:length(areas)){
	ind1 = grep(areas[i],sessions_2,ignore.case=TRUE)
	ind2 = grep(areas[i],titles97_1,ignore.case=TRUE)
	if (length(ind1)!=0){
		index1[n] <- min(ind1)
		n = n + 1
	}
	else{}
	if (length(ind2)!=0){
		index2[m] <- min(ind2)
		m = m + 1
	}
	else{}
}
seq1 = seq(1,length(sessions_2),1)
sessions_2 = sessions_2[seq1[!seq1 %in% index1]]
seq2 = seq(1,length(titles97_1),1)
titles97_1 = titles97_1[seq2[!seq2 %in% index2]]
## Recording areas for every title
areas_1 = areas
for (l in 1:length(index1)-1){
	min1 = sort(index1[l])
	min2 = sort(index1[l+1])
	areas_1[min1:min2] = areas[l]
}
## Accessing Authors and Removing unnecessary lines
authors = c()
for (i in 4:323){
	authorNodes = html_nodes(webpage,xpath = paste("/html/body/p[",i,"]/text()",sep = ""))
	authors_temp = html_text(authorNodes)
	text1 = gsub("\r\n ","",authors_temp)
	text2 = gsub("\r\n","",text1)
	text2[text2==""] <- NA
	text3 = na.omit(text2)
	authors[i] = paste(unlist(as.data.frame(text3)),collapse = " ")
}
authors[authors==""] <- NA
authors = na.omit(authors)
## Removing Chair names from author vector and creating their own vector
Chairs_1 = authors[grep("Chair:",authors)]
seq3 = seq(1,length(authors),1)
authors = authors[seq3[!seq3 %in% grep("Chair:",authors)]]
authors[grep("Harry Crisp", authors)] <- NA
authors = na.omit(authors)
titles_2 = c()
n=1
for (i in 304:323){
	titlesNodes = html_nodes(webpage,xpath = paste("/html/body/p[",i,"]/i/text()",sep=""))
	titles_temp = html_text(titlesNodes)
	text1 = gsub("\r\n ","",titles_temp)
	text2 = gsub("\r\n"," ",text1)
	if (length(text2)>0){
		titles_2[n] = text2
		n = n + 1
	}
}
titles = c(titles97_1,titles_2)
extraNodes = html_nodes(webpage,xpath = "/html/body/text()")
extra = html_text(extraNodes)
text1 = gsub("\r\n ","",extra)
text2 = gsub("\r\n","",text1)
text2[text2==""] <- NA
text2[text2==" "] <- NA
text2[text2=="  "] <- NA
extra = na.omit(text2)
BodyNodes = html_nodes(webpage,xpath ="/html/body")
text1 = gsub("Return to top ","",html_text(BodyNodes))
Body = ldply(strsplit(text1,split = '\r\n\'))
Chairs_2 = extra[grep("Chair:",extra)]
seq4 = seq(1,length(extra),1)
titles_2 = extra[seq4[!seq4 %in% grep("Chair:",extra)]]

