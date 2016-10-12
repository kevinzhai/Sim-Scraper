
#scraper for the 2010 year of http://www.informs-sim.org/
#need to extract area, session, chair, title, authors, abstract
from bs4 import BeautifulSoup
import pandas as pd
import re

soup = BeautifulSoup(open("2010.html"))
clean = soup.prettify()

session = []
chair = []
title = []
authors = []
abstract = [] 

#scrape sessions
for link in soup.body.find_all('b'):
	if link.attrs == {'style': 'font-size: 1.2em'} and not link.string.startswith('Wednesday') and not link.string.startswith('Tuesday') and not link.string.startswith('Monday') and not link.string.startswith('Sunday'):
		session.append(link.string)

#scrape chairs
for link in soup.body.find_all('span'):
	if link.attrs == {'style': 'font-size: 1.1em'}:
		chair.append(link.get_text()[7:])

#scrape titles
for link in soup.body.find_all('i'):
	title.append(link.string)
title.pop(0)

#scrape authors
##COULDNT FIGURE THIS OUT!!

#scrape abstracts
for link in soup.body.find_all('div'):
	if len(link.attrs) == 2:
		modified = re.sub('<.*?>', '', str(link))
		abstract.append(modified.decode('utf-8'))

df = pd.DataFrame({'Session' : session, 'Chair' :chair})
df2 = pd.DataFrame({'Title':title})
df3 = pd.DataFrame({'Abstract': abstract})

df3.to_excel('three.xlsx')
