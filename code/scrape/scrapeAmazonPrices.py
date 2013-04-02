import os, sys, re
import urllib2
from bs4 import BeautifulSoup

departments = {
  0 : ('All', 'ref=nb_sb_noss_1?url=search-alias%3Daps'),
  1 : ('Home & Kitchen', 'ref=nb_sb_noss_1?url=search-alias%3Dgarden'),
  2 : ('Electronics', 'ref=nb_sb_noss?url=search-alias%3Delectronics'),
  3 : ('Cloting & Accessories', 'ref=nb_sb_noss?url=search-alias%3Dapparel'),
  4 : ('Watches', 'ref=nb_sb_noss_1?url=search-alias%3Dwatches'),
  5 : ('Computer & Accessories', 'ref=nb_sb_noss_1?url=search-alias%3Dcomputers'),
  6 : ('Books', 'ref=nb_sb_noss_2?url=search-alias%3Dstripbooks')
  }

def crawlPage(query, numberOfPrices, dept):
  print 'Query: ' + query
  print 'Department: ' + departments[dept][0]

  url = 'http://www.amazon.com/s/' + departments[dept][1] + '&field-keywords=' + urllib2.quote(query)

  prices = []
  pageCount = 1
  while len(prices) < numberOfPrices and url != '':
    print 'Scraping page ' + str(pageCount)
    pageCount += 1
    soup = BeautifulSoup(urllib2.urlopen(url).read())
    prices = prices + extractPrices(soup)
    print 'Total number of prices: ' + str(len(prices))
    url = getNextPageUrl(soup)
  return prices

def getNextPageUrl(soup):
  link = soup.findAll('div', {'id':'pagn'})
  if len(link) >= 1:
    link = link[0].findAll('a', {'class':'pagnNext'})
    if len(link) >= 1:
      return link[0]['href']
  return ''

def extractPrices(soup):
  table = soup.findAll('div', {'class':'newPrice'})
  prices = []
  for t in table:
    table2 = BeautifulSoup(t.prettify()).findAll('span')
    #assert(len(table2)==1)
    #print len(table2)
    if len(table2) < 1:
      break
    #assert(len(table2)>=1)
    
    t2 = table2[0].contents
    assert(len(t2)==1)
    p = t2[0].strip()
    p = re.sub('[\$,]*', '', p)
    prices.append(p)
  return prices
  
def printPriceList(prices, fileName):
  f = open(fileName, 'w')
  print >> f, '\n'.join(prices)
  f.close()
     
def tryParseInt(s):
  try:
    return int(s)
  except ValueError, TypeError:
    return False
        
def tryParseFloat(s):
  try:
    return float(s.strip())
  except ValueError, TypeError:
    return False

if len(sys.argv) != 4:
  print >> sys.stderr, 'usage: python scrapeAmazonPrices.py <query> <number of prices> <outputFileName>'
  os._exit(-1)

query = sys.argv[1]
numPrices = sys.argv[2]
outFileName = sys.argv[3]

print '\n'.join(['%d: %s' %(k,dept) for (k,(dept, url)) in departments.items()])
# To pass: dept is '' or (dept is int and dept is in keys)
dept = raw_input('Departments: ')
while dept!='' and ( not tryParseInt(dept) or tryParseInt(dept) not in departments.keys()):
  dept = raw_input('Departments: ')

if dept == '':
  dept = 0
else:
  dept = tryParseInt(dept)

if tryParseInt(numPrices):
  numPrices = int(numPrices)
else:
  numPrices = 250

prices = crawlPage(query, numPrices, dept)

prices_clean = []
for i in range(len(prices)):
  if tryParseFloat(prices[i]):
    prices_clean.append(prices[i])

printPriceList(prices_clean, outFileName)

for i in range(len(prices_clean)):
  prices_clean[i] = float(prices_clean[i])
print 'Max price = ' + str(max(prices_clean))
print 'Min price = ' + str(min(prices_clean))
print 'Avearage price = ' + str(sum(prices_clean) / len(prices_clean))
