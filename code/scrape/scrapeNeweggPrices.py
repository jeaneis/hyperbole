import os,sys,re
import urllib2
from bs4 import BeautifulSoup

def crawlPage(query, numberOfPrices):
  print 'Query: ' + query
  # url = 'http://www.newegg.com/Laptops-Notebooks/SubCategory/ID-32?Tpk=laptop'
  prices = []
  pageCount = 1
  while len(prices) < numberOfPrices:
    print 'Scraping page ' + str(pageCount)
    url = 'http://www.newegg.com/Laptops-Notebooks/SubCategory/ID-32/Page-'+str(pageCount)+'?Tpk='+query
    page = urllib2.urlopen(url).read()
    soup = BeautifulSoup(page)
    prices = prices + extractPrices(soup)
    print 'Total number of prices: ' + str(len(prices))
    pageCount += 1
  return prices

def extractPrices(soup):
  prices = []
  table = soup.findAll('li', {'class':'price-current'})
  for t in table:
    dollar = t.find('strong')
    if dollar is not None and tryParseInt(re.sub(',','',dollar.contents[0])):
      dollar = re.sub(',','',dollar.contents[0])
      cents  = t.find('sup').contents[0]
      prices.append(''.join(dollar + cents))
  return prices

def printPriceList(prices, fileName):
  f = open(fileName, 'w')
  print >> f, '\n'.join(prices)
  f.close()
     
def tryParseInt(s):
  try:
    return int(s)
  except (ValueError, TypeError):
    return False
    
def tryParseFloat(s):
  try:
    return float(s.strip())
  except (ValueError, TypeError):
    return False

if len(sys.argv) != 4:
  print >> sys.stderr, 'usage: python scrapeNeweggPrices.py <query> <number of prices> <outputFileName>'
  os._exit(-1)

query = sys.argv[1]
numPrices = sys.argv[2]
outFileName = sys.argv[3]

if tryParseInt(numPrices):
  numPrices = int(numPrices)
else:
  numPrices = 500

prices = crawlPage(query, numPrices)

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
