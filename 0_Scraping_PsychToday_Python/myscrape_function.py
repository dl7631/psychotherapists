# Actual web scraping function for Psychology Today find a therapist
# zipcodelist is a Python list of zip codes as strings; e.g., ['10019', '10020']
from bs4 import BeautifulSoup
import requests, re, time, random
import pandas as pd
# Importing the function that parses the BeautifulSoup of one single url (one therapist)
from single_url_scrape import scrape_single_page

def myscrape(zipcodelist):
    
    headers = {
        'Connection': 'keep-alive',
        'Access-Control-Request-Headers': 'content-type',
        'Accept': '*/*',
        'User-Agent': 'Mozilla/5.0 (Macintosh; Intel Mac OS X 10_12_3) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/58.0.3029.110 Safari/537.36'
    }

    for myzip in zipcodelist:

        # Settings for headers:
        print('Starting zipcode ' + myzip)
        # Defining urls we are starting with and will be using
        starturl = 'https://therapists.psychologytoday.com/rms/prof_results.php?search=' + myzip
        longurl = 'https://therapists.psychologytoday.com/rms/prof_results.php?search=' + myzip + '&rec_next='

        # Beautifulsoup of the start url:
        response_zip = requests.get(starturl, headers=headers).text
        soup_zip = BeautifulSoup(response_zip, 'html.parser')

        # Manually construct the list of urls for ALL available search result pages in that zip - 20 therapists per page:
        pages_url_list = []
        i = 1
        while True:
            myurl = longurl + str(i)
            response = requests.get(myurl, headers=headers).text
            soup = BeautifulSoup(response, 'html.parser')
            alert = soup.find('div', attrs={'class': 'alert-alert'})
            if alert is None:
                i = i + 20
                pages_url_list.append(myurl)
                time.sleep(random.randint(0,1))
            else:
                break

        print('Number of group urls for this zip is: ' + str(len(pages_url_list)))
        if len(pages_url_list) == 0:
            continue

        # Loop through all urls found above (for pages of therapist search for that zip) and 
        # create a list of urls - one for each psychotherapist:

        therapist_urls = []
        for url in pages_url_list:
            response = requests.get(url, headers = headers).text
            soup = BeautifulSoup(response, 'html.parser')
            therapists = soup.find_all('a', {'class': "result-name"})
            for i in range(len(therapists)):
                therapist_urls.append(therapists[i]["href"])
                time.sleep(random.randint(0,1))
        print('Total number of therapists in this zip is: ' + str(len(therapist_urls)))

        # Now that we have the list of individual therapists' urls, we can scrape that zipcode's url:

        bigdf = pd.DataFrame()
        for index, url in enumerate(therapist_urls):
            response_one = requests.get(url, headers = headers).text
            one_therap = BeautifulSoup(response_one, 'html.parser')
            if one_therap.find('h1', {'itemprop': 'name'}) == None:
                continue
            smalldf = scrape_single_page(one_therap, myzip)
            bigdf = bigdf.append(smalldf)
            # Random sleep to avoid getting banned from the server
            time.sleep(random.randint(0, 1))
            # Log the progress
            print('Finished url ' + str(index + 1))

        # Drop duplicate rows in case there are some:
        bigdf = bigdf.drop_duplicates()
        # Save big data frame as a csv file
        bigdf.to_csv(("zip_" + myzip + ".csv"))