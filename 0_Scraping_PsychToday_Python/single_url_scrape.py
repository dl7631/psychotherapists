# This function scrapes one psychotherapist's url
# one_therap is the BeautifulSoup object of one requests.get or the url for one therapist.
# zipcode is the zipf code of that therapist as a string, e.g. '10019'
# def scrape_single_page(onetherapist, zipcode):

# Importing packages
from bs4 import BeautifulSoup
import requests, re, time, random
import pandas as pd

def scrape_single_page(one_therap, zipcode):
    
    dic = {}
    
    # Therapist name:
    therap_name = one_therap.find('h1', {'itemprop': 'name'}).text.strip()
    
    # Write down the zip defined above:
    zipcode = zipcode
    
    # Grab qualifications of one therapist
    qualif_div = one_therap.find('div', {'class': "profile-qualifications"})
    allqualif = qualif_div.find_all('li')   # they are saved in 'li'

    # Collecting Years in Practice:
    years_check = [i.text.split('\n')[2].strip() for i in allqualif if "Years in Practice" in i.text]
    if len(years_check) > 0:
        years_in_practice = years_check[0].replace(' Years', '')
    else:
        years_in_practice = ''
    
    # Collecting School:
    school_check = [i.text.split('\n')[2].strip() for i in allqualif if "School" in i.text]
    if len(school_check) > 0:
        school = school_check[0]
        school = re.sub(r'\([^)]*\)', '', school).strip()
        school = school.replace(', ', '_')
        school = school.replace(',', '_')
        school = school.replace('/ ', '_')
        school = school.replace('/', '_')
        school = school.replace(' - ', '_')
        school = school.replace(': ', '_')
        school = school.replace(':', '_')
        school = school.replace(' :', '_')
        school = school.replace('&', 'and')
        school = school.replace(' ', '_').lower()
        school = 'uni_' + school
    else:
        school = ''
    
    # Collecting Year Graduated:
    yeargrad_check = [i.text.split('\n')[2].strip() for i in allqualif if "Year Graduated" in i.text]
    if len(yeargrad_check) > 0:
        yeargrad = int(yeargrad_check[0])
    else:
        yeargrad = ''
    
    # Collecting Board Certification:
    board_check = [i.text.split('\n')[2].strip() for i in allqualif if "Board Certification" in i.text]
    if len(board_check) > 0:
        board_certif = 1
    else:
        board_certif = 0
    
    # Collecting License number & State:
    license_check = [i.text.split('\n')[2].strip() for i in allqualif if "License No. and State" in i.text]
    if len(license_check) > 0:
        license = license_check[0].split()[0]
        license_state = " ".join(license_check[0].split()[1:])
    else:
        license = ''
        license_state = ''
    
    # Grabbing all his/her titles:
    titles = one_therap.find('div', {'class': "profile-title"}).text.strip()
    titles = titles.replace("\n", "")
    titles = titles.replace('/', '_').strip()
    titles = titles.replace(', ', ',')
    titles = titles.replace('-', '_')
    titles = titles.replace(' ', '_')
    titles = titles.replace(',', ', ')
    
    # Grabbing financial info:
    finances = one_therap.find('div', {'class': "profile-finances"})
    if finances == None:
        per_session = ''
        sliding = ''
        insurance_yes = ''
        payment_methods = 'xNotListed'
    else:
        # Grabbing several financial metrics:
        lis = finances.find_all('li')
        
        # Grab cost per session:
        persession_check = [i.text.split(': ')[1].strip() for i in lis if "Avg Cost (per session)" in i.text]
        if len(persession_check) > 0:
            per_session = persession_check[0].replace('$', '')
            per_session = per_session.replace(' ', '')
        else:
            per_session = ''
            
        # Grab sliding scale (1 = yes, 0 = no):
        sliding_check = [i.text for i in lis if 'Sliding Scale' in i.text]
        if sliding_check != None:
            sliding_check = " ".join(sliding_check)
            if 'Sliding Scale: Yes' in sliding_check:
                sliding = 1
            else:
                sliding = 0
        else:
            sliding = 0
        
        # Grab Accepts Insurance - yes or no:
        insur_check = [i.text.split(': ')[1].strip() for i in lis if "Accepts Insurance" in i.text]
        if len(insur_check) > 0:
            insurance_yes = 1   # insur_check[0]
        else:
            insurance_yes = 0    # ''
        
        # Grab Methods of Payment:
        # payment_check = [i.text for i in lis if "Accepted Payment Methods" in i.text]
        payment_check = [i.text.split('\n')[2].strip().split(',') for i in lis if "Accepted Payment Methods" in i.text]
        if len(payment_check) == 0:
            payment_methods = 'xNotListed'
        else:
            payment_methods = [item for sublist in payment_check for item in sublist]
            for index, item in enumerate(payment_methods):
                out = item.strip().lower()
                out = out.replace(" ", "_")
                out = 'paym_' + out
                payment_methods[index] = out
            payment_methods = ', '.join(payment_methods)
        
    # Grab insurance plans s/he accepts:
    long = one_therap.find_all('div', {'class': "col-xs-12 col-sm-6 col-md-6 col-lg-6 col-tight-right"})
    if len(long) > 0:
        result = []
        for i in long:
            result.extend(i.find_all('li'))
        plans = []
        for i in range(len(result)):
            out = result[i].text
            out = re.sub(r'\([^)]*\)', '', out)
            out = out.replace(' ', '_')
            out = out.replace('|', '_')
            out = out.replace('\|', '_')
            out = out.replace('&', 'and')
            out = out.replace('-', '_')
            out = out.replace('/', '_')
            out = 'ins_' + out
            plans.append(out)
        plans = ', '.join(plans).lower()
    else:
        plans = 'xNotListed'

    # Grab specialties:
    spec_check = one_therap.find_all('li', {'class': "highlight"})
    if len(spec_check) == 0:
        specialties = 'xNotListed'
    else:
        specialties = []
        for i in range(len(spec_check)):
            out = spec_check[i].text.replace(', ', '_')
            out = out.replace(' ', '_').lower()
            out = 'spec_' + out
            specialties.append(out)
        specialties = ", ".join(specialties)
    
    # Grab issues:
    issues_check = one_therap.find_all('div', {'class': 'col-xs-12 col-sm-12 col-md-6 col-lg-6'})
    if len(issues_check) == 0:
        issues = 'xNotListed'
    else:
        result = []
        for i in issues_check:
            result.extend(i.find_all('li'))
        issues = []
        for i in range(len(result)):
            out = result[i].text.strip()
            out = re.sub(r'\([^)]*\)', '', out)
            out = re.sub(r' $', '', out)
            out = out.replace(', ', '_')
            out = out.replace('-', '_')
            out = out.replace('\'', '')
            out = out.replace(' ', '_').lower()
            out = 'iss_' + out
            issues.append(out)
        issues = ", ".join(issues)
    
    # Grab preferred ages:
    
    # In rare cases when no specialties were selected, we need to grab the 3rd, not the 4th element:
    if specialties == '':
        age_check = one_therap.find_all('div', {'class': 'col-xs-12 col-sm-12 col-md-12 col-lg-12'})[2]
    else:
        age_check = one_therap.find_all('div', {'class': 'col-xs-12 col-sm-12 col-md-12 col-lg-12'})[3]
    
    age_lis = age_check.find_all('li')
    ages = []
    for i in age_lis:
        ages.append(i.text)
    
    # Creating values for age categories:
    
    if "Toddlers / Preschoolers (0 to 6)" in ages:      # Age - toddlers:
        age_toddlers = 1
    else:
        age_toddlers = 0
    
    if "Children (6 to 10)" in ages:                # Age - children:
        age_children = 1
    else:
        age_children = 0
    
    if "Preteens / Tweens (11 to 13)" in ages:      # Age - preteens:
        age_preteens = 1
    else:
        age_preteens = 0
    
    if "Adolescents / Teenagers (14 to 19)" in ages:      # Age - teens:
        age_teens = 1
    else:
        age_teens = 0
    
    if "Adults" in ages:              # Age - adults:
        age_adults = 1
    else:
        age_adults = 0
    
    if "Elders (65+)" in ages:        # Age - elders:
        age_elders = 1
    else:
        age_elders = 0
    
    # Sum of ages:
    agesum = age_toddlers+age_children+age_preteens+age_teens+age_adults+age_elders
    
    
    # Grab subcategories:
    
    if specialties == '' and agesum == 0:
        subcat_check = one_therap.find_all('div', {'class': 'col-xs-12 col-sm-12 col-md-12 col-lg-12'})[2]
    elif specialties == '' and agesum > 0:
        subcat_check = one_therap.find_all('div', {'class': 'col-xs-12 col-sm-12 col-md-12 col-lg-12'})[3]
    elif specialties != '' and agesum == 0:
        subcat_check = one_therap.find_all('div', {'class': 'col-xs-12 col-sm-12 col-md-12 col-lg-12'})[3]
    else:
        subcat_check = one_therap.find_all('div', {'class': 'col-xs-12 col-sm-12 col-md-12 col-lg-12'})[4]
    
    subcat_lis = subcat_check.find_all('li')
    subcats = []
    for i in subcat_lis:
        subcats.append(i.text)
    
    if "Aviation Professionals" in subcats:  # Subcategories - Aviation:
        sub_pilots = 1
    else:
        sub_pilots = 0
    
    if "Bisexual Clients" in subcats:  # Subcategories - HIV:
        sub_bisexuals = 1
    else:
        sub_bisexuals = 0
    
    if "Cancer" in subcats:      # Subcategories - Cancer:
        sub_cancer = 1
    else:
        sub_cancer = 0
    
    if "Gay Clients" in subcats:  # Subcategories - Gays:
        sub_gays = 1
    else:
        sub_gays = 0
    
    if "HIV / AIDS Clients" in subcats:  # Subcategories - HIV:
        sub_hiv = 1
    else:
        sub_hiv = 0
    
    if "Heterosexual Clients" in subcats:  # Subcategories - Heterosexual:
        sub_heteros = 1
    else:
        sub_heteros = 0
    
    if "Lesbian Clients" in subcats:   # Subcategories - Lesbian:
        sub_lesbians = 1
    else:
        sub_lesbians = 0
    
    if "Transgender Clients" in subcats:  # Subcategories - Transgender:
        sub_transgender = 1
    else:
        sub_transgender = 0
    
    if "Veterans" in subcats:   # Subcategories - Veterans:
        sub_veterans = 1
    else:
        sub_veterans = 0
    
    # Creating 2 additional sums
    subsum = sub_pilots+sub_bisexuals+sub_cancer+sub_gays+sub_hiv+sub_heteros+sub_lesbians+sub_transgender+sub_veterans
    age_and_sub_sum = agesum + subsum
    
    # Check if Treatment Approach is even there:
    approach_check = one_therap.find_all('h2')
    asalist = []
    count = 1
    for i in approach_check:
        try:
            out = i.text
        except:
            print("some error")
        asalist.append(out)
    if 'Treatment Approach' in asalist:
    
        # Grab treatment approaches:
        approach_check = one_therap.find_all('div', {'class': 'col-xs-12 col-sm-12 col-md-12 col-lg-12'})
        if specialties == '' and age_and_sub_sum == 0:           # if all 3 are not there
            approach_check = approach_check[2]
        elif specialties != '' and age_and_sub_sum == 0:         # if only 2 out of 3 are not there
            approach_check = approach_check[3]
        elif specialties == '' and agesum == 0 and subsum > 0:
            approach_check = approach_check[3]
        elif specialties == '' and agesum > 0 and subsum == 0:
            approach_check = approach_check[3]
        elif specialties != '' and agesum == 0  and subsum > 0:  # if only 1 out of 3 is not there
            approach_check = approach_check[4]
        elif specialties != '' and agesum > 0  and subsum == 0:
            approach_check = approach_check[4]
        elif specialties == '' and agesum > 0  and subsum > 0:
            approach_check = approach_check[4]
        else:
            approach_check = approach_check[5]

        approach_lis = approach_check.find_all('li')
        approaches = []
        for i in approach_lis:
            approaches.append(i.text)

        for i in range(len(approaches)):
            out = approaches[i]
            out = out.replace(' / ', '_')
            out = re.sub(r'\([^)]*\)', '', out)
            out = re.sub(r' $', '', out)
            out = out.strip().replace('-', '_')
            out = out.replace(' / ', '_')
            out = out.replace('/ ', '_')
            out = out.replace('/', '_')
            out = out.replace('\'', '')
            out = out.replace(' ', '_').lower()
            out = 'appr_' + out
            approaches[i] = out
        approaches = ', '.join(approaches)
    else:
        approaches = 'xNotListed'
    
    # Grab ethnicities, languages, religious orientation:
    spec_subcat = one_therap.find_all('div', {'class': "spec-subcat"})
    list_of_children = [None] * len(spec_subcat)
    for i in range(len(spec_subcat)):
        list_of_children[i] = spec_subcat[i].findChildren()
    whatsthere = []
    for i in range(len(list_of_children)):
        for z in range(len(list_of_children[i])):
            for t in [text for text in list_of_children[i][z].stripped_strings]:
                whatsthere.append(t)
    for i in range(len(whatsthere)):
        whatsthere[i] = whatsthere[i].replace(',', '')

    # Finding indexes for Ethnicity, Languages, and Religious Orientation:
    if one_therap.find_all("strong", string="Ethnicity:") == []:
        ethnicities = 'X'
        index_ethnic = ''
    else:
        index_ethnic = whatsthere.index("Ethnicity:")

    if one_therap.find_all("strong", string="Alternative Languages:") == []:
        languages = 'X'
        index_languages = ''
    else:
        index_languages = whatsthere.index("Alternative Languages:")
    index_languages
    if one_therap.find_all("strong", string="Religious Orientation:") == []:
        religion = 'X'
        index_religion = ''
    else:
        index_religion = whatsthere.index("Religious Orientation:")

    # Grabbing applicable info - depending on what's there and what's not:
    if index_ethnic != '' and index_languages != '' and index_religion != '':  # if all 3 are present
        ethnicities = whatsthere[(index_ethnic+1):index_languages]
        languages = whatsthere[(index_languages+1):index_religion]
        religion = whatsthere[(index_religion+1):len(whatsthere)]

    if index_ethnic == '' and index_languages != '' and index_religion != '':   # if only languages and religion present
        languages = whatsthere[(index_languages+1):index_religion]
        religion = whatsthere[(index_religion+1):len(whatsthere)]

    if index_ethnic != '' and index_languages == '' and index_religion != '':   # if only ethnicities and religion present
        ethnicities = whatsthere[(index_ethnic+1):index_religion]
        religion = whatsthere[(index_religion+1):len(whatsthere)]

    if index_ethnic != '' and index_languages != '' and index_religion == '':   # if only ethnicities and languages present
        ethnicities = whatsthere[(index_ethnic+1):index_languages]
        languages = whatsthere[(index_languages+1):len(whatsthere)]

    if index_ethnic != '' and index_languages == '' and index_religion != '':   # if only ethnicities and religion present
        ethnicities = whatsthere[(index_ethnic+1):index_religion]
        religion = whatsthere[(index_religion+1):len(whatsthere)]

    if index_ethnic != '' and index_languages == '' and index_religion == '':   # if only ethnicities present
        ethnicities = whatsthere[(index_ethnic+1):len(whatsthere)]

    if index_ethnic == '' and index_languages != '' and index_religion == '':   # if only languages present
        languages = whatsthere[(index_languages+1):len(whatsthere)]

    if index_ethnic == '' and index_languages == '' and index_religion != '':   # if only religion present
        religion = whatsthere[(index_religion+1):len(whatsthere)]

    ethnicities = ", ".join(ethnicities)
    languages = ", ".join(languages)
    religion = ", ".join(religion)
    
    dic['atherapist'] = [therap_name]
    dic['zipcode'] = [zipcode]
    dic['years_in_practice'] = [years_in_practice]  #.encode('utf-8', 'ignore')
    dic['school'] = [school]
    dic['yeargrad'] = [yeargrad]
    dic['board_certif'] = [board_certif]
    dic['license'] = [license]
    dic['license_state'] = [license_state]
    dic['titles'] = [titles]
    dic['per_session'] = [per_session]
    dic['per_session'] = [per_session]
    dic['sliding'] = [sliding]
    dic['insurance_yes'] = [insurance_yes]
    dic['payment_methods'] = [payment_methods]
    dic['plans'] = [plans]
    dic['specialties'] = [specialties]
    dic['issues'] = [issues]
    dic['age_toddlers'] = [age_toddlers]
    dic['age_children'] = [age_children]
    dic['age_preteens'] = [age_preteens]
    dic['age_teens'] = [age_teens]
    dic['age_adults'] = [age_adults]
    dic['age_elders'] = [age_elders]
    dic['sub_pilots'] = [sub_pilots]
    dic['sub_bisexuals'] = [sub_bisexuals]
    dic['sub_cancer'] = [sub_cancer]
    dic['sub_gays'] = [sub_gays]
    dic['sub_hiv'] = [sub_hiv]
    dic['sub_heteros'] = [sub_heteros]
    dic['sub_lesbians'] = [sub_lesbians]
    dic['sub_transgender'] = [sub_transgender]
    dic['sub_veterans'] = [sub_veterans]
    dic['approaches'] = [approaches]
    dic['client_ethnicities'] = [ethnicities]
    dic['client_languages'] = [languages]
    dic['client_religion'] = [religion]

    return pd.DataFrame(dic)