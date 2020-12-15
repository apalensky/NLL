#!/usr/bin/env python
# coding: utf-8

# In[1]:


import pandas as pd
import requests
from bs4 import BeautifulSoup
import datetime
import time
import csv


# # Functions

# In[2]:


def tableText(table):    
    """
    Returns a list of column rows and a header if applicable. 
    """
    def getRowText(tr, coltag='td'):     
        return [td.get_text(strip=True) for td in tr.find_all(coltag)]  
    rowsList = []
    tableRows = table.find_all('tr')
    tableHeader = getRowText(tableRows[0], 'th')
    if tableHeader:
        rowsList.append(tableHeader)
        tableRows = tableRows[1:]
    for i in tableRows:
        rowsList.append(getRowText(i, 'td') )       
    return rowsList


# In[3]:


def date_conversion(date):
     return datetime.datetime.strptime(date, ' %b %d, %Y')


# In[4]:


def addTeams(df):
    i = 0
    q = int(df[df['Name']== 'Name'].index.values) +1

    while i < len(df)-1:
        if i == (q-2):
            i += 1
        elif i == (q-1):
            df.loc[(q-2), 'Team'] = teamsList[0]
            i += 1
        elif int(df[df['Name'] == df.Name[i]].index.values) < (q-1):
            mask = int(df[df['Name'] == df.Name[i]].index.values)
            df.loc[mask, 'Team'] = teamsList[0]
            i += 1
        else:
            mask = int(df[df['Name'] == df.Name[i]].index.values)
            df.loc[mask, 'Team'] = teamsList[1]
            i += 1  


# In[5]:


def StatsTable(url):
    r = requests.get(url)
    soup = BeautifulSoup(r.content, "html.parser")
    table = soup.find(lambda tag: tag.name=='table' and tag.has_attr('id') and tag['id']=="goalies") 
    rows = table.findAll(lambda tag: tag.name=='tr')[1]
    teams = table.findAll(lambda tag: tag.name=='tr')[0]
    
    teamsList = [team.text for team in teams.findAll(lambda tag: tag.name=='td')]
    list_table = tableText(rows)
        
    dftable = pd.DataFrame(list_table[1:], columns=list_table[0])
    dftable['Gameday'] = soup.title.string
        
    split1 = dftable['Gameday'].str.split(' - ', n = 1, expand = True)
    dftable['Date'] = split1[1]
        
    split2 = split1[0].str.split(' at ', n = 1, expand = True)
    dftable['Location'] = split2[1]
    dftable.drop(columns = ['Gameday'], inplace = True)
        
    split3 = dftable['Date'].str.split(',', n = 1, expand = True)
    dftable['Day'] = split3[0]
    dftable['Date'] = split3[1]
    dftable['Date'] = dftable['Date'].apply(date_conversion)
    
    q = int(dftable[dftable['Name']== 'Name'].index.values) + 1

    for i in range(0,(len(dftable)-1)):
        if i == q-2:
            continue
        else: 
            mask = int(dftable[(dftable['Name'] == dftable.Name[i]) & 
                               (dftable['#'] == dftable['#'][i])].index.values)

            if mask < (q-1):
                dftable.loc[mask, 'Team'] = teamsList[0]
            elif mask > (q-1):
                dftable.loc[mask, 'Team'] = teamsList[1]
            
    return dftable


# In[6]:


url = 'https://laxreports.sportlogiq.com/nll/GS179.html'
# url1 = 'https://laxreports.sportlogiq.com/nll/GS2276.html'
# url2 = 'https://laxreports.sportlogiq.com/nll/GS800.html'
# url3 = 'https://laxreports.sportlogiq.com/nll/GS900.html'
# url4 = 'https://laxreports.sportlogiq.com/nll/GS810.html'


# In[7]:


dftable1 = StatsTable(url)
df_list = [dftable1]
dftable1.head()


# In[8]:


urlsList = []

for i in range(236,2277):
    url = 'https://laxreports.sportlogiq.com/nll/GS{}.html'.format(i)
    r = requests.get(url)
    soup = BeautifulSoup(r.content, "html.parser")
    
    if bool(soup.findAll(text="html")):
        urlsList.append(url)


# In[9]:


# Irregular formatting in box score, will add back in by hand later
urlsList.remove('https://laxreports.sportlogiq.com/nll/GS925.html') 


# In[13]:


def StatsTableUpdate(urlsList):
    for url in urlsList:
        #print(url)
        dftablei = StatsTable(url)
        # print(dftablei.head(3))
        df_list.append(dftablei)
        time.sleep(1)


# In[14]:


#urlsList


# In[15]:


StatsTableUpdate(urlsList)


# In[20]:


fulldf = pd.concat(df_list, ignore_index=True)


# In[21]:


fulldf.tail(25)


# In[17]:


#fulldf.to_csv('NLLGoaliesGameStats.csv')

