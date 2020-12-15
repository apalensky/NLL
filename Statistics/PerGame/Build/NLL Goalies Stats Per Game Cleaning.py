#!/usr/bin/env python
# coding: utf-8

# ## NLL Goalies Stats Cleaning

# Work by Alexander Palensky
# 
# For questions, 
# contact me on [Twitter](https://twitter.com/AlPalensky)
# or view my [Kaggle Account](https://www.kaggle.com/apalensky) for supplemental content

# In[75]:


import numpy as np
import pandas as pd
import datetime
from datetime import date
import re


# In[76]:


pd.set_option('display.max_rows', None)
pd.set_option('display.max_columns', None)
pd.set_option('display.width', None)
pd.set_option('display.max_colwidth', None)


# In[77]:


df = pd.read_csv('NLLGoaliesGameStats.csv')


# In[78]:


split1 = df['Name'].str.split('\n ', n = 1, expand = True)
df['Name'] = split1[0]
df['Credit'] = split1[1]


# In[79]:


df.head()


# In[80]:


df.drop(df[df['#'] == '#'].index, inplace = True) 


# In[81]:


df.drop(df[df['#'] == 'Totals:'].index, inplace = True) 


# In[82]:


len(df)


# In[83]:


new_row1 = {'Unnamed: 0':10293, '#':39, 'Name':"P.O'Toole", 'MIN':'59:50','SOG':50,'SV Q1':5, 
           'SV Q2':8, 'SV Q3':16, 'SV Q4':10, 'SV':39, 'GA':11, 'Date':'2006-02-10', 
           'Location':'Toronto','Day':'Fri', 'Team':'Rochester Goalies', 'SV OT':0, 'Credit':'(loss)'}
#append row to the dataframe
df = df.append(new_row1, ignore_index=True)


# In[84]:


new_row2 = {'Unnamed: 0':10294, '#':52, 'Name':"G.Crawley", 'MIN':np.nan,'SOG':np.nan,'SV Q1':np.nan, 
           'SV Q2':np.nan, 'SV Q3':np.nan, 'SV Q4':np.nan, 'SV':np.nan, 'GA':np.nan, 'Date':'2006-02-10', 
           'Location':'Toronto','Day':'Fri', 'Team':'Rochester Goalies', 'SV OT':np.nan, 'Credit':'(b)'}
#append row to the dataframe
df = df.append(new_row2, ignore_index=True)


# In[85]:


new_row3 = {'Unnamed: 0':10295, '#':29, 'Name':"B.Watson", 'MIN':'60:10','SOG':43,'SV Q1':10, 
           'SV Q2':4, 'SV Q3':3, 'SV Q4':16, 'SV':33, 'GA':10, 'Date':'2006-02-10', 
           'Location':'Toronto','Day':'Fri', 'Team':'Toronto Goalies', 'SV OT':0, 'Credit':'(win)'}
#append row to the dataframe
df = df.append(new_row3, ignore_index=True)


# In[86]:


new_row4 = {'Unnamed: 0':10296, '#':90, 'Name':"P.Wetherup", 'MIN':np.nan,'SOG':np.nan,'SV Q1':np.nan, 
           'SV Q2':np.nan, 'SV Q3':np.nan, 'SV Q4':np.nan, 'SV':np.nan, 'GA':np.nan, 'Date':'2006-02-10', 
           'Location':'Toronto','Day':'Fri', 'Team':'Toronto Goalies', 'SV OT':np.nan, 'Credit':'(b)'}
#append row to the dataframe
df = df.append(new_row4, ignore_index=True)


# In[87]:


df = df.reset_index()
df.tail(25)


# In[88]:


df = df[df.columns[[14, 12, 13, 2, 3, 17, 15, 4, 6, 7, 8, 9, 16 , 10, 5, 11]]]


# In[89]:


split2 = df['Team'].str.split('Goalies', n = 1, expand = True)
df['Team'] = split2[0]


# In[ ]:





# In[90]:


df['Date'] = pd.to_datetime(df['Date'])


# In[91]:


for i in range(0,len(df)):
    if df.Credit[i] == None:
        continue
    else:
        df.loc[i, 'Credit'] = df.Credit[i].strip().replace('(','').replace(')','')


# In[92]:


df[["SV Q1", "SV Q2", "SV Q3", "SV Q4", "SV OT", "SV", "SOG", "GA"]] = df[["SV Q1", "SV Q2", "SV Q3", "SV Q4", "SV OT", "SV", "SOG", "GA"]].apply(pd.to_numeric)


# In[93]:


for i in range(0, len(df)):
    if type(df.MIN[i]) == float:
        continue
    else:
        df.loc[i, 'MIN'] = int((df['MIN'][i]).split(':')[0]) + (int((df['MIN'][i]).split(':')[1]) / 60)


# In[94]:


df.tail(10)


# In[95]:


#df.to_csv('CleanedNLLGoaliesGameStats.csv')


# In[ ]:




