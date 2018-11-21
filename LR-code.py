
# coding: utf-8

# In[12]:


import pandas as pd
import numpy as np
from sklearn import preprocessing
#import matplotlib.pyplot as plt
#plt.rc("font", size=14)
from sklearn.linear_model import LogisticRegression
from sklearn import metrics
from sklearn.cross_validation import train_test_split
import seaborn as sns
sns.set(style="white")
sns.set(style="whitegrid", color_codes=True)


# In[4]:


data=pd.read_csv('clean_train.csv', header=0)
data=data.dropna()
print(data.shape)  #Array dimensions 
print(list(data.columns))


# In[5]:


data.head()


# In[15]:


traindim=2000
testdim=500
x=data.iloc[:,:-1] #We use iloc because the array indexes are numeric. We select ALL the rows ALL and the columns exceptes the last one 'Target'
#print(x)
y=data.iloc[:,-1] #We select ALL the rows and the last one column 'Target'
#print(y)

#We prepare the train set
x_train=x.iloc[:traindim,:] #We select the 2000 first rows (without the column Target of course)
#print(x_train)
y_train = y.iloc[:traindim] #We select the 2000 first rows of Target
#print(y_train)

#We prepare the test set
x_test = x.iloc[-testdim:,:] #we remove ???
#print(x_test)
y_test = y.iloc[-testdim:] #we remove ???
print(y_test)


# In[18]:


logreg = LogisticRegression()
logreg.fit(x_train, y_train)


# In[19]:


#Predicting the test set results and calculating the accuracy
y_pred = logreg.predict(x_test)
print('Accuracy of logistic regression classifier on test set: {:.2f}'.format(logreg.score(x_test, y_test)))


# In[ ]:


#Confusion Matrix to add


# In[20]:


from sklearn.metrics import roc_auc_score
from sklearn.metrics import roc_curve
logit_roc_auc = roc_auc_score(y_test, logreg.predict(x_test))
fpr, tpr, thresholds = roc_curve(y_test, logreg.predict_proba(x_test)[:,1])
#plt.figure()
#plt.plot(fpr, tpr, label='Logistic Regression (area = %0.2f)' % logit_roc_auc)
#plt.plot([0, 1], [0, 1],'r--')
#plt.xlim([0.0, 1.0])
#plt.ylim([0.0, 1.05])
#plt.xlabel('False Positive Rate')
#plt.ylabel('True Positive Rate')
#plt.title('Receiver operating characteristic')
#plt.legend(loc="lower right")
#plt.savefig('Log_ROC')
#plt.show()

