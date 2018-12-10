
# coding: utf-8

# In[12]:


import pandas as pd
import numpy as np
from sklearn import preprocessing
#import matplotlib.pyplot as plt
#plt.rc("font", size=14)
from sklearn.linear_model import LogisticRegression
from sklearn import metrics
import seaborn as sns
sns.set(style="white")
sns.set(style="whitegrid", color_codes=True)


# In[4]:


train=pd.read_csv('train_features.csv', header=0)
train=train.dropna()
#print(train.shape)  #Array dimensions 
#print(list(train.columns))

test=pd.read_csv('test_features.csv', header=0)
test=test.dropna()
#print(test.shape)  #Array dimensions 
#print(list(test.columns))

# In[5]:


train.head()


# In[15]:


#We use iloc because the array indexes are numeric. We select ALL the rows ALL and the columns exceptes the last one 'Target'
#print(x)
 #We select ALL the rows and the last one column 'Target'
#print(y)

#We prepare the train set
x_train=train.iloc[:,:-1] #We select the 2000 first rows (without the column Target of course)
#print(x_train)
y_train = train.iloc[:,-1] #We select the 2000 first rows of Target
#print(y_train)

#We prepare the test set
x_test = test.iloc[:,:-1] #we remove ???
#print(x_test)
y_test = test.iloc[:,-1] #we remove ???
#print(y_test)


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
logit_roc_auc = roc_auc_score(y_test, y_pred)
y_pred_lr=logreg.predict_proba(x_test)[:,1]
fpr, tpr, thresholds = roc_curve(y_test,y_pred_lr)
from sklearn.metrics import auc
auc = auc(fpr, tpr)
print('Test accuracy:', auc)
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

