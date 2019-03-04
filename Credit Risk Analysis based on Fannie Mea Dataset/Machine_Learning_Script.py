
# coding: utf-8

# In[1]:


import pandas as pd
import numpy as np


# In[3]:


df = pd.read_csv("dataset.csv")


# In[6]:


data.head()


# In[8]:


data = data.dropna(axis=1, how='any')


# In[10]:


data.head()


# In[11]:


def getdummies(df):
    columns = df.columns[df.isnull().any()]
    nan_cols = df[columns]

    df.drop(nan_cols.columns, axis=1, inplace=True)

    cat = df.select_dtypes(include=['object'])
    num = df.drop(cat.columns, axis=1)

    data = pd.DataFrame()
    for i in cat.columns:
        tmp = pd.get_dummies(cat[i], drop_first=True)
        data = pd.concat([data, tmp], axis=1)

    df = pd.concat([num,data,nan_cols], axis=1).reset_index(drop=True)
    return df


# In[12]:


df = getdummies(data)


# In[17]:


df.describe()


# In[20]:


df.to_csv("data_final.csv")


# In[9]:


df.drop([df.columns[0]], axis=1, inplace=True)


# In[10]:


row1 = df


# In[11]:


row1.head()


# In[12]:


y = row1['Default'].values
X = row1.drop(['Default'], axis=1).values


# In[13]:


from sklearn.model_selection import train_test_split
from sklearn.ensemble import RandomForestClassifier, RandomForestRegressor


# In[14]:


X_train, X_test, y_train, y_test = train_test_split(X, y, test_size = 0.25, random_state=0)


# In[15]:


model = RandomForestClassifier(n_estimators=200)
model = model.fit(X_train, y_train)


# In[16]:


predict = model.predict(X_test)


# In[17]:


from sklearn.metrics import classification_report


# In[18]:


print(classification_report(y_test, predict))


# In[59]:


cm = confusion_matrix(y_test, predict).T
cm = cm.astype('float')/cm.sum(axis=0)

fig, ax = plt.subplots()
sns.heatmap(cm, annot=True, cmap='Blues');
ax.set_xlabel('True Label')
ax.set_ylabel('Predicted Label')
ax.xaxis.set_label_position('top')


# In[61]:


fpr, tpr, thresholds = roc_curve(y_test, model.predict_proba(X_test)[:,1])
roc_auc = roc_auc_score(y_test, predict)

plt.plot(fpr, tpr, lw=1, label='AUC = %0.2f'%(roc_auc))
plt.plot([0, 1], [0, 1], '--k', lw=1)
plt.xlabel('False Positive Rate')
plt.ylabel('True Positive Rate')
plt.title('Random Forest ROC')
plt.legend(loc="lower right", frameon = True).get_frame().set_edgecolor('black')


# In[19]:


from sklearn.linear_model import LogisticRegression


# In[20]:


logisticRegr = LogisticRegression()

logisticRegr = logisticRegr.fit(X_train, y_train)


# In[21]:


predict1 = logisticRegr.predict(X_test)


# In[22]:


print(classification_report(y_test, predict1))


# In[69]:


cm = confusion_matrix(y_test, predict1).T
cm = cm.astype('float')/cm.sum(axis=0)

fig, ax = plt.subplots()
sns.heatmap(cm, annot=True, cmap='Blues');
ax.set_xlabel('True Label')
ax.set_ylabel('Predicted Label')
ax.xaxis.set_label_position('top')


# In[32]:


from sklearn.svm import SVC


# In[36]:


clf = SVC(gamma='auto', probability=True)


# In[37]:


clf = clf.fit(X_train, y_train)


# In[38]:


predict3 = clf.predict(X_test)
print(classification_report(y_test, predict2))


# In[23]:


from sklearn.naive_bayes import GaussianNB
gnb = GaussianNB()
gnb = gnb.fit(X_train, y_train)


# In[25]:


predict2 = gnb.predict(X_test)
print(classification_report(y_test, predict2))


# In[65]:


fpr, tpr, thresholds = roc_curve(y_test, logisticRegr.predict_proba(X_test)[:,1])
roc_auc = roc_auc_score(y_test, predict1)

fpr1, tpr1, thresholds1 = roc_curve(y_test, model.predict_proba(X_test)[:,1])
roc_auc1 = roc_auc_score(y_test, predict)

fpr2, tpr2, thresholds2 = roc_curve(y_test, gnb.predict_proba(X_test)[:,1])
roc_auc2 = roc_auc_score(y_test, predict2)

fpr3, tpr3, thresholds3 = roc_curve(y_test, clf.predict_proba(X_test)[:,1])
roc_auc3 = roc_auc_score(y_test, predict3)

plt.plot(fpr, tpr, lw=1, label='Logistic Regression = %0.2f'%(roc_auc))
plt.plot(fpr1, tpr1, lw=1, label='Random Forest = %0.2f'%(roc_auc1))
plt.plot(fpr2, tpr2, lw=2, label='Naive Bayes = %0.2f'%(roc_auc2))
plt.plot(fpr3, tpr3, lw=3, label='SVM = %0.2f'%(roc_auc3))


plt.plot([0, 1], [0, 1], '--k', lw=1)
plt.xlabel('False Positive Rate')
plt.ylabel('True Positive Rate')
plt.title('Random Forest ROC')
plt.legend(loc="lower right", frameon = True).get_frame().set_edgecolor('black')

