import tensorflow as tf
import keras
import pandas as pd
import numpy as np
from keras import backend as K
from tensorflow import metrics

#path = '/Users/alessandrobusato/Desktop/ESILV/Project/Code/train.csv'
trainpath = 'train_features.csv'
testpath = 'test_features.csv'
simupath = "simulated.csv"
nsimupath = "nsimulated.csv"
train = pd.read_csv(trainpath, header=0)
test = pd.read_csv(testpath, header=0)
simu = pd.read_csv(simupath, header=0)
nsimu = pd.read_csv(nsimupath, header=0)
train = train.values
test = test.values
simu = simu.values
nsimu = nsimu.values
trrows, trcolumns = train.shape
terows, tecolumns = test.shape
x_train=train[:,:-1]
y_train=train[:,-1]
x_test=test[:,:-1]
y_test=test[:,-1]
x_simu=simu[:,:-1]
x_nsimu=nsimu[:,:-1]

model = keras.Sequential()
model.add(keras.layers.Dense(trcolumns-1, input_dim=trcolumns-1, kernel_initializer='normal', activation='relu'))
model.add(keras.layers.Dense(trcolumns-1, input_dim=trcolumns-1, kernel_initializer='normal', activation='relu'))
model.add(keras.layers.Dense(trcolumns-1, input_dim=trcolumns-1, kernel_initializer='normal', activation='relu'))
model.add(keras.layers.Dense(trcolumns-1, input_dim=trcolumns-1, kernel_initializer='normal', activation='relu'))
model.add(keras.layers.Dense(1, kernel_initializer='normal', activation='sigmoid'))
model.compile(loss='binary_crossentropy', optimizer='adam', metrics=['accuracy'])
model.fit(x_train,y_train,epochs=3)

from sklearn.metrics import roc_curve
y_pred_keras = model.predict(x_test).ravel()
simulation = model.predict(x_simu).ravel()
nsimulation = model.predict(x_nsimu).ravel()
fpr_keras, tpr_keras, thresholds_keras = roc_curve(y_test, y_pred_keras)
from sklearn.metrics import auc
auc = auc(fpr_keras, tpr_keras)
print('Test accuracy:', auc)
