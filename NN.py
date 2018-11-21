import tensorflow as tf
from tensorflow import keras
import pandas as pd
import numpy as np
from keras import backend as K
from tensorflow import metrics

#path = '/Users/alessandrobusato/Desktop/ESILV/Project/Code/train.csv'
path = 'clean_train.csv'
data = pd.read_csv(path, header=0)
data = data.values
rows, columns = data.shape
traindim = 2000
testdim = 500
x=data[:,:-1]
y=data[:,-1]
x= tf.keras.utils.normalize(x, axis=1)
x_test = x[-testdim:,:]
y_test = y[-testdim:]
x_train = x[:traindim,:]
y_train = y[:traindim]

def roc(y_true, y_pred):
   score, up_opt = metrics.roc(y_true, y_pred)
   K.get_session().run(tf.local_variables_initializer())
   with tf.control_dependencies([up_opt]):
       score = tf.identity(score)
   return score

model = keras.Sequential()
model.add(keras.layers.Dense(141, input_dim=141, kernel_initializer='normal', activation='relu'))
model.add(keras.layers.Dense(141, input_dim=141, kernel_initializer='normal', activation='relu'))
model.add(keras.layers.Dense(1, kernel_initializer='normal', activation='sigmoid'))
model.compile(loss='binary_crossentropy', optimizer='adam', metrics=['accuracy'])
model.fit(x_train,y_train,epochs=3)

from sklearn.metrics import roc_curve
y_pred_keras = model.predict(x_test).ravel()
fpr_keras, tpr_keras, thresholds_keras = roc_curve(y_test, y_pred_keras)
from sklearn.metrics import auc
auc = auc(fpr_keras, tpr_keras)
print('Test accuracy:', auc)
