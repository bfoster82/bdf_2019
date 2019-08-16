
#@author: bfoster2
# -*- coding: utf-8 -*-
"""
Created on Tue May 28 10:05:23 2019
@author: bfoster2
"""
import os
#os.system("!pip install gensim --upgrade")
#os.system("pip install keras --upgrade")
#os.system("pip install pandas --upgrade")
# DataFrame
import pandas as pd
# Matplot
import matplotlib.pyplot as plt
#from matplotlib import inline
# Scikit-learn
from sklearn.model_selection import train_test_split
from sklearn.preprocessing import LabelEncoder
from sklearn.metrics import confusion_matrix, classification_report, accuracy_score
from sklearn.manifold import TSNE
from sklearn.feature_extraction.text import TfidfVectorizer
# Keras
from keras.preprocessing.text import Tokenizer
from keras.preprocessing.sequence import pad_sequences
from keras.models import Sequential
from keras.layers import (Activation, Dense, Dropout, Embedding, Flatten, Conv1D, MaxPooling1D,
LSTM)
from keras import utils
from keras.callbacks import ReduceLROnPlateau, EarlyStopping
# nltk
import nltk
from nltk.corpus import stopwords
from nltk.stem import SnowballStemmer
from nltk.tokenize import sent_tokenize, word_tokenize
# Word2vec
import gensim
# Utility
import re
import numpy as np
import os
from collections import Counter
import logging
import time
import pickle
import itertools
filelocation=('C:/Users/bfoster2/desktop/iyb.txt')
data=[line.strip() for line in open (filelocation,'r')]
texts=[[word.lower() for word in text.split()] for text in data]
dataS=''.join(str(e) for e in data)
x=(sent_tokenize(dataS))
df=pd.DataFrame([x], index=['string_values'])
#df = pd.DataFrame(list_of_lists)
dft=df.transpose()
score=[]
for sent in dft['string_values']:
    score.append(predict(sent))
dft['score']=score
dft.to_csv('dft.csv',index = False)