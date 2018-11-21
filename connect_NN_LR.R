setwd("C:/Users/Marina/Documents/Semestre 9 (2018-2019)/Pi²/Test_Connecting_R_Python NN LR")

install.packages("reticulate")
library(reticulate)

py_run_file("NN.py")
py_run_file("LR-code.py")

auc_NN <- py$auc
auc_LR <- py$logit_roc_auc

y_pred_keras_NN <- py$y_pred_keras
y_pred_LR <- py$y_pred

#Pour connecter R et Python quand il y a des graphes sur Python il y a une erreur dûe à Qt pluggin window... donc le code du plot en python a été commenté

plot(py$fpr,py$tpr,xlab="False Positive Rate", ylab="True Positive Rate",cex=.5,col="red")
lines(c(py$fpr),c(py$tpr),type="s",col="red")
abline(0,1)
