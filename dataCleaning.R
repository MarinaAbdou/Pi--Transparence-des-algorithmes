library(fBasics)
library(caret)
rm(list=ls())


data <- read.csv("C:/Users/user/Desktop/Progetto Machine Learning/data/train.csv", header=TRUE)



# CLEANING OF THE DATA:
# 1)variables costant in all observation
# 2)variables duplicates
# 3)observations duplicates
# 4)variable less correlated with the output between all the the couple of variables correlated more than 0.95 
# 5)variables with less than 0.001% of nzero value


#------------------------------------------------------------------------#
# 1)Delete colums that are costant in all observation
# basicStat[13,]=variance
bs=basicStats(data)
indKost=c()
for (i in 1:length(data)){
  if (bs[13,i]==0)
    indKost=c(indKost,i)
}
data=data[-indKost]


#2)look and delete duplicates colums:
indDuplicated=c()
for (i in 1:length(data)-1){
  for (j in (i+1):length(data)){
    if(identical(data[,i],data[,j])==TRUE){
      indDuplicated=c(indDuplicated,j)
    }
  }
}
data=data[-indDuplicated]


#3) remove duplicated rows
#before i have to delete the ID colums
#we can not delete the ID colums and work with the index but the computation time will be very high)
data=data[-1]
data=unique(data)


#now we split x and y, and we will work only on the x dataset
yname="TARGET"
yind=match(yname,colnames(data))
y=data[yind]
x=data[-yind]


#4)settalbe: delete variables with less than d% non-zero elements
percOfNZero=0.001
limit=length(x[,1])*percOfNZero
indlessLNZero=c()
for (i in 1:length(x)){
  if(colSums(x[i]!=0)<limit){
    indlessLNZero=c(indlessLNZero,i)
  }
}
x=x[-indlessLNZero]

#5)settable: look for couple of variable whit absolute correlation p>plimit 
#delete the variable less correlated with y

plimit=0.95
corv=abs(cor(x,y))
corM=abs(cor(x))
toDel=c()

for (i in 1:(dim(corM)[1]-1)){
  for(j in (i+1):dim(corM)[1]){
    if(corM[i,j]>plimit){
      
      if (corv[i]>corv[j]){
        toDel=c(toDel,j)
      }
      else{
        toDel=c(toDel,i)
      }
    }
  }
}
x=x[-toDel]

# reassemble x and y and save it on a csv file
clean_data=x
clean_data[,length(x)+1]=y
write.csv(clean_data[1:length(clean_data[,1]),],file="C:/Users/user/Desktop/Progetto Machine Learning/data/clean_data.csv", row.names = FALSE)



























