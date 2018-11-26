
# this is a function that create a list of array of index
# in position 1 there is the index of the binaric data
# in pos 2 categoric data, in pos 3 numeric data
# i used this function only in the data exploration to count how much variable was binaric, numeric , ecc
indices_bin_num_cat=function(x){
  ndiffval=c()
  max=c()
  for (i in 1:length(x)){
    ndiffval=c(ndiffval,length(unique(x[,i])))
    max=c(max,max(x[,i]))
  }
  ind=list()
  ind[[1]]=which(ndiffval==2)
  ind[[2]]=which(ndiffval!=2 & ndiffval==1+max)
  ind[[3]]=which(ndiffval!=2 & ndiffval!=1+max)

  return(ind)
}

#this function take the inputs columns and the output column, and give back a dataset
# with information about each variable
#i will use the output of this function for other functions in this scripts
#1corr 2min 3max 4nDiffVal 5cat 6q1 7med 8q3
infoVar=function(x,y){
  bs=basicStats(x)
  ndiffval=c()
  for (i in 1:length(x)){
    ndiffval=c(ndiffval,length(unique(x[,i])))
  }
  
  toRet=data.frame("corr"=c(cor(x,y)),"min"=c(t(bs)[,3]),
                   "max"=c(t(bs)[,4]),"nDiffVal"=c(ndiffval),
                   "cat"=0,"q1"=c(t(bs)[,5]),"med"=c(t(bs)[,8]),"q3"=c(t(bs)[,6]))
                   
  isCat=c()
  for (i in 1:length(x)){
    if((toRet[i,4]-toRet[i,3])==1){
      if(toRet[i,4]==2){
        isCat=c(isCat,0)
      }
      else{
        isCat=c(isCat,1)
      }
        
    } 
    else {
      isCat=c(isCat,2)
    }
  }
  
  toRet["cat"]=isCat
  return(toRet)
  
}

#this function take as inputs the x data, the y data and a percentuale.
#and come only the variables more correlated
selectSubsetOfVariablesByCorrelation=function(x,y,percVar){
  n=as.integer(percVar*length(x))
  corr=abs(cor(x,y))
  NewX=x[order(corr,decreasing=TRUE)]
  NewX=NewX[1:n]
  return (NewX)
}


# this function is used to select a subset of the observation, putting a filter related to the value of 
# a selected variable
selectSpecificSubsets=function(data,info){
  # try to do a code that can be used more than one time
  toRet=list()
  var=readline(prompt="Select a variable: ")
  
  print(var)
  #add  an error if the variable name does not exist
  #or add in R-shyny something that allow the users to choose among all the possible variables
  if (info[var,5]==0){
    val=readline(prompt="Choose between 0 and 1: ")
    indices=which(data[,var]==val)
    newData=data[indices,]
    toRet[[1]]=newData
    toRet[[2]]=paste(var, "value =", val)
  }
  else if(info[var,5]==1){
    max=info[var,3]
    min=info[var,2]
    
    val=readline(prompt=paste("Chose the value between " , min, "and ", max, ":  " ) )
    indices=which(data[,var]==val)
    toRet[[1]]=data[indices,]
    toRet[[2]]=paste(var, "value =", val)
  }
  else {
    #here i want to add the possibility to select directly the 1st 2nd... 4th quartile of the observations among the variable selected
    max=readline(prompt="Select the maximum value for the variable: ")
    min=readline(prompt="Select the minum value for the variable: ")
    indices=which(data[,var]>=min & data[,var]<=max)
    toRet[[1]]=data[indices,]
    toRet[[2]]=paste(var,min, "<=value<=", max)
  }
 
  
  # modify this in order to get a list with the data filtered and the info about the subset
  return(toRet)
}  

# this function iterate the previous function as much time as the user want
# the output of the code is a list in which at position 1 there is the subset selected and
# in position 2 a string with the info about the subset selected
# it will be nice to modify this function in a way that the user can select the variable
# by a list that appear in R-shiny
selectSubset=function(data,info){
  toRet=selectSpecificSubsets(data,info)
  ok=readline(prompt="Continue? yes=1 : ")
  while (ok==1){
    infoSub=toRet[[2]]
    toRet=selectSpecificSubsets(toRet[[1]],info)
    toRet[[2]]=paste(toRet[[2]],infoSub)
    ok=readline(prompt="Continue? yes=1 : ")
  }
  return(toRet)
}






