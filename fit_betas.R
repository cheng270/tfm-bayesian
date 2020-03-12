#Function that return the coeffient for each value of class
#Input variables:
#                 data.frame: data frame Type: data.frame
#                 parents: parent variable. Type: String
#                 class: class variable. Type: String
#                 variable: children variable. Type: c()
#output variable: 
#                 the coeffient for each value of class

#Program:Rstudio Version 1.2.1578
#Package: None
#Date:06/03/2020

fit_beta<-function(data.frame,parents,class,variable){
  
  data <- subset(data.frame, Species == class) 
  
  formula<-paste(parents,"~")
  for (i in 1:length(variable)){
    
    formula<-paste(formula,variable[i:i])#generate the formula
    i=i+1
    
    if( is.na(variable[i:i])){ # if no more children variable, return the oeficient
      result<-lm(formula,data)$coef
      return(result)
      break
    }
    
    else{#if there are more children variable, continue to generate the formula
      formula<-paste(formula,'+')
    }
    
  }
}


#example

betas<-fit_beta(data.frame(iris),'Sepal.Length','versicolor',c('Sepal.Width','Petal.Width'))
betas
