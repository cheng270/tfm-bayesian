#Function that return the coeffient for each value of class
#Input variables:
#                 input: name of the data 
#                 parents: parents variable. Type: c()
#                 class: name of the class. Type: String
#                 variable: children variable. Type: String
#output variable: 
#                 the coeffients for each value of class

#Program:Rstudio Version 1.2.1578
#Package: None
#Date:19/03/2020
fit_beta<-function(input,parents,Class,variable){
  
  dataFrame<-data.frame(input)
  formula<-paste(variable,"~")
  for (i in 1:length(parents)){
    
    formula<-paste(formula,parents[i:i])#generate the formula
    i=i+1
    
    if( is.na(parents[i:i])){ # if no more parent variable to add in the formula
      
      valueClass<-unique(dataFrame[Class])#obtain differet values of theclass
      classPosition<-grep(Class,colnames(dataFrame),value=F)#position of the property class in the data frame

      for (k in 1:nrow(valueClass)){

        data <- subset(dataFrame, dataFrame[classPosition] == as.character(valueClass[k,1]))#subset according to the value of the class
        e<-lm(formula,data)$coef
        
        #generate the result
        if (k==1){
          result<-cbind(e)
          newRowName<-as.character(valueClass[k,1])
        }
        else{
          result<-cbind(result,e)
          newRowName<-c(newRowName,as.character(valueClass[k,1]))
        }
      
      }
      
      colnames(result)<-newRowName
      return(result)
      break
    }
    
    else{#if there are more parent variable, continue to generate the formula
      formula<-paste(formula,'+')
    }
    
  }
  
}


#example
x<-fit_beta(iris,c('Sepal.Length','Sepal.Width','Petal.Width'),'Species','Petal.Length')
x


