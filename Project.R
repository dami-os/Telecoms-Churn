# load the library
library(dplyr)
library(ggplot2)
library(ggpubr)
library(Hmisc)
library(corrplot)
library(dummies)
library(mlbench)
library(caret)
library(ROSE)
library(e1071)
library(randomForest)
library(ROCR)
library(vtable)
library(rpart.plot) 
library(rpart)
#Specify file name
file <- 'Telco-Customer-Churn-MANM354.csv'

#Load Data
df <- read.csv(file)


#look at top 5 rows
head(df)


#Look at Data structure and summary
str(df)
summary(df)

#Print summary statistics for report
is.chr <- select_if(df, is.character)
chr.data <- colnames(is.chr)

is.num <- select_if(df, is.numeric)
num.data <- colnames(is.num)

sumtable(df[,c(num.data,chr.data)],summ=c('notNA(x)',
                   'mean(x)',
                   'median(x)',
                   'propNA(x)',
                   'max(x)',
                   'min(x)',
                   'sd(x)'),out='csv',file='summary.csv')


data_cleaning <- function(df){
  # all character columns to factor:
  df <- mutate_if(df, is.character, as.factor)
  
  
  # all integer columns to numeric:
  df <- mutate_if(df, is.integer, as.numeric)
  
  ## removes duplicate rows based on each columns
  print(paste('Row count:',nrow(df),'Duplicate count:',nrow(df)-dim(distinct(df))[1]))
  df <- distinct(df)
  
  
  #Looking at missing value percentage
  print('Missing value percentage')
  print(sum(is.na(df$TotalCharges))/nrow(df))
  
  # extracting positions of NA values
  print ("Row and Col positions of NA values")
  out <- which(is.na(df), arr.ind=TRUE)
  print('Positions of NA values in the dataset')
  print(out[,1])

  #rows
  print('Row count before removing nulls')
  print(nrow(df))
  
  #We remove all instances of null
  df <- na.omit(df)
  
  
  #rows
  print('Row count after removing nulls')
  print(nrow(df))

  return(df)  
}

df <- data_cleaning(df)


#removing customer ID
df <- df[,-c(1)]



#Converting senior citizen to yes no and then to a factor 
df$SeniorCitizen <- as.factor(mapvalues(df$SeniorCitizen,
                                                 from=c(0,1),
                                                 to=c("No", "Yes")))


#checking if Phone service is no then is multiple lines also no
df[df$PhoneService=='No',]%>%group_by(PhoneService,MultipleLines)%>%dplyr::summarise(count=n()) 

#Changing No Phone service to No
df$MultipleLines <- as.factor(gsub("No phone service",'No', df$MultipleLines))

is.fact <- select_if(df, is.factor)
factors.data <- colnames(is.fact)
for(i in factors.data){
  #picks all factor columns

  df[,i] <- as.factor(mapvalues(df[,i],
                                from= c("No internet service"), to= c("No")))
}




#Plotting and Visualization
#set display = 0 to save plot as png or set = 1 to display
bar_plot <- function(data,display=1){
  
  #picks all factor columns
  is.fact <- select_if(data, is.factor)
  factors.data <- colnames(is.fact)
  #factors.data <- factors.data[-length(factors.data)]
  
  #plots categorical columns against churn

  for( i in factors.data){
        
        p <- ggplot(data, aes(x=data[,c(i)])) + geom_bar(aes(fill=Churn)) + 
        xlab(i) +
        ylab('Count') +
        ggtitle(paste(i,'barplot'))+
        geom_text(aes(y = ..count.. -200, 
                      label = paste0(round(prop.table(..count..),4) * 100, '%')), 
                  stat = 'count', 
                  position = position_dodge(.1), 
                  size = 7)+ theme(axis.text.x = element_text(angle = 30, vjust = 0.5,size=15),
                                   axis.text.y = element_text(angle = 30, vjust = 0.5,size=10),
                                   legend.title=element_text(size=12), 
                                   legend.text=element_text(size=10) )
            if(display==1){
              print(p)
             
              
            }
            else if(display==0){
              png( file=paste('plots/',i,'wise barplots.png',sep=''))
              print(p)
              dev.off()
              
            }
  }

}
bar_plot(df,1)
bar_plot(df,0)

#set display = 0 to save plot as png or set = 1 to display
histogram <- function(data,display=1){
  #picks all integer columns
  is.int <- select_if(data, is.numeric)
  int.data <- colnames(is.int)
  print(int.data)
  plot_list <- list()
  x <- 1
  
  #plots integer columns 
  for( i in int.data){
  
  if(display==1){
      print(ggplot(data, aes(x=data[,c(i)])) + geom_histogram(color='black',fill='orange') + 
              xlab(i) +
              ylab('Frequency') +
              ggtitle(paste('Histogram of ', i)))
    
    print(ggplot(data, aes(x=data[,c(i)],color=Churn,fill=Churn)) + geom_histogram() + 
            xlab(i) +
            ylab('Frequency') +
            ggtitle(paste('Histogram of ', i,'Churn wise')))
    }    
    else if(display==0){
      png(height=720, width=1080,res=100, file=paste('plots/',i,' histogram.png',sep=''))
      print(ggplot(data, aes(x=data[,c(i)])) + geom_histogram(color='black',fill='orange') + 
        xlab(i) +
        ylab('Frequency') +
        ggtitle(paste('Histogram of ', i)))
      dev.off()
      
      png(height=720, width=1080,res=100, file=paste('plots/',i,' histogram churn wise.png',sep=''))
      print(ggplot(data, aes(x=data[,c(i)],color=Churn,fill=Churn)) + geom_histogram() + 
              xlab(i) +
              ylab('Frequency') +
              ggtitle(paste('Histogram of ', i,'Churn wise')))
      dev.off()      
    }

  }
  
}
histogram(df,0)
histogram(df,1)

#Correlation plot
correlation <- function(df,display=1){
  
  #picks all numeric columns
  print(select_if(df, is.numeric))
  is.num <- select_if(df, is.numeric)
  num.data <- colnames(is.num)
  

  if(display==0){
    png(paste('plots/correlation.png',sep=''))
    df[,c(num.data)]%>%
      cor() %>%
      corrplot.mixed(upper = "circle", tl.col = "black")
    dev.off()  
  }
  else if(display==1){
    df[,c(num.data)]%>%
      cor() %>%
      corrplot.mixed(upper = "circle", tl.col = "black")
  }
  
}
correlation(df,1)
correlation(df,0)


#Machine Learning Model 

# Split into Train and Validation sets
# Training Set : Validation Set = 70 : 30 (random)
set.seed(100)
train <- sample(nrow(df), 0.7*nrow(df), replace = FALSE)
TrainSet <- df[train,]
ValidSet <- df[-train,]

prop.table(table(TrainSet$Churn))
prop.table(table(ValidSet$Churn))

# We will first make a Decision Tree model
decision_tree <- function(train,test,display=1){
 
  model_dt = rpart(Churn ~ ., data = train)
  if(display==1){  rpart.plot(model_dt) }
  else if (display==0){
    png(paste('plots/decistion_tree.png',sep=''))
    rpart.plot(model_dt)
    dev.off()
    }
  
  
  model_dt_1 = predict(model_dt,type="class")
  
  print('Train set prediction')
  print(table(prediction=model_dt_1, actual=train$Churn))
  print('Train set Accuracy')
  print(mean(model_dt_1 == train$Churn))
  
  # Running on Validation Set
  model_dt_vs = predict(model_dt, newdata = test,type='class')
  
  print('Validation Set accuracy and confusion matrix')

  print(confusionMatrix(table(prediction=model_dt_vs, actual=test$Churn),positive='Yes'))
  return(model_dt)
  
}

dtree <- decision_tree(TrainSet,ValidSet)
decision_tree(TrainSet,ValidSet,0)
#Since our positive (churned) cases are less we oversample

#Number of total samples we need to generate for the trainset
#we are generating samples twice the number of 'No' cases

over_s <- function(data){
  print('Before oversampling trainset class count')
  n <- data.frame(data%>%group_by(Churn)%>%dplyr::summarise(count=n()))
  print(n)
  n <- n[n$Churn=='No',]$count
  n<- n*2
  over <- ROSE(Churn~., data = data, N = n)$data
  print('After oversampling trainset class count')
  print(table(over$Churn))
  return(over)
}

TrainSet.over <- over_s(TrainSet)
#Accuracy have seemed to drop but sensitivity has increased significantly
dtree.over <- decision_tree(TrainSet.over,ValidSet)

# Create a Random Forest full model with default parameters
r_forest <- function(train,test){
  
  model <- randomForest(Churn ~ ., data = train, importance = TRUE)
  model
  
  # Predicting on train set
  print('Train set prediction')
  predTrain <- predict(model,type = "class")
  # Checking classification accuracy
  print(table(prediction=predTrain, actual=train$Churn))
  print('Train set Accuracy')
  print(mean(predTrain == train$Churn))                    
  
  # Predicting on Validation set
  predValid <- predict(model, test, type = "class")
  # Checking classification accuracy
  print(confusionMatrix(table(prediction=predValid, actual=test$Churn),positive='Yes'))
  return(model)
  
}

rf <- r_forest(TrainSet,ValidSet)

#Random forest with over sampled Trainset
rf.over <- r_forest(TrainSet.over,ValidSet)

### Improving random forest 

# Using random forest for variable selection  
rf_var_selection <- function(df,display=1){
  if(display==1){
    # Using random forest for variable selection  
    rfModel <-randomForest(Churn ~ ., data = df, importance = TRUE)
    
    
    #Display variable importance from random tree
    varImpPlot(rfModel, sort=T, n.var = 15, 
               main = 'Top 15 important variables',col='red',pch=19)
    #imp <-  varImp(rfModel, scale=FALSE,n.var=15)
    
  } 
  else if(display==0){
    # Using random forest for variable selection  
    
    rfModel <-randomForest(Churn ~ ., data = df, importance = TRUE)
    
    
    #Display variable importance from random tree
    print('display')
    png(paste('plots/varImportance.png',sep=''))
    varImpPlot(rfModel, sort=T, n.var = 15, 
               main = 'Top 15 important variables',col='red',pch=19)
    dev.off()  
    #imp <-  varImp(rfModel, scale=FALSE,n.var=15)
  }

  
}
rf_var_selection(df,1)
rf_var_selection(df,0)

#Fine tuning Random Forest model

#First select top 10 important features
TrainSet.imp <- TrainSet[,c('Churn','TotalCharges','MonthlyCharges','tenure','Contract','PaymentMethod',
                              'InternetService','TechSupport','OnlineSecurity','PaperlessBilling','PaymentMethod','MultipleLines')]

ValidSet.imp <- ValidSet[,c(c('Churn','TotalCharges','MonthlyCharges','tenure','Contract','PaymentMethod',
                          'InternetService','TechSupport','OnlineSecurity','PaperlessBilling','PaymentMethod','MultipleLines'))]


#Now we balance the classes in the train set
TrainSet.imp.over <- over_s(TrainSet.imp)

#Using this data with important features and balanced classes for training
rf.imp.over <- r_forest(TrainSet.imp.over,ValidSet.imp)

#Logistic Regression

lr <- function(train,test){
  lr_fit <- glm(Churn ~., data =train,
                family=binomial(link='logit'))
  print('Model Summary')
  print(summary(lr_fit))
  
  lr_prob1 <- predict(lr_fit, type="response")
  lr_pred_1 <- ifelse(lr_prob1 > 0.5,"Yes","No")
  
  print('Train set prediction')
  print(table(prediction=lr_pred_1, actual=train$Churn))
  print('Train set Accuracy')
  print(mean(lr_pred_1 == train$Churn))
  
  # Running on Validation Set
  lr_prob_vs <- predict(lr_fit, newdata=test, type="response")
  lr_pred_vs <- ifelse(lr_prob_vs > 0.5,"Yes","No")
  


  
  print('Validation Set accuracy and confusion matrix')
  
  print(confusionMatrix(table(prediction=lr_pred_vs, actual=test$Churn),positive='Yes'))
  return(lr_fit) 
}
logist <- lr(TrainSet,ValidSet)

logist.over <- lr(TrainSet.over,ValidSet)


### Comparison

comp <- function(model,modelname,test,type='class'){
  #To get TP, FP, TN, FN
  
  if(type=='class'){
    pred <- predict(model,test,type='class')  
  }else if(type=='response'){
    pred <- predict(model, newdata=test, type="response")
    pred <- ifelse(pred > 0.5,"Yes","No")
  }
  
  pred_table <- table(prediction=pred, actual=test$Churn)
  mat <- confusionMatrix(pred_table,positive='Yes')
  
  cm <- data.frame(mat$table)
  
  #To get accuracy
  ac <- data.frame(values=mat$overall)
  
  #To get precision, recall
  ov <- data.frame(values=mat$byClass)
  
  #Chaning rownames to column
  ac <- tibble::rownames_to_column(ac, "metric")
  ov <- tibble::rownames_to_column(ov, "metric")
  
  cm$metric <- ''
  cm[cm$prediction=='No'&cm$actual=='No',]$metric <- 'TN'
  cm[cm$prediction=='Yes'&cm$actual=='Yes',]$metric <- 'TP'
  cm[cm$prediction=='Yes'&cm$actual=='No',]$metric <- 'FP'
  cm[cm$prediction=='No'&cm$actual=='Yes',]$metric <- 'FN'
  
  colnames(cm) <- gsub('Freq','values',colnames(cm))
  
  #Combining all the metrics and values
  final <- rbind(ac,ov,cm[,c('metric','values')])
  final$model <- modelname
  
  #Making the final dataframe with relevant metrics
  output <- final[final$metric%in%c('Accuracy','Precision','Recall','Specificity','TP','TN','FP','FN'),]
  
  return(output)  
}
c1 <- comp(dtree,'Decision Tree',ValidSet,'class')
c2 <- comp(dtree.over,'DT Oversampled',ValidSet,'class')
c3 <- comp(rf,'Random Forest',ValidSet,'class')
c4 <- comp(rf.over,'RF Oversampled',ValidSet,'class')
c5 <- comp(rf.imp.over,'RF Important Variables Oversampled',ValidSet.imp,'class')
c6 <- comp(logist,'Logistic Regression',ValidSet,'response')
c7 <- comp(logist.over,'LR Oversampled',ValidSet,'response')

final <- rbind(c1,c2,c3,c4,c5,c6,c7)

plot_comparison <- function(df,display=1){
  
  for(i in df$metric){
    
    if(display==0){
      png(paste('plots/Model',i,'Comparison.png',sep=''))
      print(
      ggplot(df[df$metric==i,], aes(x=model,y=values)) + geom_bar(aes(fill=model),stat='identity') + 
        xlab('Models') +
        ylab(i) +
        ggtitle(paste('Model', i, 'Comparison barplot'))+ theme(axis.text.x = element_text(angle = 30, vjust = 0.5,size=8),
                                                                axis.text.y = element_text(angle = 30, vjust = 0.5,size=10),
                                                                legend.title=element_text(size=12), 
                                                                legend.text=element_text(size=10) ) )
      dev.off()

    }
    else if(display==1){
      print(ggplot(df[df$metric==i,], aes(x=model,y=values)) + geom_bar(aes(fill=model),stat='identity') + 
              xlab('Models') +
              ylab(i) +
              ggtitle(paste('Model', i, 'Comparison barplot'))+ theme(axis.text.x = element_text(angle = 30, vjust = 0.5,size=8),
                                                                      axis.text.y = element_text(angle = 30, vjust = 0.5,size=10),
                                                                      legend.title=element_text(size=12), 
                                                                      legend.text=element_text(size=10) ) ) 
    }
    
  }
  
  
}
plot_comparison(final[final$metric%in%c('Accuracy','Precision','Recall'),])
plot_comparison(final[final$metric%in%c('Accuracy','Precision','Recall'),],0)




#Getting Total Charges
pred <- predict(logist.over, newdata=ValidSet, type="response")
pred <- ifelse(pred > 0.5,"Yes","No")

tot <- merge(ValidSet,pred,by='row.names')
tot$metric <- ''
tot[tot$y=='No'&tot$Churn=='No',]$metric <- 'TN'
tot[tot$y=='Yes'&tot$Churn=='Yes',]$metric <- 'TP'
tot[tot$y=='Yes'&tot$Churn=='No',]$metric <- 'FP'
tot[tot$y=='No'&tot$Churn=='Yes',]$metric <- 'FN'

output <- tot%>%group_by(metric)%>%dplyr::summarise(sumTotalCharges=sum(TotalCharges),count=n())
output
##TP, FP, TN, FN Total Charges for LR oversampled
write.csv(output,'TP_FP_TN_FN_TotalCharges.csv')
