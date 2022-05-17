######Function 1:  NA_position######
NA_position <- function(x, y){
  all(is.na(x)==is.na(y))
  
}

#####Function 2: smart_test########
smart_test <- function(x){
  a<-table(x)
  if(a[1]>=5&a[2]>=5&a[3]>=5&a[4]>=5){vec<-c(chisq.test(a)$statistic,chisq.test(a)$parameter,chisq.test(a)$p.value)}else{vec<-c(fisher.test(a)$p.value)}
  return(vec)
}

#####Function 3: most_significant##
most_significant <-  function(x){
  x <- as.data.frame(x)
  #assinging numeric values to nucleotide codes:
  x[x=='A'] <- 1
  x[x=='C'] <- 2
  x[x=='G'] <- 3
  x[x=='T'] <- 4
  x <-sapply(x, as.numeric)
  x <- as.data.frame(x)
  chisq <- sapply(x,chisq.test)#storing chisq.test results in the variable chisq
  chisq <- as.data.frame(chisq[3,])#assingning only p-values to chisq variable
  a <- colnames(chisq)[which(chisq==min(chisq))]#a contains the colname with the minimal chisq
  return(a)
}

######Function 4: get_important_cases# ##
library("dplyr")
get_important_cases <- function(x){
  l <- length(select_if(x,is.numeric))##calculate the number of numeric variables
  a <- select_if(x,is.numeric)#select only numeric variables
  #The new variable 'important_case' equals 'Yes' if more than a half of values of variables corresponding to this observation are higher than the mean of all the observations of the variable.
 # Otherwise, the variable 'important case' equals 'No'
  x$important_cases <- factor(apply(x,1,function(x){ifelse(sum(x > colMeans(a)) >l%/%2, 1, 2)}),levels=c(1, 2),labels=c('Yes','No'))
  return(x)
}

#######Funcion 5:  NA.position###
NA.position <- function(x){
  which(is.na(x))
}

######Function 6:  NA.counter###
NA.counter <- function(x){    
  return(sum(is.na(x)))}

#####Function 7: filtered.sum###
filtered.sum <- function(x){    
  return(sum(x[x > 0], na.rm = T))}

#####Function 8: outliers.rm#####
outliers.rm <- function(x){
  e <- quantile(x, probs = c(0.25, 0.75))
  x=x[x<(e[2]+1.5*IQR(x))&x>(e[1]-1.5*IQR(x))]
  return(x)
}

####Function 9: corr.calc ########
library(psych)
corr.calc <- function(x){
  a <- c(0,0)
  colnames(x) <- c("col1","col2")#assign names to the data frame variables
  res_cor <- cor.test(~ x$col1 + x$col2, x)#applying the correlation test function
  a[1] <- res_cor$estimate##assign the correlation coefficiet value to the fist element of the vector
  a[2] <- res_cor$p.value#assign the p-value to the second element of the vector
  return(a)
}

####Function 10: filtered.cor#######
filtered.cor <- function(x){    
  num_var <- sapply(x, function(x) is.numeric(x))#identify numeric variables   
  cor_mat <- cor(x[, num_var])#create a correlation matrix with the values of correlation coefficients between pairs of numeric variables
  diag(cor_mat) <- 0 #change the diagonal elememnts of the correlation matrix from 1 to 0   
  return(cor_mat[which.max(abs(cor_mat))])}#return the corr. coefficient with the maximum absolute value


####Function 11: smart_cor #########
library(psych)
smart_cor <- function(x){
  a <- c(0)#create a vector 'a' with a a[1]=0
  shapiro_col1 <- shapiro.test(x$col1)#apply Shapiro test to the first variable
  shapiro_col2 <- shapiro.test(x$col2)#apply Shapiro test to the second variable
  if (shapiro_col1$p.value<0.05|shapiro_col2$p.value<0.05){
    b <- cor.test(x$col1,x$col2, method="spearman") 
  }else{
    b<- cor.test(x$col1,x$col2, method="pearson")
  }#If the p-value when applying the Shapiro test is less than 0.05 for the two variables,
  #the Spearman's correlation coefficient is computed; otherwise if the two variables
  #are normally distributed, the Pearson's correlation coefficient is computed
  a[1] <- b$estimate #a[1] now equals to the value of the correlation coefficient
  return(a)
}

###Function 12: regr.calc#########
regr.calc <- function(x){
  b<- cor.test(x[,1],x[,2], method="pearson")#Apply Pearson's correlation test
  if(b$p.value<0.05){
    fit3 <- lm(x[,1]~x[,2],x)
    fit=fit3$coefficients[2]*x[,2]+fit3$coefficients[1]
    x$fit <- fit
    return(x)
  }else{print("There is no sense in prediction")
  }#If the two variables significantly correlate, create a new variable 'fit' containing the predicted values of the dependent variables
  #If the two variables do not significantly correlate, print a message saying that there is no sense in prediction
}

###Function 13: fill_na ###### 
fill_na <- function(x){
  a <- lm(y~x_1+x_2,data=x)#building a linear regression model using x_1 and x_2 as independent variables and y as the dependent variable 
  b <- a$coefficients[1]+a$coefficients[2]*x$x_1+a$coefficients[3]*x$x_2#finding the predicted values of the dependent variable
  for (i in 1:nrow(x)){
    ifelse(is.na(x$y[i]),x$y_full[i] <- b[i],x$y_full[i] <- x$y[i])
  }#filling in the new 'y_full' variable with either the values of y or a value predicted by the model (in cases when y values are missing)
  return(x)
}

###Function 14: beta.coef ##### 
beta.coef <- function(x){
  a <-scale(x,center= TRUE, scale=TRUE) #apply scale function performs Z-transformation
  c <- lm(a[,1]~a[,2],data=x)#build a linear regression model using the standardized values
  return(c(c$coefficients[1],c$coefficients[2]))#return the model coefficients
}

###Function 15: normality.test ###
normality.test  <- function(x){
  shap <- sapply(x,shapiro.test)#apply shapiro.test to all the variable in a dataframe; saving the output of shapiro.test into the list 'shap'
  vec <- unlist(shap[2,])#taking the second column of the shap dataframe (containing p-values from the shapiro test) and putting it into the vector 'vec'
  names(vec) = gsub(pattern = ".p.value*", replacement = "", x = names(vec))#assign names to the 'vec' vector elements according to the correspondent names of the variables from the input dataframe
  return(vec)
}

###Function 16: resid.norm #########
###The function works in the following way:
#> fit <- lm(mpg ~ disp, mtcars)
#> my_plot <- resid.norm(fit)
#> my_plot
library(ggplot2)
resid.norm  <- function(fit){
  a <- fit$residuals#saving the values of the fit residuals into a list 'a'
  a <- as.data.frame(a)
  if(shapiro.test(a$a)$p.value>0.05){
    my_plot <- ggplot(a, aes(x = a)) + 
      geom_histogram(fill = 'green', col = 'green')
  }else{
    my_plot <- ggplot(a, aes(x = a) )+ 
      geom_histogram(fill = 'red', col = 'red')
  }#If the residuals are normally distributed, a histogram of the distribution painted in green color is generated
  #otherwise a histogram painted in red color is generated
  return(my_plot)
}

###Function 17: high.corr ##### 
high.corr <- function(x){
  cor_matr <- cor(x)#create a correlation matrix with with correlation coefficients between pairs of variables
  diag(cor_matr) <- #assign 0 to the diagonal elements of the matrix(instead of 1)
  cor_matr <- round(cor_matr,digits=3)#round the values of the correlation coefficients to the third digit
  cor_matr <- abs(cor_matr)#take the absolute values of the correlation coefficients 
  r_names <- which(cor_matr==max(cor_matr),arr.ind=TRUE )#identify the maximum absolute value/values of all the correlation coefficients
  var_names <- rownames(r_names)#take the column name/names that correspond to the maximum value/values of the correlation coefficients
  return(var_names)
}

###Function 18:  stat_mode ###
stat_mode <- function(x){
  t <- as.data.frame(table(x)) #create a frequency table for the vector x
  i <- which(t$Freq==max(t$Freq))#finding the maximum value/values in the frequancy table
  return(as.numeric(levels(t$x[i]))[i]) #return the maximum value/values in the frequency table in the form of a numeric vector
}

###Function 19: max_resid ####
max_resid <- function(x){
 chi_test<-as.data.frame(chisq.test(table(x))$stdres)
 i<-which(chi_test$Freq==max(chi_test$Freq))
 c<-c(as.character(chi_test$Drugs[i]))
 c<-append(c,as.character(chi_test$Result[i]))
 return(table(x))
}


