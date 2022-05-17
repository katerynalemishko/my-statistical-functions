# my_statistical_functions

The R script in this repository contains the following useful statistical functions.

FUNCTION 1
NA_position

This function takes two vectors of the same length with numeric values as an input and returns TRUE if the NA positions in both vectors coincide or there are no NA values,
and it returns FALSE if the NA positions in the vectors do not coincide


FUNCTION 2
smart_test

This function takes as an input a data frame with two nominal variables with an arbitrary number of levels.
The functions tests the hypothesis about independency of the two variables (with Pearson's chi-squared test or Fisher's exact test).

If at least in one of the cells the number of observations is less than 5, the function computes the Fisher's exact criteria and returns a vector that contains the computed p-value.

Otherwise, if the number of observations is enough for computing the chi-squared criteria (the number of observations in all the cells is higher than or equal to 5, then the function applies the chi-squared test and returns a vector containing three values: the value of chi-squared, the number of degrees of freedom and the p-value.


FUNCTION 3
most_significant

This function takes as an input a dataframe with an arbitrary number of variables, where each variable contains a nucleotide sequence (e.g. GCTAGGTTACTGAATTT...).
The function returns a vector with the name of the variable (or several variables) with the minimal p-value obtained when applying the chi-squared test.


FUNCTION 4
get_important_cases

This function takes as an input a dataframe with an arbitrary number of values (but it should contain at least two variables).
The function returns a data frame with a new added variable 'important_cases' which is a factor. 

The variable 'important_case' equals 'Yes' if more than a half of values of variables corresponding to this observation are higher than the mean of all the observations of the variable.
Otherwise, the variable 'important case' equals 'No'

So the variable 'important_case' is a factor with two levels: 0 - "No", 1  - "Yes"


FUNCTION 5
NA.position

This functions gives the positions of the missed values in a vector.
The function takes as an input a vector with missed values and it returns a new vectors containg the positions of the missed values.


FUNCTION 6
NA.counter

This function counts the number of the missed values in a vector


FUNCTION 7
filtered.sum

This function takes as an input a verctor with missed, positive and negative values and it returns the sum of the positive values of the vector.


FUNCTION 8
outliers.rm

This function finds and removes outliers. If the value is below (25th percentile-1.5*IQR) or above (75th percentile+1.5*IQR).
Where IQR is the interquartile range.
The function takes as an input a vector with numeric values and returns a vector without the outliers.


FUNCTION 9
corr.calc

This function takes as an input a dataframe with two numeric variables and computes the Pearson correlation coefficient. It returns a vector with two values: the correlation coeffictient and the p-value.


FUNCTION 10
filtered.cor

This function takes as an input a dataframe with the arbitrary number of variables (of all the types). It computes the Pearson correlation coefficients between all the pairs of numeric variables and returns the value with the highest absolute value (i.e. the function will return -0.9 if this is the correlation coefficient with the highest absolute value).
The input dataframe should contain at least two numeric variables.


FUNCTION 11
smart_cor

This function takes as an input a dataframe with two numeric variables. It applies the Shapiro-Wilk test to check if the data from both variables is normally distributed 
If at least one of the vector contains data that is not noramally distributed (p-value<0.05),then the function returns the Spearman's rank correlation coefficient (a numeric vector with one element).
If the distribution of the data in the two vectrors does not significantly deviate from a normal distribution, the function returns the Pearson's correlation coefficient. 


FUNCTION 12
regr.calc

This function takes as an input a dataframe with two variables.
If the two variables significantly correlate (the p-value of the Pearson's correlation coefficient is lower than 0.05), then the function builds a regression model , where the first variable is dependent and the second variable is independent.
Then the function creates a new variable 'fit' in the dataframe containing the values of the dependent variable predicted by the linear regression model.
If the two variables in the input dataframe do not significantly correlate , then the functions returns a string "There is no sense in prediction".


FUNCTION 13
fill_na

This function takes as an input data with three variables:

x_1 - a numeric vector;
x_2 - a numeric vector;
y - is a numeric vector with missing values.

In the first step, the function uses the observations from the variables that do not contain missing values to build a liner regression model (without an interaction effect), where y is the dependent variable, x_1 and x_2 are independent variables.
Then, in the second step, the function uses the build regression model to fill in the missing values of y.
The function returns a dataframe with the new variable 'y_full' containing the variable y whose missing values are filled in by the values predicted with the model.


FUNCTION 14
beta.coef

This function takes as an input a dataframe with two numeric variables and returns standardized coefficients of the linear regression model, in which the first variable of the dataframe is dependent and the second variable of the dataframe is independent.
Standardized coefficients are obtained by running a linear regression model on the standardized form of the variables (their means are equal to 0 and standard deviations are equal to 1).


FUNCTION 15
normality.test

This function takes as an input a dataframe with numeric variables. It tests the normality of the distribution of each of the variables using Shapiro-Wilk test.
The function returns a vector containing computed p-values for each variables. The names of the elements of the output vector coincide with the names of the variables of the input dataframe.


FUNCTION 16
resid.norm

This function takes a linear regression model as an input.
The function tests the distribution of the model residuals for normality using Shapiro-Wilk test and creates a histogram which is painted red, if the distribution of the residuals significantly deviates from normal, and the histogram is painted in green color, if the distribution does not significantly deviates from normal.

The function works like this:

> fit <- lm(mpg ~ disp, mtcars)
> my_plot <- resid.norm(fit)
> my_plot


FUNCTION 17
high.corr

This function takes as an input a dataset with an arbirary number of variables.
The function computes the correlation coefficients for all the pairs of variables and it returns a vector containing the names of the two variables with the maximum absolute value of the correlation coefficient.

The fuction forks in the following way:

> high.corr(swiss)
[1] "Examination" "Education"


FUNCTION 18
stat_mode

This function takes as an input a numeric vector of an arbitrary length and finds the mode of the set of values in the input vector.
The function returns a vector with the most commonly represented value from the input vector. If there are more than one most frequently represented value in the input vector, the output vector will contain all of them.


FUNCTION 19
max_resid

This function helps to compare the efficacy of three tested drugs.
It takes as an input a dataframe with two variables: the type of drug and the result of evaluation of its efficacy.  

The variable 'Drugs' (which represents the type of drug) has three levels:drug_1, drug_2, drug_3;
the variable 'Result' (which represent the result of evaluation of the drug efficacy) has two levels: positive, negative.

The function max_resid performs chi-square test on the contengency table with the results of efficacy evaluation of different drug types. 
The function finds the maximum value of standardized residuals and returns a vector that contains the name of the type of the drug and its evaluated efficacy which correspond to the maximum value of standardized residuals.


