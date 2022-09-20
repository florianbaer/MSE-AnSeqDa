#MSE course: Analysis of Sequential Data
#Giorgio Corani 
#This function shows some basic R commands 

#In Rstudio, you run a chosen line by placing there the cursor 
#ctrl+return (command + return on MacOS) 

# Declare variables of different types
my_numeric <- 42
my_character <- "universe"
my_logical <- FALSE 

# Check value and class of my_numeric in console
my_numeric
class(my_numeric)

#In R, you create a vector with the combine function c(). 
numeric_vector <- c(1, 10, 49)
character_vector <- c("a", "b", "c")


#sum of vectors works elementwise
A_vector <- c(1, 2, 3)
B_vector <- c(4, 5, 6)

# Take the element-wise sum of A_vector and B_vector
total_vector <- A_vector + B_vector 

# Print out total_vector  
total_vector #5 7 9

#selection of elements from a vector
#To select multiple elements from a vector, you can add square brackets at the end of it. 
#You can indicate between the brackets what elements should be selected. 

#For example: suppose you want to select the first element of A_vector:
A_vector[1] #1
B_vector[2] #5

#first and second element of B_vector
idx <- c(1,2)
B_vector[idx]
#more commonly:
B_vector[c(1,2)]

#getting a sequence of elements is done via the colon (":") operator 
B_vector[1:2]#get element 1 and 2
B_vector[1:3]#get element 1, 2 and 3
B_vector[-1]#get all elements avoiding the first one

#selection of element of a matrix
myMatrix <- matrix(data=c(1,2,3,4,5,6), nrow=2, ncol = 3)
#plot the matrix in console
myMatrix 

#select the element in position 1,1
myMatrix[1,1] #1

#select the first row (the empty index after the comma select the entire row)
myMatrix[1,] #1 3 5

#select the second column
myMatrix[,2] #3 4

#setting the name of columns
colnames(myMatrix) <- c("first_col","second_col","third_col")

#accessing a column using its name.
myMatrix[,"second_col"]

#tools and data sets for time series analysis
library(fpp2)

#if the library is not present on your system, you can install it
#by typing:
#install.packages("fpp2")

#the  lines below follow the slides of "time series graphics"
#accessing the time series objects
#?ausbeer provides information about the quarterly data set ausbeer
?ausbeer 
ausbeer[1:10] #data of the first ten quarters

#check that ausbeer is a time series object
class(ausbeer)#ts

frequency(ausbeer)#this a quarterly time series
#4

#a better way to extract part of a time series is to use the window function, which 
#select both the values and the associated time. We need to specify start (default: first value of the time 
#series) and end (default: last value of the time series)

#select data from the second quarter of 1990
ausbeerSince1990 <- window(ausbeer, start=c(1990,2))

#select data up to  the fourth quarter of 1995
ausbeerUpTo1995 <- window(ausbeer, end=c(1995,4))

#produce a nice plot
autoplot(ausbeer)

#easy to customize with labels and title
autoplot(ausbeer) + ggtitle("Australian beer sales") + ylab("Sales") + xlab("Years")

#arrivals is a quarterly multiple time series, i.e. it contains quarterly arrivals 
#for different countries ("UK","NZ","Japan","US")

#autoplot by default plots the four time series together
autoplot(arrivals)

#you can get a separate plot for each time series by specifying facets = TRUE
autoplot(arrivals, facets = TRUE)

#if you wanna plot only Japan: first select Japan from arrivals
#you select the whole time series of Japan as follows:
japan  <- arrivals[,"Japan"] #we select a column by its name 
autoplot(japan)  + ggtitle("Japan") + xlab("Years") + ylab("Arrivals")
