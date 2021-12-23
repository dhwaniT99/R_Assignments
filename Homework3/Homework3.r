#Question 1 

#Storing salary values in a vector
c <- c(3.79,2.99 ,2.45, 2.14, 3.36, 2.05, 3.14, 3.54,2.77, 2.91, 3.10, 1.84, 2.52, 3.22, 2.67, 2.52, 2.71, 2.75, 3.57, 3.85, 2.89, 2.83, 3.13, 2.44, 2.10, 3.71, 2.37,2.68, 3.51, 3.37)
#Calculating mean using mean function
sample_mean <- mean(c)
sample_mean

#Calculating standard deviation using sd()

stan_dev <- sd(c)
stan_dev

#Calculating the relative frequency histogram of data

#Approach 1: (Please consider this)

#Using lattice library we can compute the percentage of occurance of values in our data
lattice::histogram(c)

#Approach 2: (Attempt)
#Computing relative frequency manually
#First we need to calculate range of values and then compute the frequency of the values
#Store the values in table
freq <- table(c)

#To produce the list of frequency we need to use the as.vector to extract teh list of the frequency and then to extract teh values from teh table

freq_values <- as.vector(freq)
freq_names <- names(freq)


temp <- data.frame(freq_values)
temp

temp_1 <- table(temp)
cnt <- as.vector(temp_1)

rel_freq <- (cnt/total)*100
rel_freq


#Now to calculate the relative frequency in we need to compute the the total freq_values and then divide the them with the individual values in r
f_size <- length(freq_values)

library(dplyr)
rel_frequency <- freq_values/total
rel_f <- as.numeric(rel_frequency)

#Calculating the sum of the relative frequency to check whether we are obtaining perfect accuracy or not.
test <- sum(rel_frequency)

#Storing all freq_values, f_size, rel_frequency in data a data frame
df_freq <- data.frame(freq)

df_freq$rel_freq <- rel_frequency

#Plotting the histogram
library(ggplot2)
ggplot(df_freq, aes(rel_freq)) + geom_bar(color = 'gray', bins = 6)

#Answer c
typeof(freq)
stem(c, scale = 0.5)

#Answer d
df <- data.frame(c)
df
boxplot(df, horizontal = TRUE, xlab = "Samples")

#Question 2 
#There are 6 lots on one side of street and 3 on the opposite side
#Possible ways he can connect 6 lots are 

n <- 9
lots <- 6
x <- factorial(n)/factorial(lots)
x
opp_n <- 3
y<- factorial(opp_n)
arrange <- x*y
arrange

#Question 3 
#Number of ways no 2 students should have the same birth-date is 
perm <- function(k,f){
  factorial(k)/factorial(k-f)
}
perm(365,60) 

#The answer is 3.211830504E +151

#Question4
#Here the probabiliy of customer will invest in tax-free bonds with a probability of 0.6 
P1 <- 0.6

# Customer will invest in mutual funds with a probability of 0.3,
P2 <- 0.3

#Customer will invest in both i.e P1 intersection P2

P1P2 <- 0.15

#answer a 
ans1 <- P1 + P2 - P1P2
ans1

#answer b
ans2 <- 1 -ans1
ans2

#Question 5 

#Defining a function for calculating combinations
combination <- function(n,r){
  factorial(n)/(factorial(r)*(factorial(n-r)))
}
#Calculating sample space
sample_space <- combination(52,5)
sample_space

#Probability of getting 3 ace cards from 4 aces
aces <- combination(4,3)

#Probability of getting 2 non aces from the 5 cards
non_aces <- combination(48,2)

#Probability of getting 3 aces from 5 cards

ans <- (aces * non_aces)/sample_space
ans

#Answer 5.b
#Selecting 4 hearts and 1 club from 5 cards
# No of hearts in a deck of 52 is 13, hence probability of getting 4 hearts from 13 cards is 
hearts <- combination(13,4)

#No of clubs 
club <- combination(13,1)

#Probability of getting 4 hearts and 1 club
ansb <- (hearts*club)/sample_space
ansb

#Question 6, 7, 8 in PDF file attached

#All members contributed equally