# # # # DO NOT MODIFY THE CODE BELOW # # # #
if(!exists("red")){
  red=sample(2:5,1)
  blue=sample(3:7,1)
  green=sample(4:8,1)
}
showplot<-function(){
  # Plot is plotted at height of 5, not to scale
  plot(c(0,3,5,0),c(0,5,0,0), type='l', yaxt = "n",
       xlab='x', ylab='density', main="Probability distribution of x")
  axis(2, at=c(0,5), labels=c("0","height"))
  lines(c(3,3),c(0,5), lty=3)
  polygon(c(0,3,5,0),c(0,5,0,0))
}
# # # # DO NOT MODIFY THE CODE ABOVE # # # #

# Consider a vase (or vase if you prefer that pronunciation) containing 
#  rubber balls of 3 different colors, red, blue, and green.
# The number of balls of each color is random, but will be saved in the
#  variables 'red', 'blue', and 'green'. 

# You will need to provide 'solutions', not 'answers', to these problems. IE:
#  your solution should use the variables 'red', 'blue', and 'green', rather
#  than numbers, which will be subject to change. Thus answers should be
#  correct no matter how many of each ball is in the vase (but there will
#  always be enough of each color)

# Be sure to pay attention to whether the sample is with our without replacement

# Question 1.a
# If we draw a single ball at random, what is the probability that we
#  draw a red ball?
# Save your answer in the variable q1.a
q1.a <- red / (red + blue + green)
q1.a

# Question 1.b
# If we drew a red ball and do not return it to the vase, what is the
#  probability that the next draw is blue?
# Save your answer in the variable q1.b
q1.b <- blue / (red - 1 + blue + green)
q1.b

# Question 1.c
# If we draw 4 balls at random, replacing them each time we draw one,
#  what is the probability that we draw 4 green balls?
# Save your answer in the variable q1.c
q1.c <- (green / (red + blue + green))^4
q1.c

# Question 1.d
# If we draw 4 balls at random, not replacing them when we draw, what
#  is the probability that we draw 4 green balls?
# Save your answer in the variable q1.d
total <- red + blue + green
q1.d <- (green * (green-1) * (green-2) * (green-3)) / (total * (total-1) * (total-2) * (total-3))
q1.d

# Question 1.e
# In how many unique orders can we draw 1 blue, 1 green, and 2 red balls?
# Save your answer in the variable q1.e
q1.e <- factorial(4) / (factorial(2) * factorial(1) * factorial(1))
q1.e

# Question 1.f
# What is the probability of drawing (with replacement) 2 red balls, followed
#  by 1 blue ball, followed by 1 green ball.
# Save your answer in the variable q1.f
total <- red + blue + green
q1.f <- (red/total)^2 * (blue/total) * (green/total)
q1.f

# Question 1.g
# What is the probability of drawing (without replacement) 2 red balls,
#  1 blue ball, and 1 green ball in any order?
# Save your answer in the variable q1.g
total <- red + blue + green
# Number of ways to arrange 2 red, 1 blue, 1 green
arrangements <- factorial(4) / (factorial(2) * factorial(1) * factorial(1))
# Probability of specific sequence (2 red, 1 blue, 1 green)
prob_sequence <- (red * (red-1) * blue * green) / (total * (total-1) * (total-2) * (total-3))
q1.g <- arrangements * prob_sequence
q1.g

# Question 1.h (Answer hidden, consider carefully)
# If we add a single yellow ball to the urn, what is the probability
#  of drawing (without replacement) 2 red balls, 1 blue ball, and 1 yellow
#  ball in any order?
# Save your answer in the variable q1.h
total_with_yellow <- red + blue + green + 1
# Number of ways to arrange 2 red, 1 blue, 1 yellow
arrangements_h <- factorial(4) / (factorial(2) * factorial(1) * factorial(1))
# Probability of specific sequence (2 red, 1 blue, 1 yellow)
prob_sequence_h <- (red * (red-1) * blue * 1) / (total_with_yellow * (total_with_yellow-1) * (total_with_yellow-2) * (total_with_yellow-3))
q1.h <- arrangements_h * prob_sequence_h
q1.h

# Question 1.i (Answer hidden, consider carefully)
# If we add a single yellow ball to the urn, what is the probability
#  of drawing (without replacement) 2 red balls, 1 blue ball, and 1 other
#  colored ball (ie not red or blue) in any order?
# Save your answer in the variable q1.i
total_with_yellow <- red + blue + green + 1
other_colored <- green + 1  # green balls + yellow ball
# Number of ways to arrange 2 red, 1 blue, 1 other
arrangements_i <- factorial(4) / (factorial(2) * factorial(1) * factorial(1))
# Probability of specific sequence (2 red, 1 blue, 1 other)
prob_sequence_i <- (red * (red-1) * blue * other_colored) / (total_with_yellow * (total_with_yellow-1) * (total_with_yellow-2) * (total_with_yellow-3))
q1.i <- arrangements_i * prob_sequence_i
q1.i


# Consider generating a random number continuously from the range [0,5]
#  following a triangular distribution peaking at x=3, as can be seen if
#  running the command "showplot()" defined at the top. 
# NOTE: You must NOT include a call to "showplot()" in your final submission
#  as gradescope autograder will crash when attempting to make plots.

# showplot()

# Question 2.a
# What is the height of the triangular distribution at its peak
#  such that it is a valid probability distribution?
# Save your answer in the variable "height"
# Area of triangle = (1/2) * base * height = (1/2) * 5 * height = 1
# Therefore: height = 2/5
height <- 2/5
height

# Question 2.b
# Create a function "area_to_the_left", which takes a single variable
#  (x1) and calculates the area to the left, returning a value between
#  0 and 1. The basic setup of the function is defined below. Note that
#  you will need to use a slightly different equation if x1 is less than or
#  greater than 3.

# Hint: for a triangle with height H and base B, the height of the subtriangle
#  with width Y% of B is Y% of H.

area_to_the_left<-function(x1){
  if(x1<0){
    # Area to the left of 0 (or less) is 0.
    return(0)
  } else if (x1<3) {
    
    return(x1^2/15)
  } else if (x1<5) {
    left_area <- 3/10
    right_area <- (1/2) * (x1-3) * ((2/5) + (5-x1)*(2/5)/2) 
    right_area <- (2/5) * (x1-3) - (x1-3)^2/10
    return(left_area + right_area)
  } else {
    # Area to the left of 5 (or more) is 1.
    return(1)
  }
}

# Question 2.c
# Create a function "area_to_the_right", which takes a single numeric variable
#  (x1) and calculates the area to the right, returning a value between
#  0 and 1. The skeleton of the function is defined below. You can attempt
#  to copy the setup used above, but students may find it easier to utilize
#  area_to_the_left() already defined.

area_to_the_right<-function(x1){
  return(1 - area_to_the_left(x1))
}

# Question 2.d
# Create a function "area_between", which takes two numeric variables 
#  (x1 and x2) and calculates the area between them, returning a value between 
#  0 and 1. You can assume that x1 < x2. The skeleton of the function is defined
#  below, and again students may find it easiest to utilize the previously 
#  defined functions area_to_the_left and/or area_to_the_right.
area_between<-function(x1, x2){
  return(area_to_the_left(x2) - area_to_the_left(x1))
}

# Question 2.e
# If we generate a random number utilizing this distribution, what is the 
#  probability that we get a number less than 1 or greater than 4?
# Save your answer in the variable q2.e
q2.e <- area_to_the_left(1) + area_to_the_right(4)
q2.e

# Question 2.f
# If we generate a random number utilizing this distribution, what is the 
#  probability that we get a number between 2.5 and 3.75?
# Save your answer in the variable q2.f
q2.f <- area_between(2.5, 3.75)
q2.f

# Question 2.g
# If we generate a random number utilizing this distribution, but we discard
#  any value greater than 4, what is the probability that we get a value greater
#  than 1.5? 
# Save your answer in the variable q2.g
q2.g <- area_between(1.5, 4) / area_to_the_left(4)
q2.g

# Question 2.h
# If we generate two random numbers utilizing this distribution, what is the 
#  probability that one of the numbers is greater than 2.5, and one is less
#  than 2.5?
# Save your answer in the variable q2.h
p_less_2_5 <- area_to_the_left(2.5)
p_greater_2_5 <- area_to_the_right(2.5)
# P(one > 2.5 and one < 2.5) = 2 * P(first > 2.5) * P(second < 2.5)
q2.h <- 2 * p_greater_2_5 * p_less_2_5
q2.h

# Question 2.i (Answer hidden, consider carefully)
# If we generate two random numbers utilizing this distribution, what is the 
#  probability that the first number generated is less than the second number?
# Save your answer in the variable q2.i
q2.i <- 0.5
q2.i

# Question 2.j (Answer hidden, consider carefully)
# If we generate a random number X and calculate the value 
#  Y=(X-3)*(X-3)=(X-3)^2, what is the probability that Y is greater than 1? 
# Save your answer in the variable q2.j
q2.j <- area_to_the_left(2) + area_to_the_right(4)
q2.j



# A researcher at a streaming service is assessing user preferences and finds
#  the following facts:
# 63% of users liked action movies.
# 78% of users liked comedy movies.
# 36% of users liked horror movies.
# 42% of users liked romance movies.
# 26% of users liked comedy and romance movies.
# 48% of users that enjoyed action movies also enjoyed horror movies.

# Question 3.a
# What proportion of users that enjoyed romance movies also enjoy
#  comedy movies?
# Save your answer in the variable q3.a
q3.a <- 0.26 / 0.42
q3.a

# Question 3.b
# What proportion of users enjoyed action and horror movies?
# Save your answer in the variable q3.b
q3.b <- 0.48 * 0.63
q3.b

# Question q3.c
# If a user enjoys horror movies, what is the probability 
#  that they enjoy action movies?
# Save your answer in the variable q3.c
q3.c <- q3.b / 0.36
q3.c

# Question q3.d
# If liking action and comedy movies was independent,
#  what would be the probability of a user liking both 
#  action and comedy movies?
# Save your answer in the variable q3.d
q3.d <- 0.63 * 0.78
q3.d

# Question q3.e
# Answer TRUE or FALSE
# If 40% of users like action and comedy movies,
#  users that likes action movies like comedy movies 
#  at a higher probability than users in general.
# Save your answer (TRUE or FALSE) in the variable q3.e
q3.e <- FALSE
q3.e

