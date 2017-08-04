##########
## 10.0 ##
##########
# Functional Programming

# One of the things I've always liked about R is that from a math background,
# it's very intuitive to think about functions and wow they operate on data.
# We're told that R has 'first class functions' meaning that they can be part of assignment
# statements, be input/output of other functions, and be created in less than deliberate ways
# The three buidling blocks of functional programming are anonymous functions, closures, and
# lists of functions. We're told that we should distinguish between 'functionals' which
# take a function as an input and return data (vectors), and 'function operators' which
# take functions as both input and output.

##########
## 10.1 ##
##########
# Motivation

# Here's a toy data set with -99 as the standin for NA. We want to replace
# the -99 with actual NAs
set.seed(1014)
df <- data.frame(replicate(6, sample(c(1:10, -99), 6, rep = TRUE)))
names(df) <- letters[1:6]

# The 'simplest' way to do this is a bunch of these statements:
df$a[df$a == -99] <- NA
# and lord knows I've written some massive blocks of code like that

# In order to apply the 'Don't repeat yourself' principle, we would want
# to write a more succinct description of what we'd like to accomplish. The author gives this
# slightly less naive example at how to accomplish this task
fix_missing <- function(x) {
  x[x == -99] <- NA
  x
}
df$a <- fix_missing(df$a)

# Unfortunately, this is still a bunch of repeated lines that have almost all the same
# problems of errors when copy pasting and a lot of text. It has the advantage of
# being easy to modify the value to be replaced with NA and also we can't copy that
# value incorrectly.

# As I've come to love, the solution is lapply. lapply is a functional since it
# takes in a function as an argument.
fix_missing <- function(x) {
  x[x == -99] <- NA
  x
}
df[] <- lapply(df, fix_missing)
# This is the intended solution. One thing here that I like and didn't start
# doing until recently was df[]
# Which is different than just saying df <- lapply(...) since it preserves
# the structure that df already had. It's saying, just assign the list from lapply
# to the contents of df
str(df)
df <- lapply(df, fix_missing)
str(df)
# in that second case we end up with a list but not a data.frame

# Mr Wickham says that this method is better because it reduces the capicity for errors,
# treats the columns of df in an identical way, and can be modified quickly. It's sort of
# like we have two widgets: one to fix the missing values, and one to spread the function
# to all the data we're interested in. They combine to do the job.

# next we're confronted with the situation where we might want to vary what is
# counted as missing and replaced with NA? We might write a bunch of functions:
fix_missing_99 <- function(x) {
  x[x == -99] <- NA
  x
}
# but I'm guessing that this is where we get to 'function operators' so that we can
# feed one function the value to be replaced, it will hand us a new function that does that,
# and then we'll send that function to lapply

missing_fixer <- function(na_value) {
  function(x) {
    x[x == na_value] <- NA
    x
  }
}

# So this function returns another functio with the parameter we chose
# so maybe we can do this?
set.seed(1014)
df <- data.frame(replicate(6, sample(c(1:10, -99), 6, rep = TRUE)))
names(df) <- letters[1:6]
df
df[] <- lapply(X = df, FUN = missing_fixer(-99))

# Here's a related problem, what happens when we want to apply a bunch of
# functions to some data. The example given sis wanting to compute
# mean, median, sd, mad (mean absolute deviation), and IQR of every column of a data frame

# I've done things like this many times but we're told it's not the best solution
summary <- function(x) {
  c(mean(x), median(x), sd(x), mad(x), IQR(x))
}
lapply(df, summary)

# From my perspective there are two things about this that strike me as a little off
# First is that the result is an unnamed list which is not easy to vizualize and
# not clear. The second is that it doesn't have the flexibility of changing
# which summary functions it uses. O shit here's a sexy beast:

summary <- function(x) {
  funs <- c(mean, median, sd, mad, IQR)
  lapply(funs, function(f) f(x, na.rm = TRUE))
}

# That's pretty awesome. So what this does is it loops through the list of functions
# and applys that function with the na.rm parameter

lapply(X = df, FUN = summary)
# each column of df will then call another lapply which runs through the funs and applies each of them

##########
## 10.2 ##
##########
# Anonymous Functions

# Apparently most programming languages requrie functions to be named but in R,
# we can use unnamed (anonymous) functions. I've been doing this without knowing it
# for a while. It looks like one case is where the function is so simple it's not
# worth naming
lapply(mtcars, function(x) length(unique(x)))
# we could go
n_unique = function(x){
  length(unqiue(x))
}
lapply(mtcars, n_unique)
# But n_unique is so simple why bother?

# Anonymous functions have the same properties as a normal function (besides name)
# so you can still call body, formals, and environment on them if you like.

# This next part is pretty whack. We can call or reference anonymous functions just
# with parentheses
(function(x) 3)()
# this defines an anonymous function and then calls it with an empty argument
body(function(x) 10*sin(x))
# Hmmm I'm not qutie sure why/when I would want this but okay!
# We're warned at the end of this section that if we're calling anonymous functions
# that are complicated enough to need named arguments, maybe the function should be given a name

# EXERCISES

# 1. Given a function, like "mean", match.fun() lets you find a function.
# Given a function, can you find its name? Why doesn’t that make sense in R?
match.fun("mean")
# Well I guess it might not make sense if it was an anonymous function...
# Is there something more to this question? I mean one thing that comes to mind
# is that you would also need to know the environment it's in to even look for
# the name. I am not thinking of any situations where you could somehow be given
# a named function but not know the name..

# 2. Use lapply() and an anonymous function to find the coefficient of variation
# (the standard deviation divided by the mean) for all columns in the mtcars dataset.
lapply(X = mtcars, FUN = function(x) {sd(x, na.rm = TRUE)/mean(x, na.rm = TRUE)})

# 3. Use integrate() and an anonymous function to find the area under the curve
# for the following functions. Use Wolfram Alpha to check your answers.
#  a) y = x ^ 2 - x, x in [0, 10]
integrate(f = function(x){x^2-x}, lower = 0, upper = 10)
#  b) y = sin(x) + cos(x), x in [-π, π]
integrate(f = function(x){sin(x)+cos(x)}, lower = -pi, upper = pi)
#  c) y = exp(x) / x, x in [10, 20]
integrate(f = function(x){exp(x)/x}, lower = 10, upper = 20)

# 4. A good rule of thumb is that an anonymous function should fit on one line and shouldn’t need to use {}.
# Review your code. Where could you have used an anonymous function instead of a named function?
# Where should you have used a named function instead of an anonymous function?
# I'm guessing that this question is asking me to review my code in a general sense, not what's written here
# I rarely use anonymous functions and when I do there almost always super short. I do like to use
# braces even when they're short though, somehow that makes me feel more clean. My style is often
# very verbose and I'm much more likely to name small functions and write a lot of comments than
# violate the rule of thumb given. So far, I don't see any real need for anonymous functions other
# than to save a few keystrokes and make the code clean. I think you're rarely, if ever, going to
# prevent errors by using anonymous functions instead of named ones but it's nice to know. I think one of the
# bigger uses of this is when I'm doing more exploratory work and tying things into the console to see
# what happens. That situation is where function(x){length(unique(x))} kind of thing is nicest IMO

##########
## 10.3 ##
##########
# Closures

# Right off the bat, we're told that anon functions are also used in closures which are functions
# written by other functions. THey're called closures because they enclose the environment of the
# parent function. Here's an example:
power <- function(exponent) {
  function(x) {
    x ^ exponent
  }
}
square <- power(2)
square(3)
# so there is an anonymous function as the output from the power function. This makes sense as we're
# defining the function within power's enclosing environment (is that the right one?) so assigning
# it a name there doesn't make sense if we want to add 'square' to the global environment. Interestingly
# The function defined as square doesn't contain exponent == 2 in its body. This is actually
# saved within the execution environment
as.list(environment(square))
body(square)
# So when we call square, it doesn't find exponent, looks up the search path into its own environment
# and finds it. The pryr function undenclose() shows this too
library("pryr")
unenclose(square)

power <- function(exponent) {
  print(environment())
  function(x) x ^ exponent
}
zero <- power(0)
environment(zero)
# so the environment that zero lives in is the execution environment of power which would normally
# be closed but the output of power retains the information about where it was created.

# The name 'function factory' is given to a function that can produce multiple other functions as output.
# The examples given so far were a bit contrived as power and missing_fixer could easily just be
# two argument functions that return vectors instead of one argument functions that return a function
# sometimes, however, we might want to have the variation in output be diverse enough
# that returning the custom function is preferrred.

new_counter <- function() {
  i <- 0
  function() {
    i <<- i + 1
    i
  }
}
x = new_counter()
unenclose(x)
x()
x()
y = new_counter()
unenclose(y)
y()
# Both x and y are examples of closures : functions created by other functions. The enclosing environments
# of x and y are both created when new_counter is run however they are not the same environments
# which is why their counters are kept seperate. This is further demonstrated by thinking about the
# search path. i <<- i + 1 would ordinarily modify the global environment or whatever environment
# the function is in but with a closure function, its most immediate environment is the execution environment
# of its parent.
environment(y)
environment(x)

# We're asked to make predictions about these functions instead of new_counter

i <- 0
new_counter2 <- function() {
  i <<- i + 1
  i
}

# So this does not return closures. I don't believe that this one will keep independence
# between two calls. I think that if I assign two variables new_counter2()
# they will stack i

x <- new_counter2()
x
y <- new_counter2()
y
# YEP!

new_counter3 <- function() {
  i <- 0
  function() {
    i <- i + 1
    i
  }
}

# new_counter3 does return a closure but the difference is that it is not using <<-
# Hmmm.... so now when you call a function created by new_counter3, it will look for i
# find it and then assign i in the execution environment (not enclosing) and return
# So I think here it's just always going to return 1?

rm(i)
x <- new_counter3()
x()
x()
y <- new_counter3()
y()
y()

# maybe that's just luck but I'm a little proud of myself

# EXERCISES

# 1. Why are functions created by other functions called closures?
# They're called this because they enclose the parent environment they're created in

# 2. What does the following statistical function do? What would be a better name for it? (The existing name is a bit of a hint.)
bc <- function(lambda) {
  if (lambda == 0) {
    function(x) log(x)
  } else {
    function(x) (x ^ lambda - 1) / lambda
  }
}
# So this function returns the function log(x) if lamda == 0
# a polynomial function in the case that lambda !=0 so the output form bc is a closure
# Apparently this is called the box-cox transformation which I had never heard of.
# but it appears to be a type of data transformation that is useful when trying to
# make data look more normal, probably for modeling purposes. The wikipedia
# page says that the BC transformation is a specific case of power transforms which
# involve essentially the same thing but we scale the output by the geometric mean
# of the data when lambda ==0 and 1 over the geometric mean to the (lambda -1) power
# when lambda !=0.

# Lets see what this looks like
q = rpois(n = 25, lambda = 2)
bc(1)(q)+

# 3.What does approxfun() do? What does it return?
?approxfun
# It looks like this does some interpolation which is like trying to connect the dots
# and get other 'data' points that are in line with what we already have
# Probably the author asks this question becuase it returns closures. Let's make some
# fake data
q = 1:10
p = rnorm(n = 10, mean = 0, sd = 1) + q
plot(q,p)
f = approxfun(x = q, y = p)
str(f)
unenclose(f)
f
curve(f, from = 1, to = 10)
# so f is probably some stepwise function that maps values between 1-10 to y values
# near p

# 3. What does ecdf() do? What does it return?
?ecdf
# This computes an 'empircal cummulative density function' which is a stepwise
# function that shows the proportion of observations less than or equal to the input
# Again, this returns closures. Let's see how well it works with a normal.
q = rnorm(25)
str(ecdf(q))
plot(ecdf(q))
s = seq(from = -2, to= 2)
cdf = pnorm(s)
lines(s,cdf)
# looks good! So again this returns a function that depends on the input to ecdf

# 4. Create a function that creates functions that compute the ith central moment of
# a numeric vector. You can test it by running the following code:
m1 <- moment(1L)
m2 <- moment(2L)

x <- runif(100)
stopifnot(all.equal(m1(x), 0))
stopifnot(all.equal(m2(x), var(x) * 99 / 100))

# Sweet. So the moment functions are expected values of powers of values centered at
# their mean. It's E((x - xbar)^n)
# For data, we can replace expected value with mean since all obs would be assumed
# to have equal likelihood

moment <- function(n){
  if(!is.integer(n) | n<=0 | length(n)>1){
    stop("n must be a natural number")
  }
  function(x){
    mean((x - mean(x))^n)
  }
}

# hot

# 5. Create a function pick() that takes an index, i, as an argument and returns a
# function with an argument x that subsets x with i.
# Is it this simple?
pick <- function(n){
  function(x){
    x[[n]]
  }
}
lapply(mtcars, pick(5))
lapply(mtcars, function(x) x[[5]])
# guess so

##########
## 10.4 ##
##########
# Lists of functions

# The more R programming I do the more lists appear in my code. They were somewhat nebulous
# to start but it seems like the most versitile data structure and works well with our favorite
# function, lapply. The author states that if we want to allow for a lot of different behaviors
# storing functions in a list is a nice way to organize our work. The book gives this example
# of computing a mean in three ways and comparing the run times
compute_mean <- list(
  base = function(x) mean(x),
  sum = function(x) sum(x) / length(x),
  manual = function(x) {
    total <- 0
    n <- length(x)
    for (i in seq_along(x)) {
      total <- total + x[i] / n
    }
    total
  }
)
x <- runif(1e5)
system.time(compute_mean$base(x))
system.time(compute_mean[[2]](x))
system.time(compute_mean[["manual"]](x))
# Hmm maybe my settings don't offer the precision that the book shows....
options(digits =5)
system.time(compute_mean$base(x))
system.time(compute_mean[[2]](x))
system.time(compute_mean[["manual"]](x))
# unsurprisingly the for loop sucks but point taken we can use
# a list sort of like a switch function to choose the appropriate behavior

# What's really sexy is this next line:
lapply(compute_mean, function(f) f(x))
# What this does is run through the elements of compute mean (a list)
# and each time it executes the anonymous function with argument f (an element from compute_mean which is a functino)
# passes that function to the body of the anonymous function, and executes with x as the arugment
# Basically we're testing all the functions and finding that they do compute the same mean
# Or test all the times:
lapply(compute_mean, function(f) system.time(f(x)))

# We can also supply arugments to the anonymous function assuming that all the
# functinos in the list can accept that argument. The example given was
# wanting to compute mean, sum, and median with na.rm == TRUE.
# instead of including the na.rm in the arugments of each function in the list,
# we can instead write something like lapply(funs, function(f) f(x, na.rm = TRUE))

# The author suggests  that sometimes we may want to move function lists to the global
# environment. Here's a function factory and function list that deal with
# HTML tags which are surrounded by <>
simple_tag <- function(tag) {
  force(tag)
  function(...) {
    paste0("<", tag, ">", paste0(...), "</", tag, ">")
  }
}
tags <- c("p", "b", "i")
html <- lapply(setNames(tags, tags), simple_tag)

# So the author notes that by keeping the functions in a list, they don't conflict with
# global environment but it also makes for a lot of keystrokes to use it a lot
html$p("This is ", html$b("bold"), " text.")
# whereas if these functions were in the global it would be 
p("This is ",b("bold"), " text.")

# Three suggestions are given to make this easier to deal with. The first is with()
# which creates another environment with the names of the data supplied
with(html, p("This is ", b("bold"), " text."))
# Secondly is attaching the function list
attach(html)
# but I hate attaching things
# Lastly, we can use the function list2env
list2env(html, globalenv())
# which seems easy enough
# The author recommends using with which I agree with. I need to get more comfy using
# with and within as they seem like a good way to make code shorter and more readable

# EXERCISES

# 1. Implement a summary function that works like base::summary(), but uses a list of 
# functions. Modify the function so it returns a closure, making it possible to use it as a function factory.

