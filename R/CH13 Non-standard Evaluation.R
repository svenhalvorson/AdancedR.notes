##########
## 13.0 ##
##########
# Non-standard evaluation

# To start, we're given this example:
x <- seq(0, 2 * pi, length = 100)
sinx <- sin(x)
plot(x, sinx, type = "l")

# which demonstrates something interesting about R. Something about the plot function
# knows the names of the arguments, not just what they literally are. This is called
# non-standard evaluation; R can access the code executed, in addition to the values supplied.
# This is the method used by ggplot and other tidyverse functions in order to allow us to use
# unquoted variable names.

##########
## 13.1 ##
##########
# Capturing expressions

# The substitute function captures what the code looks like for an expression but
# doesn't evaluate it.
substitute(1:10)
str(substitute(1:10))
# has an interesting class
class(substitute(1:10))
x <- 10
substitute(x)

# We're told that this works because the arguments to a function are called promises.
# These encompass what the argument is as well as what enviroment it's in. The return value
# of substitute looks like a character vector but it's not actually. If we want a character equivalent
# we can deparse() it
deparse(substitute(1:10))
str(deparse(substitute(1:10)))

# EXERCISES
# 1. One important feature of deparse() to be aware of when programming is that it can return multiple
# strings if the input is too long. For example, the following call produces a vector of length two:
g <- function(x) deparse(substitute(x))
g(a + b + c + d + e + f + g + h + i + j + k + l + m +
    n + o + p + q + r + s + t + u + v + w + x + y + z)
#Why does this happen? Carefully read the documentation for ?deparse.
# Can you write a wrapper around deparse() so that it always returns a single string?

# So it looks like deparse has a line cutoff length that you can set. How about this:
deparse1 = function(x){
  x = deparse(expr = substitute(x))
  paste0(x, collapse = "")}
deparse1(a + b + c + d + e + f + g + h + i + j + k + l + m +
    n + o + p + q + r + s + t + u + v + w + x + y + z)
# hm seems like there are some weird spacing characters in here...
# Even if you write is as one line:
deparse1(a + b + c + d + e + f + g + h + i + j + k + l + m + n + o + p + q + r + s + t + u + v + w + x + y + z)
# Seems like there is some feature of deparse that adds this seperator when there is a new line....
# well good to know

# 2. Why does as.Date.default() use substitute() and deparse()? Why does pairwise.t.test() use them? Read the source code.

# as.Date.default uses deparse(substitute()) to deliver a warning message:
fakedate = 1:10
as.Date.default(fakedate)

# pairwise.t.test() uses NSE to capture the names of the variables you're testing
pairwise.t.test(airquality$Ozone, airquality$Month)

# 3. pairwise.t.test() assumes that deparse() always returns a length one character vector.
# Can you construct an input that violates this expectation? What happens?

# uhh... maybe if we make the input have a really long name
airquality$longass_name_yoooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo = airquality$Ozone
pairwise.t.test(airquality$longass_name_yoooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo, airquality$Month)
# nope... it looks like the paste just handles it nicely
pairwise.t.test(2, 1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1+1)
# so it looks like we can trick it this way although I'm not exactly sure what's different about this example

# 4. f(), defined above, just calls substitute(). Why can't we use it to define g()?
# In other words, what will the following code return? First make a prediction. Then run the code and think about the results.
f <- function(x) substitute(x)
g <- function(x) deparse(f(x))
# I think this is going to cause problems since g is going to try to deparse the expression f(x), not the return value of f(x)

g(1:10)
# returns 'x'
g(x)
# returns 'x'
g(x + y ^ 2 / z + exp(a * sin(b)))
# returns 'x'
# So this is a problem due to environments. By default, substitute is evaluated
# in the evaluation environment so within g's environment.
f <- function(x) {browser();substitute(x)}
g <- function(x) {browser();deparse(f(x))}
g(1:10)
# Yeah so ... it seems like within f's environment that argument is just called
# x so substitute(x) yields x. In g's environment, the argument is whatever
# promise you feed it

##########
## 13.2 ##
##########
# Non-Standard Evaluation in Subset

# I never really used this function as I came across dplyr::filter() first but subset
# performs a similar role
sample_df <- data.frame(a = 1:5, b = 5:1, c = c(5, 3, 1, 4, 1))
subset(sample_df, a >= 4)
# subset() is using NSE to determine that we meant with(sample_df, sample_df[a>=4,])
# How does this happen? Basically we want a way to evaluate the supplied code within
# an environment 'within' the supplied object. This feels like silly ole' attach()

# I see something! The author writes:
# 'we want x to be interpreted as sample_df$x, not globalenv()$x'
# This makes me want to rephrase how I think of $. I usually said 'access that component of'
# when thinking about the $. A better phrase might be 'within the environment of sample_df'

# The next function we're given is quote() which is simpler than substitute
str(quote(1:10))
quote(1:10)
substitute(1:10)
# Hmmm... struggling to get the difference. They both have class() == "call"

# We're told that eval and quote are inverses of sorts. Note the difference between:
eval(quote(quote(2 + 2)))
eval(eval(quote(quote(2 + 2))))

# Like substitute, we can choose an environment to evaluate quote within:
e <- new.env()
e$x <- 20
eval(quote(x), e)
# The second argument can also be a list or data frame as 'they bind names to values in a similar way'
eval(quote(x), list(x = 30))
eval(quote(x), data.frame(x = 40))
# So part of subset is :
eval(quote(a >= 4), sample_df)
# but now we have to reduce the data frame according to that rule

# The author doesn't give any explanation for this but it seems like if you give eval something
# in the global environment, and tell it to evaluate it in a data frame, it will ignore objects with
# that name inside the df:
a <- 1
eval(a, sample_df)
eval(quote(a), sample_df)
# Kinda curious, I think this is because we're supplying the object, not a call. A call would be evaluated
# as a call in that environment, in which case the scoping rules would look where it is.

# so here's a simplified version of substitute:
subset2 <- function(x, condition) {
  condition_call <- substitute(condition)
  r <- eval(condition_call, x)
  x[r, ]
}
subset2(sample_df, a >= 4)
# What's it doing? First we take the condition supplied, and make a call of it with substitute()
# Next we evaluate that call within the environment of the supplied x
# lastly we take the rows of x that are TRUE

# So would that work with quote?
subset3 <- function(x, condition) {
  browser()
  condition_call <- quote(condition)
  r <- eval(condition_call, x)
  x[r, ]
}
subset3(sample_df, a >= 4)
# interesting. So there is a difference between how quote and substitute work here
# When going through the browser here I see that quote(condition) has a value of 'condition' while
# substitute(condition) has the value 'a>=4'. So it seems like quote takes the name of the object
# within that environment, and substitute takes it's value as an expression.
# they also have different classes within that execution. substitute() provides a call while
# quote provides a name

# EXERCISES
# 1. Predict the results of the following lines of code:
eval(quote(eval(quote(eval(quote(2 + 2))))))
# should be 4 since there are an equal number of quotes and evals
eval(eval(quote(eval(quote(eval(quote(2 + 2)))))))
# also should be 4 as this simply has another eval, which will be told to eval an eval call which will eval
# lol
quote(eval(quote(eval(quote(eval(quote(2 + 2)))))))
# This will just return the quoted : eval(quote(eval(quote(eval(quote(2 + 2))))))
# This last example kinda shows how they aren't inverse functions in the way that we think of in
# math. eval(quote(x)) != quote(eval(x))

# 2. subset2() has a bug if you use it with a single column data frame.
# What should the following code return? How can you modify subset2() so it returns the correct type of object?
sample_df2 <- data.frame(x = 1:10)
subset2(sample_df2, x > 8)
# so the problem is that
class(subset2(sample_df2, x > 8))
# should be a data.frame


subset4 <- function(x, condition) {
  condition_call <- substitute(condition)
  r <- eval(condition_call, x)
  x[r,,drop = FALSE]

}
# kind of a question from the first chapter ._.

# 3. The real subset function (subset.data.frame()) removes missing values in the condition.
# Modify subset2() to do the same: drop the offending rows.

subset5 <- function(x, condition) {
  condition_call <- substitute(condition)
  r <- eval(condition_call, x)
  x[r & !is.na(r),,drop = FALSE]
}
sample_df3 = data.frame(x = c(1, NA, 0))
subset5(sample_df3, x<1)

# 4. What happens if you use quote() instead of substitute() inside of subset2()?

# already answered this :)

# 5. The third argument in subset() allows you to select variables. It treats variable names
# as if they were positions. This allows you to do things like subset(mtcars, , -cyl)
# to drop the cylinder variable, or subset(mtcars, , disp:drat) to select all the variables between
# disp and drat. How does this work? I've made this easier to understand by extracting it out into its own function.
select <- function(df, vars) {
  vars <- substitute(vars)
  var_pos <- setNames(as.list(seq_along(df)), names(df))
  pos <- eval(vars, var_pos)
  df[, pos, drop = FALSE]
}
select(mtcars, -cyl)

# okay so it takes the data frame, makes a list, and gives the names of the list as the colnames of df:
var_pos <- setNames(as.list(seq_along(sample_df)), names(sample_df))

# Next we evaluate vars in the environment of the list we created
pos <- eval(vars, var_pos)
# lastly we take the columns that are hits within that list
df[, pos, drop = FALSE]

# 6. What does evalq() do? Use it to reduce the amount of typing for the examples above that use both eval() and quote().
# It's basically eval(quote(x))
evalq(a,sample_df)

##########
## 13.3 ##
##########
# Scoping Issues

# So here's an interesting example using subset2
subset2 <- function(x, condition) {
  condition_call <- substitute(condition)
  r <- eval(condition_call, x)
  x[r, ]
}
y <- 4
x <- 4
condition <- 4
condition_call <- 4

# We would hope that all of these return the same thing:
subset2(sample_df, a == 4)
subset2(sample_df, a == y)
subset2(sample_df, a == x)
subset2(sample_df, a == condition)
subset2(sample_df, a == condition_call)

# But they don't. The reason for this is when eval tries to run these expressions
# it first looks within x but if it doesn't find it, it will go to the environment
# of subset2 and then up the chain. We would really like it to try in x, and then
# not go up the scoping chain.

# eval() has a third argument, enclos, which specifies where to begin scoping if
# something is not found when trying to evaluate the expression. Note that this is only used when
# x is a list or data frame as it's ignored if env is an environment
# It's suggested to us that we use parent.frame() as enclos so that it looks to the parent frame
# of subset2. Here's the modification:
subset2 <- function(x, condition) {
  condition_call <- substitute(condition)
  r <- eval(condition_call, x, parent.frame())
  x[r, ]
}

x <- 4
subset2(sample_df, a == x)
# So I'm a little confused by this as I would have thought that by choosing
# parent.frame() we would be getting the parent of eval() being called not subset2
subset2 <- function(x, condition) {
  browser()
  condition_call <- substitute(condition)
  r <- eval(expr = condition_call,envir =  x, enclos = parent.frame())
  x[r, ]
}
subset2(sample_df, a == x)

# Hmmm the documentation for parent.frame says:
# Beware of the effect of lazy evaluation: these two functions look at the call stack
# at the time they are evaluated, not at the time they are called. Passing calls to
# them as function arguments is unlikely to be a good idea.

# Which is what I thought would happen. I thought parent.frame() wouldn't be evaluated
# until eval is called and then we would be within eval's execution environment
# making the parent frame subset2
