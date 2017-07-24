#########
## 9.0 ##
#########
# Debugging, condition handling, and defensive programming

# This chapter is all about how to deal with problems in R code
# We'll look at how to use some of the tools for interrupting
# functions as well as read the output.

# There are 3 kinds of output you can specify regarding unexpected
# input/output. stop() terminates the execution and is for situations
# where the function will not give meaningful/correct output. warning()
# is more for when output might confuse the user but isn't necessarily wrong
# I used warnings sometimes when a failed for some portion of the input
# message() can be used to give other information. This is nice for status
# updates or loading messages

# We also can use conditionals like 'try' to work with situations where
# we expect errors and want to handle them appropriatesly. This chapter will also
# go into 'defensive programming' which refers to the process of trying
# to prevent errors instead of handling them

#########
## 9.1 ##
#########
# Debugging Techniques

# We're given a 4 step process for trying to identify a bug:

# 1. Realize you have a bug. Sometimes it's hard to even know
# that your code will not perform with certain input. There is a
# package to help test functions and I loaded it up a while back
# but found it difficult to use.

# 2. Make it repeatable. In order to know when you've fixed an error,
# you'll need to be able to produce the error on command and then show
# that it doesn't happen anymore. The author suggests trying to make
# a minimal reproduceable example a-la-stackoverflow. This could be done by
# bifurcating your code a bunch of times until you find the smallest snippet
# that produces the error. It's also nice to have example input
# that doesn't produce errors but is similar to the error producing. The
# automated testers create test cases in which many combinations of
# input are submitted and a log is created of the results

# 3. Figure out where it is. We're urged to try and use the scientific method
# to locate an error: generate hypotheses, test them, record the results. Hadley
# also gives the same suggestion as Bowman; don't look to patch the walls, open them
# up and kill the cockroach nest.

# 4. Fix and test it. Aftr we think we've fixed a bug, make sure to test both
# the buggy input as well as stable input to make sure your solution hasn't introduced
# more bugs.

#########
## 9.2 ##
#########
# Debugging Tools

# Here are three key debugging tools:
# 1. traceback() shows a sequence of calls that lead to an error
# 2. 're-run with debug' and options(error = browser) are ways to get
#    an interctive session to look for causes of the error
# 3. breakpoints and browser() can give arbitrary points to look at

# Determining the sequence of calls
# The call stack is the sequence of calls that lead to an error.
f <- function(a) g(a)
g <- function(b) h(b)
h <- function(c) i(c)
i <- function(d) "a" + d
f(10)
traceback()
# traceback() shows us a list of the function calls and what level (environment)
# they're at. This is helpful for identifying which portion exactly of your call failed.
# I've used this in the past to determine if it's my function that is failing or the apply
# statement I was using in conjunction

# Browsing on error
# The first method to get into interactive debug is just to run code with an error
# and then select 'rerun with debug'. This will open the browser at the point
# in which the code broke. I'm fairly comfortable with the browser
# but it is nice to know the shortcut commands and a little clarification.
# Step in (s) goes to the next line and if it's a function, we'll go into the browser
# within that function's execution environment. Finish (f) completes the current
# loop or function call.
# The function recover() allows you to enter the environment of the calls in browser
# So if we know what level of the stack we want to go into options(error = recover)
# can be nice.

# Browsing arbitrary code
# Breakpoints work almost identically to browser minus these exceptions: breakpoints cannot be
# assigned conditionally and there are a few instances where breakpoints will not stop.
# Similar to browser, we can use debug() on a function to essentially insert a browswer
# statement at the start of the chosen function whenever it is called. You have to undebug()
# it to remove this behavior or you can just use debugonce() to have it only run
# in debug mode the next time it is called. utils::setBreakpoint() can do essentially the
# same thing to a file (not function)
# So we have traceback, where, and recover to help us look at the call stack but it's also
# important to note that recover lists the levels in the opposite order.

# Other types of failure
# Turns out we can change all warnings to stops with options(warn = 2) however I don't want
# to do this as I'm used to it not interrupting me.kkkkk
# There are three basic condition handlers in R: try, tryCatch, and withCallingHandlers
# Try basically allows you to execute a function, have it print an error message if applicable
# and then continue
f1 <- function(x) {
  try(log(x))
  10
}
f1("x")
# There is also a specific class for objects created by try statements that return errors
failure <- try("a" + "b")
str(failure)
# looks like we have a call and message so if we know what kind of message
# a particular function spits out, we can do different things
if(class(failure)=="try-error" & attr(failure,"condition")$message == "non-numeric argument to binary operator"){
  print("yep, it's that kind of error")
}
# Often try is nice when we're going to use an apply statement as it may fail for some cases
elements <- list(1:10, c(-1, 10), c(TRUE, FALSE), letters)
results <- sapply(elements, function(x) try(log(x)))
# Which is nice
# This seems like something I would like to use when doing those mass file imports that plague me
# The author suggests using this function:
is.error <- function(x) inherits(x, "try-error")
# and apply it to a list after running the function with a try in it
# this will detect whether or not each element is a try-error

# In addition to errors, messages, and warnings, there are also interrupts
# which are generated when the user terminates R's execution. tryCatch can
# handle these as well as the other types of messages. tryCatch works like try
# but we can assign the output based on whether it's an error, message, or warning
?tryCatch
# I'm having a hard time understanding from this text alone but here are some
# notes from googling. tryCatch first evaluates the statement in expr line
# by line. If it encounters an error/warning/messsage/interrrupt it's sent
# to a switch-like statement that performs the code matched to each exception
# type. You can write your own functions within the try and have them
# take in the try-catch exception and do something with it. This
# is how you would differentiate behavior based on the details of the
# exception.
tryCatch(expr = log(-1),
         warning = function(warn){if(warn$message == "NaNs produced"){message(paste0(warn,warn,warn,warn))}},
         error = function(er){message(paste0("THIS BE THE ERROR: ",er$message))})
tryCatch(expr = log("-1"),
         warning = function(warn){if(warn$message == "NaNs produced"){message(paste0(warn,warn,warn,warn))}},
         error = function(er){message(paste0("THIS BE THE ERROR: ",er$message))})
# hot
# we can also use conditionMessage() instead of $message
tryCatch(expr = log("-1"),
         warning = function(warn){if(warn$message == "NaNs produced"){message(paste0(warn,warn,warn,warn))}},
         error = function(er){message(paste0("THIS BE THE ERROR: ",conditionMessage(er)))})
# Here's a neat example of using the interrupt messages:
i <- 1
while(i < 3) {
  tryCatch({
    Sys.sleep(0.5)
    message("Try to escape")
  }, interrupt = function(x) {
    message("Try again!")
    i <<- i + 1
  })
}
# So here it reacts to the user trying to terminate R
# The last feature to note here is the 'finally' argument which
# executes regardless of whether we get an exception or not

# The third technqiue introduced is withCallingHandlers()
# Here is the explanatory example given:
f <- function() g()
g <- function() h()
h <- function() stop("!")
tryCatch(f(), error = function(e) print(sys.calls()))
withCallingHandlers(f(), error = function(e) print(sys.calls()))
# We're told that the difference we're seeing here is due to the context
# in which the two functions handle exceptions. tryCatch handles them as
# exceptions to the tryCatch statement while withCallingHandlers tracks
# them in relation to the pieces of the given expression.

# A less nebulous difference is that withCallingHandlers will still exectute
# code that occurs within an exception or after one while tryCatch stops
# immediateliy upon getting an exception. Demonstrated:
message_handler <- function(c) cat("Caught a message!\n")

tryCatch(message = message_handler, {
  message("Someone there?")
  message("Why, yes!")
})
withCallingHandlers(expr = {message("Someone there?");message("Why, yes!")},
                    message = message_handler)
tryCatch(expr = {message("Someone there?");message("Why, yes!")},
                    message = message_handler)
# tryCatch sees the message, doesn't read it to you, and then immediately
# runs message_handler. withCallingHandlers sees a message, prints it, runs
# message_handler, and then continues with the next line of expr.
# The author suggests that for this reason withCallingHandlers is preferred
# when we have messages. I agree as there are a lot of functions that
# randomly spit out messages and I'd rather have the code just continue
# to execute rather than have execution stop because a function told me it's
# working fine.

# Another important difference is that tryCatch returns the value of the handler
# while withCallingHandlers returns NULL. The author suggests that
# withCallingHandlers is rarely necessary unless you want to be
# very specific about what to do with particular exceptions

# EXERCISE
# 1. Compare the following two implementations of message2error().
# What is the main advantage of withCallingHandlers() in this scenario?
# (Hint: look carefully at the traceback.)

message2error <- function(code) {
  withCallingHandlers(code, message = function(e) stop(e))
}
message2error1 <- function(code) {
  tryCatch(code, message = function(e) stop(e))
}

message2error({f =1;message("HEY BBY")})
message2error1({f =1;message("HEY BBY")})

# Hmm not only am I not able to come up with a solution but it's proving
# difficult to find one online. One thing I would say is that the traceback
# for withCallingHandlers is certainly easier to read and clearer
# where the message came from

#########
## 9.3 ##
#########
# Defensive Programming

# This section just has some advice on how to write functions that 'fail fast'
# meaning that they send out an error as soon as something is wrong, rather
# than when it eventually breaks on its own. We're told that there are
# three principles:
# 1. Be strict about accepting arguments. I take this to mean
# that you should do a lot of checks on the data types of the function's
# arguments. Trying to allow a variety of inputs is a lot harder
# than just forcing and knowing what the arguments will be.

# 2. Avoid nonstandard evaluation. It's a little funny that this is
# advice given because the author also wrote a lot of the tidyverse
# which uses NSE heavily. For my purposes, NSE in functions
# doesn't seem that appealing anyways. I don't write things for
# an end user often so having it be easier to interact with is not high
# on my list

# 3. Avoid functions that return different types our output. This makes
# sense to me because of experience. I've written some functions that
# violate this and it can be kinda confusing. The two examples listed
# are sapply and `[]`. We're told that vapply will throw an error if
# the inputs are the incorrect types. I'm not that familiar with
# vapply but it looks like you can specify the type and legth

# The author closes with mentioning how there is tension between functions
# written for programming and those for data exploration. I think that
# this makes a lot of sense as there isn't a ton of point in safegaurding
# against everything if you're just writing a function to look at a particular
# problem. If you're planning on using that function all the time and
# using it in conjunction with other code, it's nice to make it more
# strict.

# EXERCISES
# 1. The goal of the col_means() function defined below is to compute the
# means of all numeric columns in a data frame.
col_means <- function(df) {
  browser()
  numeric <- sapply(df, is.numeric)
  numeric_cols <- df[, numeric]
  data.frame(lapply(numeric_cols, mean))
}
# However, the function is not robust to unusual inputs. Look at the following results,
# decide which ones are incorrect, and modify col_means() to be more robust.
# (Hint: there are two function calls in col_means() that are particularly prone to problems.)

# Alright, I'll try to make a prediction for each of these inputs and then i'll re-write the
# function after considering all of them

col_means(mtcars)
# I don't believe that this willl work because I don't believe sapply will be able to turn `numeric`
# into an object that can be interpreted by `[`
# Well that's wrong. I was thinking of as.numeric. is.numeric asks if 'the mode' of the object is numeric
# this does work. numeric will be a logical vector and thus can be put into `[`

col_means(mtcars[, 0])
# Okay now we're putting in a data frame with no columns. What does sapply do with it?
# Well it's supposed to run along the items of a list but there are non so it returns an
# empty list. The subscript function then tries to put an empty list into it's column names/indices
# but can't resolve this. I do find it a little funnyh that we're allowd to enput 0 to `[` but not
# an empty list

col_means(mtcars[0, ])
# This time we're sending a data frame with zero rows. I think this time it's going to remember the
# data types of the columns (even though there are no obs) but then fail when we try to take a mean
# Nope, I guess it just returns NaN but at least I was right about it being able to is.numeric and
# subscript! This looks like the desired output for this input

col_means(mtcars[, "mpg", drop = F])
# Now we're feeding it a single column data frame. I believe this will work fine as sapply should return
# a single TRUE and then the subset function will keep the one column we feed col_means
# NOPE. Let's see.. so we feed `[` that single true we get back an atomic vector?
col_means(mtcars[,c("mpg","cyl"), drop = F])
# So I think this is coming from numeric_cols <- df[, numeric] not having drop == FALSE. What happened
# is that by default `[` has drop = TRUE which reduces to the simplest structure which in the case of
# having only one column, is atomic. As a result, lapply tries to coerce that atomic vector into
# a list and thus applies mean to each element of the vector instead of the whole thing. This is different
# than the case where I fed col_means a two column, the subset operator left df as a data frame since
# it's simplest form is not a vector.

col_means(1:10)
# Now we're just feeding it an atomic vector. IThis is going to fail sooner as we're going to have
# an incorrect dimensions error when we try and ask for df[,numeric]

col_means(as.matrix(mtcars))
# I think this one will work as intended but I'm not 100% sure I know what lapply will do with
# a matrix. I assume it will coerce to list such that the columns are the elements of the list.
# Hmm it says that df[,numeric] is too long. This says to me that sapply didn't return what we had hoped
# which is a logical vector the same length as ncol(df).
str(as.list(as.matrix(mtcars)))
# So it looks like sapply coerces its first argument as list but as.list makes every entry of a matrix an
# element of that list. You can see that these are not 'inverse functions' in the mathematical sense
all.equal(mtcars, as.list(as.matrix(mtcars)))
is.list(mtcars)

col_means(as.list(mtcars))
# Again I think this will function properly (We'll see) because mtcars is already a list so coerceing it shouldn't
# make it meaningfully different
all.equal(mtcars,as.list(mtcars))
# so we lose some of the metadata when we coerce a data.frame to a list. The attributes are lesser
# but in any case, sapply will return a logical vector with the same lenght as ncols(mtcars)
# and then the subscript will properly interpret this. This does make me think that if we
# just by coincidence only had one numeric vector then this would fail in the same way
# that we saw col_means(mtcars[, "mpg", drop = F])
# NOPE WRONG AGAIN
# Ah! I see. So there was a critical difference here between a list and a data frame as input.
# lists don't accept multiple dimensions for subsetting.

mtcars2 <- mtcars
mtcars2[-1] <- lapply(mtcars2[-1], as.character)
col_means(mtcars2)
# Okay this syntax took me a second to get xD
# What it's doing is taking everything but the first column of mtcars, then turning it
# all into a character list and then squashing it back onto mtcars[2:11]. Now the question is, what does col_means do with this?
# it seems like sapply will return a vector of 11 that is TRUE followed by 10 FALSE. But here we encounter the problem
# that I mentioned a bit ago which is that only one column is numeric so we'll have the same problem as when
# we fed col_means a one column data frame.

# So what I'm getting out of this exercise is what the author said about sapply and `[` being
# kinda suspicious. I hadn't used sapply much in the past because I've found lapply and apply
# to be sufficient for my purposes. This exercise does not make me want to use it as even
# though it's billed as a 'user friendly lapply' the output seems unpredictable enough
# I am leery. I'm also not sure if there is a clean way to use `[` without having
# some sort of control structure based on the input. What I wish this chapter had were some
# more examples of how to control the data types in input. I feel like I have a tendancy
# to have these seriously large decision trees regarding the data types and it gets ugly.
# Let's make an attempt or two at re-writing this function

# I think that the reasonable inputs for this function are data.frames of
# at least one column and observation, matrices, and lists
col_means2 <- function(df) {

  # Check that we have one of those types
  if(!(is.matrix(df) | is.data.frame(df) | is.list(df))){
    stop("df must be a matrix, data.frame, or list")
  }

  # We'll also check to see if there are any rows
  if(is.data.frame(df)){
    if(nrow(df)<1 | ncol(df)<1){
      stop("df must have at least one row and one column")}
  }

  # I think we'll deal with the matrix situation first. Let's just force it into
  # a data.frame
  if(is.matrix(df)){
    df = as.data.frame(df)
  }
  # I wonder if there are any other reasons not to use sapply... Seems like this
  # works just as well:
  numeric <- unlist(lapply(df, is.numeric))

  # We'll also change this to not reference columns so it accomidates the case where
  # df is a list. We're also not using [[]] since we want to keep df as a list (or data.frame)
  numeric_cols <- df[numeric]

  # now we should have list or data frame, of just the numeric columns, and then taking
  # a mean should be fine.
  data.frame(lapply(numeric_cols, mean))
}

# Try the inputs with our new function
col_means2(mtcars)
col_means2(mtcars[, 0])
col_means2(mtcars[0, ])
col_means2(mtcars[, "mpg", drop = F])
col_means2(1:10)
col_means2(as.matrix(mtcars))
col_means2(as.list(mtcars))
mtcars2 <- mtcars
mtcars2[-1] <- lapply(mtcars2[-1], as.character)
col_means2(mtcars2)
# cool we did it
# These things always make me nervous though. I'm always thinking there are some other
# whack cases that I'm not going to think of that will still be sorta reasonable
# but break the function

# 2. The following function “lags” a vector, returning a version of x that is n values behind the original.
# Improve the function so that it (1) returns a useful error message if n is not a vector,
# and (2) has reasonable behaviour when n is 0 or longer than x.
lag <- function(x, n = 1L) {
  xlen <- length(x)
  c(rep(NA, n), x[seq_len(xlen - n)])
}
lag(1:10,2)

# Okay so we're asked to give a message for when x is not a vector.
# I'm kinda curious if they meant to allow lists but why not?
# We also need to assign behavior for n = 0 and n >= length(x)
# I think just returning x if n==0 seems reasonable, we lagged it by 0!
# if the lag is bigger than or equal to x let's just stop lag
lag <- function(x, n = 1L) {
  if(!is.vector(x)){
    stop("x must be a vector")
  }
  if(n==0){
    return(x)
  }
  if(n>=length(x)){
    stop("n must be less than the length of x")
  }
  xlen <- length(x)
  c(rep(NA, n), x[seq_len(xlen - n)])
}
lag(1:10, n = 0L)
lag(1:10, n = 10)
lag(mtcars)
lag(list(1:10, letters[1:3], "DeVargas"))

# One last thing to do is see if we can answer the quiz questions now:

# 1. How can you find out where an error occurred?
# browser, traceback, and line breaks

# 2. What does browser() do? List the five useful single-key commands that you can use inside of a browser() environment.
# browswer() opens up an interactive environment at the point in your code where browswer appears (or on erro if opts(error = browswer))
# It's nice because we can see what the actual objects in memory look like at that moment in execution.
# Within the browswer environment, we might want these commands next line (n), step in (s) to go one level deeper in the environment shells,
# finish (f) to continue the current loop or function call, continue (c) runs the rest of the code without interruption., and quit (q)
# terminates execution.

# 3. What function do you use to ignore errors in block of code?
# That would be try or trycatch. Possibly withExceptionHandlers

# 4. Why might you want to create an error with a custom S3 class?
# You can extract the error message and change the behavior of your code based
# on what exactly the error says. The solution also says that relying on
# the error message itself is not as reliable (might be translated) but
# making an S3 class can give you more precision



