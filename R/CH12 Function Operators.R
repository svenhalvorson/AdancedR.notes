##########
## 12.0 ##
##########
# Function Operators

# This chapter is about function operators. Recall that functionals are
# functions that take in some combination of functions and data but return data.
# Function operators take in one or more functions and return a function. Like functionals,
# their main purpose is to make code clearer to write and read, not to do something impossible
# without them. The analogy given is functionals : loops as function operators : anonymous functions
# Here is an example of a function operator:

chatty <- function(f) {
  function(x, ...) {
    res <- f(x, ...)
    cat("Processing ", x, "\n", sep = "")
    res
  }
}
f <- function(x) x ^ 2
s <- c(3, 2, 1)
chatty(f)(2)
vapply(s, chatty(f), numeric(1))
# so basically this like an all purpose wrapper that just executes f and tells
# you what the first input was.

# Function operators help us deal with anonymous functions in that we won't have to
# re-write them in funny ways to supply extra arguments
library("plyr")
library("pryr")
library("memoise")


##########
## 12.1 ##
##########
# Behavioural Functional Operators

# This first class of functional operators are not designed to change the arguments or
# main purpose of a function, they just add some additional behavior. The examples given
# are adding delay to server requests, printing output to console, and cache-ing output
# to improve performance.

# Here is an example of modifying download.file
download_file <- function(url, ...) {
  download.file(url, basename(url), ...)
}
lapply(urls, download_file)
# okay so I wasn't familiar with basename but what it does is get's the deepest level
# of a path. So what we're doing here is downloading files and defaulting them to
# the end of the url that we feed. Otherwise, we would probably end up using Map() to
# supply a list of urls and a list of output names
# Here is an extension of this idea using a loop to notify us every 10 entries and
# add a delay to the output:

i <- 1
for(url in urls) {
  i <- i + 1
  if (i %% 10 == 0) cat(".")
  Sys.delay(1)
  download_file(url)
}

# notice that this would be difficult to accomplish with functionals as we would
# need to use some sequence as an input to the funtion in order to capture every
# 10th entry. The author notes that this is a little bit garbled in that the delay and
# notifications are kinda lumped together. We're told that something like this would help:
lapply(urls, dot_every(10, delay_by(1, download_file)))
# so the idea here is that we can use a functional but have the function being sent
# to lapply is being modified by the functional operators delay_by and dot_every
# in this way, the behavior is not the same for every iteration of lapply but
# we can still use the functional

# Here's an example of how delay_by could be written:

delay_by <- function(delay, f) {
  function(...) {
    Sys.sleep(delay)
    f(...)
  }
}
# pretty simple, it returns a function that is essentially a copy of f but
# with a delay of delay before it's normal behavior
# this takes essentially not time:
system.time(runif(100))
system.time(delay_by(0.1, runif)(100))
# uh wat... how?? This might be just the way the output is formatted but
# the times change when I run this repeatedly and sometimes it says that 0.09
# seconds elapsed which seems impossible if it's actually delaying correctly...
# I don't even see how this could be a rounding error. Well whatever...

# here's a version of dot_every:
dot_every <- function(n, f) {
  cat("!")
  i <- 1
  function(...) {
    if (i %% n == 0) cat(".")
    i <<- i + 1
    f(...)
  }
}
# let's see.. so within the execution environment of dot_every, we have
# i and when the function returned by dot_every is called, it increments that i.
# This makes me think I don't really understand environments as well as I should as
# I would have thought that once you call dot_every it would return a function and
# then garbage collect the i. Hmm so I also don't think it's changing the definition of
# dot_every as
for(i in 1:20){
  dot_every(10, runif)(i)
}
x <- lapply(1:20, dot_every(10, runif))

# Okay so I had to ask stack overflow about this and learned something valuable. Here is
# what's happening:

# 1. lapply is called
# 2. Which function do we want? call dot_every
# 3. dot_every creates its execution environment returns the closure function
# but dot_every stay's open for the duration of lapply
# 4. the same anonymous function is called many times across lapply, each time
# changing i within dot_every's environment.
# 5. lapply hits the end of it's scope, closes dot_every, i thrown to garbage collection

# That is why these two examples are different.

# as a last note, the author mentions that it's easier to read if the function is the last argument
# of the function operator
download <- dot_every(10, delay_by(1, download_file))
# vs.
download <- dot_every(delay_by(download_file, 1), 10)
# that last 10 seems to be floating and it's less obvious that it's an argument for dot_every

# This next section is on memoisation (memoization) which is an optimization
# technique where we cache routines and use them instead of repeating calls
# here's an example
library("memoise")
slow_function <- function() {
  Sys.sleep(1)
  10
}
system.time(slow_function())
system.time(slow_function())
fast_function <- memoise(slow_function)
system.time(fast_function())
system.time(fast_function())
# So what this is doing is saving in memory the output of particular function calls
# so that they can be reused without actually calling the function. I did a hack version
# of this with my lable_schools command but instead of literally using memoization, I
# just made a table of completed matches and scanned that first. Similar but I bet this is a bit
# faster

# The next example given is the fibonacci sequence which is a recursive relationship
# so computing any value of the sequence will require computing all the values before it
# (if you're not aware of the explicit formula that is)
fib <- function(n) {
  if (n < 2) return(1)
  fib(n - 2) + fib(n - 1)
}
system.time(fib(23))
# system.time(fib(100))
# holy crap that slows down in a hurry. I let it run for about 10 minutes on that one... let's do a little experiment
times = lapply(X = 1:35, FUN = function(x){system.time(fib(x))[3]})
plot(x = 1:35, y = times)
# Unsurprisingly this looks like an exponential growth situation
# so this seems like a pretty reasonable way to calculate this but it will start to bifurcate
# and calculate every value below n-1 two or more times. With memoization we can reduce these computations
fib2 <- memoise(function(n) {
  if (n < 2) return(1)
  fib2(n - 2) + fib2(n - 1)
})
system.time(fib2(23))
# so even running it for the first time is a little faster
system.time(fib2(23))
# but then the fact that fib2(23:1) have already been memoised means that this will run quicker the first time
system.time(fib2(24))
times2 = lapply(X = 1:35, FUN =  function(x){system.time(fib2(x))[3]})
plot(1:35, times2)
# that's weird... oh yeah I told it to compute some already
# The author also mentions that it's not a good idea to just run around memoizing
# things willy nilly. For example, if you memoize RNG, it will cease to be
# random after the first call as runif or whatever will just look through the stored
# results and return them instead of executing the function.

# here is kinda the punchline:
download <- dot_every(10, memoise(delay_by(1, download_file)))
# will help if we've got duplicates in our list and prevent them from calling the
# server again.

# The next section is about capturing function invocations. This means that we're interested
# in seeing the guts of a functional while/after it runs. Here's a helper function:
ignore <- function(...) NULL
tee <- function(f, on_input = ignore, on_output = ignore) {
  function(...) {
    on_input(...)
    output <- f(...)
    on_output(output)
    output
  }
}

# So what does tee do... I think we are to presume that on_input and on_output are
# supposed to be functions. So we get back the output of f but it now will execute
# the on_input function on the arguments of f, then execute on_output to the output of f.
# Probably on_input and on_output should be functions that print things or maybe have <<-
# statements that will let us see some of the results of the functional.

# The next example involves the uniroot function which is a function to find roots
poly = function(x){(x-2)*(x+3)}
uniroot(f = poly, interval = c(-10,0))

# using tee:
g <- function(x) cos(x) - x
zero <- uniroot(g, c(-5, 5))
show_x <- function(x, ...) cat(sprintf("%+.08f", x), "\n")
# So g is our function to find a zero of, zero is using uniroot on g, and then
# show_x is going to priont the resulting x coordinate

# The location where the function is evaluated:
zero <- uniroot(tee(g, on_input = show_x), c(-5, 5))
# So this demonstrates how tee plays nicely with recursive functions as
# uniroot probably calls itself a bunch of times until some threshold is hit and
# as a resultwe get to see on_input executed multiple times. Alternatively, we
# can see what the y coordinates it's getting are:
zero <- uniroot(tee(g, on_output = show_x), c(-5, 5))

# or both
show_x2 <- function(x, ...) cat(sprintf("x: %+.08f", x), "\n")
show_y <- function(x, ...) cat(sprintf("y: %+.08f", x), "\n")
zero <- uniroot(tee(g, on_input = show_x2, on_output = show_y), c(-5, 5))

# While this function prints all the inputs and outputs of g, that uniroot is trying
# it doesn't actually provide a list of them as output. I know the next section is going
# to introduce a function called remember() which will record these but let's see if we can
# create our own crappy version before reading it.

# maybe we'll use a helper
add_to <- function(f,...){

  results[[length(results)+1]] <<- f(...)
  names(results[[length(results)]]) <<- ...

}

dont_forget <- function(f,...){
  # So we need to create a list and maybe the values of the list will be outputs and
  # names will will be inputs
  results = list()
  add_to(f(...))

}
dont_forget(uniroot(g, c(-5, 5)))
# hmm yeah I don't think this is going to work since tee returns a function and mine just tries to work with the function
# Let's see what the author does;
remember <- function() {
  memory <- list()
  f <- function(...) {
    # This is inefficient!
    memory <<- append(memory, list(...))
    invisible()
  }

  structure(f, class = "remember")
}
# okay so that's not super different than what I was aiming for. This function creates a list
# then generates a function that appends (nice function I wasn't aware of) memory with
# the arguments of f. It then returns f as a member of the remember class

as.list.remember <- function(x, ...) {
  environment(x)$memory
}
# So this takes in an environment and returns the memory attribute of a remember object
print.remember <- function(x, ...) {
  cat("Remembering...\n")
  str(as.list(x))
}
# And this will help us display what iteration the function is on and what we're saving

# Example execution:
locs <- remember()
vals <- remember()
zero <- uniroot(tee(g, locs, vals), c(-5, 5))
# Okay so what we're doing here is having locs and values be the arguments of on_input
# and on_output. Instead of using just a simple function for on_input, we're using a version of
# it that is a member of the remember class. And as a result, when on_output is called, it will
# store the results in memory
x <- unlist(as.list(locs))
error <- unlist(as.list(vals))
plot(x, type = "b")
abline(h = 0.739, col = "grey50")

# I don't know if I'm feeling especially dumb today or if this got a lot harder all the sudden
# but this chapter is not intuitive to me at all. Well whatever, we'll probably end up looking
# back over this book many times




