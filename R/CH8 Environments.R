#########
## 8.1 ##
#########
# Environment Basics

# We're told that environments are the heart of scoping as we learned
# in the chapter about functions. It is said to be a way of associating
# a set of names to a set of values.
e <- new.env()
e$a <- FALSE
e$b <- "a"
e$c <- 2.3
e$d <- 1:3
# So e is a place where the names a:d are stored with an association to FALSE, "a",
# 2.3 and 1:3
# The names aren't in the environment though so we can have multiple names pointing
# at the same object or different names pointing at different objects with the same data
e$b <- e$d
e$b
# and this is not the same as
e$b = 1:3
# because the second example has two objects consisting of 1:3 with the names d and b.
# The first one had b and d just pointing at the same 1:3 object

# All environments are nested within another environment except for the empty environment
# Remember that lexical scoping in R works so that if a name is not found in one environment
# then R will look up to the parent environment and so on. The author says something a little
# strange here. We're told that we usually speak about envirnments as having parents and
# ancestors (the environments they're nested in) but we're told that we can't talk so easily
# about children. We're told : 'given an environment we have no way to find its children'
# This is pretty surprising to me. I wonder if he means something different than I'm thinking of
# because it seems like we could just use is.environment in an apply statement and get all the
# objects in an environment that are again environemnts.

# Environments are very similar to lists but they have a few important differences:
# 1. Every name in an environment is unique
w = list(ff = 1, ff =2)
w
e = new.env()
e$a = 1
e$a = 2
e$a
# 2. The names in an environment do not have a natural order to them. There is no first
# element of an environment
# 3. Environments have parents, lists do not.
# 4. Environments have reference semeantics. I believe this is the point already made
# about how multiple names could point to the same object

# Environments are thought of as being made up of two parts. The 'frame' is where the binding
# between names and objects is stored. Environments also contain 'parent frames' which is where
# that environment lives

# Four special environments are listed:
#  1) globalenv() is the place you normally work. We're told that the parent of globalenv is
#     the last package you called with library(). This is very strange to me. I would have
#     thought that the relationship would be the other way around.
#  2) baseenv() is the base environment where the base package lives. basenv lives in emptyenv
#  3) emptyenv() is the largest universe under which all other environments live.
#  4) environment() calls whatever the current environment is
# This part about the global environment being in whatever environment the last loaded package
# is in seems really weird. Does that mean that it copies/moves that environment when you
# load a new package? You certainly don't lose the data so I would think so...

# search() allows us to go up the environment path towards base
search()
library("SvenR")
search()
parent.env(globalenv())
# I guess it's true...
# We can also call up environments from their string names using as.environment()
as.environment("package:graphics")
as.environment("FAKE ENV")
# yeah so it does actually check that there is that environment and not just create one
# Also, the pictures in this chapter seem pretty helpful.

# We can also see the frame of an environment using ls.str
e = new.env()
e$a = 1
e$b = 2
e$c = lm(formula = Sepal.Width ~ Sepal.Length, data = iris)
ls.str(e)
# Cool, this just got me to go create a mini function for my package to clear console and
# an environment's memory

# If we want to work within an environment and grab elements from the environment, we have
# $, [[]], and get() to access objects
e$a
e[["a"]]
get("a", envir = e)

# Unlike lists where we just set elements to null to 'delete' them, here we have to use
# rm(). Let's see
l = list(a = 1:3, b = letters[1:3], c = "OHBOYOHBOYOHBOY")
l[["a"]]
l[["d"]]
l[["a"]] = NULL
l[["a"]]
l
# Yeah I guess I kinda forgot this aspect of lists.
e$a = NULL
e$a
ls(envir = e)
# and with the environment setting it to null still preserves the name in the environment
# a is now just a pointer that points at null

# We can use the exists function to try and determine if something is in memory. The envir
# chooses where exists() starts looking. inherits determines whether or not it will look
# up the chain
x = 10
# note x is in globalenv
exists("x", envir = e)
exists("x", envir = e, inherits = FALSE)

# Finally, == cannot accept environments as arguments so we gotta do
e = new.env()
e$a = 1
e$b = 2
f = new.env()
identical(f,e)
f$a = 1
f$b = 2
identical(f,e)
# oh so.... just having all identical contents doesn't make two environments
# identical. They literally need to be the same piece of memory?
# Here's the example given in the text:
identical(globalenv(), environment())
?identical

# EXERCISES

# 1) List three ways in which an environment differs from a list.
# Environments have parents, environments do not have a natural order to their
# elements, and environments use pointers to objects instead of copy-on-replace

# 2) If you don’t supply an explicit environment, where do ls() and rm() look?
#    Where does <- make bindings?
# These all default to globalenv()

# 3) Using parent.env() and a loop (or a recursive function), verify that the
#    ancestors of globalenv() include baseenv() and emptyenv(). Use the same basic
#    idea to implement your own version of search().

search2 <- function(e = globalenv()){

  if(!is.environment(e)){
    stop("e must be an environment")
  }
  # Hold the chain here
  inher = c(e)
  # Now loop till we get to empty
  while(!identical(e, emptyenv())){
    e = parent.env(e)
    inher = c(inher, e)
  }
  print(inher)

}
# meh, the output is a little large but w/e. We could replace the values
# of inher with the names() of the environments when applicable


#########
## 8.2 ##
#########
# Recursing over environments

# In this section we'll be looking at the nested structure of environments
# Recall that the where function can look through environments for a name
# Here's the code for it:
where <- function(name, env = parent.frame()) {
  if (identical(env, emptyenv())) {
    # Base case
    stop("Can't find ", name, call. = FALSE)

  } else if (exists(name, envir = env, inherits = FALSE)) {
    # Success case
    env

  } else {
    # Recursive case
    where(name, parent.env(env))

  }
}

# This seems pretty straightforward and although I'm not quite sure that I would
# have come up with it. It's recursive because it calls itself. I probably would
# have written a while loop but this seems better.

# And in fact the author supplies a version:
is_empty <- function(x) identical(x, emptyenv())

f2 <- function(..., env = parent.frame()) {
  while(!is_empty(env)) {
    if (success) {
      # success case
      return()
    }
    # inspect parent
    env <- parent.env(env)
  }

  # base case
}

# EXERCISES

# 1. Modify where() to find all environments that contain a binding for name.

where2 <- function(name, env = parent.frame()) {
  # Make a vector to hold results
  browser()
  if(!exists("ret", where = globalenv())){
    assign(x = "ret", value = c(), envir = globalenv())
    }

  if(identical(env, emptyenv())) {
    # Base case
    message("Reached base environment looking for ", name)
    return(ret)

  }
  if (exists(name, envir = env, inherits = FALSE)) {
    # Success case
    assign(x = "ret", value = c(ret, env), envir = globalenv())

  }

  # Call the function again, rely in it terminating when we hit emptyenv
  where2(name, parent.env(env))
}

# So here we'll create an object called mean in en but it should also be in
# the base environment since the mean function lives there
en = new.env()
f = function(x){3*x}
en$mean = f
rm("ret")
where2("mean",en)

# LET'S COME BACK TO THESE, I"M IN THE MOOD TO KEEP READING

#########
## 8.3 ##
#########

# Function Environments

# We're told that there are four types of envirments associated with functions

# 1. When a function is called the 'enclosing' environment is the environment
# where the function was created.
y <- 1
f <- function(x) x + y
environment(f)
# So global environment is the enclosing environment to f

# 2. When a function is assigned to a name, <-- defines a 'binding environment'
# The author says 'The enclosing environment determines how the function finds
# values; the binding environments determine how we find the function.'
# So this means that the lexical scoping of a function is sorta defined when
# it is created regardless of where it's moved.
# We're told that namespaces are environments used by packages to distinguish
# functions with the same name in different packages.
environment(sd)
where("sd")
# So this is saying that the binding environment is the namespace of stats
# but the environment that sd lives in the package stats
# What I'm getting out of this diagram is that we have a chain of environments
# starting with empty, to base, to all the packages, to global. Then within
# the global there is the base namespace and then within it are all the namespaces
# for the packages. So a function like label_schools lives in package::SvenR
# however when it is called, it's scoping will go through namespace:SvenR first
# meaning that it will find data for that function in SvenR before any other package

# 3. Calling a function creates an 'execution' environment in which the
# necessary data is stored in to execute the function. It's parent is the function's
# evironment so the execution environment for label_schools lives in SvenR

# 4. Lastly, executing a function also creates a 'calling environment' in which
# the location of the call is stored. The function parent.frame() gives
# the environmen in which a function was called
f2 <- function() {
  x <- 10
  function() {
    def <- get("x", environment())
    cll <- get("x", parent.frame())
    list(defined = def, called = cll)
  }
}
g2 <- f2()
x <- 20
str(g2())
# Very cool. What this is showing is that within the execution environment of f2,
# x == 10. Within the calling environment of f2, x == 20. We're told that some
# languages (not R) have scoping through the calling environment rather than
# an enclosing one. This is more complicated IMO since the functions won't have the
# independant feel that they do in R

# EXERCISES

# 1. List the four environments associated with a function. What does each one do?
# Why is the distinction between enclosing and binding environments particularly important?

# We have a binding environment which is where the name for the function is located. It's also
# where the function will look for other functions in that package.
# There is also the enclosing environment which is where the function lives. Probably global
# or in a package.
# Next there is the execution environment which is where data defined by the function is
# located. This is the first place on the search path for functions within the function.
# Lastly there is a calling environment which is the environment from which the function
# was called. Ex: if sd() calls var() then then the calling environment for var when
# sd is called is the execution environment of sd.

# I'm a little confused still about the difference between enclosing and binding arguments
# It sounds like the binding environment is where the function is tied to the name given
# (this is often the namespace for a package). The enclosing environment is where the function
# itself lives (not the name).

# 2. Draw a diagram that shows the enclosing environments of this function:
f1 <- function(x1) {
  f2 <- function(x2) {
    f3 <- function(x3) {
      x1 + x2 + x3
    }
    f3(3)
  }
  f2(2)
}
f1(1)
# Can't include a drawing in this editor but this is what it should look like.
# Each successive function is defined within the execution environment of the previous.
# x3 is defined within f3, then f3 looks up a level to find x2 defined in f2 and so on.

# 5. Write an enhanced version of str() that provides more information about functions.
# Show where the function was found and what environment it was defined in.

env_str <- function(x){
  print(paste0("The calling environment is ",environmentName(parent.frame())))
  print(paste0("The binding environment is ",environmentName(environment(x))))

  library('pryr')
  print(paste0("The enclosing environment is ",environmentName(where(deparse(substitute(x))))))
  str(x)
}
env_str(filter)

#########
## 8.4 ##
#########

# Binding Names to Values

# Objects are bound to names in R via the assignment operators <- and =.
# We can use letters, numbers, . and _ but it must not begin with . or _.
# The reserved names:
?Reserved
# mostly different nulls. This is here so we can't do something like
TRUE <- 13
# Backticks get us around the normal naming conventions
`._.` = "SvenBot"

# This is something very strange that I was unaware of :
x <- 0
f <- function() {
  x <<- 1
}
f()
x
# This double arrow is called the 'deep assignment arrow' and looks
# for the path up the search chain for the object 'x' and modifies it
# in whatever environment x is found in
label_schools <<- "hi"
# nice that package functions appear to be protected

# We're told that there are a couple of other types of bindings. From the
# pryr, package we have the delayed binding %<d-%
library('pryr')
system.time(b %<d-% {Sys.sleep(1); 1})
b
# As you'll notice, this doesn't actually assign b to memory unitl
# it's invoked and then it does on a 1 sec delay. Another interesting thing
# about this is that you feed it a promise (expression) and it evaluates
# that promise when it's invoked. Do we know how it works
delayed_model %<d-% {f = lm(formula = Sepal.Length~Sepal.Width, data = iris); f$coefficients}
# cool. We're told that this is how functions in packages work. They're loaded as a promise
# and are only put into memory when called

# The next special binding is the active binding which re-runs its promise every time the
# assigned name is invoked
x %<a-% runif(1)
x
x
# hm so these are sorta hard to think about why we would wantt them. I guess at least the delayed
# binding seems helpful with memory management. Maybe you would want the active binding for
# making a game with a constantly changing object?

# EXERCISES
# 1. What does this function do? How does it differ from <<- and why might you prefer it?
ebind <- function(name, value, env = parent.frame()) {
  if (identical(env, emptyenv())) {
    stop("Can't find ", name, call. = FALSE)
  } else if (exists(name, envir = env, inherits = FALSE)) {
    assign(name, value, envir = env)
  } else {
    rebind(name, value, parent.env(env))
  }
}
rebind("a", 10)
# This is quite similar to <<- however it allows you to specify which environment to
# start in. <<- also never looks in the current environment, only up the search path
# whereas ebind defaults to the current environment (often global)

# 2. Create a version of assign() that will only bind new names, never re-bind old names.
# Some programming languages only do this, and are known as single assignment languages.

# I'm assuming that by 'new names' the author means 'new in that environment' and not
# 'present in any environment'

assign_safe <- function(name, obj, env = parent.frame()){
  if(exists(x = name, where = env, inherits = FALSE)){
    stop(paste0(name," already exists there"))
  }
  else{
    assign(x = name, value = obj, envir = env)
  }

}
q = new.env()
assign_safe("grondo", lm(formula = Sepal.Width~Sepal.Length, data = iris), env = q)
ls(envir = q)
assign_safe("grondo", lm(formula = Sepal.Width~Sepal.Length, data = iris), env = q)

# 3. Write an assignment function that can do active, delayed, and locked bindings.
# What might you call it? What arguments should it take?
# Can you guess which sort of assignment it should do based on the input?

# One interesting thing here is that we can actually lock active bindings. This doesn't
# freeze the value as the promise is still re-run, but locking an active binding
# does prevent new bindings being placed on that name

assign_multi <- function(name, obj, env = parent.frame(), type, lock = FALSE){
  if(type == "active"){
    makeActiveBinding(sym = name, fun = obj, env = env)
  }

}
# SO we should accept a name to bind to, the actual object to bind to (or function in the case of active),
# the environment to bind to, an argument to determine which kind of binding, and whether it should be locked


#########
## 8.5 ##
#########

# Here's an example that demonstrates the difference between the copy on replace
# aspect of objects vs the reference semantics of an environment
modify <- function(x) {
  x$a <- 2
  invisible()
}

x_l <- list()
x_l$a <- 1
modify(x_l)
x_l$a

x_e <- new.env()
x_e$a <- 1
modify(x_e)
x_e$a

# So here we're seeing that when we use modify on a list, a copy of x_1 is passed as an argument
# and that argument is modified within the function's execution environment, but the x_1 in the
# global environment remains unchanged since no assignment statement was made.

# Assignment statements for environments, on the other hand, work such that they are modified
# rather than copy on replace

# The author suggests that just like a list (or data.frame) can pass information to functions
# or other objects, environments can have this use as well. Perhaps we would like to
# store a bunch of related information together. An environment can be a nice way to accomplish this
# We're also cautioned to put our custom environmetns into the empty environment to stop search paths
# going through global ect.
x <- 1
e1 <- new.env()
get("x", envir = e1)
e2 <- new.env(parent = emptyenv())
get("x", envir = e2)

# We're told that there are three major uses for environments:
# 1. Avoiding copies
# The reference semantics make it so that changing the data in an environment are less
# computationally expensive. We don't make copies of anything as parts of the environment
# are modified, not copied, on assignment statements. The author mentions that this
# is useful for large data sets but is less important with newer versions of R

# 2. Package states
# Environments also allow packages to modify internal data easily. This means that
# it will not affect other packagaes or environments but also that the functions
# within a package can look up information in that environment

# 3. Efficient lookup
# Environments simulate what is called a 'hasmap'. This is a data structure that takes
# constant time to find an object given a name.

# QUIZ
# Let's see if we can answer the quiz questions now

# 1. List at least three ways that an environment is different to a list.
# Environments use reference semantics, not copy on replace
# Environments cannot have multiple elements with the same name
# Environments are not ordered

# 2. What is the parent of the global environment? What is the only environment that doesn’t have a parent?
# The global environment's parent is whatever the most recent packaged called with library.
# The empty environment has no parents

# 3. What is the enclosing environment of a function? Why is it important?
# The enclosing environment is the environment that a function lives in. This is the one
# given by environment(). It's important because it is where the function will start to look for variables
# not defined in it's execution environment

# 4. How do you determine the environment from which a function was called?
# The calling environment can be determined by paren.frame()

# 5. How are <- and <<- different?
# <- assins a object to a name in the current environment
# <<- looks up the search path (towards global) and tries to find an object with
# the given name. It overwrites if it finds that name and writes it to global otherwise


