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
  ret = c()
  if(identical(env, emptyenv())) {
    # Base case
    stop("Can't find ", name, call. = FALSE)

  }
  if (exists(name, envir = env, inherits = FALSE)) {
    # Success case
    ret = c(ret,env)

  }

  # Call the function again, rely in it terminating when we hit emptyenv
  where2(name, parent.env(env))
}

en = new.env()
f = function(x){3*x}
en$mean = f

where2("mean",en)
