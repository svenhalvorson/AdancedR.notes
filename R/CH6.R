#######
## 6 ##
#######
# Functions

# This chapter is all about functions which are the the central technology of R (IMO)
# We're told that we're going to want the pryr package
library("pryr")
library("SvenR")

# The three parts of a function are the body
body(label.schools)
# The formals (arguments)
formals(label.schools)
# and the environment where the function's variables are storeed
environment(label.schools)
# I'm not sure what the output means here but it seems like it's saying
# that the variables are stored somewhere associated with my package
# Notice that this is different than when we load a function into the memory
ff = function(x) {x+2}
environment(ff)


# some functions are so basic that they go back to the C structure that I'm told
# R is built on. These dont have the three properties mentioned above
body(sum)
formals(sum)


#EXCERCISES

# 1. What function allows you to tell if an object is a function? What function allows you to tell if a function is a primitive function?
# I'm guessing it's these
is.function(label.schools)
is.primitive(label.schools)
is.primitive(sum)

# 2. This code makes a list of all functions in the base package.

objs <- mget(ls("package:base"), inherits = TRUE)
funs <- Filter(is.function, objs)
# so we use the mget function to return all the ojbects within the base package
# the the Filter function applies is.function() to every element of objects and returns the TRUE entries

# Use it to answer the following questions:

# Which base function has the most arguments?
str(funs)
# so we have a list of 1206 functions
# apply
nargs = lapply(funs, FUN = function(x)length(formals(x)))
nargs.int = as.integer(nargs)
maximum = (nargs.int == max(nargs.int))
funs[maximum]


# How many base functions have no arguments? What’s special about those functions?
noargs = (nargs.int == 0)
sum(noargs)
#it looks like these are all primitive but let's varify
primatives = as.logical(lapply(funs, FUN = function(x)is.primitive(x)))
sum(primatives) == sum(primatives == noargs)
# interesting, so it looks like there's a few functions that have no arguments but are not primitive
funs[primatives != noargs]
#an example is sys.time
is.primitive(Sys.time())
formals(Sys.time())


# How could you adapt the code to find all primitive functions?
# I basically did that but you could write
prims <- Filter(is.primitive, objs)
# looks like a lot of these are math and data type functions


# 3. What are the three important components of a function?
# the body, formals, and environment


# 4. When does printing a function not show what environment it was created in?
# in the case of a primitive it does not
print(mean)
print(sum)


#########
## 6.2 ##
#########
# Lexical Scoping




