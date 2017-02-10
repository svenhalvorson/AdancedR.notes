#######
## 7 ##
#######
# OO Field Guide

# We're into a little primer on object oriented programming when it
# comes to R. I learned a little bit about OO years ago but it's probably a good
# idea to review the basics. We're told that a class is like a template for
# objects and determines how they relate to other objects. On the other hand methods
# are functions that depend on the class of the object fed to them. There is also
# an inheritance structure such that if no method exists for a child, the method
# for the parent is used when possible.

# There are 3 types of OO sysstems:

# S3: This is called 'generic-function' OO and implements something called message
# passing OO. In this style, methods are sent to objects and and the object
# determines which function to call. I'm not entirely sure I understand this paragraph.
# It says that S3 is very informal because you call a generic function (ex: drawRect())
# feed it the object like this: drawRect(canvas, "blue"). The author sort of implies that
# other message-passing systems might look more like canvas.drawRect("blue") which sort
# of feels like you're saying 'pick the canvas object, call it's drawRect method with the
# parameter blue. This is unlike how S3 functions which is 'call the generic function
# drawRect, feed it the canvas and parameter, and it will figure out how to handle
# whatever class canvas is. Really not sure about this but hopefully the chapter will
# clear this up for me.

# S4: This is described as being similar to s4 more formal with a few major differences. S4
# objects have formal class definitions that make inheritance explicit. They also have
# special helper functions for method. S4 objects are also said to have 'multiple dispatch'
# which means that "generic functions can pick methods based on the class of any number of
# arguments, not just one." I'm getting a little lost here but this last part seems to be
# saying that if we called drawRect(canvas, paint) then drawRect would be able to look
# at the classes of canvas and paint, then decide which method to execute.

# Reference Classe s (RC): These are more different from S3 and S4 than they are from eachother.
# RC objects are different in that the methods belong to the object, not the function the object
# is passed to. The example given is canvas$drawRect("blue") which gives the feeling of
# taking the canvas object, finding its drawRect method, and feeding the parameter blue.
# The author says that these objects also behave differently than S3/S4 objects in that
# they do not copy on modify, instead they're modified in place. If I remember right,
# this means that their address will not change when they're modified.

# Finally, there are is another system that isn't OO but similar : base types.
# They're C level types (remember that R was built from C) and we're told that they
# underly the other object types but we interact with them mostly in C


#########
## 7.2 ##
#########
# Base Types

# Because R was built upon C, all the objects have a type which is determined by their C structure.
# This classification can be obtained by the typeof() function. Unfortunately, these names
# don't always correspond that well with the names we use for objects
f <- function(){}
typeof(f)
# So even though we refer to f as function
is.function(f)
# The base type that f has is not called a functinon. Other types have more recognizable names
typeof(1:3)
# We're able to tell if an object is only a base type (and not S3/S4/RC) if is.object() returns FALSE
is.object(1:3)
is.object(f)
is.object(lm(formula = Sepal.Length~Sepal.Width,data = iris))
# So the way linear model objects are organized is such that it has some further structure
# beyond just a base type.


#########
## 7.3 ##
#########
# S3 Objects

# S3 objects are the simplest and most common type of object in R. We'll need  the plyr package to be
# able to reliably identify them.
library("pryr")
model = lm(formula = Sepal.Length~Sepal.Width,data = iris)
otype(model)
otype(iris)
otype(iris$Sepal.Length)
# so the contents of a data frame are likely to be base types but the container (data.frame) is S3

# Remember that in S3 world, methods belong to functions (generics) and not objects or classes.
# We can determine if an S3 function is generic if we see UseMethod() in the source code
mean
# So mean is a generic function
ftype(mean)
# The function mean() takes in an S3 objet and already contains the methods necessary to interpret
# some types of S3 object

# there are some S3 functions, called internal generics, which do not call UseMethod() because they
# are even more basal and are computed in C. ftype can pick these out
ftype(`[`)
`[`

# When an S3 generic is called to an object, it must recognize which method to use. These variations
# can be seen through the naming convetion generic.class(). The mean function is generic and acts
# differently when it is sent to different objects. Date objects call the method mean.date() when applied
# where as an integer object probably calls mean.default
methods(mean)

# We're advised not to use periods in the names of our own functions because it makes them look
# like S3 methods. This means I may want to change the names of several of my functions
# to include underscores. We can see that
ftype(t.data.frame)
# is a method for data frames while
ftype(t.test)
# is a generic function. Probably both of these should have been named differently. Better choices
# could have been t.data_frame and ttest because the first one is the transpose method of data frames
# and the second one is not a method

# We can also go the other way and see which methods are defined for a particular class
methods(class = "ts") #note that ts = time series

# Unlike java that has a specific constructor for objects of a given class, S3 objects can just be
# created using the structure command
foo <- structure(list(), class = "foo")
str(foo)
# So this is an empty list that is the class 'foo'. One of the attributes of foo is its class
attr(foo, "class")
class(foo)
# We're also allowed to assign a class to objects after they're created
foo <- list()
str(foo)
class(foo) <- "BOJANGLES"
class(foo)
# This feels a bit strange to me as I would think that the class would have to be defined somehow
# and not just be a character attribute. I'm guessing how we'll learn how this interacts
# with predefined structures soon. What happens if we try to assign an empty list the class
# that linear models fit into
class(model)
foo = list()
class(foo) = "lm"
class(foo)
str(foo)
# Hmmm so it doesn't seem to care that I'm making a lm object without any of the stuff that's normally
# in a model
# Just to be super clear about what the difference between class and type
class(iris)
class(model)
typeof(iris) == typeof(model)
# The internal storage types of data frames and linear models are the same; they're both lists
# The classes are not the same because their class attributes are different. This means that in terms
# of what they look like in memory, they're pretty similar. Both are just a series of (possibly) named
# bags that could contain anything. How they behave in relation to generics is different. When applicable
# functions will have different effects on these two objects based on what methods those objects have
# For example:
plot(model)
# Has a whole long routine of showing you different aspects of the model like residual plots wheras
plot(iris)
# makes pairwise scatters of all the variables. So when the plot generic is sent to a lm or data.frame
# each of those classes has different ways of interpreting plot. We can see that they have a plot methods
methods(class = "lm")

# It's possible to see the inheritance structure
foo <- structure(list(), class = "foo")
inherits(foo,"foo")
# I wonder if data frames inherit matrix
inherits(iris, "matrix")
# interesting
model2 <- glm(formula = Sepal.Length ~ Sepal.Width, data = iris)
class(model2)
# So a glm() object returns both lm and glm as it's class (actually that might not be true if its nonlinear xD)
# but the linear model
class(model)
# just says it is lm
inherits(model, "glm")
inherits(model2,"lm")
# So my understanding is that methods for lm should be interpretable by model2 however methods for glm
# should not be interpretable by model

# While an object does not need to be created from a constructor to have a particular class, we can still write
# that kind of function.
foo <- function(x) {
  if (!is.numeric(x)) stop("X must be numeric")
  structure(list(x), class = "foo")
}
# So what this is doing is returning a structure with the class foo. There doesn't seem to be any control on
# this being the only way to create a foo object though:
foo2 <- function(x) {
  if (is.numeric(x)) stop("X must not be numeric")
  structure(list(x), class = "foo")
}
f1 = foo(1:3)
f2 = foo2(letters[1:3])
class(f1) == class(f2)
# It seems like what makes an S3 object a member of its class is soley dependent on the class attribute
# Therefore, it seems like we should be pretty cautious about the naming conventions and constructors
# for these objects. If we were mixing our own objects with those from a package and happened to pick the
# same name for that class, then we can get into some weird situations
x = 1
class(x) = "lm"
plot(x)
# This example illustrates how this can kinda go awry. plot.lm is expecting a list (can use $) but we
# supplied an atomic vector. Note that it's perfectly happy to
plot(1)
# but plot(x) is unnacceptable because x is technically a lm and plot.lm fails for an atomic vector

# Creating new generics. From the text : "To add a new generic, create a function that calls UseMethod().
# UseMethod() takes two arguments: the name of the generic function, and the argument to use for method dispatch."
# We're also told that the second argument will be the same as the first if not specified
# Here's the first example
f <- function(x) UseMethod("f")
f.a <- function(x) "Class a"
a <- structure(list(), class = "a")
# So this generates a generic f, that uses the method f.
# We also have f.a which is a method for objects of class a when fed the function f.
# f.a just prints "Class a" and a is an object of class a that is an empty list
f(a)
# it looks like if we remove f.a, R get's angry about not knowing how to interpret f on a
rm(f.a)
f(a)
# because when f is called, it says 'better use method for a' and then can't find f.a()

# Method dispatch is the way that a function decides which method to use. Supposedly it's
# something like this : paste0("generic", ".", c(class(x), "default")) so if we dont
# pick a method it will go to default but otherwise we can get things like mean.date
# Here's another example from the text
f <- function(x) UseMethod("f")
f.a <- function(x) "Class a"
f.default <- function(x) "Unknown class"

f(structure(list(), class = "a"))
f(structure(list(), class = c("b", "a")))
f(structure(list(), class = "c"))
# So you can see that the default method of used when no other method is recognized
# Additionally, the second execution has two classes so it inherits to a
# What happens if we make a method for b and feed it an object of both classes?
f.b <-function(x) "Class b"
f(structure(list(), class = c("b", "a")))
# interesting, maybe that's because of the order of classes listed?
f(structure(list(), class = c("a", "b")))
# looks like it does
# You can also force R to call the 'wrong' method
f.a(structure(list(), class = "b"))
# So the difference in these methods is really only for helping us shortcut methods to
# objects because there isn't really any requirement. I suppose you could put in
# if(class(x) != "a") stop("Ya dun goofed") but this isn't inherent to methods


# There are groups of generics that share some abiliteis that make method dispatch easier
# One group is math, which includes abs(), sqrt(), and what not. The book doesn't really
# dive into these but my guess is that all the functions in the group want to coerce
# data types in the same way so they may all do that, then feed to a numerical version,
# and return. So I bet sqrt(TRUE) and abs(TRUE) both do something to change TRUE to 1
# and then do their numeric method.
?groupGeneric

# It's also possible to pass non S3 objects to S3 generics. They will be interpreted by
# their 'implicit class'. The method of determining this class is as follows:
iclass <- function(x) {
  if (is.object(x)) {
    stop("x is not a primitive type", call. = FALSE)
  }

  c(
    if (is.matrix(x)) "matrix",
    if (is.array(x) && !is.matrix(x)) "array",
    if (is.double(x)) "double",
    if (is.integer(x)) "integer",
    mode(x)
  )
}
iclass(matrix(1:5))
# I'm not really sure if this function was written in a way that explains the priority if
# there are multiple classes. As I found out a bit ago, the order that the classes
# are listed in determines how the method dispatch plays out. If this is in correct order
# I would have thought that array would come before matrix to be in decreasing complexity order

# EXERCISES

# 1. Read the source code for t() and t.test() and confirm that t.test() is an S3 generic and not
# an S3 method. What happens if you create an object with class test and call t() with it?
body(t)
body(t.test)
# SO yeah, t.test is not a method of t (surprise!). t() transposes matrices while t.test is our
# favorite neighborhood statistical test
mat = structure(matrix(1:4, nrow = 2), class = "test")
t(mat)
# oh wow.... so it does just say here's the method we're supposed to use paste("t",".","test")
# and the function that has that name is t.test Well... I can't say that I think that's a good
# thing. I guess this is one of the consequences of the lax structure of S3

# 2. What classes have a method for the Math group generic in base R? Read the source code.
# How do the methods work?
?groupGeneric
methods("Math")
# So it looks like there are data frames, dates, difftimes, fators, and something called POSIXt
# I'm not quite sure what the comma methods are: it lists Math,nonStructure-method which makes me
# think that's something for primitives

# 3. R has two classes for representing date time data, POSIXct and POSIXlt, which both inherit
# from POSIXt. Which generics have different behaviours for the two classes? Which generics
# share the same behaviour?

# Ask and you shall receive. I think we just gotta look at the generic functions methods
# and see if they differentiate between the two. I would assume that if methods were written
# specifically for these two, then in at least one case, they would behave differently

#first let's look for generics
objs <- mget(ls("package:base"), inherits = TRUE)
funs <- Filter(is.function, objs)
genericFlag = lapply(X = names(funs), FUN = isGeneric)
genericFlag = as.logical(genericFlag)
generics = funs[genericFlag]
# seems odd that only 87 of the 1205 functions in the base package are generic... did I fup?
# After lookign at them
generic_methods = lapply(X = generics, FUN = methods)
