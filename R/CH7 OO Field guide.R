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


v#########
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
# After lookign at them it looks fine
generic_methods = lapply(X = names(generics), FUN = methods)
has_POSIXt = lapply(X = generic_methods, FUN = grepl, pattern = "POSIXt")
has_POSIXt = lapply(X = has_POSIXt, FUN = max)
has_POSIXt = as.logical(has_POSIXt)
has_POSIXct = lapply(X = generic_methods, FUN = grepl, pattern = "POSIXct")
has_POSIXct = lapply(X = has_POSIXct, FUN = max)
has_POSIXct = as.logical(has_POSIXct)

# 4. Which base generic has the greatest number of defined methods?

# I think we can get it at it this way
objs <- mget(ls("package:base"), inherits = TRUE)
funs <- Filter(is.function, objs)
genericFlag = lapply(X = names(funs), FUN = isGeneric)
genericFlag = as.logical(genericFlag)
generics = funs[genericFlag]
generic_methods = lapply(X = names(generics), FUN = methods)
num_methods = lapply(X = generic_methods, FUN = length)
num_methods = as.numeric(num_methods)
most_methods = num_methods == max(num_methods)
generics[most_methods]
# the or symbol?
methods(`|`)
# Okay then

# 5. UseMethod() calls methods in a special way. Predict what the following code will return, then
# run it and read the help for UseMethod() to figure out what’s going on. Write down the rules in
# the simplest form possible.
y <- 1
g <- function(x) {
  y <- 2
  UseMethod("g")
}
g.numeric <- function(x) y
g(10)

h <- function(x) {
  x <- 10
  UseMethod("h")
}
h.character <- function(x) paste("char", x)
h.numeric <- function(x) paste("num", x)

h("a")

# Alright, well I'll read the documentation after guessing. The first one looks like it's going to print
# 1 for y. I don't think it matters that we assign y <- 2 because that will not overwrite the y
# in the global environment. NOPE. Looks like when we call useMethod it's looking one level up
# to tye y=2 inside the generic fuction g
# My prediction for h is that it will print "char a". YEP. So it looks like there might be
# some sort of prediction for which methdo to call
# The documentation says that it uses the class of the first object to help determine which method to use

# 6. Internal generics don’t dispatch on the implicit class of base types. Carefully read ?"internal generic"
# to determine why the length of f and g is different in the example below. What function helps distinguish
# between the behaviour of f and g?
f <- function() 1
g <- function() 2
class(g) <- "function"

class(f)
class(g)

length.function <- function(x) "function"
length(f)
length(g)
# So I think that the conflict the author is trying to highlight here is thatf is a function and g is something
# with it's class set as "function". When length is called, is it supposed to use length or length.function
# in the case of f and g?
?`internal generic`
# So we're told that the method dispatch uses is.object() which keys off the class attribute being set
# It looks like the method of construction for f, never triggers is.object
is.object(f)
is.object(g)
class(f)
attr(f,"class")
attr(g,"class")
# interesting. So the method dispatch for the internal generics uses is.object. If TRUE, then it looks for methods
# within (length for example). Since g's class attribute was never set, length does not look for another method
# when called to f.


#########
## 7.4 ##
#########
# S4

# This is the slightly more rigid version of S3. We're told that unlike S3
#  a) Classes have formal definitions which describe their fields and relationships
#  b) Method dispatch can be done using multiple arguments
#  c) S4 objects also have slots with the @ operator to access them

# We can identify S4 objects pretty easily from str(), pryr::otype() or isS4()
model = lm(formula = Sepal.Width~Sepal.Length, data = iris)
isS4(model)
str(model)
# K so that looks like an S3 object
library("stats4")
library("pryr")
y <- c(26, 17, 13, 12, 20, 5, 9, 8, 5, 4, 8)
nLL <- function(lambda) - sum(dpois(y, lambda, log = TRUE))
fit <- mle(nLL, start = list(lambda = 5), nobs = length(y))
isS4(fit)
str(fit)
otype(fit)
# Here's an example of an S4 generic
?nobs
isS4(nobs)
ftype(nobs)
# and examples of S4 methods
mle_nobs <- method_from_call(nobs(fit))
isS4(mle_nobs)
ftype(mle_nobs)
# So this is definitely thought of as a method, not a generic

# The generic is() adapts to the type of object passed to it.
# if we want to test inheritance, we can put the object and class name in as arguments
is(fit,"mle")
class(fit)

# We have some ability to wrangle all the classes and methods in the S4 world
getGenerics()
getClasses()
showMethods()

# S4 does have some of the class level safegaurds that you might have expected to be in S3
# The function setClass() has informatin on what it means to be a member of that class
# New objects can be created with new(). We can get more information with class?className
class?mle
# kinda nice that you can see the methods that mle can invoke

# S4 also has a validity method to test whether or not an object has all the necessary qualities
# to count as a member
validObject(fit)

# Here is an example of how to set up a class constructor and inheritance structure:
setClass("Person",
         slots = list(name = "character", age = "numeric"))
setClass("Employee",
         slots = list(boss = "Person"),
         contains = "Person")

alice <- new("Person", name = "Alice", age = 40)
john <- new("Employee", name = "John", age = 20, boss = alice)
?setClass

# Interesting, so I'm not exactly sure what setClass is doing here. It seems to be defining
# (somewhere in memory) what it means to be a person or employee. I'm not seeing it
# in the global environment. The documentation suggests that the class constructor is created
# invisibly.
is(john, "Person")
# So will the constructor allow us to create a person without an age?
svenboy <- new("Person", name = "Sven")
# yeah so it seems like it will still create empty slots for attributes not specified
# Access slots:
svenboy@name
john@boss

# S3 versions of S4 objects can be recovered (when applicable) with the .Data slot
setClass("RangedNumeric",
         contains = "numeric",
         slots = list(min = "numeric", max = "numeric"))
rn <- new("RangedNumeric", 1:10, min = 1, max = 10)
rn@min
rn@.Data
otype(rn@.Data)

# We're allowed to create new S4 generics and methods for those generics
setGeneric("union")
str(union)
setMethod("union",
          c(x = "data.frame", y = "data.frame"),
          function(x, y) {
            unique(rbind(x, y))
          }
)
union(data.frame(x = 0:3),data.frame(x = 2:4))
# so we've added a method for data frames and I'm guessing the method dispatch by internal
# type makes it so we don't really need to put controls on the type of input allowed

# If you're creating a new S4 generic, it needs to have a 'useMethod()'  equivalent:
setGeneric("myGeneric", function(x) {
  standardGeneric("myGeneric")
})

# EXERCISES

# 1. Which S4 generic has the most methods defined for it? Which S4 class has the most
# methods associated with it
gens = getGenerics()
generics = as.character(gens@.Data)
gens = lapply(X = generics, FUN = getGeneric)
s4flag = lapply(X = gens, FUN = isS4)
sum(as.logical(s4flag))
# So it loooks like we only got s4 functions to start with
meth = list()
for(i in 1:length(generics)){

  meth[[i]] = as.character(methods(generics[i]))
}
# Damnation, there is some invisible functions called 'complete' that's listed
# i have no idea what that means but let's go on
meth = list()
for(i in c(1:39,41:54, 56:length(generics))){
  print(i)
  meth[[i]] = as.character(methods(generics[i]))
}
nmethods = as.numeric(lapply(X = meth, FUN = length))
mostmeth = nmethods == max(nmethods)
gens = gens[c(1:39,41:54, 56:length(generics))]
gens[mostmeth]
# it's the pipe again

# Hopefully we can do basically the same thing with classes
# clas <- getClasses()
# meth = list()
# for(i in 2:length(clas)){
#   print(i)
#   meth[[i]] = as.character(methods(clas[i]))
# }
# whateva

# 2. What happens if you define a new S4 class that doesn't containian an existing
# class? (Hint: read about virtual classes in ?setClass.)
# The documentation says that trying to call the generator of an S4 class that
# doesn't reference an exisiting class will result in an error

# 3. What happens if you pass an S4 object to an S3 generic? What happens if you
# pass an S3 object to an S4 generic? (Hint: read ?setOldClass for the second case.)
setClass("Person", slots = list(name = "character", age = "numeric"))
sven = new("Person", name = "Sven", age = 28)
isS4(sven)
is_s3_generic("plot")
plot(sven)
# It looks like it tries toerce the s4 objects into an s3 object that it can use
setClass("Robot", slots = list(beeboop = "numeric", serial = "character"))
svenbot = new("Robot", beeboop = 1:3, serial = "SVENBOT3431a3")
plot(svenbot@beeboop)
plot(svenbot)
# Hmm so it seems like as.numeric doesn't know how to deal with robots. What if it's really simple
setClass("dat data", slots = list(data = "numeric"))
sticky = new("dat data", data = 1:3)
plot(sticky)
# if it's pobbible, we'll probably have to let these data conversion methods know how
# to deal with robots or dat data.

# how about trying to send and S3 object to an S4 generic?
?setOldClass
# So this method is for taking S3 objects and making them into S4 objects. Here's the 
# example in the documentation
require(stats)
setOldClass(c("mlm", "lm"))
# Calling setOldClass with the character list c("mlm", "lm") says these type of objects 
# will be converted. The next line creates a s4 generic called dfResidual and dispatch
# on the generic.
setGeneric("dfResidual", function(model)standardGeneric("dfResidual"))
setMethod("dfResidual", "lm", function(model)model$df.residual)
# Then we set a method for the generic, dfResidual, for how to handle
# lm objects: return the degrees of freedom for the residual

## dfResidual will work on mlm objects as well as lm objects
myData <- data.frame(time = 1:10, y = (1:10)^.5)
myLm <- lm(cbind(y, y^3)  ~ time, myData)
# So this is an s3 object of class lm
isS4(myLm)
class(myLm)
showClass("data.frame")# to see the predefined S4 "oldClass"

## two examples extending S3 class "lm", class "xlm" directly
## and "ylm" indirectly
setClass("xlm", representation(eps = "numeric"), contains = "lm")
setClass("ylm", representation(header = "character"), contains = "xlm")
ym1 = new("ylm", myLm, header = "Example", eps = 0.)
str(ym1)

#########
## 7.5 ##
#########
# RC

# This is the last (and most recent) form of OO programming. They're different
# than the two previous methods in that RC methods belong to the object, not
# the function. RC objects objects are also mutable. Unlike the copy-on-modify
# system that we're used to, these objects can keep the same address on 
# modification and have their data change.

# We can create these objects in more or less the same way. Here's a generic account
Account <- setRefClass("Account")
# This saves the generator to 'Account'
f = Account$new()
# It's kinda strange that RStudio writes that f is an environment but
str(f)
# lists this as a reference class 'Account'
# We are also allowed to attach 'fields' which are equivalent to slots in S4 land
Account <- setRefClass("Account", fields = list(balance = "numeric"))
a <- Account$new(balance = 100)
a$balance
# So now a is an account class member and has a balance that can be accessed in the
# same way lists get their elements through $. Note that class works perfectly well
class(a)
# We can also modify the fields
a$balance = -1
a$balance
# Notice how assignment is somewhat different
b <- a
b$balance
a$balance = 0
b$balance
# This is different than s3/s4 objects because b would remain static to whatever
# a was set as when b was defined. This demonstrates the difference:
c = 4
d = c
d
c = 5
d
# As a result of this funny difference, there is a copy method to get that same type
# of behavior out of reference class objects.
c = a$copy()
c$balance
a$balance <- 2
c$balance
# now in this example, c just is a copy of a, not a pointer to a

# Here is a more complicated example with methods. Remember that RC objects have
# methods, not generics
Account <- setRefClass("Account",
                       fields = list(balance = "numeric"),
                       methods = list(
                         withdraw = function(x) {
                           balance <<- balance - x
                         },
                         deposit = function(x) {
                           balance <<- balance + x
                         }
                       )
)
a <- Account$new(balance = 100)
a
a$withdraw(45)
a
# So it looks like these methods modify the objet in place, we don't need to do something
# like a <- a$withdraw(45) as you might with the s3/s4 methods

# The inheritance behavior is pretty intuitive. We need to include a 'contains' argument
# in the setRefClass definition.
NoOverdraft <- setRefClass("NoOverdraft",
                           contains = "Account",
                           methods = list(
                             withdraw = function(x) {
                               if (balance < x) stop("Not enough money")
                               balance <<- balance - x
                             }
                           )
)
# So this means noOverdraft objects will inherit account properties, as well as a different 
# withdrawl method that will (presumably) take prescedent over the withdraw method from
# account.
john <- NoOverdraft$new(balance = 100)
john
is(john,"Account")
john$withdraw(200)
# It looks like we can call the method from account
john$`withdraw#Account`(200)
john
# yeah so, by default it will call it's own method. If it fails to find a method
# in it's own class definition, it will look up the inheritance chain

# RC objects are technically S4 objects but are still considered RC
isS4(john)
otype(john)

# EXERCISES
# 1. Use a field function to prevent the account balance from being directly manipulated.
# (Hint: create a "hidden" .balance field, and read the help for the fields argument in setRefClass().)
Account <- setRefClass("Account",
                       fields = list(.hiddenbalance = "numeric",
                                     getbal = function(x) .hiddenbalance)
                       )
# I don't think that's quite what the author meant by hidden
# I can't really figure this out. I've looked at some forum posts and I don't see a way to make the
# balance field completely inaccesible from the outside. Here's something closer:
Account <- 
  setRefClass("Account",
              fields = list(balance = "numeric"),
              methods = list(
                withdraw = function(x) {
                  balance <<- balance - x
                },
                deposit = function(x) {
                  balance <<- balance + x
                }
              ))
Account$accessors("balance")
a<-Account$new("balance"=0)
a$setBalance(10)
a$getBalance()
# So this gets the feeling of the get/set style but there's still nothing stopping you from just writing
a$balance = 100
a

# 2. I claimed that there aren't any RC classes in base R, but that was a bit of a simplification.
# Use getClasses() and find which classes extend() from envRefClass. What are the classes used for?
# (Hint: recall how to look up the documentation for a class.)

baseClasses <- getClasses()
extRef <- lapply(X = baseClasses, FUN = extends, class2 = "envRefClass")
extRef = baseClasses[as.logical(extRef)]
extRef
# So these look like helpers for the RC OOP system. I see why the author wouldn't think of these as
# 'counting' because they're somewhat intrinsic to the use of the RC classes

#########
## 7.6 ##
#########
# Picking a system

# The author suggests that S3 systems almost always sufficient for what we want to do but there are some
# examples (bioconductor, marices) of people using S4 systems for good reasons. The RC sytem is thought
# to be the easiest to understand for people with other programming backgrounds. There

# Quiz
# I'm actually gonna go through this again because this chapter was quite a bit more confusing than the
# last few.

# 1. How do you tell what OO system (base, S3, S4, or RC) an object is associated with?
# Well I know we can use the isS4 and the is() function 
# The given solution is that we can do a process of elimination. Only base objects will return false
# with is.object(), if it's not base but !isS4 then it's S3. If it's !is(x, "refCLass") then it's S4

# 2. How do you determine the base type (like integer or list) of an object?
# use typeof()
typeof(`+`)
typeof(1:3)

# 3. What is a generic function?
# Generic functions are functions that have methods within them that cause the function to react
# differently to different types of objects. We talked about how the plot() generic reacts differently
# to integers and lm objects. This is the result of the different plot methods

# 4. What are the main differences between S3 and S4? What are the main differences between S4 & RC?
# S3 differs from S4 mainly in that to be a member of a S3 class, the only real requirement is taht
# the class attribute be set appropriately. S4 objects are created from new() and as a result
# their structure is a little bit more controlled. RC objects are different from S4 objects in that
# the methods for those objects are stored in the objects, not the generics. RC is also different in that
# it does not use copy-on-modify. This means that a lot of the functions called by RC objects change the values
# stored in the object. THis is like account$setbalance(5) vs. account$balance <- 5
