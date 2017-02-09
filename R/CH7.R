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


