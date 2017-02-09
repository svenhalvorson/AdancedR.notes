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

# Lexical scoping refers to the method of looking up symbols for objects in R. The author says
# that there is another type called 'dynamic' scoping which refers to some interactive functions
# The way dynamic scoping is described makes me think that this is how the functinos of dplyr
# (like rename) work. For some reason you don't need to feed strings to them.

# We're also told that there are four asic principals of lexical scoping:

# Name Masking
# Here are the first two examples given:

f <- function() {
  x <- 1
  y <- 2
  c(x, y)
}
f()
rm(f)
# We can see that the funciton knows what x and y are since they're definied within the function
# but we're not left with x and y in memory


x <- 2
g <- function() {
  y <- 1
  c(x, y)
}
g()
rm(x, g)
# and here the function says 'x is not defined in my environment, I'll look up a level'

x <- 1
h <- function() {
  y <- 2
  i <- function() {
    z <- 3
    c(x, y, z)
  }
  i()
}
h()
rm(x, h)
# And the pattern continues with further nested functions


# Functions vs. Variables
# The type of masking shown previously works in the same way. Here's an example of the function
# m looking for the function l defined in its body instead of the l in the global environment
l <- function(x) x + 1
m <- function() {
  l <- function(x) x * 2
  l(10)
}
m()


# This rule is sorta broken when you're trying to treat a variable like a functino
n <- function(x) x / 2
o <- function() {
  n <- 10
  n(n)
}
o()
# So here the 'closest' n is the variable n <- 10 however treating this like a function doesn't make much
# sense soinstead it finds the function n in the global environment. When it looks for the argument for n()
# R again looks within o first and finds 10


# A Fresh Start
# This section is essentially saying that each time a function is called, it creates a new environment
# and then closes (deletes) the environment when it's done running.

j <- function(){
 a <- 1
 exists("a")
}
j()
exists("a")
#So while j is running, it a is an object in memory. When it's done running, R doesn't know what "a' is.


# Dynamic lookups
# R looks up values when functions are run, not when they're created
a <- 10
f <- function(){
  return(a)
}
f()
a <- 11
f()
# so as a result, f doesn't have any value for a in mind until it's called. The author suggests that
# writing functions in this manner can cause problems. This makes a lot of sense to me as it seems sorta
# reckless to just hope the environment has the variables your function needs to execute. There doesn't
# seem to be much of a drawback to just having the function take in the necessary variables as arguments
# or just define the variables within the function's body.

# the book mentions a functionfindGlobals which can tell us which globals the function is dependent on
library("codetools")
findGlobals(f)
# hm I was not expecting it to tell me that it's dependent on the braces. I guess the later section titled
# every operation is a function call will probably make this clear.
findGlobals(label.schools)
# neat, I guess this could be nice if I find a better function to replace things I'm doing, findGlobals()
# could help me find the routines that are dependent
# The author also points out that you can't make a function truly independent as it will always
# need some things from the global, but that doesn't mean the gloabl environment needs to hold its variables

# Here's a sneaky function the author demos
`(` <- function(e1) {
  if (is.numeric(e1) && runif(1) < 0.1) {
    e1 + 1
  } else {
    e1
  }
}
replicate(50, (1 + 2))

# Since the parentheses are actually a function, they can be redefined. This acts like the math parentheses
# 90% of the time but occassionally adds 1 to the output
rm('(')


# EXCERCISES

# 1. What does the following code return? Why? What does each of the three c’s mean?
c <- 10
c(c = c)
# I think this is going to return a named vector of length one. The contents will be 10 labeled as c
# the first c is a variable with the value 10 stored
# the second is the function combine, the third is a name for the first entry of the vector which is 10

# 2. What are the four principles that govern how R looks for values?
# Name masking, functions vs. variables, a fresh start, and dynamic lookup
# Summaries
#  Name masking: R looks at the closest environment and then starts going outward if it fails to find
#  Functions vs. Variables: r will differentiate between functions and variables, even if they have the same name
#  A fresh start: functions open and close environments on execution so local variables will be lost upon completion
#  Dynamic lookup: functions do not look for variables until they're executed so chaning global dependencies change
#    how they are evaluated.

# 3. What does the following function return? Make a prediction before running the code yourself.
f <- function(x) {
  f <- function(x) {
    f <- function(x) {
      x ^ 2
    }
    f(x) + 1
  }
  f(x) * 2
}
f(10)

# I think it's going to return 202 because it should run the outermost, write over f with the second function,
# write over it again, then run x^2, add 1 to it, then go back a layer, run x^2 + 1 then double it

#########
## 6.3 ##
#########
# Every operation is a function call

# Here we're told that everything that happens is a function call. Even the most basic stuff like addition counts
# and we can refer to those functions if we want with the backticks
`*`(2,3)
# o this makes it more explicit that * is a function with 2 arguments that returns their product
# Here's another example
`for`(i, 1:2, print(i))
# so the for loop is a function with arguments index, range, and execution
?`for`
# kinda strange that you can't call the help file on this, maybe none exists

# This ability to refer to the simplest (primitve?) functions is needed to specity them as the arguments for some
# other functions like the apply family
lapply(X = list(1:3),FUN = `+`, 1)
# We have to refer to the function in this way because otherwise the + symbol means the output of the function, not
# the function itself

#########
## 6.4 ##
#########
# Function Arguments

# The author makes the distinction between formal and actual arguments. In the call:
lapply(X = list(1:3),FUN = `+`, 1)
# X is a formal argument and list(1:3) is an actual argument

# arguments can be matched by position, full name, or partial name.
# I thought you had to spell out the whole name but I guess we can do stuff like:
label.schools(df = data.frame(esquela = c("Gilberto University")), sch = "esquela")
# instead of spelling out the entire formal argument 'school'

# The author suggests to not use position matching except for the first argument or two but I just don't like it ever.
# I find it much harder to read and probably introduces a little extra chance of error. I do see it as overkill
# (as the author suggests) in some cases:
mean(x = 1:10)
# but other than super simple functions, I'll probably stick to writing out the arguments. We're also advised to
# still list the arguments in order even if we're using name matching

# another trick to call functions cleanly is to spelll out the arguemnts in a list and then use do.call
args <- list(1:10, na.rm = TRUE)
do.call(what = mean, args = args)
# do.call just executes a function with the arguments specified.
# The author doesn't really suggest why you would want to call functions this way but I can see it being nice.
# Some functions end up with really long, arduous lists of functions and spelling them out in a very explicit
# fashion first could make the code more readable.

# something else neat that  I didn't know: you can define the arguments of the function in terms of eachother
f <- function(x, y = x+2){
  return(c(x,y))
}
f(1)
f(1,2)
# This seems useful but I don't have a neat example in mind. I suspect that it will come when you're often
# making a choice based off one argument but occassionally you might not make that choice.

# The arguments of functions in R are 'lazy' in the sense that they are only evaluated if they're called upon
f <- function(x) {
  10
}
f(stop("This is an error!"))
# Even though the stop function would normally interrupt a function and print that warning, x is ne ver evaluated so
# the function just goes straight through. Contrast that with this modification
f <- function(x) {
  x
  10
}
f(stop("This is an error!"))
# The output would still be 10 with most other arguments but the fact that R actually says 'we're supposed to execute x'
# makes the stop actually happen
# the author also says you could use force() to make R evaluate an expression

# The method R uses for evaluating default arguments is different from specified arguments
# Default arguments are evaluated within the function's environment while specified are evaluated in the environment
# containing the function. The function below will know only about a and x
f <- function(x = ls()){
  a <- 1
  x
}
f()

#but if we just call it with ls() as the named argument, it evaluates ls() before executing f
b <- 2
f(x = ls())
#so it's NOT aware of a, but it is aware of b

# conditional statements also use lazy evaluation. As soon as the statement is determined to be false
# the rest of the conditions are discarded. This is a problem I ran into before but I think this example
# clears it up pretty well:

x <- NULL
if (!is.null(x) && x > 0) {
  print("SUPER")
}
# So this is nice because if we write:
x>0
# it doesn't return TRUE or FALSE and thus we'll get an error if we use it in the if statement
# by checking the null first, R never tries to ask NULL > 0 ?

# The elipses are used to allow unspecified arguments, usually passed to other functions within your function
# I've worked with these a few times and found them to be a little tricky, albeit useful.
# The author suggests, as I've done, to capture the elipses arguments immediately in a list
f <- function(...) {
  names(list(...))
}
f(a = 1, b = 2)


#EXERCISES
# 1. Clarify the following list of odd function calls:

x <- sample(replace = TRUE, 20, x = c(1:10, NA))
#could be:
x <- sample(x = c(1:10, NA), size = 20, replace = TRUE)

y <- runif(min = 0, max = 1, 20)
#could be:
y <- runif(n = 20, min = 0, max = 1)

cor(m = "k", y = y, u = "p", x = x)
#could be:
cor(x = x,  y = y, method = "kendall", use = "pairwise.complete.obs")
#probably would also benefit from not picking x and y as the names of the vectors but w/e


# 2. What does this function return? Why? Which principle does it illustrate?
f1 <- function(x = {y <- 1; 2}, y = 0) {
  x + y
}
# Okay so I hadn't seen the curly braces this way. My understanding is that this {y <- 1; 2}
# part just says assign y 1, return 2.
# I suspect that this is trying to illustrate the difference between default and specified
# arguments. The result should either be 2 or 3.

f1()
# It's 3. So.... perhaps this is saying that we're not using the default argument
# ohhh I think it's because when we call x for the addition, it changes the value of y
# Let's try this to see:
f2 <- function(x = {y <- 1; 2}, y = 0) {
  print(y)
  x + y
}
f2()
# Yep, when we print y, it says 'change the value of y to 1. x is not evaluated until the function is called
# so y has no value until it's invoked. I wonder what will happen if we reverse the addition:
f3 <- function(x = {y <- 1; 2}, y = 0) {
  y + x
}
f3()
# yea so the order of the addition even matters

# 3. What does this function return? Why? Which principle does it illustrate?
f2 <- function(x = z) {
  z <- 100
  x
}

# This should return 100 because x is not invoked intill after z is defined. The function call is using
# the default value for x so x is loaded lazily
f2()


#########
## 6.5 ##
#########
# Special Calls

# The author describes 3 categories of functions
# prefix functions start wtih the name of the functino followed by arguments (mean(1:3))
# infix functions have the function between the arguments (1:3). User defined infix functions should have %%
# These functions can also have more irregular names but escape characters are needed in some cases
`%/\\%` <- function(a, b) paste(a, b)
"a" %/\% "b"
# this is different from a prefix function because we can't use the characters \ or /
# Here's another function supplied by the author:
`%||%` <- function(a, b) if (!is.null(a)) a else b
# So the idea here is that we can call a function and gaurd ourself against nulls

# Replacement functions
# These functions have a very strange looking syntax. They're given a name that ends with <- and
# often modify the argument
`second<-` <- function(x, value) {
  x[2] <- value
  x
}
x <- 1:10
second(x) <- 5L
x
# So it takes in x, modifies the second value to the integer 5
# most of these functions have two arguments, the object and the modification

# there is a function in pryr that let's us see an 'address' which I gather is some sort of serial number
# for where the object is stored
library("pryr")
x <- 1:10
address(x)
second(x)<- 5L
address(x)

# The author is trying to make the point that it's not really modifying x, it's making a copy with the requested
# change and saving it elsewhere. I already knew about this but it does remind me of a question I've been having
# How to you choose when to overwrite an existing object with changes vs. giving it a new name.
# This shows up a lot when I'm cleaning data because I'm usually inclined to just overwrite whne I use things like
# rename() but if I use melt(), for some reason I want to give it a different name. Overwriting has the advantage
# of not needing to come up with and track a bunch of different names. It has the drawback of having to re-execute
# code when you mess up and also not have the ability to use unmodified versions later in the script

# Anyways... The author also notes that primitive functions do not modify the address which I did not know
a = address(x)
x[2] <- 7L
a == address(x)
# hmmm that doesn't seem to be what the book says
# maybe this has been changed? I executed this directly as it's written in the book and it still gives different addresses

# Back to replacement functions. We can write them with more than two arguments. The first argument will always be the object
# to be modified and the last one will come on the RHS of the <-
`regex<-` <- function(str,pat,value){
  str = gsub(pattern = pat, replacement = value, x = str)
  str
}
string = c("Should I stay", "or should I go", "if I go there will be trouble", "if I stay there will be double")
regex(string, "I") <- "BIG POPPA"
string

# this might actually be a nice addition to my package as I end up doing these kinds of things a lot
# Note that you cannot modify the object in the function call because it will assign the changes to
# The modified version of the object

# The author points out that I've been using one of these functions for quite a while
# Every time we cal names(x) <- newnames, we're using a replacement function

#EXERCISES

# 1. Create a list of all the replacement functions found in the base package. Which ones are primitive functions?

# hmm so we have to detect what's different about replacement functions in their syntax
# We can start with this same snippet
objs <- mget(ls("package:base"), inherits = TRUE)
funs <- Filter(is.function, objs)



# 2. What are valid names for user-created infix functions?
# The text says that they must have %% and anything in between. Escape characters may be necessary

# 3. Create an infix xor() operator.
`%xor%` <- function(a, b){
  #we could put a control on fro 0/1 too but whatever
  if(!(is.logical(a) & is.logical(b))){
    stop("a and b must be logical")
  }

  if((a & b) | (!a & !b)){
    ret = FALSE
  }
  else {ret = TRUE}

  ret

}

# 4. Create infix versions of the set functions intersect(), union(), and setdiff().

# Let's do one:
`%intr%` <- function(a,b){
  x = intersect(a,b)
  return(x)
}
set1 = 1:5
set2 = -3:2
set1 %intr% set2

# I think that the challange here was mostly to use of an infix function more than how we would program
# intersect() so I'll just leave it there. Union and setdiff would be done in basically the same way.

# 5. Create a replacement function that modifies a random location in a vector.


`rand<-` <- function(vec, value){
  index = sample(x = 1:length(vec), size = 1)
  vec[index] = value
  vec
}
x = 1:15
rand(x)<- 100
x


#########
## 6.6 ##
#########
# Return Values

# This section is about the use of return(). The author suggests that it's better to only use this function
# in super simple cases of the function or for error messages. I tend to use it by default because I feel
# that it makes it obvious and deliberate what we're trying to give back from a function. I don't really feel
# convinced that I shouldn't use return at the end of every function.

# We're also told that functions should aspire to have 'no side effects' as in modifying the workspace
# aside from objects that we assign as the output of functions. I know I'm guilty of this in one case:
# the svenpacks() function I wrote loads library packages. I'm interested to see if the assumptions
# that the packages do not interfere with eachother eventually becomes a problem.

# The author also mentions the invisible() function which can be used to prevent R from printing function
# output. This seems nice when the output is really large or unhelpful you can force invisible text to appear
# with parentheses
(invisible(1))
invisible(1)

# you can also assign multiple variables to a value
a <- b <- c <- 3
# that's kinda cool but I'm struggling a little bit to see how this fits in with the section

# We are also made aware of the on.exit() function which executes some statement when the function terminates
# The author gives an example of preserving your working directory, doing some code in another directory
# and then revert back to the original on exit.
# Apparently the default for on.exit is add = FALSE which means that multiple on.exit() statements will
# overwrite eachother unless we change this parameter
?on.exit

# EXERCISES

# 1. How does the chdir parameter of source() compare to in_dir()? Why might you prefer one approach to the other?

# It looks like the chdir parameter changes the directory before it works and then back to the original.
# in_dir changes the directory upon exit on exit. We probably want source when we want to work somewhere
# and get back to where we started but in_dir seems better when

# 2. What function undoes the action of library()? How do you save and restore the values of options() and par()?

# You can remove a package from the workspace with
detach("package:SvenR", unload = TRUE)

# Saving and restoring the values of options isn't that hard:
opt <- options()
options()$ylbias
options(ylbias = .3)
# restore
options(opt)
options()$ylbias

# Hopefully restoring the graphical parameters is just as easy
p <- par()
par(yaxt = "t")
par()
par(p)
par()
# yay

# 3. Write a function that opens a graphics device, runs the supplied code, and closes the graphics device
# (always, regardless of whether or not the plotting code worked).

# I'm not 100% sure I understand what the author means by graphics device but I'm thinking this means
# writing a png or something like that

png.plot <- function(x, y, file){
  on.exit(dev.off())
  png(filename = file)
  plot(x, y)
}

png.plot(iris$Sepal.Length,iris$Sepal.Width, "nonNull.png")
png.plot(iris$Sepal.Length, NULL, "broked.png")
dev.cur()
# So it looks like the default graphics parameter is the RStudioGD but we can actually tell the png got closed
# by this:
png()
dev.cur()
dev.off()
dev.cur()

# 4. We can use on.exit() to implement a simple version of capture.output().
capture.output2 <- function(code) {
  temp <- tempfile()
  on.exit(file.remove(temp), add = TRUE)

  sink(temp)
  on.exit(sink(), add = TRUE)

  force(code)
  readLines(temp)
}
capture.output2(cat("a", "b", "c", sep = "\n"))

# Compare capture.output() to capture.output2(). How do the functions differ? What features have
# I removed to make the key ideas easier to see? How have I rewritten the key ideas to be easier to understand?

# ok let's take a look
?capture.output
# So it looks like the options to choose file names, append, and the options to print is mandatory
# capture.output2 just shows us the skelton of capture.output. We create a tempfile and choose it
# as the target for sink, execute the code, print it so we can see what's in the tempfile, and
# clean up afterwords.

#The quiz answers seems pretty intuitive now so maybe I learned something! maybe...
