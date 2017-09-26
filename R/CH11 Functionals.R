##########
## 11.0 ##
##########
# Functionals

# This chapter is about a class of functions, called functionals, that
# are essentially more descriptive for loops. They take in a function and
# data and create some interaction that returns data.

# The author notes that for loops are much maligned in R but in reality,
# the main reason they're not preferable is that they're not easy to read.
# While it's true that many functionals are faster than explicit for loops,
# the real value is that you can read the functional and know what the coder is
# trying to do.

##########
## 11.1 ##
##########
# lapply

# I'm pretty familiar with lapply but we'll make a note of anything new.

# Here's an R equivalent to lapply:
lapply2 <- function(x, f, ...) {
  out <- vector("list", length(x))
  for (i in seq_along(x)) {
    out[[i]] <- f(x[[i]], ...)
  }
  out
}

# As the author mentions, a good chunk of the performance improvement is initializing
# out and filling it in. I often write stupid loops with stuff like vec = c(vec, new)
# but this copy-replace stuff slows the loop down.

# Here's a little tricker to get around the fact that lapply always injects the elements
# of X to the first argument of f.
trims <- c(0, 0.1, 0.2, 0.5)
x <- rcauchy(1000)
unlist(lapply(trims, function(trim) mean(x, trim = trim)))
# So I had never seen the trim parameter used but I guess it's a way of kicking out the extreme
# values of a vector. Basically we just write a new function that has our desired intput and
# then set it to the second (or whatever) parameter of the function we wanna use. Very nice.
# The cauchy distribution is centered at zero so when we trim the data, the mean gets closer to 0

# Next we have three examples of how to write the header of a for loop
# for(x in xs)
# for(i in seq_along(xs))
# for(nm in names(xs))

# The author writes tha the first form (which I often use) is undesireable as
# 'it leads to inefficient ways of saving output.' I find this a bit strange
# as I don't see why....

# given example:
xs <- runif(1e3)
res <- c()
for (x in xs) {
  # This is slow!
  res <- c(res, sqrt(x))
}

# Okay I tried to write this with declaring res beforehand but I see the problem. Basically
# by writing a loop this way, we don't have a way of telling which indices of res we want to
# overwrite. The author says that instead of just replacing one value in a vector, c(res, sqrt(x))
# copies all the values again and then adds the one new one. So this will end up doing n(n+1)/2 replacements
# instead of n. That difference can be pretty big after a while

seq = 1:1000
exp = seq*(seq+1)/2

plot(seq,seq)
lines(seq, exp)
#not even close

# Actually I kinda wanna see how much faster that is now
t1 = Sys.time()
set.seed(1)
xs <- runif(1e3)
res <- c()
for (x in xs) {
  # This is slow!
  res <- c(res, sqrt(x))
}
t1 = Sys.time() - t1

t2 = Sys.time()
set.seed(1)
xs <- runif(1e3)
res2 = unlist(lapply(X = xs, FUN = sqrt))
t2 = Sys.time() - t2

# So for 1000 observations, it's about 3.5 times faster to us a functional.

# So the suggested method is to an indexed sequence:
t3 = Sys.time()
set.seed(1)
xs <- runif(1e3)
res <- numeric(length(xs))
for (i in seq_along(xs)) {
  res[i] <- sqrt(xs[i])
}
t3 = Sys.time() - t3

# Yeah so those improvements in speed through C are evident here as
# the indexed loop was still took more than twice the time of lapply

# here are some variations of lapply that mimic those loop types:
lapply(xs, function(x) {})
lapply(seq_along(xs), function(i) {})
lapply(names(xs), function(nm) {})

# So I basically always end up writing for loops when I want to work with the indices
# but this makes a good point that you can just lapply across that sequence.

# EXERCISES
# 1. Why are the following two invocations of lapply() equivalent?
trims <- c(0, 0.1, 0.2, 0.5)
x <- rcauchy(100)

lapply(trims, function(trim) mean(x, trim = trim))
lapply(trims, mean, x = x)

# So it looks like when we use lapply it doesn't exactly take the first arguemnt
# It looks like it takes the first argument not named in ...
# so here when we specify that the x argument of mean will be x of lapply, it decides
# that the next unnamed argument is trim, so then trims goes there.

# 2. The function below scales a vector so it falls in the range [0, 1].
scale01 <- function(x) {
  rng <- range(x, na.rm = TRUE)
  (x - rng[1]) / (rng[2] - rng[1])
}
# How would you apply it to every column of a data frame?
mtcars[] = lapply(X = mtcars, FUN = scale01)
# How would you apply it to every numeric column in a data frame?
nums = unlist(lapply(X = iris, FUN = is.numeric))
iris[nums] = lapply(X = iris[nums], FUN = scale01)

# Use both for loops and lapply() to fit linear models to the mtcars using the formulas stored in this list:
formulas <- list(
  mpg ~ disp,
  mpg ~ I(1 / disp),
  mpg ~ disp + wt,
  mpg ~ I(1 / disp) + wt
)
rm(mtcars)
models = lapply(X = formulas, FUN = lm, data = mtcars)

# with a loop:
out = vector(mode = "list", length = length(formulas))
for(i in seq_along(formulas)){
  out[[i]] = lm(formula = formulas[[i]], data = mtcars)
}

# 4. Fit the model mpg ~ disp to each of the bootstrap replicates of mtcars
# in the list below by using a for loop and lapply(). Can you do it without an anonymous function?

bootstraps <- lapply(1:10, function(i) {
  rows <- sample(1:nrow(mtcars), rep = TRUE)
  mtcars[rows, ]
})

# seems easy enough!
models2 = lapply(X = bootstraps, FUN = function(dat){lm(formula = mpg~disp, data = dat)})

# 5. For each model in the previous two exercises, extract R2 using the function below.
rsq <- function(mod) summary(mod)$r.squared
rsq1 = lapply(X = models, rsq)
rsq2 = lapply(X = models2, rsq)

##########
## 11.2 ##
##########
# Friends of lapply

# Although lapply is my favorite, there are a few variants that we'll look at in this section

# sapply and vapply produce vectors, matrices, or arrays instead of lists
# map and mapply iterate over multiple inputs
# mclapply and mcMap are parallel (?) versions of lapply and map
# We'll also write our own apply statement

# sapply and vapply do similar things but sapply tries to guess how you want the output formulated
# and vapply takes directions. We're advised to use sapply interactively and vapply in scripts
# since sapply makes it easy to read on the fly but is vulnerable to unusual output.

# Examples:
sapply(mtcars, is.numeric)
vapply(mtcars, is.numeric, logical(1))
vapply(mtcars, is.numeric, numeric(1))
# So with vapply we gotta supply the FUN.VALUE argument (god i'm glad they moved away from the all caps names)
# which tells us how to format the output. I'm not entirely sure what types of values FUN.VALUE
# can accept

# Non examples:
sapply(list(), is.numeric)
vapply(list(), is.numeric, logical(1))
# So when sapply has a hard time figuring out how to simplify, it goes to a list. Vapply
# will throw an error. Let's see if we can make that happen:
set.seed(4)
ww = vector("list",10)
for(i in 1:10){
  ww[[i]] = sample(x = 1:10, size = 4, replace = TRUE)
}
sapply(X = ww, FUN = unique)
vapply(X = ww, FUN = unique, numeric(4))

# oh I guess he's got a better example
df2 <- data.frame(x = 1:10, y = Sys.time() + 1:10)
sapply(df2, class)
vapply(df2, class, character(1))
# yeah that was a rude awakening for me when I realized objects can have more than one class

# Here are some shells of what vapply and sapply look like under the hood:
sapply2 <- function(x, f, ...) {
  res <- lapply2(x, f, ...)
  simplify2array(res)
}

vapply2 <- function(x, f, f.value, ...) {
  out <- matrix(rep(f.value, length(x)), nrow = length(f.value))
  for (i in seq_along(x)) {
    res <- f(x[[i]], ...)
    stopifnot(
      length(res) == length(f.value),
      typeof(res) == typeof(f.value)
    )
    out[ ,i] <- res
  }
  out
}

# So sapply is basically lapply with whatever simplify2array does. I'm kinda scared to look
# at the source for that because it seems like it would have to handle a LOT of cases

# Next on our functional tour is map. I recently learned this beaut and love it.
# Map is capable of taking in multiple inputs. Remember how we used lapply across the
# trims as a the second argument for mean()? Map basically lets you vary all those
# parameteres easily.

# This example is taking the xs and then computing a weighted mean with ws
xs <- replicate(5, runif(10), simplify = FALSE)
ws <- replicate(5, rpois(10, 5) + 1, simplify = FALSE)

# Can't do this easily with lapply as we want the weights to vary for each
# piece of xs. Behold, map:

Map(f = weighted.mean, x = xs, w = ws)

# And here's the shell of map:
stopifnot(length(xs) == length(ws))
out <- vector("list", length(xs))
for (i in seq_along(xs)) {
  out[[i]] <- weighted.mean(xs[[i]], ws[[i]])
}

# So you can seee the idea that we just put the arguments of Map (after f) into the
# arguments of f. I kinda think we can name them instead of just position matching, right?
all.equal(Map(f = weighted.mean, x = xs, w = ws), Map(f = weighted.mean, w = ws, x = xs))
# Sweet, yeah I'm pretty anal about naming arguments but I think it catches errors and is easy to read.

# We're advised not to use mapply. Basically sapply:lapply as mapply:map
# but the the simplification on Map is not often needed. The author also notes that it
# breaks some semantic conventions.

# This last point in the section is about how we can write our own variants of the existing
# functionals. The example given is a rolling mean:
rollmean <- function(x, n) {
  out <- rep(NA, length(x))
  
  offset <- trunc(n / 2)
  for (i in (offset + 1):(length(x) - n + offset + 1)) {
    out[i] <- mean(x[(i - offset):(i + offset - 1)])
  }
  out
}
x <- seq(1, 3, length = 1e2) + runif(1e2)
plot(x)
lines(rollmean(x, 5), col = "blue", lwd = 2)
lines(rollmean(x, 10), col = "red", lwd = 2)

# okay let's unpack this first. So we feed it a vector of values (x) and n which 
# presumably should be integer(1). The offset is half of n, rounded down. This is trying to tell
# us how far back/forward we'll go to calculate the rolling mean and it's set so if you feed
# odd numbered bandwidth, it halves it and rounds down so n=4 and n=5 both yield an offset of 2.
# Then if we want to iterate, we can't compute a rolling mean for entries indexed less than
# or equal to the offset, and our last entry would be the one for which it's position plus the offset
# equals the length of x. I kinda don't get the purpose of n ATM, and how this differs with odd
# vs even n. This is not really the point but whatever

# The author then offers this alternative:

rollapply <- function(x, n, f, ...) {
  out <- rep(NA, length(x))
  
  offset <- trunc(n / 2)
  for (i in (offset + 1):(length(x) - n + offset + 1)) {
    out[i] <- f(x[(i - offset):(i + offset - 1)], ...)
  }
  out
}
plot(x)
lines(rollapply(x, 5, median), col = "red", lwd = 2)

# Which is more general since any function that can produce output across an
# attomic can be used. Median is show in this case.

# The last piece in this section is about parallelisation. The first point made is that 
# the order that we compute f(x) in does not matter since it does not depend on the other 
# values. This example is given:
lapply3 <- function(x, f, ...) {
  out <- vector("list", length(x))
  for (i in sample(seq_along(x))) {
    out[[i]] <- f(x[[i]], ...)
  }
  out
}
unlist(lapply(1:10, sqrt))
unlist(lapply3(1:10, sqrt))
# So essentially lapply3 permutes the indices (might not start at i == 1) but
# still assigns them to the same locations since out[[i]] will hit whatever
# index it's fed. The reason this example is given is to demonstrate that 
# the computer can potentially use multiple cores to compute f(x) and then 
# reassemble the list. This is called parallelisation and it can be helpful
# if f(x) is slow to compute. The parallel package can do this. THe functions that 
# take advantage of this start with mc

library(parallel)
unlist(mclapply(1:10, sqrt, mc.cores = 4))

# note that we can specify the number of cores. The author mentions that
# these functions are actually slower than their single core equivalents if
# the computations are simple. We should reserve them for times when it's going to 
# go slow. I might try and use this next time I do a mass upload to google
# drive although that may be delayed by internet speed more than the computer

# Exercises
# 1. Use vapply() to:
#   a. Compute the standard deviation of every column in a numeric data frame.
vapply(X = mtcars, FUN = sd, FUN.VALUE = numeric(1))
#   b. Compute the standard deviation of every numeric column in a mixed data frame. (Hint: you'll need to use vapply() twice.)
types = vapply(X = iris, FUN = is.numeric, FUN.VALUE = logical(1))
vapply(X = iris[types], FUN = sd, FUN.VALUE = numeric(1))

# 2. Why is using sapply() to get the class() of each element in a data frame dangerous?
# An object may have multiple classes so the output sometimes will be character(1) and  longer other times

# 3. The following code simulates the performance of a t-test for non-normal data. Use sapply()
# and an anonymous function to extract the p-value from every trial.
trials <- replicate(
  100, 
  t.test(rpois(10, 10), rpois(7, 10)),
  simplify = FALSE
)
sapply(X = trials, FUN = function(x){x$p.value})
# Extra challenge: get rid of the anonymous function by using [[ directly.
sapply(X = trials, FUN = `[[`, "p.value")

# 4. What does replicate() do? What sort of for loop does it eliminate? 

# I'm having a little bit of a hard time understanding how replicate() differs from rep(), which I
# use frequently. The documentation says:
# replicate is a wrapper for the common use of sapply for repeated evaluation of an expression 
# (which will usually involve random number generation).
# So maybe something like this this:

ww = replicate(n = 10, expr = rpois(n = 100, lambda = 3))
str(ww)
# So this looks like I conducted 10 samples of 100 observations from poisson(3) and replicate returns a matrix
# and it seems like we have some control over that structure. simplify = FALSE gives a list.
# Okay, so it looks like rep()'s output is going to be a vector (atomic only?). It seems like rep() is more
# geared towards repeating known elements in memory while replicate is aimed at using functions, however, it does look
# like rep can evaluate functions:
rep(x = rpois(n = 100, lambda = 3), times = 10)
# replicate eliminates for loops as you are explicitly saying how many copies to make.

# Why do its arguments differ from lapply() and friends?
# replicate() doesn't need to  have a function, it can simply be an expression like 1:3
# but the inputs for the expression can't change. I see why the documentation mentions
# random number generation as you likely want your samples to have the same parameters. 

# 5. Implement a version of lapply() that supplies FUN with both the name and the value of each component.
# Uhhh not sure if I'm interpreting this correctly... Does this mean that we want to be able 
# to have the output be labeled?

lapply_named <- function(X, FUN, names, ...){
  if(length(names) != length(X)){
    stop("names and X must have the same length")
  }
  ww = lapply(X = X, FUN = FUN, ... = ...)
  names(ww) = names
  ww
  
}
samps = replicate(n = 10, expr = sample(x = c(1:4,NA), size = 5, replace = TRUE), simplify = FALSE)
lapply_named(X = samps, FUN = mean, names = paste0("Sample ",1:10), na.rm = TRUE)

# Here's someone else's solution:
lapply_nms <- function(X, FUN, ...){
  Map(FUN, X, names(X), ...)
}
# which is nice and concise but it doesn't allow the user to supply names. Not sure if there's any real
# disadvantages here as you can just set the names for X first and then run this version

# 6. Implement a combination of Map() and vapply() to create an lapply() variant that iterates 
# in parallel over all of its inputs and stores its outputs in a vector (or a matrix). 
# What arguments should the function take?

# 7. Implement mcsapply(), a multicore version of sapply(). 
# Can you implement mcvapply(), a parallel version of vapply()? Why or why not?

##########
## 11.3 ##
##########
# Maniuplating matrices and data frames

# Now we'll get into some of the functionals that are helpful when working with 2D structures.
# apply() is the first function that is mentioned which was the first functional I started using
# it's a little annoying that the output is always a matrix (which I rarely want) but the idea
# is good. Basically we supply a 2D structure and apply iterates over one of the dimensions,
# applying a function. The author says that apply summarises each row (or column) to a single
# number but it doesn't have to collapse it. You can run FUN = is.na to just get a matrix of
# T/F buth the dimensions will be the same as X. Here's the given example:
a <- matrix(1:20, nrow = 5)
apply(a, 1, mean)
# so this is computing row means.
# As with sapply, we're warned that the output is not completely predicitable and thus apply 
# generally should not be used within functions. 
a1 <- apply(a, 1, identity)
identical(a, a1)
identical(a, t(a1))
a2 <- apply(a, 2, identity)
identical(a, a2)

# O s$@! this is important.... so basically it looks like what ever we iterate over becomes
# the columns of the output. 
mat = matrix(data = sample(x = c(NA,1:3), size = 30, replace = TRUE),nrow = 5)
apply(X = mat, MARGIN = 1, FUN = is.na)
apply(X = mat, MARGIN = 2, FUN = is.na)
# Yeah that's kinda whack but good to know at least

# The next function mentioned is sweep() which I found to be a little weird/silly last time
# I saw it but maybe I'll change my mind now. Sweep has the following arguments:
# x is an array, MARGIN is similar to apply's margin, STATS is supposed to be a a vector
# of summary values. Here's the given example:
x <- matrix(rnorm(20, 0, 10), nrow = 4)
x1 <- sweep(x, 1, apply(x, 1, min), `-`)
x2 <- sweep(x1, 1, apply(x1, 1, max), `/`)

# So what this is doing is generating the row minimums and subtracting them from each value by row
# Then we take that matrix, generate the row maximums and divide each value by that. Each statement
# we're creating the STATS value by summarising the row and then sweep applies the function given to
# the value in X and the corresponding value in STATS. Important to note here is that
# the value in X is the first argument to these binary operators. I like this example a bit 
# better than what I saw in the past but I'm wondering how general you get with sweep(). It seems very
# suited for this type of task, where you want to do something to a matrix by row/column, but probably
# not much else.

# Lastly we have outer() which takes in two vectors and creates combinations of them via a function
outer(1:3, 1:10, "*")
# creates a matrix of the products of 1:3 and 1:10. Can we do it with strings?
adjectives = c("hairy", "cold", "portable", "dilapitated")
nouns = c("bear", "code", "Saturday", "launchpad")
outer(X = adjectives, Y = nouns, FUN = paste)
# nice
# There are some nice links here including one I was already aware of. Am learning!

# There are also some functions that work like other apply statements but are done with
# an additional grouping structure. We're told that these are done with 'ragge arrays' which
# the number of rows/columns is not necessarily consistent. Here's the exdample:
pulse <- round(rnorm(22, 70, 10 / 3)) + rep(c(0, 5), c(10, 12))
group <- rep(c("A", "B"), c(10, 12))

tapply(X = pulse, INDEX = group, FUN = length)
# So tapply is taking in the pulse vector, organizing it by the group vector,
# and producing the length. So I think in this case, we're operating on ragged arrays because
# the sizes of each group are not the same. We're introduced to the split() function:
split(x = pulse,f = group)
# of note, whatever we choose as the grouping variable f, it is coerced into a factor.
# It also has a drop parameter but it's not so clear how this works. Mabe it's NA values?
group2 = rep(c("A", "B", NA), c(10, 11, 1))
split(x = pulse,f = group2)
split(x = pulse,f = group2, drop = TRUE)
# uh well it does drop that last value associated with NA.... but it does so with both valeus of drop
# Okays so from the documentation: ogical indicating if levels that do not occur should be dropped (if f is a factor or a list).
# so maybe this:
group = factor(x = group, levels = c("A","B","C"))
split(x = pulse,f = group)
split(x = pulse,f = group, drop = TRUE)
# Yep okay, so if f contains levels which do not have a representative, then the drop parameter will kick in

# So tapply uses split to group the elements of X and then sapplies them. Essentially this:
tapply2 <- function(x, group, f, ..., simplify = TRUE) {
  pieces <- split(x, group)
  sapply(pieces, f, simplify = simplify)
}
# so as with sapply, tapply may produce output of different dimensions than you expect

# The author mentions that the apply family was written by a number of authors and thus there are some
# inconsistenceis:

# With tapply() and sapply(), the simplify argument is called simplify. With mapply(),
# it's called SIMPLIFY. With apply(), the argument is absent.

#vapply() is a variant of sapply() that allows you to describe what the output should be,
# but there are no corresponding variants for tapply(), apply(), or Map().

# The first argument of most base functionals is a vector, but the first argument in Map() is a function.

# This kinda brings up my gripe with the suggested style of not naming the arguments. While it's a little
# silly that these discrepancies exist, it seems like you can catch your errors just by tabbing through
# the arguments. 

# Another issue mentioned is that we often want to work with lists, data.frames and arrays as the input
# and output of apply statements. Unfortunately, not every combination of these exists and data.frames are
# excluded entirely as outputs. (I often do this though df[] <- lapply()). The plyr package has some 
# apply functions that cover all these combinations in an orderly way. The all have the form of 
# selecting the input and ouput from ("l", "d", "a") and then attaching "ply" so laply would take in a list
# and output an array.
iris2 = as.list(iris[1:4])
library("plyr")
laply(.data = iris2, .fun = mean)
llply(.data = iris2, .fun = mean)
ldply(.data = iris2, .fun = mean)
# This seems pretty nice. I kinda wish the author said something more about decisions to use these
# vs the base package ones. It seems like you might just want to use plyr whenever possible but
# maybe there are some drawbacks. I haven't looked through the entire documentation of plyr
# but it seems like mabye map is not mimicked here. I'm also a bit embarassed to say that I thought
# plyr was an old version of dplyr. While they have some overlap, it's not exactly true.

# EXERCISES
# 1. How does apply() arrange the output? Read the documentation and perform some experiments.
# I mentioned this a bit ago. It looks like whatever we iterate over becomes the columns

# There's no equivalent to split() + vapply(). Should there be? When would it be useful? Implement one yourself.
# Hmm my intuition says that it would be nice to have the control over output that vapply gives...
# let's see if we can do it

tapply3 <- function(x, group, f, f.values, ...) {
  pieces <- split(x, group)
  vapply(X = pieces,FUN =  f, FUN.VALUE = f.values)
}
tapply3(x = iris[[1]], group = iris["Species"], f = mean, f.values = numeric(1))

# 3. Implement a pure R version of split(). (Hint: use unique() and subsetting.) Can you do it without a for loop?

split2 <- function(x, f){
  # first get the unique values in f
  browser()
  vals = unique(f)
  pick <- function(val){
    x[f == val]
  }
  out = lapply(X = vals, FUN = pick)
  names(out) = vals
  out
  
}
split2(x = as.numeric(iris[[1]]), f = as.character(iris[[5]]))
# So obviously this is kinda janky as I didn't really account for data types well
# and it can't handle 2D input but I get the idea

# 4. What other types of input and output are missing? Brainstorm before you look up some answers in the plyr paper.
# I feel like functions are sort of a different ball game than we're talking about here, that sounds like function factories
# or perhaps that a list containing functions would suffice. I'm not that familiar with data.table but that seems to be missing.
# We might also want some variations that work with database connectinos.

##########
## 11.4 ##
##########
# Manipulating lists

# The next section is about manipulating lists. This seems like an important topic as the longer I've been using R
# the more often I find lists the easiest structure. The fact that they're very generalized makes them well suited for
# large grain maipulations. The author mentions that map is often a useful tool and we've talked about that already.

# The first function mentioned here is Reduce() which which 
# "reduces a vector, x, to a single value by recursively calling a function, f, two arguments at a time."
# So it does some sort of pairwise function wit the first two elements and then the result and the third element ect.
# Reduce(f, 1:3) = f(f(1,2),3). 

# Here's the skeleton of what reduce does:
Reduce2 <- function(f, x) {
  out <- x[[1]]
  for(i in seq(2, length(x))) {
    out <- f(out, x[[i]])
  }
  out
}

# There are some other options on Reudce() like reversing the order and using intermediates:
Reduce(f = `+`, x = 1:10)
Reduce(f = `+`, x = 1:10, right = TRUE, accumulate = TRUE)

# In some sense this is kind of like changing a binary function to have an unlimited number of arguments
# to me it seems important to think about whether the function is associative or commutative because
# That's when you'll wonder about the right parameter. 

# Here's an example of finding all the common elements of a list:
l <- replicate(5, sample(1:10, 15, replace = T), simplify = FALSE)
Reduce(f = intersect, x = l)
l
# This also has some applications with merging and appending. I bet this is how bind_rows and bind_cols from dplyr
# work on lists. unique() also seems right to use with Reduce()

# The next piece for manipulating lists are predicate functionals. Predicates are functions that return logical(1)
# like is.character(). The three that are mentioned for working with lists are Filter(), Find(), and Position()
# Just as a side note, I kinda hate that the naming conventions for functinos are not uniform. It seems like the
# tidyverse has a nice consistant pattern but the base and other packages do not. We have Filter() from base and 
# filter() from dplyr that are not the same. 
# Filter picks only the elements that match the predicate, Find returns the first element that matches, Position
# returns the position of the first match.

df <- data.frame(x = 1:3, y = c("a", "b", "c"), z = c("boogie mama", "they're destroying our city", "dog eat dog"))
str(Filter(is.factor, df))
# So this is sorta like filter but for the columns of a data frame instead of the rows. 
str(Find(is.factor, df))
Position(is.factor, df)

# I can see how these would be nice but to be honest, I kinda don't get why you would want this more than
# lapply(FUN = is.factor). The fact that you're pidgeonholed into getting the first or last hit makes
# them seem less useful than juust having the complete sumamry. Filter seems aight though and it's probably
# just something like l[lapply(FUN = is.factor)]

# EXERCISES
# 1. Why isn't is.na() a predicate function? What base R function is closest to being a predicate version of is.na()?
# is.na is vectorized so it will check if each element is NA, thus it will usually be longer than logical(1)

# 2. Use Filter() and vapply() to create a function that applies a summary statistic to every numeric column in a data frame.

sum_stat <- function(df, f){
  
  numerics = Filter(is.numeric, df)
  vapply(X = numerics, FUN = f, FUN.VALUE = numeric(1))
  
}
sum_stat(df = iris, f = sd)
# That's all well and goood but if I was actually going to make a function like this I would prefer to just
# output something for each element but put in NA for the non numeric elements. That way you know that 
# no operation on them has been conducted

# 3. What's the relationship between which() and Position()? What's the relationship between where() and Filter()?
# which returns all the indices that match the predicate while Position returns the first index that is true
# among which(). where() returns a logical of the predicate on each element while Filter returns the elements 
# from the list that match the predicate.


# 4. Implement Any(), a function that takes a list and a predicate function, 
# and returns TRUE if the predicate function returns TRUE for any of the inputs. Implement All() similarly.

Any <- function(f, l){
  ifelse(test = is.null(Find(f, l)), yes = FALSE, no = TRUE)
}
Any(is.character, iris)
Any(is.factor, iris)

All <- function(f, l){
  # okay let's be silly
  ifelse(test = length(l) == length(Filter(f,l)), yes = TRUE, no = FALSE)
}
All(is.numeric, mtcars)
All(is.numeric, iris)


# 5. Implement the span() function from Haskell: given a list x and a predicate function f,
# span returns the location of the longest sequential run of elements where the predicate is true. 
# (Hint: you might find rle() helpful.)

where <- function(f, x) {
  vapply(x, f, logical(1))
}

span <- function(f, l){
  
  runs = rle(where(f = f, x = l))
  max(runs$lengths[runs$values == TRUE])
}
span(is.numeric, iris)
span(is.factor, iris)

##########
## 11.5 ##
##########
# Mathematical Functionals

# The author opens this section by stating that many of the mathematical tools built into R are
# actually done via iteration and functionals. This makes a lot of sense to me as things
# like Newton's method are sort of iterative processes of getting better guesses.

# We can use optimise function to help us with things like MLEs and it's no surprise
# that this would be an iterative process. The mere fact that finding roots is an iterative process
# means that a maximum would have to be as well because the human method involves taking some derivatives
# and then finding roots.
str(optimise(sin, c(0, pi), maximum = TRUE))
# so looks like we get the max value of the functino at 'objective' and the input that generates that at 'maximum'



