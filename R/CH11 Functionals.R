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



