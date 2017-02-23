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
# ancestors (the environments they're nested in) but we're told that we can't talke so easily
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
ls.str(e)
