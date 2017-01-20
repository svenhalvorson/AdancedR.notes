###########
## 4.1 ###
##########

#Okay this chapter is all about making sure we've got a good foundation to build upon
#Basically the author just lists a bunch of functions that we should be aware of
#I'm just going to make a note or example of which ones I didn't know before
#looking at this list

#match
#is simliar to %in% but returns positions instead of simple TRUE/FALSE
x = 1:3
y = 5:0
match(x,dy)
#so it looks like it returns positions of the first input


#with
#this says takes in 'data' and expression. It evaluates the expression within the environmetn
#here's an example from the documentation
with(data.frame(u = c(5,10,15,20,30,40,60,80,100),
                lot1 = c(118,58,42,35,27,25,21,19,18),
                lot2 = c(69,35,26,21,18,16,13,12,12)),
     list(summary(glm(lot1 ~ log(u), family = Gamma)),
          summary(glm(lot2 ~ log(u), family = Gamma))))
#so it seems pretty similar to attaching the data frame and then you can call functions on the columns
#from reading other sources this seems to be something designed to help SAS users adapt
#could use this to make changes to a data set
df <- data.frame(x=1:3,y=3:1)
within(df,rm(y))
#note the within and with are not quite the same in that within makes a copy of the data
#and returns the modified version. With simply returns the output of expr
#so when we write
print(with(df,rm(y)))
#we get a null because it's just saying 'we removed y'
#where as within takes df and returns df with the excecuted expression


#assign
#this functions gives names to objects
#the example from the documentation creates 6 vectors
for(i in 1:6) { #-- Create objects  'r.1', 'r.2', ... 'r.6' --
  nam <- paste("r", i, sep = ".")
  assign(nam, 1:i)
}
#and then the assign function takes the name that they chose and gives it to nam
#but then nam now has a new name so it still exists in memory the nexdt go around of the loop
#if you comment out the assign statement you'll just end up with nam as the last iteration of the loop

ls(pattern = "^r..$")

#get
#seemingly simple function retrieves the object named by the first argument
#it's a little hard to see how this is useful but I'm guessing that it's nice in
#spots where you're creating objects that you don't want to explicitly name
#maybe you have a function that creates a differently named object based on the input
#then you could use get() to get a hold of it despite not knowing what the name will be
rm(list = ls())
for(i in 1:6) { #-- Create objects  'r.1', 'r.2', ... 'r.6' --
  nam <- paste("r", i, sep = ".")
  assign(nam, 1:i)
}
#get the 4th object in memroy
get(ls()[4])
#the documentation also lists mget() which retrieves a list of objects if you want more than one
get(ls()[4:5])
mget(ls()[4:5])
#so get only takes the first element of the list where as get returns a lits of both


#all.equal, identical
#these are methods that try to compare to objects, all.equal can be used to do exact comparisons
#or you can set some sort of tolerance to allow fuzzy equality. Identical only does exact comparisons
all.equal(22/7,pi)
identical(22/7,pi)
identical(1,1L)
#looks like the method of storing the data plays into this.
ww = mtcars
all.equal(mtcars,ww)
ww$mpg[1] = 21.1
all.equal(ww,mtcars)
#hmm so it looks like theres some method of computing the difference that aggregates the columns


#complete.cases
#basically what it seems, returns logical of whether or not the rows are complete (not NA)
df = data.frame(x = 1:3, y = c("Bubba",NA,"Tep"), z = c("a","b",NA))
df$complete = complete.cases(df)
df
#this actually seems kinda nice for cleaning purposes


#cummulative functions
#I knew these were in here somewhere but nice to know
cummin(c(1,3,0,5,4,-1))


#parallel min/max
#these take a max/min of vectors for each index
pmin(10:1,3:12)


#rle
#this is 'run length encoding' which is list of how many repeated values in a row you have
ww = sample(x = letters[1:3],size = 20,replace = TRUE)
rle(ww)
#oh, so it's not quite what I had hoped for
#it's element combined with number of repitions
#How could we transformt the rle back into the sequence?
seq = c()
rl = rle(ww)
for(i in 1:length(rl$values)){
  seq = c(seq,rep(rl$values[i],times = rl$lengths[i]))
}
all.equal(seq,ww)


#missing
#this is a function to be used while defining other functions
#basically you can use it to determine if one of the arguments was unspecified
#probably could use this in some of the functions in my package instead of the clunky way I used is.na()
ff = function(x){
  if(missing(x)){
    print("You forgot x, moran")
  }
  else(return(x))
}
ff(1)
ff()


#on.exit
#performs a specified action on exit of a function
logwarning <- function(x){
  warn = ""
  if(!is.numeric(x)){
    warn = "x was not numeric"
  }
  if(x<0){
    warn = "x is negative"
  }
  on.exit(print(warn))
  return(log(x))

}
#yeah I realize this is pretty pointless but it shows what the on.exit does
logwarning(exp(1))
logwarning(-1)
logwarning("a")


#invisible
#hides output. From the documentation:
f1 <- function(x) x
f2 <- function(x) invisible(x)
f1(1)  # prints
f2(1)  # does not
#seems mostly like something that makes functions more readable but not super important


#xor
#this is the exclusive or that we normally think of in English
#different from | only in the case that both arguments are true
T | T
xor(T,T)


#sweep
mat = matrix(1:10,nrow = 2)
sweep(x = mat,MARGIN = 2, STATS = apply(X = mat,MARGIN = 2,FUN = mean),FUN = "-")
#this is sorta like apply but what it does is applies some statistics to each element of a matrix
#in the exdample above we compute all the column means and subtract them from each element


#rep_len
#nice shortcut for rep if you know you want times = ...
rep_len("x",3)


