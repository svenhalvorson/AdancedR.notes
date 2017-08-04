##########
## 4.1 ###
##########
#The basics

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


#expand.grid
#this function seems pretty awesome. It makes a data frame of every combination of vectors you feed it
#make every combination of a-d, and 1-3
expand.grid(letters[1:4],1:3)
#I'm pretty sure I've done this kind of thing before by combining rep with times and  each but I'm sure it
#would get pretty gnarly if we had to get to 4+ vectors

##########
## 4.2 ###
##########
#Common Data Structures


#ISOdate
#this converts numeric representation of dates via multiple columns to a date object
ISOdate(1993,2,14)
ISOdate(1993:1995,2:4,14:16)
#seeems okay but a lot of the time we get date information as a single vector so
#lubridate looks a lot better IMO

#date()
#returns the current date according to the system
date()

#date extraction
#the base package has some functions like lubridate to get out portions of date objects
quarters(as.Date())


#agrep
#this is an approximate string matching. Seems very useful for some things I've worked on in the past
#let's see if we can figure it out. It says that it uses the 'generalized Levenshtien edit distance'
#wikipedia says it's the minimum number of insertions deletions and substitutions necesary to turn
#one string into another.
strings = c("darla","dorla","Darla","daral","dardog")
agrepl(pattern = "darla", x = strings,max.distance = 1)
#the output options are indices or strings of matches but I kinda prefer logical so agrepl gives this
#it's saying that within one move we can get from "darla" to any of the first 4 but not the last
#this is kinda weird in the case of daral as I thought this would have to be two moves...
adist("darla","daral", partial = FALSE)
#hmnmm this seems to be saying that the distance is 2 which is what I thought: one insertion one deletion
agrepl(pattern = "^darla$", x = strings,max.distance = 1,fixed = FALSE)
#ah okay so there's some sort of substring behavior involved with agrepl. The literal levenshtien distance
#between darla and daral is 2 however
#Okay so I think what the first example is doing with agrepl is that it's looking
#to match the string "darla" into "daral" and it says, take the substring
# "darl" and having one insertion of an a. Very cool
agrepl(pattern = "ABC",x = "QAQBQCEWGEGEIH",max.distance = 2)
agrepl(pattern = "ABC",x = "QAQBQCEWGEGEIH",max.distance = 1)
agrepl(pattern = "^ABC",x = "QAQBQCEWGEGEIH",max.distance = 2)
agrepl(pattern = "^ABC",x = "QAQBQCEWGEGEIH",max.distance = 3)
#sweet!

#chat
x <- "MiXeD cAsE 123"
chartr(old = "iXs", new ="why", x)
chartr(new = "a-cX", new = "D-Fw", x)
#okay so this looks to be sort of like a lookup table for single character swaps.
#Let's see if we can get a a secret code thing
code = paste0(sample(x = LETTERS[1:26], size = 26, replace = FALSE), collapse = "")
message = "SECRET MESSAGE"
encoded = chartr(old = paste0(LETTERS[1:26], collapse = ""), new = code, x = message)
chartr(old = code, new = paste0(LETTERS[1:26], collapse = ""), x = encoded)

#findInterval
#helps us classify continuous variables into intervals. Here's an example of sorting U[0,1]
#into quarters
data = runif(10,min = 0,max = 1)
interval = findInterval(x = data, vec = c(0,0.25,0.5,0.75,1))
cbind(data, interval)
#looks like you've got some options to help it determine what to do with cases on the boundary as well


##########
## 4.3 ###
##########
#statistics


#rank
#tells you the rank of each element in a vector. I bet some of the non-parametric tests use this
samp = sample(x = 1:20,size = 10,replace = TRUE)
cbind(samp,rank(samp))
#so the default mode gives average ranks between ties but it looks like you have some choice in that matter
#I wish i had known about this when making the teacher highlights as this could be used to simplify how
#I determined what the two highest and two lowest categories are instead of that silly merging thing


#ftable
#nice flat table version of a contingency table. It's useful because it can create tables with more than 2 variables
#basically creates every covariate pattern and tallies the rows in that
#You can specify the order for which the elements are presented
df = data.frame(x = sample(x = c("foo","bar","forever"), size = 10, replace = TRUE),
                y = sample(x = 1:2, size = 10, replace = TRUE),
                z = sample(x = letters[1:3], size = 10, replace = TRUE))
ftable(df)
ftable(df, col.vars = 1)
#kinda looks like these row.names and col.names arguments don't really reduce the number of variables being fed.
#they just alter which variables are presented as rows and which as columns
#I guess we could just feed it a subset
ftable(df[,1:2])


#model functions
#Unsurprisingly there are quite a few functions to help us with linear models
#let's just review them all using the iris set since I'm rusty AF
#check the pairwise plots for a stronger relationship
pairs(~Sepal.Length+Sepal.Width+Petal.Length+Petal.Width, data = iris)
#petal width and petal length seem good
model = lm(formula = Petal.Width~Petal.Length,data = iris)
summary(model)
plot(model)
plot(x = iris$Petal.Length, y = iris$Petal.Width)
abline(a = model$coefficients[1], b = model$coefficients[2])

#hatvalues
hatvalues(model)
#gives the leverages for the dataset. Let's use find interval to color code the leverages according to quartiles
hats = round(hatvalues(model), digits = 3)
colors = findInterval(x = hats, vec = summary(hats)[c(1:3,5,6)], all.inside = TRUE)
plot(x = iris$Petal.Length, y = iris$Petal.Width, col = colors)
abline(a = model$coefficients[1], b = model$coefficients[2])
#cool so we see the values on the edges of the independent variable having larger leverage
#as a review, high leverage and outlier are not the same thing but a point could fall into both categories
#high leverage points are off to the edges of x's range and outliers do not follow the trend of the data

#influence measures
str(influence.measures(model))
#this gives a matrix listing several measures of influence for a given model
#we get cook's distance, the leverages, dfbetas, dffits, as well as something I'm not familiar with: covratio

#logLik
#returns the log likelihood
logLik(model)
#Trying to remember... The likelihood function is a function of the parameters given a particular data set (iris)
#so we're taking our data and jamming it into the pdf

#deviance
#This is a statistic used to compare nested models. We can take the difference of the deviance of a smaller model and the
#the larger model and use a chi squared test to determine if the smaller model fits as well
#let's consider a model that also takes in to account Sepal.Length
model2 = lm(formula = Petal.Width~Petal.Length + Sepal.Length,data = iris)
#we have 1 degree of freedom for this test because there is one more parameter in the larger model
pvalue = pchisq(q = deviance(model)-deviance(model2), df = 1, lower.tail = FALSE)
#so we conclude that the larger model is no better


#formula
#I've used a number of formulas but one thing I didn't know is that the colon is for interaction terms so we could
#make a model like
model3 = lm(formula = Petal.Width ~ Petal.Length + Sepal.Length + Petal.Length:Sepal.Length ,data = iris)
#or using the star
model4 = lm(formula = Petal.Width ~ Petal.Length*Sepal.Length ,data = iris)
model3$coefficients == model4$coefficients
#there's also some other ways you can cross groups of factors and get higher order terms


#Statistical tests
apropos("\\.test$")
#nice
#only recognize about half of these so that's cool


##########
## 4.4 ###
##########
#working with R


#exists
#we can tell if an object is in memory at the time with this function. It's also possible to specify the environment
f = 1
exists("f")
rm(f)
exists("f")

#q()
#terminates the R session. Interesting....
#I guess that's nice? Maybe if I wrote some program that is automated to run at a specified time I might wanna close
#the door after I leave


#apropos
#looks for objects by partial name
apropos("GLM")
#so it helps you refine your search by giving objects with some sort of regex match


#RSiteSearch
#can help look through the r-project website. You can restrict your search to functions, vignettes...
RSiteSearch("knitr")
#seems slightly better than just going to the website since you don't have to remember the url


#demo
#can run some demonstrations of how to use certain packages. It looks like this is an attribute to the function
#as I don't see this code directly into the documentation
demo(lm.glm, package = "stats", ask = TRUE)
#but might be nice if we really are struggling to understand a new function


#example
#this does just run the examploe portion of the documentation
library("SvenR")
example("school.year")
#cool


#vignette
#same idea but for vignettes
library("lubridate")
vignette("lubridate")ge


#traceback
#can help us identify the locations of errors
f = function(x){
  x = log(x)
  return(x)
}
f(1:5)
f("a")
traceback()
#probably wanna try this with some of the functions I've been writing.


#browser
#this interrupts evaluation so you can see what's in the environment at the time
f = function(x){
  r = x*x
  browser()
  return(r)
}
f(2)
#so we can see that x and r are in the environment where'er at. Rstudio's GUI lets us
#continue with execution. THIS IS AWESOME. I often end up writing a ton of silly
#print statements just to tell what parts of the function actually exectue


#recover
#very similar to browser but it can be done post error. If we run something and get an
#error, we can use recover to browse from that point
#the help documentation suggests setting options(error = recover)
#to do this automatically


#dput
#we can create a file that contains an R object. Pretty cool
model = lm(formula = Petal.Width~Petal.Length,data = iris)
dput(x = model,file = "model.txt")
loadmodel = dget(file = "model.txt")
model$coefficients == loadmodel$coefficients
#hmm looks like maybe the data gets rounded when we use dput
round(model$coefficients, digits = 5) == round(loadmodel$coefficients, digits = 5)
#yep


#format
#Generalized formatting function. Seems like I was trying to write a similar function, just for data frames
df = data.frame(f = factor(x = letters[1:4]))
df = format.data.frame(df)
#converts this to characters from factors. There are a lot of other things that format can do but this
#isn't the most interesting thing to me at the moment


#sink
#documentation says that it sends R output to a file. We need to open the connection to the file and then close it
sink("sink_test.txt")
model = lm(formula = Petal.Width~Petal.Length,data = iris)
summary(model)
sink()
#cool. So it masks the output and writes it to a text file. I could see this being a quick and dirty way to produce some
#documents. It seems like knitr and other things like that are going to produce much more high quality documents however
#this could be good if we're in a hurry or don't care how it looks


#capture.output
#similar to sink but it wants you to name the expressions when calling capture.output. We can capture either as
#a text file or as a string within the environment
model.summary = capture.output(summary(model))
model.summary
#could be nice if we are planning on getting some output in a loop or function and want to get certain parts of the text
#I suspect that if we're doing that there will probably be an easier way. because doing something like
model.summary[3]
#is sort of worse version of
model$call


#sprintf
#this is definitely a mysterious function to me. I used it several times when working with knitr to get the formatting correct
#but I was doing it in a way that was pretty haphazard with little understanding on my part
#The documentation says that it is a wrapper for a C function (probably why i don't get it) that formats character vectors
#it looks like we have the ability to choose some number and character formatting like double, integer ect
#woahhhhh the examples have my name in them CREEPY
sprintf("%s is %f feet tall\n", "Sven", 7.1)
#so it looks like it's taking in a string and a double and formatting them appropriatley
#let's see how it works with multiple strings
sprintf("%s is %f feet tall\n", c("Sven","Gangsta"), c(7.1,6.9))
#ok so it looks like it wants to create a character vector with each instance of the character vector we supply
#but it doesn't take every combination, it just takes the corresponding elements. If we choose different lengths:
sprintf("%s is %f feet tall\n", c("Sven","Gangsta"), c(7.1,6.9,8.2874))
#it doesnt want to... perhaps one has to be a multiple of the other?
sprintf("%s is %f feet tall\n", c("Sven","Gangsta"), c(7.1,6.9,8.2874, 3.921))
#yep
#Well... still not sure exactly how to use this and when you want to but we'll keep our eyes open


#count.fields
#helps us in trying to diagnose problems with reading data sets. We'll get a count back of the fields the version
#of read.table (or equiv) thinks there should be. Might be nice if we get some damaged data or the Rstudio assistant
#fails us somehow


#saveRDS & readRDS
#these seem very similar to dput but I guess these methods 'serialize' the output so it's more clear what's what
saveRDS(women, "women.rds")
women2 <- readRDS("women.rds")
all.equal(women,women2)
#okie... I guess since save() is mentioned after the RDS methods, it's not as good?
#some forums I'm looking at suggest that some types of R objects are harder to recover after they're written with save()


#library(foreign)
??foreign
#some methods in this package to read and write files for other statistical packages. This could be nice to use the
#extracts John creates in STATA. I'm not super excited about trying to get these to work but the next time
#I need a snapshot, I'll give it a shot


#basename, dirname, fuke)ext
#we can get portions of the path of a file with these
basename("C:/Users/shalvorson/Documents/R files/AdancedR.notes/sink_test.txt")
dirname("C:/Users/shalvorson/Documents/R files/AdancedR.notes/sink_test.txt")
library("tools")
file_ext("C:/Users/shalvorson/Documents/R files/AdancedR.notes/sink_test.txt")
#aight. Can be nice if we need to use this for generic files


#file.path
#this is kind of like a specialized version of paste() that helps us create file paths
file.path("C:","Users","shalvorson","Documents","R files","AdvancedR.notes") == getwd()


#path.expand
path.expand("~/sink_test.txt")
#can get the longer version of a file path of something in our wd

#normalizePath
#this says that it converts a file path in to the 'canonical form for the platform'. I think this is saying that if you're
#on a windows machine it's going to turn our \ into // and function differently on a mac
normalizePath(getwd())
#I was really hoping that it would be the reverse and take the formatting we get from pasting the clipboard
#still not sure how to write a function that does that.


#file.exists
#there are a bunch of file manipulation methods and I've used most of them but this one seems most interesting to me at the moment
#it returns a logical if the file we specify is there. This could be useful for trying to not overwrite other files
#and do things like : if it exists, load it , if not create a new one.
file.exists("sink_test.txt")
#looks in wd first.


#file.info
#gives some vitals on a file of our choosing
file.info("sink_test.txt")
#oh that's nice. I could see this being very useful because we can get creation, modification times out of this and maybe say something
#like 'get the most recent file'. If I ever get that curl thing going this will be nice


#tempfile and tempdir
#Create temporary files and directories. One of the things that I like about R compared to STATA is that we don't need to rely on tempfiles
#as much because R can hold multiple objects in memory. This still seems like it could be good though if we had a program
#that wanted to create objects and then load them again. We'll preserve some memory and order by not multiplying our files all over the place

#download.file
#Does what it says. You specify the url and it fetches. I wonder if I can get it to download iStation data?
#I'm gonna log in first because bypassing the secure login is a little beyond me at the moment. This
download.file("https://secure.istation.com/Report/AssessmentResultExport/d80679?Pid=ISIPEN&Year=2016&period=3", destfile = "downloadfile_test.txt")
#okay well that didn't work. It seems like it tried to just download some of the metadata for the site
#Here's an example from a blog:
URL      <- "http://rfunction.com/code/1202/120222.R"
destfile <- "downloadfile_test2.R"
download.file(URL, destfile)
#okay so this example is simpler because that URL just leads to a plain text page. The book also mentions the 'downloader'
#package which says that it can do some of the things curl can without external dependencies
#may want to look into this as it says that downloader is much simpler than Rcurl

