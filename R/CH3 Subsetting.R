##########
## 3.1 ###
##########

#Section 3 is all about subsetting: the process of reducing and sampling our data
atom <- letters[1:5]

#We can subset atomic vectors in 5 different ways:

#Positive integers will select the indices we choose
atom[c(1,5)]
#dupicated entries provide duplicated results
atom[c(1,1)]
#and non-integer values are rounded down
atom[1.9]

#Negative numbers select everything except that index
#but these cannot be combined with positives
atom[c(-1,-3)]

#Logical vectors select the entries that are TRUE.
#This could be a literal sequence of true and false or a statement evaluated along a vector
atom[atom!="c"]

#NOthing, returns the original
atom[]
#I guess we'll find out why this is even mentioned soon

#Character vectors match the names attribute. Note that when we try to do this with atom
#it doesn't return anything.
atom["a"]
#because we have no names
#but if we set them
names(atom) <- letters[6:10]
atom[c("g","h")]

#subsetting a list works in the same way. We will get a list back if we subset with single bracekts
l = list(x=1:4,y="bananaphone")
str(l[1])


#Subsetting matrices and data frames can be done in a very similar way but we must specify rows
#and columns

df = data.frame(x=1:3,y=letters[1:3])
df[df$x>1,"y"]

#Data frames can be subsetted by a single vector, in which case the columns will be chosen
df[1]
#Notice however, that the method of specifying columns changes what the output looks like
str(df[1])
str(df[,1])

#EXCERCISES

#1. Fix the following subsetting errors
#mtcars[mtcars$cyl=4,] needs logical operator, not assignment
mtcars[mtcars$cyl==4,]

#mtcars[-1:4,] Cannot combine negative and positive indices. I assume they wanted all but the first 4
mtcars[-(1:4),]

#mtcars[mtcars$cyl<=5] Needs comma because the first part is not equal to the number of columns :P
mtcars[mtcars$cyl<=5,]

#mtcars[mtcars$cy==4 | 6,] 6 is neither true nor false. Need to compare cyl to both
mtcars[mtcars$cyl %in% c(4,6),]

#2. Why does x<- 1:5;x[NA] yield 5 missing values
x<- 1:5
str(x[NA])
str(x[NA_real_])
#######
# !!! #
#######

#3. What does upper.tri() return? how does supsetting a matrix with it work? Do we need any additional
#subsetting rules to describe it's behavior?

mat = matrix(1:9,nrow = 3)
upper.tri(mat)
#this gives us the upper triangle of a matrix
mat[upper.tri(mat)]
#this just gives a vector of those cells so maybe if we want the actual uppper triangle we should write
mat[!upper.tri(mat)] = 0
mat

#4. Why does mtcars[1:20] return an error? How does it differ from the similar mtcars[1:20,]?

#The first command is trying to get the first 20 columns when there are only 11
#The second is getting the first 20 rows

#5. Implet your own function that extracts the diagonal entries of a matrix. Should behave like diag
mat = matrix(1:9,nrow = 3)

diag2 <-function(m){
  if(class(m) == "data.frame"){
    m = as.matrix(m)
  }
  if(class(m) != matrix){
    stop("m must be a matrix or data frame")
  }

  #well there's probably a super slick way to do this
  #but this seems simple enough
  mat = mat[!upper.tri(mat) & !lower.tri(mat)]
  return(mat)
}
#well that doesn't do exactly what diag does but it satisfies the question.

#6. What does df[is.na(df)]<-0 do? How does it work?
df = data.frame(x = c("SUP","WICU",NA),y=1:3, stringsAsFactors = FALSE)
#What it does is creates a logical matrix based on which entries of df are NA
#it then makes those entries which are true, 0

##########
## 3.2 ###
##########

#Because the elements of a list may be more complicated than an atomic vector,
#subsetting them is also more nuanced.
l = list(1:3,"babby",c("how girl", "get pregnart"))

#subsetting with a single bracket returns a list of the selected elements
l[1:2]
#subsetting with two brackets returns the contents of the selected elements
l[[1]]
mtcars[["cyl"]]


#The author makes a distinction between simplifying and preserving when you subset
#To me, simplifying means to take the simplest form of the subset regardless of what
#the structure of the parent set was. Preserving forces the structure to be the same
#with a smaller portion of the data left. He says that preserving is generally better
#because you control the data type. If you simplify, the data may simplify to one type in
#some circumstances and another with different input

#From the table here, it looks like I mostly used preserving subsets in the past except in the
#case of data frames.

#Let's look at the difference with mtcars
#simplifying returns a double vector
str(mtcars[,1])

#Preserving returns a data frame of that single column
str(mtcars[,1,drop=FALSE])

#Another aspect of this that I was not aware of is that we can choose to drop factor levels when we subset
str(Orange$Tree)
str(Orange$Tree[1:3,drop=TRUE])

#The dollar sign does not actually preserve. It returns the simplified version of that column

#The author also mentions that the way the single and double brackets handle funny input is different.
#Out of bounds subscripts and null values return null values for the single bracket and errors for the double
#This seems like it might be just another reason to lean on preserving subsets because it can help
#me catch when I'm asking for something I probably don't intend to

#EXCERCISE
#1. Given the following model, extract the residual degrees of freedom and R squared

mod <- lm(mpg~wt, data = mtcars)
summary(mod)
residDF <- mod$df.residual
rsquare <- summary(mod)$r.squared

#Portions of obejcts can be subsetted and then overwritten
x <- 1:10

#it can accept duplicat indices
x[c(1, 2)] <- 2:3

#R reacts differently to NA when combined with logical vs integer
x <- c(1:10)
#I'm not sure if I understand this section of the book. THe author writes that it makes a difference
#whether you combine NA with logical or integer. Giving this example:

x = c(3, 4, 3, 2, 1)
x[c(1, NA)] <- c(1, 2)
x[c(T, F, NA)] <- 1

#but it seems more like the problem is that they're trying to assign a 1 to the element x[1] and 2 to the element x[NA]
#where as the second assignment assigns all the elements 1
#if you write
x[c(3,NA)] = 100
#it seems fine with being fed a combination of integer and NA
#and assigning in the following way throws the same error
x[c(TRUE,NA)] = c(1,2)
#Maybe there's something deeper here that I don't get


#subsetting an object with nothing is not the same as just assigning to that spot in memory
#if we use a blank box [] the object will retain it's structure
str(mtcars[])
str(mtcars)
str(lapply(mtcars,as.integer))
#since lapply returns a list, assignnig mtcars lapply(mtcars,as.integer) will change it to a list
#where as this command keeps it's structure as a data frame
mtcars[] = lapply(mtcars,as.integer)
str(mtcars)

#assignning NULL to the contents of an element of a list essentially deletes it
x <- list(a = 1, b = 2)
length(x)
x[["b"]] <- NULL
str(x)
length(x)

#but this is not the same as assingning a NA value to that element of the list
x <- list(a = 1, b = 2)
length(x)
x[["b"]] <- NA
str(x)
length(x)

#which is also not the same as making the element a null list
y <- list(a = 1, b = 2)
y[["b"]] <- list(NULL)
str(y)
str(y[[2]])
str(x[[2]])

#The next section is on lookup tables which is something I've used in different ways that the technique mentioned here
#I often create a little crosswalk and merge or do a series of find and replaces if it's short
#This looks nice though
x <- c("m", "f", "u", "f", "f", "m", "m")
lookup <- c(m = "Male", f = "Female", u = NA)
lookup[x]
#I'm not entirely sure I understand the logic of this. We're taking a named vector lookup, and subsetting it based off x

#You can remove the names from an ojbect with unname()
unname(lookup[x])


#Another nice use of the subsetting methods is to take samples of data
nrow(mtcars)
#take a sample of size 6
mtcars[sample(x = nrow(mtcars),size = 5,replace= FALSE),]



