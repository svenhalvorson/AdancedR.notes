#Alright, we're here with Wickaham's 'Advanced R' trying to get a little less
#bad at R programming. I'm going to take notes in order to try and get it
#to stick in my brain a little better. I think I'm going to take notes on
#things that are new or not solid in my head

##########
## 2.1 ###
##########

#Vectors are one dimensional objects that can be homegenous in their contents
# (atomic) or heterogeneous (list). They all respond to tyepof(),
# length(), and attributes(). Unfortuantely, is vector tests specifically
# if it's a vector with no attributes other than names.

#Here's an atomic vector
x = 1:4
is.atomic(x)
typeof(x)
length(x)
attributes(x)

#And a list
y = list("fifteen", 15, "bacon", 1:3)
is.list(y)
attributes(y)
length(y)
#notice that the length of y is 4, not 6 because the atomic
#vector 1:3 is treated as one element of y
length(y[[4]])

#Atomic vectors can be logical, integer, double, or characters
#Integer values are followed by a L
int <- c(1L,13L)
is.integer(int)
#kinda curious why you would care to make a vector integer as opposed to
#double. Perhaps there are times when you know that all the values should
#be of that type and that type only

#The combine function will flatten all atomic vectors so it doesn't behave
#like list
int <- c(1L,c(2L,13L))
length(int)==3

#NA values can be in any of these types, they are considered the type of the vector
int <- c(1L,0L,NA)
typeof(int[3])
#these can be specified with suffixes as well
typeof(NA_character_)

#You can use the is family of functions to determine if vectors are of a particular type
is.character(int)
#is.numeric returns true for double and integer vectors

#lists work similarly but can have more complex and heterogeneous elements
y = list("fifteen", 15, "bacon", 1:3)
str(y)
#so this consists of two character vectors, a numeric vector, and an integer vector of length 3

#lists can be recursive so lists can contain lists
y = list(1:3,list(c("bananphone","donanaphone"),100:102,list(letters[1:3])))
str(y)
is.recursive(y)
#The combine function flattens everything
c(list(1,2),c(3,4))

#unlist can flatten lists as well
unlist(y)


#EXERCISES

#1. What are the 6 types of atomic vector? How does a  list differ from an atomic vector?

#The six types are logical, integer, double, character, raw, and complex. Lists differe in taht they
#can have different data types as their elements, including other lists.

#2. What makes is.vector() and is.numeric() fundamentally different than is.list() and is.character()?

#The first two functions don't refer directly to the typeof() an object where is.list() and is.character()
#do.

#3. Predict the output of the following data coercions

typeof(c(1,FALSE))
#double

typeof(c("a",1))
#character

typeof(c(list(1),"a"))
#character NOPE IT'S A LIST. I guess this is the most general type so it probably takes prescedent in the
#same way the second one was turned into a character

typeof(c(TRUE,1L))
#integer

#4. Why do we need unlist() to convert a list to an atomic vector? Why doesn't as.vector work?

#lists are already considered vectors so as.vector doesn't change a list

#5. Why are the following statements true/true/false?

1 == "1"
#arithmetic comparisons coerce before comparing. The LHS is changed to "1"

-1 < FALSE
#FALSE is coerced to a 0

"one" < 2
#R tries to do an 'alphabetizing' comparison here. The ordering puts digits before letters so
#2 comes before o

#6 Why is the default missing value, NA, a logical vector? What's special about logical vectors?
# Think about
str(c(FALSE,NA_character_))

#I think it is because the boolean data type is the most basic and will always be coerced up to
#another data type. By having the NA default be boolean, you can throw it in any other atomic
#vector and it will assume the data type of that vector. This would not be the case if it was any
#other data type. I think people don't want the NA value to affect the overall structure


##########
## 2.2 ###
##########

#All objects can have arbitrary attributes attached to them. This does not affect their basic type
#and can be thought of as a named list attached
x = 1:3
attr(x,"Does Sven Rule?") <- TRUE
attr(x,"Does Sven Rule?")
str(x)
str(attributes(x))

#attributes are generally lost when modifying vectors
attributes(x[1])
#the three attributes that are always retained are
#names, dimension, and class
class(x)
class(x[1])
names(x) = letters[1:3]
x[1:2]

#names of vectors can be set by assigning the names() function a character vector or when you define
#the vector.

x  = c(a=1,b=2,c=3)
#names do not need to be unique and can be null.
#names can be removed with unname or setting the names to null

#Factors are special versions of integer vectors. They have a fixed range of values and names associated
#with those levels.
x = factor(c("a","b","c","a"))
x
class(x)
typeof(x)
#notice that R still thinks of x as an integer
x == 1
x == "a"
#but it doesn't respond to the integer
attributes(x)

#Factors are nice because the tables and graphs respond to empty categories better than characters
sex = factor(c("f","f"),levels = c("f","m"))
table(sex)

#Sometimes seemingly numeric vectors are read into R as factors. This is generally caused by nonumeric characters
#You can smash these by going to character and then back to double and the values that do not coerce will be NA

#Wickham suggests not monkeying with the global options like options(stringsAsFactors = FALSE) because it may
#mess with other people's code and also makes it more difficult to read as this option might not be explicit

#EXCERCISES

#1. Why doesn't this print the commment attribute?
structure(1:5,comment = "sup g")

#It appears that comment is a keyword in this instance. I'm not sure exactly what it's purpose is but you can see that
#mispelling it does display the attribute
structure(1:5,coment = "sup g")
# the documentation on comment() says that by default, it is not printed with the object

#2. What happens to a factor when you modify its levels?
f1 = factor(letters)
levels(f1) = rev(levels(f1))
f1
#it looks like it reverses the order of the elements as well

#3. How do these two commands differ from question #2
f2 = rev(factor(letters))
#This keeps the levels of the factor in alphabetical order but reverses
#the order of the elements
f3 = factor(letters,levels = rev(letters))
#this kept the order of the elements but reversed the heirarchy of levels

##########
## 2.3 ###
##########

#Arrays are multi dimensional ways of storing data. A matrix is the special
#case when we have 2 dimensions.
#in some sense they're like the atomic vectors
#Assigning dimensions to an atomic list makes it an array

nums = 1:12
a = nums
dim(a) = c(3,4)
a
b = nums
dim(b) = c(2,3,2)
b
str(b)
is.atomic(b)

#the dim, nrow, and ncol functions help get a handle on the size and shape
#length simply returns the number of elements for a matrix or array
length(b) == 12

l <- list(1:3,"a", TRUE,1.0)
dim(l) = c(2,2)
is.list(l)
is.matrix(l)
#strangely enough we can have something that is both a matrix and a list

#EXCERCISES

#1. What does dim return when applied to a vector?
dim(1:3)
#looks like null

#2. If is.matrix is true, what do we know about is.array()?

#It is TRUE as well. Matrices are special cases of arrays

#3. How would you describe these three objects?

x1 <- array(1:5,c(1,1,5))
x2 <- array(1:5,c(1,5,1))
x3 <- array(1:5,c(5,1,1))

#They are all vector-like but my spacial interpretation of them is different. They are akin
#to building on different axes in 3 dimensional space. They're different than 1:5 in that they
#have more dimensions even though these are somewhat superfluos


##########
## 2.4 ###
##########

#Data frames are probably most important data type for statistically minded people.
#At their heart, they are lists of equal length vectors. They respond to colnames, 
#rownames, and length() is the number of columns
df = data.frame(x=1:3,y=c("Aphrodite","Hestia","Poseidon"))
#it's a list
is.list(df)
#and its class is data.frame. I think there is a section later in the book about how classes differ
#from data types
class(df)

#The book mentions a plyr funtion, rbind.fill, which allows for differing column names. This has been
#a problem for me in the past so let's see if we can get it to work
library(plyr)
df2 = data.frame(x=4:5,z=c("Anubis","Ra"))
rbind.fill(df,df2)
#niceee

#it is possible to have the elements of a column be lists themselves but they cannot be sent to the 
#data.frame function.
df2$lists = list(1:3,letters[14:15])
df2

#We can make a data frame with a column of lists by using the I function which
#inhibits interpretation or conversion of objects. The data.frame function wants to 
#create columns out of the elements of a list but we can do this to stop it.

df3 = data.frame(x=1:3,z = I(list(1:3,3:2,letters[1:3])))
#It's also possible to have matrices as entries to a column

#EXCERCIS

#1. What attributes do data frmaes have?

#They have dimensions, column names, row names, class. The columns can be thought of as attributes

#2. What does as.matrix() do to a data frame with different data types among its columns?

#It coerces it into a character matrix
str(df)
str(as.matrix(df))

#3. Can you have a data frame with 0 rows? 0 columns?

#Yes, you can create a data frame with 0 rows or column
nullframe = data.frame()


#QUIZ

#1. What are the three properties of a vector, other than its contents?

#The length, the data type, and the names. (actually attributes more generally)

#2. What are the four common types of atomic vectors? The rare two?

# Common: logical, integer, double, character
# Rare: complex, raw
# My understand that raw data is 'intended to hold raw bytes' Not really sure why you would want that
# Complex numbers are you classic a+bi

#3. What are attributes? How do you get and set them?

#Attributes are additional data attached to an object. They can be accessed via attr(object,attribute)
#or with the $
#You can set them with attr(object, attribute) = "foo"

#4. How is a list different than a vector? How is a matrix different from a data frame?

#Lists can hold heterogenous data including other lists. Atomic vectors can only have data of one type.
#A similar relationship exists between matrices and data frames. Matrices are all of the same data type
#data frames hold a variety of different types

#5. Can you have a list that is a matrix? Can you have a data frame have a column that is a matrix?

#Yes to both. The elements of a list can be of any data type. A data frame can have columns that consist of 
#matrices however you will need to use the I() function when initializing
