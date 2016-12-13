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




