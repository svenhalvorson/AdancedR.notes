#######
## 5 ##
#######
#Style


#This chapter is all about making your code readable. I'm not a very good programmer
#but I do think that I'm reasonably good at leaving lengthy comments and making my work interpretable.
#I'll probably just note anything that I should start doing or disagree with

#Naming files with an order: the author suggests you prefix scripts with numbers if they
#need to be done in sequence like 0-download.R 1-Parse.R 2-Explore.R
#This does seem nice but my solution has been to just have a driver file that
#sources all the files. I'm not sure what the downside of my method is other than
#you may not want to execute them all but I prefer having the driver file than having to
#go in one by one and run scripts


#We're advised to not use case to denote words in objects and I'm a little prone to this
day_one #and
day.one
#are preferable to
DayOne


#I'm not sure how much I like the advised spacing with arithmetic operators
#The author suggests using a space between each piece but that looks a little weird to me
1 + 2 / 3
#vs
1+2/3
#I don't really like it. I feel like spacing between arguments in a function and = sign
#are good and easier to read
f = mean(c(1, 2, 3))
f=mean(c(1,2,3))
#yeah I like the first one more there but the arithmetic looks weird with spaces.


#Multiple arguments should be on seperate rows. I should probably do this more often
#when there are only a few arguments that are short I don't really see this as being useful
#but I've definitely had some function calls that are so long they go off the field of view
df = data.frame(x = 1:3, y = letters[1:3])
#vs
df = data.frame(x = 1:3,
                y = letters[1:3])
#even with this short command it does seem nice to use extra lines because the arguments are
#all aligned vertically


#Assignment is supposed to be a <- instead of =
#I usually try to use the arrow when I first define an object and = when I overwrite it with
#modifications. I don't really know why the author thinks that it's better to use arrows


#comment lines
#I usually use long sequences of ###################
#they suggest # Name -------------------------------------
#I like the dash more
