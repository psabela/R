#   qualitative(categorical) = 'factor'
#   quantititive(numeric) = 'numeric'
#   levels = categories

data <- read.csv("filename.csv", sep=' ', header=T, na.strings = '-')
#na.strings lists characters that represent nothing.

#histogram
hist(vector, breaks=seq(120,200,by=10),col='red',xlab='Exam Scores',ylab='Frequency',main='Exam Score Histogram')

plot(x,y,xlab='Exam Scores',ylab='Frequency',main='Exam Score plot')

ecdf()

#split chart window
par(mfrow=c(1,2))

li#AAFASF
2 + 2
1 : 100
print("Hello World!")
x <- 1 : 5
x
y <- c(6,7,8,9,10)  #creates a list of values, other name is a vector
y
x + y
x * 2
ls()
forecast.csv <- read.csv("C:\\Users\\psabela\\Desktop\\Forecast2.csv",header=T,sep=',')
str(forecast.csv)
search()
library(help="foreign")
vignette(package="reshape2")
browseVignettes(package="reshape2")
vignette()
browseURL("http://cran.stat.ucla.edu/web/packages/available_packages_by_name.html")
names(majors.csv)  #names of columns
c(1,2,3,4)  #creates a list of values, other name is a vector
search() #shows packages that are currently loaded
require("datasets")
data() #list datasets
str(airmiles) #to see dataset structure
fix(airmiles) #edit dataset
rm(list=ls()) #clean up environment
x3 <- seq(10)  #Counts from 1 to 10
?seq  #help on sequense
#ctrl + enter //executes the script line
#alt +        //short cut for <-
#cltr + l      //clears console
x6 <- scan() #allows you to enter a sequence one value at a time; hit return after each value; hit return twice to stop
#margin.table(dataset, column)  # marginal frequency
#prop.table(dataset)  #show proportions
#dataframe[ , -4]  remove fourth column
#admit  <-  as.data.frame.table(UCBAdmissions)   # convert to data frame

emptyFrame  <- data.frame(t(rep(NA,3)))  #create empty dataframe
names(emptyFrame)  <- c('CRN','CAMPUS','ZONE')  #name the columns

#Generic function for replacing each NA with the most recent non-NA prior to it
library(zoo)
#na.locf(column_name)

#filter based on regular expression pattern
c4 <- c1[grep("^[A-Z]",c1$OFFICE),]
c4  <- c4[c(1:45)]

#paste = concatinate 2 strings
paste(as.character(acct_distrib$FIRST_NAME),as.character(acct_distrib$LAST_NAME), sep="")[1]

#complete.cases()  //returns rows that do not have missing values

x <- c('a','b','c')
for(i in seq_along(x))
{
  
}

//or seq_len(x)

//remove NA
na.rm = TRUE

##replace new line characters froms tring
sql <- gsub("[\r\n]","",sql)

##trim 
trim <- function (x) gsub("^\\s+|\\s+$", "", x)


#contains
grepl('AUX ENTERP',temp$reporting)


ptm  <- proc.time()
#function here
proc.time() - ptm


#RESHAPING 
#idvar columns that will for the id of the rows, these rows are not to be reshaped into columns
t2<- reshape(exp.csv1,idvar=c(1:13),direction='wide',timevar='drcr')

#wide to long
#varying = column names that define different types of metrics
#v.names = column name we wish to give the containing the values in the long format
#timevar = column name we wish to give the column containing the names of different types of matrics
#times = the values the time var will have (same as varying)
#direction = long
id female race ses schtyp prog read write math science socst
1   70      0    4   1      1    1   57    52   41      47    57
2  121      1    4   2      1    3   68    59   53      63    61
3   86      0    4   3      1    1   44    33   54      58    31
4  141      0    4   3      1    3   63    44   47      53    56
5  172      0    4   2      1    2   47    52   57      53    61
6  113      0    4   2      1    2   44    52   51      63    61
7   50      0    3   2      1    1   50    59   42      53    61
8   11      0    1   2      1    2   34    46   45      39    36
9   84      0    4   2      1    1   63    57   54      58    51
10  48      0    3   2      1    2   57    55   52      50    51

l <- reshape(hsb2, 
             varying = c("read", "write", "math", "science", "socst"), 
             v.names = "score",
             timevar = "subj", 
             times = c("read", "write", "math", "science", "socst"), 
             new.row.names = 1:1000,
             direction = "long")

#long to wide
#timevar = column name that define the multiple measuresmets (timevar)
#idvar = columns that should not move
#direction = wide
