setwd("~/Google Drive/Schoolwork/2014Spring/W4249/HW4")
require(stringr)
#Problem 1
#find type
strings <- paste(readLines('problem1.txt'),sep='',collapse='\n')
strings <- str_replace_all(string=strings,pattern='\n',replacement='')



# typep <- '[a-zA-Z]{4}'
typep <- '[a-zA-Z]{4}[:punct:][ ]*[1]?[-]?([2-9][0-9]{2})[- .]([0-9]{3})[- .]([0-9]{4})'
# typep2 <- '([2-9][0-9]{2})[- .]([0-9]{3})[- .]([0-9]{4})'
type <- unlist(str_extract_all(string=strings,pattern=typep))
type <- gsub(pattern=' ',replacement='',x=type)
#find area code!!!!!!
areap <- '[\\(]([2-9][0-9]{2})[\\)][ ]*([0-9]{3})[- .]([0-9]{4})'
str_extract_all(string=strings,pattern=areap)




#Problem 2
# dat2 <- read.table('problem2.txt',header=F,sep='',fill=T)
dat2 <- scan('problem2.txt',character(0))


#only used for single text input 'a+b' or '+-a-b'
#a, b can be any number
Evaluated <- function(textformula)
{
    if(!require(stringr)) require(stringr)
    #extract number
    test <- textformula
    temp <- unlist(str_split(string=test,pattern='\\+|\\-'))
    num <- as.numeric(na.omit(as.numeric(temp)))
    if(length(num)!=2) stop('input error (must be plus or minus)')
    #determine sign
    temp.sign <- unlist(str_extract_all(string=test,pattern='\\+|\\-|\\--|\\++'))
    for(i in 1:length(temp.sign))
    {
        if(temp.sign[i]=='--') temp.sign[i]<-'+'
            
    }

    if (length(temp.sign)==1)
   {
        num[2] <- as.numeric(str_join(temp.sign,num[2]))
        result <- sum(num)
    }
else if(length(temp.sign)==2)
{
    num.new <- as.numeric(str_join(temp.sign,num))
    result <- sum(num.new)
}
else
{
    stop('input error')
}
return(result)
}

ans <- vector()
for(j in 1:length(dat2))
{
    ans[j] <- Evaluated(dat2[j])
}

