require(stringr)

#5
nchar("abcdef")
str_length("abcdef")
nchar(NA)
str_length(NA)

#6
string <- "23 mai 2000"
string2 <- "1 mai 2000"
#what is this!???
regexp <- "([[:digit:]]{2}) ([[:alpha:]]+) ([[:digit:]]{4})"
grepl(pattern = regexp, x = string)
str_detect(string, regexp)
grepl(pattern = regexp, x = string2)

#7
string <- "blabla 23 mai 2000 blabla 18 mai 2004"
textcnt(string,n=1L,method="string")

#8
cpos("abcdefghijklmnopqrstuvwxyz","p",start=1)
substring.location("abcdefghijklmnopqrstuvwxyz","def")




#9
regexp <- "([[:digit:]]{2}) ([[:alpha:]]+) ([[:digit:]]{4})"
string <- "blabla 23 mai 2000 blabla 18 mai 2004"

regexpr(pattern = regexp, text = string)
gregexpr(pattern = regexp, text = string)
str_locate(string,regexp)
str_locate_all(string,regexp)

#10
substr("simple text",1,3)
str_sub("simple text",1,3)
first.word("abc def ghk")

# 11.

grep(pattern = regexp, x = string , value = T) 
grep(pattern = regexp, x = string2 , value = T) 
grep(pattern = regexp, x = string , value = F) 
grep(pattern = regexp, x = string2 , value = F) 








library("stringr")
regexp <- "([[:digit:]]{2}) ([[:alpha:]]+) ([[:digit:]]{4})"
string <- "blabla 23 mai 2000 blabla 18 mai 2004"
str_extract(string,regexp)
str_extract_all(string,regexp)
str_match(string,regexp)
str_match_all(string,regexp)

# 12.

string <- "23 mai 2000"
regexp <- "([[:digit:]]{2}) ([[:alpha:]]+) ([[:digit:]]{4})"
sub(pattern = regexp, replacement = "\\1", x = string) # returns the first part of the regular expression
sub(pattern = regexp, replacement = "\\2", x = string) # returns the second part
sub(pattern = regexp, replacement = "\\3", x = string) # returns the third part

# 13.

tolower("ABCdef")
toupper("ABCdef")
capitalize("abcdef")

#14.

library("cwhmisc")
padding("abc",10," ","both") # adds blanks such that the length of the string is 10.
str_pad("abc",width=10,side="center", pad = "+")
str_pad(c("1","11","111","1111"),3,side="left",pad="0") 

library("memisc")
trimws("  abc def   ")
library("gdata")
trim(" abc def ")
str_trim("  abd def  ")

#15

"abc"=="abc"
"abc"=="abd"

agrep(pattern = "laysy", x = c("1 lazy", "1", "1 LAZY"), max = 2, value = TRUE)
agrep("laysy", c("1 lazy", "1", "1 LAZY"), max = 3, value = TRUE)














#16
library(stringr)

#Generate data
strings <- c(" 219 733 8965", "329-293-8753 ", "banana", "595 794 7569",
             "387 287 6718", "apple", "233.398.9187 ", "482 952 3315", "239 923 8115",
             "842 566 4692", "Work: 579-499-7527", "$1000", "Home: 543.355.3679")
phone <- "([2-9][0-9]{2})[- .]([0-9]{3})[- .]([0-9]{4})"

# Which strings contain phone numbers?
str_detect(strings, phone)
strings[str_detect(strings, phone)]

str_
# Where in the string is the phone number located?
loc <- str_locate(strings, phone)
loc

# Extract just the phone numbers
str_sub(strings, loc[, "start"], loc[, "end"])

# Or more conveniently
str_extract(strings, phone)

# Pull out the three components of the match
str_match(strings, phone)

# Anonymise the data
str_replace(strings, phone, "XXX-XXX-XXXX")




#17
library(stringr)

col2hex <- function(col) {
    rgb <- col2rgb(col)
    rgb(rgb["red", ], rgb["green", ], rgb["blue", ], max = 255)
}

# try ☺
col2rgb("peachpuff")

# Goal replace colour names in a string with their hex equivalent
strings <- c("Roses are red, violets are blue", "My favourite colour is green")
colours <- str_c("\\b", colors(), "\\b", collapse="|")

# This gets us the colours, but we have no way of replacing them
str_extract_all(strings, colours)

# Instead, let's work with locations
locs <- str_locate_all(strings, colours)
sapply(seq_along(strings), function(i) {
    string <- strings[i]
    loc <- locs[[i]]
    # Convert colours to hex and replace
    hex <- col2hex(str_sub(string, loc[, "start"], loc[, "end"]))
    str_sub(string, loc[, "start"], loc[, "end"]) <- hex
    string
}
)





