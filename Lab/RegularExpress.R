con <- url('http://www.jaredlander.com/data/warTimes.rdata')
load(con)
dir()
ls()
class(warTimes)
head(warTimes)

require(stringr)
theTimes <- str_split(string=warTimes, pattern='(ACAEA)|-',n=2)

theStart <- sapply(theTimes, FUN=function(x) x[1])
theStart
theStart <- str_trim(theStart)
theStart
str_detect(string=theStart,pattern='January')
theStart[str_detect(string=theStart,pattern='January')]
str_extract(string=theStart,pattern='January')
#search years
str_extract(string=theStart,pattern='[0-9][0-9][0-9][0-9]')
str_extract(string=theStart,pattern='[0-9]{4}')
str_extract(string=theStart,pattern='\\d{4}')
str_extract(string=theStart,pattern='\\d{1,3}')  #give 1,2 or 3 digits
str_extract(string=theStart,pattern='^\\d{4}')
str_extract(string=theStart,pattern='^\\d{4}$')

str_replace(string=theStart, pattern='\\d',replacement='x')
str_replace_all(string=theStart, pattern='\\d',replacement='x')
str_replace_all(string=theStart, pattern='\\d{1,4}',replacement='x')

commands <- c('<a href=index.html>The link is here</a>This is bold text</b>regular expression is interesting')
str_replace_all(string=commands, pattern='<.+?>',replacement='')
str_replace_all(string=commands, pattern='<.+?>(.+?)<.+?>(.+?)<.+>',replacement='\\2')
