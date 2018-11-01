# Crawl the datasets from Ruey S.Tsay web site:
# http://faculty.chicagobooth.edu/ruey.tsay/teaching/fts3/
# Main steps:
# 1. Read html file, get the html content
# 2. Get the file url list with regexp
# 3. For every url list
#   3.1 Fetch the content, a data frame(read.csv(URL))
#   3.2 Get the file name
#   3.3 Write the data frame to file
#   3.4 Sleep some seconds
library(stringr)
library(readr)

fts3_body <- read_file("fts3.html") #read the html to string
urls <- str_extract_all(fts3_body, pattern = "http[^\"]*fts3[^\"]*(txt|dat)") #extract urls 
for(url in urls[[1]]){
  url_split <- strsplit(url,"/")
  file_name <- url_split[[1]][length(url_split[[1]])] # to get the filename
  if(file.exists(file_name)) {message(paste(file_name," has existed!!!"));next}
  message(paste("File ",file_name, " begins downloading......"))
  file_content <- read_csv(url) #data frame
  write.csv(file_content, paste("./fts_data/",file_name), row.names = FALSE, col.names = TRUE, quote = FALSE) #save the frame to file
  message(paste(url," has downloaded"))
  Sys.sleep(runif(1,0,1)) #sleep random seconds
}
  
