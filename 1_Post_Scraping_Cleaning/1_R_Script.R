###########################################################################
# Dimitri post-web-scraping data cleaning script
###########################################################################

library(tidyverse)
md <- read_csv("zip_11205.csv")[-1]
names(md)
dim(md)


#------------------------------------------------------
# Function that spreads strings of one column
# and creates dummy variable for each string
# With string being the name of the new variable
#------------------------------------------------------

# Need MY function, not this one:
spread_column = function(data_frame, column){
  mydf <- data_frame[column]
  names(mydf) <- 'col'
  result = rownames_to_column(mydf, 'grp') %>% 
    separate_rows(col) %>% 
    filter(col != "")  %>% 
    count(grp, col) %>%
    spread(col, n, fill = 0) %>%
    ungroup() %>% 
    select(-grp)
  return(result)
}


View((spread_column(md, 'specialties')))
View(md)
