# Processing clean webscraped data from Psych Today NYC zips
library(tidyverse)

md <- read_csv("x Scraped Data Cleaned.csv")
dim(md)
md$NYlicense <- as.numeric(md$NYlicense)
glimpse(md)

income <- read.csv("NYC Demographic_Statistics_By_Zip_Code.csv")
dim(income)


# Selecting different groups of variables:
agevars <- names(md)[grep("^age_", names(md))]
subcatvars <- names(md)[grep("^sub_", names(md))]
approachvars <- names(md)[grep("^appr_", names(md))]
ethnicityvars <- names(md)[grep("^ethn_", names(md))]
languagevars <- names(md)[grep("^lng_", names(md))]
religionvars <- names(md)[grep("^rel_", names(md))]
insurancevars <- names(md)[grep("^ins_", names(md))]
titlevars <- names(md)[grep("^ttl_", names(md))]
specialtyvars <- names(md)[grep("^spec_", names(md))]
issuesvars <- names(md)[grep("^iss_", names(md))]


#---------------------------------------------------------------------------
# Calculating averages for numeric variables - by Zipcode
#---------------------------------------------------------------------------

numericvars <- names(md)[!names(md) %in% c("license_state", "zipcode",
                                           "school_new")]
# length(numericvars) + 3 == ncol(md)







