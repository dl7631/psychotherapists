# Processing clean webscraped data from Psych Today NYC zips
library(tidyverse)

md <- read_csv("x Scraped Data Cleaned.csv")
dim(md)
md$NYlicense <- as.numeric(md$NYlicense)
glimpse(md)

md$license_state <- as.factor(md$license_state)
levels(md$license_state)

length(unique(md$zipcode)) # 230

# Exclude zipcodes NOT from NYC:
nyczips <- read_csv("all_nyczips.csv")
dim(nyczips)

dim(md) # 14,304
md <- inner_join(md, nyczips, by = 'zipcode' )
dim(md) # 12,629
length(unique(md$zipcode)) # 168
myN <- nrow(md)

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
# Focusing on New York city as a whle - across zip codes
#---------------------------------------------------------------------------


mycor <- cor(md[c("fee_from", "fee_to", "insurance",
                  "sliding", "years_in_practice",
                  "years_since_graduation")],
             use = "complete.obs")
# The only interesting thing:
# Therapist who charge more, are less likely
# to take insurance and accept sliding scale


##########################################
# Patient Ages:

pat_age_pcnt <- md %>% select(one_of(agevars)) %>% 
  summarize(toddlers = round(mean(age_toddlers) * 100, 1),
            children = round(mean(age_children) * 100, 1),
            preteens = round(mean(age_preteens) * 100, 1),
            teens = round(mean(age_teens) * 100, 1),
            adults = round(mean(age_adults) * 100, 1),
            elders = round(mean(age_elders) * 100, 1))
pat_age_pcnt <- data.frame(t(pat_age_pcnt))
names(pat_age_pcnt) <- "percent"
pat_age_pcnt$ages <- row.names(pat_age_pcnt)
pat_age_pcnt <- mutate(pat_age_pcnt,
                       ages = factor(ages, levels = ages))

pat_ages_plot <- ggplot(pat_age_pcnt, aes(ages, percent)) +
  geom_bar(stat = 'identity', fill = "#E55A00") +
  theme_bw() +
  ggtitle("Ages Groups Therapists Specialize in") +
  xlab("Age groups") +
  ylab("% of therapists") +
  scale_y_continuous(breaks = seq(0, 90, by = 10))
ggsave('Plot Patient Ages.wmf', plot = pat_ages_plot)

##########################################
# State of the license:

nylic <- count(md, NYlicense) %>%    # 13570 (95% of all therapists) are with NY license
  mutate(n = round(n/myN * 100, 1))
nylic  # at least 1.9% (236 therapists) are breaking the law
# but most likely more - 4.9% (615 therapists)

plot_licenses <- ggplot(filter(md, NYlicense != 1), aes(license_state)) +
  geom_bar(fill = "#E55A00") +
  ggtitle("Therapists with Licenses from Outside New York State")  +
  ylab("Number of therapists with license from each state") +
  xlab("") +
  coord_flip() + theme_bw() +
  scale_x_discrete(limits = rev(levels(md$license_state))) +
  scale_y_continuous(breaks = seq(0, 130, by = 10))
ggsave('Plot Licenses.wmf', plot = plot_licenses)

##########################################
# Fees
md %>% summarize(mean(fee_from, na.rm = T), mean(fee_to, na.rm = T))
# from $135 to $199 if mean
md %>% summarize(median(fee_from, na.rm = T), median(fee_to, na.rm = T))
# from $130 to $200 if median
md %>% summarize(max(fee_from, na.rm = T), max(fee_to, na.rm = T))
# $500 to $500 if max
md %>% summarize(min(fee_from, na.rm = T), min(fee_to, na.rm = T))
# $500 to $500 if max

write.csv(byzip, "x test.csv")

##########################################
# Sliding scale - 
# announced by 80% of therapists:
md %>% summarize(mean(sliding, na.rm = T))

##########################################
# Patient type specialties

pat_sub_pcnt <- md %>% select(one_of(subcatvars)) %>% 
  summarize(bisexuals = round(mean(sub_bisexuals) * 100, 1),
            cancer_patients = round(mean(sub_cancer) * 100, 1),
            gays = round(mean(sub_gays) * 100, 1),
            heterosexuals = round(mean(sub_heteros) * 100, 1),
            with_hiv_aids = round(mean(sub_hiv) * 100, 1),
            lesbians = round(mean(sub_lesbians) * 100, 1),
            pilots = round(mean(sub_pilots) * 100, 1),
            transgender = round(mean(sub_transgender) * 100, 1),
            veterans = round(mean(sub_veterans) * 100, 1))
pat_sub_pcnt <- data.frame(t(pat_sub_pcnt))
names(pat_sub_pcnt) <- "percent"
pat_sub_pcnt$subs <- row.names(pat_sub_pcnt)
pat_sub_pcnt <- mutate(pat_sub_pcnt,
                       subs = factor(subs, levels = subs))
levels(pat_sub_pcnt$subs) <- str_replace_all(levels(pat_sub_pcnt$subs),
                                         "_", " ")
pat_subs_plot <- ggplot(pat_sub_pcnt, aes(reorder(subs, percent), percent)) +
  geom_bar(stat = 'identity', fill = "#E55A00") +
  theme_bw() +
  ggtitle("Groups Therapists are Interested in Treating") +
  xlab("Population Groups") +
  ylab("% of therapists interested") + coord_flip()
ggsave('Plot Subgroups Subs.wmf', plot = pat_subs_plot)


##########################################
# Years in practice
years_practice_plot <- ggplot(md, aes(years_in_practice)) +
  geom_histogram(fill = "#E55A00", bins = 40) +
  xlim(0, 44) + theme_bw() +
  ggtitle("Years in Practice") +
  xlab("Years in practice") +
  ylab("Number of therapists") +
  scale_x_continuous(limits = c(0, 42),
    breaks = seq(0, 45, by = 3))
ggsave('Plot Years in Practice.wmf', plot = years_practice_plot)

##########################################
# Years since graduation

temp <- md[md$years_since_graduation < 66 & 
             !is.na(md$years_since_graduation), "years_since_graduation"]
years_sincegrad_plot <- ggplot(temp, aes(years_since_graduation)) +
  geom_histogram(fill = "#E55A00") +
  theme_bw() +
  ggtitle("Years Since Graduation") +
  xlab("Years since graduation") +
  ylab("Number of therapists") +
  scale_x_continuous(breaks = round(
    seq(min(temp$years_since_graduation, na.rm = T), 
        60, 
        by = 10),1))
ggsave('Plot Years Since Graduation.wmf', plot = years_sincegrad_plot)

# 64% graduated 20 or fewer years ago
sum(md$years_since_graduation < 21 & !is.na(md$years_since_graduation)) /
  sum(!is.na(md$years_since_graduation))

# 32% graduated 10 or fewer years ago
sum(md$years_since_graduation < 11 & !is.na(md$years_since_graduation)) /
  sum(!is.na(md$years_since_graduation))

# 10.5% graduated 5 or fewer years ago
sum(md$years_since_graduation < 6 & !is.na(md$years_since_graduation)) /
  sum(!is.na(md$years_since_graduation))

# 17.5% graduated over 30 years ago
sum(md$years_since_graduation < 31 & !is.na(md$years_since_graduation)) /
  sum(!is.na(md$years_since_graduation))

# 5% graduated over 40 years ago
sum(md$years_since_graduation > 40 & !is.na(md$years_since_graduation)) /
  sum(!is.na(md$years_since_graduation))

# Correlation between them is 0.84
cor(md$years_in_practice, md$years_since_graduation, use = "complete.obs")
plot_scatter_years <- ggplot(md, aes(years_in_practice, years_since_graduation)) +
  geom_point(color = "#E55A00", position = "jitter") +
  ggtitle("Years Since Graduation vs. Years in Practice") +
  xlab("Years in practice") +
  ylab("Years since graduation") +
  theme_bw() +
  scale_x_continuous(breaks = seq(min(temp$years_since_graduation, na.rm = T), 
        60, 
        by = 5)) +
  scale_y_continuous(breaks = seq(min(temp$years_since_graduation, na.rm = T), 
                                  70, 
                                  by = 5))
ggsave('Plot Years since & inpractice.wmf', plot = plot_scatter_years)


##########################################
# Insurance

# 71% said they accept insurance
count(md, insurance_yes)[2]/myN

# How many are lying?
# How many do actually accept at least one insurance plan?
# Only 50%:
count(md, insurance)[2]/myN
# So, 21% are lying they are accepting insurance plans

##########################################
# School

library(stringr)
schools <- md %>% count(school_new) %>% arrange(desc(n)) %>% 
  filter(!is.na(school_new)) %>% mutate(school = school_new) %>% 
  select(-school_new)
schools$school <- str_replace(schools$school, '^uni_', '')
schools$school <- str_replace_all(schools$school, '_', ' ')
schools <- schools %>% mutate(percent = round(n/myN * 100,1)) 
# View(schools)
sum(schools$percent[1:22]) # top 22 schoools - 59% of all therapists
str(schools)

schools_plot <- ggplot(schools[1:22,], aes(reorder(school, percent), percent)) +
  geom_bar(stat = 'identity', fill = "#E55A00") +
  theme_bw() +
  ggtitle("22 Schools Most Therapists Graduated From ") +
  xlab(NULL) +
  ylab("% of therapists") + coord_flip() +
  scale_y_continuous(breaks = seq(0, 18, 1)) +
  theme(axis.text.y = element_text(size = 10))
ggsave('Plot Schools they are from.wmf', 
       plot = schools_plot, height = 6)

##########################################
# Approaches to the therapy

appr_percents <- round(colMeans(md[approachvars]) * 100, 1)
appr_percents <- tibble(approaches = approachvars,
                        percent = appr_percents)
appr_percents <- arrange(appr_percents, desc(percent))
appr_percents$approaches <- str_replace_all(appr_percents$approaches,
                                            'appr_', '')
appr_percents$approaches <- str_replace_all(appr_percents$approaches,
                                            '_', ' ')
dim(appr_percents) # 36

# Top 15 approaches out of top 36 
appr_percents_top <- ggplot(appr_percents[1:15,], 
                            aes(reorder(approaches, percent), percent)) +
  geom_bar(stat = 'identity', fill = "#E55A00") +
  theme_bw() +
  ggtitle("15 Most Popular Therapeutic Approaches") +
  xlab(NULL) +
  ylab("% of therapists") +
  scale_y_continuous(breaks = seq(0, 70, by = 5)) +
  coord_flip()
ggsave('Plot Approaches Top 15.wmf', plot = appr_percents_top)

# Bottom 15 approaches out of top 36 
appr_percents_bottom <- ggplot(appr_percents[22:36,], 
                               aes(reorder(approaches, percent), percent)) +
  geom_bar(stat = 'identity', fill = "#E55A00") +
  theme_bw() +
  theme(plot.title = element_text(size = 12)) +
  ggtitle("15 Less Popular Therapeutic Approaches") +
  xlab(NULL) +
  ylab("% of therapists") +
  scale_y_continuous(breaks = seq(0, 10, by = 1)) +
  coord_flip()
ggsave('Plot Approaches Bottom 15.wmf', plot = appr_percents_bottom)

##########################################
# Additional Languages 
lng_percents <- round(colMeans(md[languagevars]) * 100, 1)
lng_percents <- tibble(Languages = languagevars,
                       percent = lng_percents)
lng_percents <- arrange(lng_percents, desc(percent))
lng_percents$Languages <- str_replace_all(lng_percents$Languages,
                                          'lng_', '')
lng_percents$Languages <- str_replace_all(lng_percents$Languages,
                                          '_', ' ')
dim(lng_percents) # 42
# View(lng_percents)

# Top 15 Languages out of top 42
lng_percents_top <- ggplot(lng_percents[1:20,], 
                           aes(reorder(Languages, percent), percent)) +
  geom_bar(stat = 'identity', fill = "#E55A00") +
  theme_bw() +
  ggtitle("20 Languages (besides English) Most Therapists Speak") +
  xlab("Languages") +
  ylab("% of therapists who speak") +
  scale_y_continuous(breaks = seq(0, 12, by = 1)) +
  coord_flip()
ggsave('Plot Languages Top 20.wmf', plot = lng_percents_top,
       height = 5)

# Top 42 Languages
lng_percents_top42 <- ggplot(lng_percents, 
                           aes(reorder(Languages, percent), percent)) +
  geom_bar(stat = 'identity', fill = "#E55A00") +
  theme_bw() +
  theme(axis.text.y = element_text(size = 6)) +
  ggtitle("42 Languages (Besides English) Most Therapists Speak") +
  xlab("Languages") +
  ylab("% of therapists who speak") +
  scale_y_continuous(breaks = seq(0, 12, by = 1)) +
  coord_flip()
ggsave('Plot Languages Top 42.wmf', plot = lng_percents_top42,
       width = 7, height = 5)

##########################################
# Insurances

md$ins_blue_cross_blue_shield <- md$ins_blue_cross_blue_shield +
  md$ins_bluecross_and_blueshield
md$ins_bluecross_and_blueshield <- NULL
insurancevars <- names(md)[grep("^ins_", names(md))]

ins_percents <- round(colMeans(md[insurancevars]) * 100, 1)
ins_percents <- tibble(Insurances = insurancevars,
                       percent = ins_percents)
ins_percents <- arrange(ins_percents, desc(percent))
ins_percents$Insurances <- str_replace_all(ins_percents$Insurances,
                                          'ins_', '')
ins_percents$Insurances <- str_replace_all(ins_percents$Insurances,
                                          '_', ' ')
dim(ins_percents) # 56
# View(ins_percents)

# Top 15 Insurances
ins_percents_top <- ggplot(ins_percents[1:15,], 
                           aes(reorder(Insurances, percent), percent)) +
  geom_bar(stat = 'identity', fill = "#E55A00") +
  theme_bw() +
  ggtitle("15 Insurance Plans Accepted by Most Therapists") +
  xlab("Insurance Plans") +
  ylab("% of therapists who accept") +
  scale_y_continuous(breaks = seq(0, 35, by = 5)) +
  coord_flip()
ggsave('Plot Insurances Top 15.wmf', plot = ins_percents_top)


##########################################
# Titles

ttl_percents <- round(colMeans(md[titlevars]) * 100, 1)
ttl_percents <- tibble(Titles = titlevars,
                       percent = ttl_percents)
ttl_percents <- arrange(ttl_percents, desc(percent))
ttl_percents$Titles <- str_replace_all(ttl_percents$Titles,
                                       'ttl_', '')
ttl_percents$Titles <- str_replace_all(ttl_percents$Titles,
                                       '_', ' ')
dim(ttl_percents) # 51
# View(ttl_percents)

# Top 15 Titles
ttl_percents_top <- ggplot(ttl_percents[1:15,], 
                           aes(reorder(Titles, percent), percent)) +
  geom_bar(stat = 'identity', fill = "#E55A00") +
  theme_bw() +
  ggtitle("15 Most Frequently Used Therapist Titles") +
  xlab(NULL) +
  ylab("% of therapists who used it in their ad") +
  scale_y_continuous(breaks = seq(0, 50, by = 5)) +
  coord_flip()
ggsave('Plot Titles Top 15.wmf', plot = ttl_percents_top)

##########################################
# Specialties
spec_percents <- round(colMeans(md[specialtyvars]) * 100, 1)
spec_percents <- tibble(Specialties = specialtyvars,
                        percent = spec_percents)
spec_percents <- arrange(spec_percents, desc(percent))
spec_percents$Specialties <- str_replace_all(spec_percents$Specialties,
                                             'spec_', '')
spec_percents$Specialties <- str_replace_all(spec_percents$Specialties,
                                             '_', ' ')
dim(spec_percents) # 68
# View(spec_percents)

# Top 20 Specialties
spec_percents_top <- ggplot(spec_percents[1:20,], 
                            aes(reorder(Specialties, percent), percent)) +
  geom_bar(stat = 'identity', fill = "#E55A00") +
  theme_bw() +
  ggtitle("Specialties of Most Therapists") +
  xlab(NULL) +
  ylab("% of therapists who select") +
  scale_y_continuous(breaks = seq(0, 55, by = 5)) +
  coord_flip()
ggsave('Plot Specialties Top 20.wmf', plot = spec_percents_top)

spec_percents_bottom <- ggplot(spec_percents[49:68,], 
                            aes(reorder(Specialties, percent), percent)) +
  geom_bar(stat = 'identity', fill = "#E55A00") +
  theme_bw() +
  ggtitle("Less Popular Specialties") +
  xlab(NULL) +
  ylab("% of therapists who select")  +
  coord_flip()
ggsave('Plot Specialties Bottom 20.wmf', plot = spec_percents_bottom)


##########################################
# Issues

iss_percents <- round(colMeans(md[issuesvars]) * 100, 1)
iss_percents <- tibble(Issues = issuesvars,
                       percent = iss_percents)
iss_percents <- arrange(iss_percents, desc(percent))
iss_percents$Issues <- str_replace_all(iss_percents$Issues,
                                       'iss_', '')
iss_percents$Issues <- str_replace_all(iss_percents$Issues,
                                       '_', ' ')
dim(iss_percents) # 98
# View(iss_percents)

# Top 20 Issues
iss_percents_top <- ggplot(iss_percents[1:20,], 
                           aes(reorder(Issues, percent), percent)) +
  geom_bar(stat = 'identity', fill = "#E55A00") +
  theme_bw() +
  ggtitle("20 Issues Most Therapists Specialize in") +
  xlab(NULL) +
  ylab("% of therapists who specialize in it") +
  scale_y_continuous(breaks = seq(0, 70, by = 5)) +
  coord_flip()
ggsave('Plot Issues Top 20.wmf', plot = iss_percents_top, height = 5)

# Bottom 20 Issues
iss_percents_bottom <- ggplot(iss_percents[77:96,], 
                           aes(reorder(Issues, percent), percent)) +
  geom_bar(stat = 'identity', fill = "#E55A00") +
  theme_bw() +
  ggtitle("20 Less Popular Issues (from top 100)") +
  xlab(NULL) +
  ylab("% of therapists who specialize in it") +
  scale_y_continuous(breaks = seq(0, 4, by = 1)) +
  coord_flip()
ggsave('Plot Issues Bottom 20.wmf', plot = iss_percents_bottom, height = 5)


numericvars <- names(md)[!names(md) %in% c("license_state", "zipcode",
                                           "school_new")]
total_means <- colMeans(select(md, one_of(numericvars)), na.rm = TRUE)

# length(numericvars) + 3 == ncol(md)

#---------------------------------------------------------------------------
# Focusing on findings by Zipcode
# Correlating them with available external zipcode info:
# number of households and median income by zipcode
#---------------------------------------------------------------------------

byzip <- md %>% select(-license_state, -school_new) %>% 
  group_by(zipcode) %>% summarize_all(.funs = mean, na.rm = TRUE)
# View(byzip)
# names(byzip)
dim(byzip)  # 168 by 388
str(byzip$zipcode)

therapist_count <- md %>% group_by(zipcode) %>% count()
byzip <- left_join(byzip, therapist_count, by = 'zipcode')
dim(byzip)  # 168 by 389

income <- read_csv("median_income_ny.csv")[-2]
dim(income) # have info only for 168 zipcodes
glimpse(income)

dim(byzip)  # 168 by 389
byzip <- inner_join(byzip, income, by = 'zipcode')
dim(byzip)  # 153 by 391

# names(byzip)

##########################################
# Number of therapists vs. number of Households
# If there is justince, the correlation should be hi

# Correlation between them is small!, just 0.20:
cor(byzip$n, byzip$hh_count)

plot_n_hh <- ggplot(byzip, aes(hh_count/1000, n)) +
  geom_point(color = "#E55A00", position = "jitter") +
  ggtitle("Number of Housholds vs. Number of Therapists") +
  xlab("Number of households (thousands)") +
  ylab("Number of therapists") +
  theme_bw() +
  scale_x_continuous(breaks = seq(0, 45, by = 5)) +
  scale_y_continuous(breaks = seq(0, 700, by = 100))
ggsave('Plot Zips HH vs. n of Therapists.wmf', plot = plot_n_hh)

# Plot with dot color being the median income:
# View(byzip %>% select(median_income) %>% arrange(desc(median_income)))
plot_colored_n_hh <- ggplot(byzip[!byzip$median_income %in% 260000,], 
                            aes(hh_count/1000, n,
                                color = median_income)) +
  geom_point(position = "jitter") +   # "#E55A00"
  ggtitle("Number of Housholds vs. Number of Therapists") +
  xlab("Number of households (thousands)") +
  ylab("Number of therapists") +
  theme_bw() +
  scale_x_continuous(breaks = seq(0, 45, by = 5)) +
  scale_y_continuous(breaks = seq(0, 700, by = 100)) +
  scale_color_gradient(low = "yellow", high = "darkred", name = 'Median Income')
ggsave('Plot Zips HH vs. n of Therapists COLORED.wmf', plot = plot_colored_n_hh)


##########################################
# Number of therapists vs. median income


ggplot(byzip, aes(log(median_income), log(n))) +
  geom_point(color = "#E55A00", position = "jitter") +
  ggtitle("Median Income vs. Number of Therapists") +
  xlab("Median Income (thousands $)") +
  ylab("Number of therapists") +
  theme_bw()

# Correlation between them is higher!: 0.48
cor(byzip$n, byzip$median_income)

plot_income_hh <- ggplot(byzip, aes(median_income/1000, n)) +
  geom_point(color = "#E55A00", position = "jitter") +
  ggtitle("Median Income vs. Number of Therapists") +
  xlab("Median Income (thousands $)") +
  ylab("Number of therapists") +
  theme_bw() +
  scale_x_continuous(breaks = seq(0, 260, by = 20)) +
  scale_y_continuous(breaks = seq(0, 700, by = 100))
ggsave('Plot Zips Income vs. n of Therapists.wmf', plot = plot_income_hh)


# Colored by number of households
plot_income_hh_col <- ggplot(byzip, aes(median_income/1000, n,
                                    color = hh_count)) +
  geom_point(position = "jitter") +
  ggtitle("Median Income vs. Number of Therapists") +
  xlab("Median Income ($ thousands )") +
  ylab("Number of therapists") +
  theme_bw() +
  scale_x_continuous(breaks = seq(0, 260, by = 20)) +
  scale_y_continuous(breaks = seq(0, 700, by = 100)) +
  scale_color_gradient(low = "yellow", high = "darkred", name = 'HH count')
ggsave('Plot Zips Income vs. n of Therapists COLORED.wmf', plot = plot_income_hh_col)

# Correlation is a bit higher when both are logged: 0.51
cor(log(byzip$n), log(byzip$median_income))

plot_income_hh_log <- ggplot(byzip, aes(log(median_income), log(n))) +
  geom_point(color = "#E55A00", position = "jitter") +
  ggtitle("Median Income vs. Number of Therapists") +
  xlab("Natural log of median income") +
  ylab("Natural log of number of therapists") +
  theme_bw()+ geom_smooth(method = "lm")
ggsave('Plot Zips LOG Income vs. LOG n of Therapists.jpg', plot = plot_income_hh_log)


##########################################
# Number of therapists vs. total income:
# i.e., median income times number of households

byzip <- byzip %>% mutate(income_total = median_income * hh_count)
# names(byzip)

# Correlation between them is even higher!: 0.63
cor(byzip$n, byzip$income_total)
plot_total_income_n <- ggplot(byzip, aes(income_total/1000000, n)) +
  geom_point(color = "#E55A00", position = "jitter") +
  ggtitle("Total Zip Income vs. Number of Therapists") +
  xlab("Total Zip Income ($ millions)") +
  ylab("Number of therapists") +
  theme_bw() +
  scale_x_continuous(breaks = seq(0, 4000, by = 500)) +
  scale_y_continuous(breaks = seq(0, 700, by = 100))
ggsave('Plot Zips Total Income vs. n of Therapists.wmf', plot = plot_total_income_n)

###########################################3
### googleVis scatter
library(googleVis)
# names(byzip)
forscatter1_zip <- byzip %>% select(income_total, n)
forscatter1_zip$income_total.html.tooltip <- byzip$zipcode
forscatter1_zip$income_total <- forscatter1_zip$income_total/1000000

my_options <- list(height = "400px",  # width = "1000px",
                   title = "Total Zip Income vs. Number of Therapists",
                   hAxis = "{title:'Total Zip Income ($ mil)'}",
                   vAxis = "{title:'Number of Therapists'}",
                   legend = "{position: 'none'}",
                   colors = "['#f03b20']")
my_options$explorer <- "{actions:['dragToZoom', 'rightClickToReset']}"
my_options$trendlines <- "{0: { type: 'exponential', color: 'green'}}" # a trendline
plot(gvisScatterChart(forscatter1_zip, options = my_options))
###########################################3

##########################################
# Median income vs. session fees

# Correlation between them is even higher!:
cor(byzip$median_income, byzip$fee_from)  # 0.44
cor(byzip$median_income, byzip$fee_to)    # 0.31

plot_total_income_fee <- ggplot(byzip, aes(median_income/1000, fee_from)) +
  geom_point(color = "#E55A00", position = "jitter") +
  ggtitle("Total Median Income vs. Session Fee ('from')") +
  xlab("Median Income") +
  ylab("Session Fee ('from')") +
  theme_bw() +
  scale_x_continuous(breaks = seq(0, 270, by = 30)) +
  scale_y_continuous(breaks = seq(50, 200, by = 10))
ggsave('Plot Zips Median Income vs. Session Fee.wmf', plot = plot_total_income_fee)

##########################################
# Median income vs. Accepting Insurance

cor(byzip$median_income, byzip$insurance) # Negative -.23

plot_income_insur <- ggplot(byzip, aes(median_income/1000, insurance*100)) +
  geom_point(color = "#E55A00", position = "jitter") +
  ggtitle("Total Median Income vs. % Accepting Insurance") +
  xlab("Median Income ($ thousands)") +
  ylab("% accepting insurance") +
  theme_bw() +
  scale_x_continuous(breaks = seq(0, 350, by = 50)) +
  scale_y_continuous(breaks = seq(0, 100, by = 10))
ggsave('Plot Zips Median Income vs. Accept Insurance.wmf', plot = plot_income_insur)



##########################################
# Number of therapists per 1000 households:
# byzip$n
# byzip$hh_count
byzip <- byzip %>% mutate(n_per_1000 = round(n/hh_count * 1000, 1))
cor(byzip$n_per_1000, byzip$income_total)

# names(byzip)[1]

temp <- byzip %>% select(zipcode, n_per_1000) %>% arrange(desc(n_per_1000))
View(temp)
dim(temp)

zip_density_top <- ggplot(temp[c(1:20),], 
                      aes(reorder(zipcode, n_per_1000), n_per_1000)) +
  geom_bar(stat = 'identity', fill = "#E55A00") +
  theme_bw() +
  ggtitle("Number of Therapists per 1000 Households") +
  xlab("Zip Codes") +
  ylab("therapists per 1000 households") +
  coord_flip() +
  theme(axis.text.y = element_text(size = 8)) +
  scale_y_continuous(breaks = seq(0, 180, by = 10))
ggsave('Plot Zipcodes w. Most Therapists.wmf', plot = zip_density_top,
       width = 7, height = 5)

zip_density_bottom <- ggplot(temp[c(144:153),], 
                          aes(reorder(zipcode, n_per_1000), n_per_1000)) +
  geom_bar(stat = 'identity', fill = "#E55A00") +
  theme_bw() +
  ggtitle("Number of Therapists per 1000 Households") +
  xlab("Zip Codes") +
  ylab("therapists per 1000 households") +
  coord_flip() +
  theme(axis.text.y = element_text(size = 8)) +
  scale_y_continuous(breaks = seq(0, 0.7, by = 0.1))
ggsave('Plot Zipcodes w. Fewest Therapists.wmf', plot = zip_density_bottom,
       width = 7, height = 5)

View(byzip[c("zipcode", "n")])

#############################################
# Coloring NYC zipcodes by therapist density

library(choroplethr)
library(stringr)
library(tidyverse)
library(devtools)
install_github('arilamstein/choroplethrZip@v1.3.0')
library(choroplethrZip)
mynyczips <- byzip[c("zipcode", "n_per_1000")]
dim(mynyczips) # 153 rows
mynyczips$zipcode <- str_replace(mynyczips$zipcode, "US", "")
names(mynyczips) <- c('region', 'density')
head(mynyczips)

# Joining my zips and therapist density with df_zip_demographics:
data(df_zip_demographics)
df_zip_demographics <- left_join(df_zip_demographics, 
                                 mynyczips, by = 'region')
dim(df_zip_demographics)
names(df_zip_demographics)
sum(!is.na(df_zip_demographics$density))  # 153
df_zip_demographics$value = df_zip_demographics$density

# Creating the map
zip_map = ZipChoropleth$new(df_zip_demographics)
zip_map$ggplot_polygon = geom_polygon(aes(fill = value))
zip_map$set_zoom_zip(state_zoom  = NULL, 
                     county_zoom = c('36005', '36061', '36047',
                                     '36081', '36085'), 
                     msa_zoom    = NULL, 
                     zip_zoom    = NULL)
zip_map$title = "Therapist Density in NYC"

choro = zip_map$render()
densitymap <- choro + scale_fill_brewer(palette = 'OrRd') +
  labs(fill = "Therapists/1000 HHs ") + 
  theme(legend.text = element_text(size = 7),
        legend.title = element_text(size = 9))
ggsave('Therapist Density in NYC.wmf', plot = densitymap)

#---------------------------------------------------------------------------
# Dendrogram of top 3 specialties (not used in presentation)
#---------------------------------------------------------------------------
library(stringr)
MyMatrix <- as.matrix(md[specialtyvars])
tempnames <- str_replace(specialtyvars, "spec_", "")
tempnames <- str_replace_all(tempnames, "_", " ")
tempnames <- str_replace_all(tempnames, "and", "&")
tempnames <- str_replace_all(tempnames, " or ", "/")

dimnames(MyMatrix)[[2]] <- tempnames
library(cluster)
DistMatrix <- daisy(t(MyMatrix))
mycluster <- agnes(DistMatrix, method = "ward")
par(cex = .7)
plot(main = "", sub="", mycluster, which.plot = 2,
     axes = FALSE, ylab = NULL, xlab = NULL)

plot(main = "", sub= "", mycluster, which.plot = 2,
     axes = FALSE, ylab = NULL, xlab = NULL, hang = -1)

install.packages('ape')
library(ape)

temp <- t(as.matrix(md[specialtyvars]))
row.names(temp) <- str_replace(row.names(temp), "spec_", "")
row.names(temp) <- str_replace_all(row.names(temp), "_", " ")
dim(temp)
hc <- hclust(dist(temp))
plot(as.phylo(hc), cex = 0.9, label.offset = 1)
