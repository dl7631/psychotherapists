###########################################################################
# Dimitri post-web-scraping data cleaning script
###########################################################################

# Grab the names of ALL .csv files for different zip codes:
filenames <- list.files(path = ".", pattern = "^zip_")
# Check the number of therapists across all zips (all zip_XXX.csv files)
nr_of_rows <- 0
for (file in filenames) {
  nr_of_rows <- nr_of_rows + nrow(read.csv(file))
}
nr_of_rows  # 14,304

# Creating my data set with ALL therapists from ALL zipcodes:
md <- do.call(rbind, lapply(dir(pattern = "zip_"), read.csv, 
                            stringsAsFactors = F))
dim(md)
# names(md)
# Remove the first column nobody need:
md <- md[-1]
dim(md)    # 14,304 by 35
# names(md)
# head(md)

# Replace certain characters in client_ethnicities

# unique(md$client_ethnicities)
library(tidyverse)
md$client_ethnicities <- str_replace(md$client_ethnicities, '-', '_')
md$client_ethnicities <- str_replace(md$client_ethnicities, ' and ', '_')
md$client_ethnicities <- str_replace(md$client_ethnicities, 
                                     'Other Racial or Ethnic Background', 
                                     'Other')
md$client_ethnicities <- str_replace(md$client_ethnicities, 
                                     'Pacific Islander', 
                                     'Pacific_Islander')
md$client_ethnicities <- str_replace(md$client_ethnicities, 
                                     'Native American', 
                                     'Native_American')
md$client_ethnicities <- str_replace(md$client_ethnicities, '^', 'ethn_')
md$client_ethnicities <- str_replace_all(md$client_ethnicities, ', ', ', ethn_')
# unique(md$client_ethnicities)

# Replace certain characters in client_languages
# unique(md$client_languages)
md$client_languages <- str_replace(md$client_languages, 
                                   'Spanish, Spanish', 'Spanish')
md$client_languages <- str_replace(md$client_languages, 
                                   'French, French', 'French')
md$client_languages <- str_replace(md$client_languages, 
                                   'Hindi, Hindi', 'Hindi')
md$client_languages <- str_replace(md$client_languages, 
                                   'Russian, Russian', 'Russian')
md$client_languages <- str_replace(md$client_languages, 
                                   'None', 'X')
md$client_languages <- str_replace(md$client_languages, 
                                   'Danish Norwegian', 'Danish, Norwegian')
md$client_languages <- str_replace(md$client_languages, 
                                   'Culturally competent in Japanese', 'Japanese')
md$client_languages <- str_replace(md$client_languages, 
                                   ', Other', '')
md$client_languages <- str_replace(md$client_languages, 
                                   'Greek, Greek; Spanish for limited communication only',
                                   'Greek')
md$client_languages <- str_replace(md$client_languages, 
                                   'Conversational Spanish', 'Spanish')
md$client_languages <- str_replace(md$client_languages, 
                                   'Some French and German', 'X')
md$client_languages <- str_replace(md$client_languages, 
                                   'Spanish \\(Basic\\)', 'X')
md$client_languages <- str_replace(md$client_languages, 
                                   'Fully fluent in both English and Spanish, Spanish',
                                   'Spanish')
md$client_languages <- str_replace(md$client_languages, 
                                   'Fully fluent in both English, Spanish',
                                   'Spanish')
md$client_languages <- str_replace(md$client_languages, 
                                   'Spanish \\(office manager Carmen Rue\\)', 'X')
md$client_languages <- str_replace(md$client_languages, 
                                   'Colloquial French and Yidish, Hebrew, Spanish',
                                   'French, Yiddish, Hebrew, Spanish')
md$client_languages <- str_replace(md$client_languages, 
                                   'Hebrew, Other, some Yiddish', 'Hebrew')
md$client_languages <- str_replace(md$client_languages, 
                                   'Haitian Creole', 'Haitian')
md$client_languages <- str_replace(md$client_languages, 
                                   'Korean, Korean', 'Korean')
md$client_languages <- str_replace(md$client_languages, 
                                   'For South Asian languages call \\(347\\) 878 8608, Hindi, Punjabi, Urdu',
                                   'X')
md$client_languages <- str_replace(md$client_languages, 
                                   'French, French\\/Creole', 'Haitian')
md$client_languages <- str_replace(md$client_languages, 
                                   'French\\/Creole', 'Haitian')
md$client_languages <- str_replace(md$client_languages, 
                                   'Some Spanish but not fluent as of yet', 
                                   'X')
md$client_languages <- str_replace(md$client_languages, 
                                   'Some Hebrew and Russian', 'X')
md$client_languages <- str_replace(md$client_languages, 
                                   'Conversant in Hebrew', 'Hebrew')
md$client_languages <- str_replace(md$client_languages, 
                                   'Greek, Some spanish', 'Greek')
md$client_languages <- str_replace(md$client_languages, 
                                   'No English Services Only Spanish Services, Spanish', 
                                   'Spanish')
md$client_languages <- str_replace(md$client_languages, 
                                   ', Other, ', ', ')
md$client_languages <- str_replace(md$client_languages, 
                                   ', Other', '')
md$client_languages <- str_replace(md$client_languages, 
                                   'SPANISH, Spanish', 'Spanish')
md$client_languages <- str_replace(md$client_languages, 
                                   'Italian, Spanish, Spanish Italian', 
                                   'Italian, Spanish')
md$client_languages <- str_replace(md$client_languages, 
                                   'Italian, Italian Dialect : Calabrese', 
                                   'Italian')
md$client_languages <- str_replace(md$client_languages, 
                                   ', Some Spanish', '')
md$client_languages <- str_replace(md$client_languages, 
                                   'Referral to Spanish speaking colleague available',
                                   'X')
md$client_languages <- str_replace(md$client_languages, 
                                   ' and ', ', ')
md$client_languages <- str_replace(md$client_languages, 
                                   'Working French', 'French')
md$client_languages <- str_replace(md$client_languages, 
                                   'Chinese Arabic Portuguese via interpreter, ', '')
md$client_languages <- str_replace(md$client_languages, 
                                   ', some Yiddish', '')
md$client_languages <- str_replace(md$client_languages, 
                                   'X but English but learning Spanish', 'X')
md$client_languages <- str_replace(md$client_languages, 
                                   'French, Hatian - Creole', '')
md$client_languages <- str_replace(md$client_languages, 
                                   'Gujarati, Hiindi Urdu Kannada Telugu, Hindi, Punjabi, Urdu', 
                                   'Gujarati, Hiindi, Urdu, Kannada, Telugu, Punjabi')
md$client_languages <- str_replace(md$client_languages, 
                                   'French Creole', 'Haitian')
md$client_languages <- str_replace(md$client_languages, 
                                   'A Staff Therapist speaks: ', '')
md$client_languages <- str_replace(md$client_languages, 
                                   'Elementary Japanese, ', '')
md$client_languages <- str_replace(md$client_languages, 
                                   'Moderate Spanish', 'Spanish')
md$client_languages <- str_replace(md$client_languages, 
                                   'Finnish Norwegian', 'Finnish, Norwegian')
md$client_languages <- str_replace(md$client_languages, 
                                   'French but rusty\\.', 'French')
md$client_languages <- str_replace(md$client_languages, 
                                   'ASL, American Sign Language', 'ASL')
md$client_languages <- str_replace(md$client_languages, 
                                   'Understand some German', 'X')
md$client_languages <- str_replace(md$client_languages, 
                                   'Other, Thai', 'Thai')
md$client_languages <- str_replace(md$client_languages, 
                                   'Some Spanish', '')
md$client_languages <- str_replace(md$client_languages, 
                                   'Mindfulness Therapies', 'X')
md$client_languages <- str_replace(md$client_languages, 
                                   'The Arts', 'X')
md$client_languages <- str_replace(md$client_languages, 
                                   'Working knowledge of French, ', '')
md$client_languages <- str_replace(md$client_languages, 
                                   'Hebrew, Millenial ;-\\)', 'Hebrew')
md$client_languages <- str_replace(md$client_languages, 
                                   'French, Spanish, Turkish, Turkish French Spanish', 
                                   'French, Spanish, Turkish')
md$client_languages <- str_replace(md$client_languages, 
                                   'Spanish-Not fluent', 'X')
md$client_languages <- str_replace(md$client_languages, 
                                   'I specialize in ESL: English as a second language\\.', 
                                   'X')
md$client_languages <- str_replace(md$client_languages, 
                                   'Minimal AS', 'X')
md$client_languages <- str_replace(md$client_languages, 
                                   'Some Greek', 'X')
md$client_languages <- str_replace(md$client_languages, 
                                   'Limited Spanish', 'X')
md$client_languages <- str_replace(md$client_languages, 
                                   'French a bit of German', 'French')
md$client_languages <- str_replace(md$client_languages, 
                                   'French, Italian, Spanish Italian French', 
                                   'French, Italian, Spanish')
md$client_languages <- str_replace(md$client_languages, 
                                   ', I understand French', '')
md$client_languages <- str_replace(md$client_languages, 
                                   'Limited Japanese', '')
md$client_languages <- str_replace(md$client_languages, 
                                   'Czech Slovak', 'Czech, Slovak')
md$client_languages <- str_replace(md$client_languages, 
                                   'German, Hebrew, I understand but am not fluent, Yiddish',
                                   'German, Hebrew')
md$client_languages <- str_replace(md$client_languages, 
                                   'Specialist with Immigrant Populations', 'X')
md$client_languages <- str_replace(md$client_languages, 
                                   'Multi-cultural counseling', 'X')
md$client_languages <- str_replace(md$client_languages, 
                                   'Brazilian Portuguese, Portuguese', 'Portuguese')
md$client_languages <- str_replace(md$client_languages, 
                                   'N/A', 'X')
md$client_languages <- str_replace(md$client_languages, 
                                   'Spanish but it has to be spoken slowly', 'X')
md$client_languages <- str_replace(md$client_languages, 
                                   'Other, some spanish', 'X')
md$client_languages <- str_replace(md$client_languages, 
                                   'American, French', 'French')
md$client_languages <- str_replace(md$client_languages, 
                                   'Understand some yiddish, Hebrew', 'Hebrew')
md$client_languages <- str_replace(md$client_languages, 
                                   'Some German, Spanish', 'Spanish')
md$client_languages <- str_replace(md$client_languages, 
                                   'I understand Hebrew, Yiddish', 'Hebrew, Yiddish')
md$client_languages <- str_replace(md$client_languages, 
                                   'Conversational spanish, german', 'Spanish, German')
md$client_languages <- str_replace(md$client_languages, 
                                   'Other', 'X')
md$client_languages <- str_replace(md$client_languages, 
                                   'Spanish native speaker', 'Spanish')
md$client_languages <- str_replace(md$client_languages, 
                                   'Translators available for all languages', 'X')
md$client_languages <- str_replace(md$client_languages, 
                                   'German, German Spanish', 'German, Spanish')
md$client_languages <- str_replace(md$client_languages, 
                                   'Bilingual English, Spanish', 'Spanish')
md$client_languages <- str_replace(md$client_languages, 
                                   'Some French not totally fluent', 'X')
md$client_languages <- str_replace(md$client_languages, 
                                   'Minimal italian', 'X')
md$client_languages <- str_replace(md$client_languages, 
                                   'Hebrew, Some Yiddish', 'Hebrew')
md$client_languages <- str_replace(md$client_languages, 
                                   'Farsi, X', 'Farsi')
md$client_languages <- str_replace(md$client_languages, 
                                   'Conversational Italian, Greek, Polish', 'Greek, Polish')
md$client_languages <- str_replace(md$client_languages, 
                                   'Fully fluent in both English, Spanish', 'Spanish')
md$client_languages <- str_replace(md$client_languages, 
                                   'Yoruba', 'X')
md$client_languages <- str_replace(md$client_languages, 
                                   'I lived in Brasil for 3 years', 'Portuguese')
md$client_languages <- str_replace(md$client_languages, 
                                   'XL', 'X')
md$client_languages <- str_replace(md$client_languages, 
                                   'Some Italian', 'X')
md$client_languages <- str_replace(md$client_languages, 
                                   'Igbo', 'X')
md$client_languages <- str_replace(md$client_languages, 
                                   'Bengali \\(Bangla\\)', 'Bengali')
md$client_languages <- str_replace(md$client_languages, 
                                   'Engllsh, Italian', 'Italian')
md$client_languages[md$client_languages %in% ''] <- "X"
md$client_languages <- str_replace(md$client_languages, '^', 'lng_')
md$client_languages <- str_replace_all(md$client_languages, ', ', ', lng_')
# unique(md$client_languages)

# Replace certain characters in client_religion

# unique(md$client_religion)
md$client_religion <- str_replace(md$client_religion, 
                                   'Other Spiritual or Religious Affiliations', 'X')
md$client_religion <- str_replace(md$client_religion, '^', 'rel_')
md$client_religion <- str_replace_all(md$client_religion, ', ', ', rel_')
# unique(md$client_religion)

# Cleaning up the license column and adding
# a new column NYlicense (True or False)
# table(md$license_state, useNA = 'ifany')
md$license_state <- gsub('[ 0-9-]+', '', md$license_state)
md$license_state <- str_replace_all(md$license_state,
                                    '\\(Psychoanalysis\\)NewYork', 'NewYork')
md$license_state <- str_replace_all(md$license_state, '#NewYork', 'NewYork')
md$license_state <- str_replace_all(md$license_state, '#RNewYork', 'NewYork')
md$license_state <- str_replace_all(md$license_state, 'LicNewYork', 'NewYork')
md$license_state <- str_replace_all(md$license_state, 'PNewYork', 'NewYork')
md$license_state <- str_replace_all(md$license_state, 'RNewYork', 'NewYork')
md$license_state[md$license_state == ''] <- NA
table(md$license_state, useNA = 'ifany')
md$NYlicense <- md$license_state == 'NewYork'
table(md$NYlicense, useNA = 'ifany')

# Payment methods:
md$payment_methods <- str_replace(md$payment_methods, 
                                  'xNotListed', 'paym_notlisted')

# Per session:
# sort(unique(md$per_session))
md$per_session <- str_replace(md$per_session, '\\+', '')
md$per_session <- str_replace(md$per_session, 'upto100', '60')
md$per_session <- str_replace(md$per_session, 'upto110', '65')
md$per_session <- str_replace(md$per_session, 'upto120', '70')
md$per_session <- str_replace(md$per_session, 'upto130', '80')
md$per_session <- str_replace(md$per_session, 'upto140', '85')
md$per_session <- str_replace(md$per_session, 'upto150', '90')
md$per_session <- str_replace(md$per_session, 'upto160', '100')
md$per_session <- str_replace(md$per_session, 'upto170', '110')
md$per_session <- str_replace(md$per_session, 'upto180', '120')
md$per_session <- str_replace(md$per_session, 'upto190', '130')
md$per_session <- str_replace(md$per_session, 'upto200', '140')
md$per_session <- str_replace(md$per_session, 'upto220', '145')
md$per_session <- str_replace(md$per_session, 'upto230', '150')
md$per_session <- str_replace(md$per_session, 'upto250', '150')
md$per_session <- str_replace(md$per_session, 'upto270', '155')
md$per_session <- str_replace(md$per_session, 'upto300', '200')
md$per_session <- str_replace(md$per_session, 'upto50', '50')
md$per_session <- str_replace(md$per_session, 'upto70', '70')
md$per_session[md$per_session == ''] <- NA
md <- separate(md, per_session, c("fee_from", "fee_to"), remove = F, convert = T)
str(md$fee_from)
str(md$fee_to)

# Insurance Plans:
length(unique(md$plans))
length(md$plans[md$plans %in% 'xNotListed'])/nrow(md)  # 18% insurance not listed
md$plans <- str_replace(md$plans, 'xNotListed', 'ins_notlisted')
md$plans <- str_replace(md$plans, 'ins_will_accept_assignment_of_benefits, ', '')
md$plans <- str_replace(md$plans, ', ins_will_accept_assignment_of_benefits', '')
md$plans <- str_replace(md$plans, 'ins_will_accept_assignment_of_benefits', 
                        'ins_notlisted')
md$plans <- str_replace(md$plans,
                        'ins_out_of_network, ins_out_of_network, ins_out_of_network', 
                        'ins_out_of_network')
md$plans <- str_replace(md$plans, 'ins_out_of_network, ins_out_of_network', 
                        'ins_out_of_network')
md$plans <- str_replace(md$plans, 'ins_chp_', 'ins_chp')
md$plans <- str_replace(md$plans, 'ins_oon_=_out_of_network, ins_your_out_of_network_benefit_reimburses_up_to_80%, ins_out_of_network',
                        'ins_out_of_network')
md$plans <- str_replace(md$plans, 'ins_call_about_session_fee\\., ', '')
md$plans <- str_replace(md$plans, 'ins_receipts_for_reimbursement', 'ins_out_of_network')
md$plans <- str_replace(md$plans, 'ins_ppo_plans_reimburse_you_when_you_pay_for_therapy\\., ', 'ins_out_of_network')
md$plans <- str_replace(md$plans, 'ins__reduced_fee_available_on_a_limited_basis, ', '')
md$plans <- str_replace(md$plans, 'ins_invoices_provided_for_reinbursement', 'ins_out_of_network')
md$plans <- str_replace(md$plans, 'ins_consolidated_health_plan_', 'ins_consolidated_health_plan')
md$plans <- str_replace(md$plans, 'ins_call_for_exam_prices,', '')

# Creating a new variable insurance that is = 1 only
# if a person accepts more than one insurance or
# if it is just one insurance, then it's not 'ins_out_of_network'
# and NOT 'ins_notlisted'
nchar("ins_out_of_network")  # 18 characters
more_than_one_insurance <- sapply(str_split(md$plans, pattern = ", "), length) > 1 
only_one_insurance <- sapply(str_split(md$plans, pattern = ", "), length) == 1 
out_of_network_instring <- str_extract(md$plans,'ins_out_of_network') %in% "ins_out_of_network"
notlisted <- str_extract(md$plans,'ins_notlisted') %in% "ins_notlisted"

md$insurance <- 0
md$insurance[more_than_one_insurance] <- 1
md$insurance[only_one_insurance & !out_of_network_instring & !notlisted] <- 1
count(md, insurance)

# school

md$school[md$school == 'uni_-'] <- NA
md$school[md$school == ''] <- NA

md$school_new <- md$school
md$school_new[str_extract(md$school,'adelphi') %in% "adelphi"] <- "uni_adelphi_uni"
md$school_new[str_extract(md$school,'albert_einstein') %in% "albert_einstein"] <- "uni_albert_einstein_college_of_medicine"
md$school_new[str_extract(md$school,'alliance_graduate_school') %in% "alliance_graduate_school"] <- "uni_alliance_graduate_school_of_counseling"
md$school_new[str_extract(md$school,'alliant_international') %in% "alliant_international"] <- "uni_alliant_international_university"
md$school_new[str_extract(md$school,'american_institute_for_psychoanalysis') %in% "american_institute_for_psychoanalysis"] <- "uni_american_institute_for_psychoanalysis"
md$school_new[str_extract(md$school,'antioch') %in% "antioch"] <- "uni_antioch_university"
md$school_new[str_extract(md$school,'argosy') %in% "argosy"] <- "uni_argosy"
md$school_new[str_extract(md$school,'boston_college') %in% "boston_college"] <- "uni_boston_college"
md$school_new[str_extract(md$school,'boston_university') %in% "boston_university"] <- "uni_boston_university"
md$school_new[str_extract(md$school,'brooklyn_college') %in% "brooklyn_college"] <- "uni_brooklyn_college"
md$school_new[str_extract(md$school,'brown') %in% "brown"] <- "uni_brown_university"
md$school_new[str_extract(md$school,'jung_institute') %in% "jung_institute"] <- "uni_jung_institute"
md$school_new[str_extract(md$school,'uni_california_school') %in% "uni_california_school"] <- "uni_california_school_of_professional_psychology"
md$school_new[str_extract(md$school,'uni_california_state') %in% "uni_california_state"] <- "uni_california_state_university_long_beach"
md$school_new[str_extract(md$school,'case_western') %in% "case_western"] <- "uni_case_western_reserve_university"
md$school_new[str_extract(md$school,'uni_catholic') %in% "uni_catholic"] <- "uni_catholic_university"
md$school_new[str_extract(md$school,'uni_chicago_school') %in% "uni_chicago_school"] <- "uni_chicago_school_of_professional_psycholog"
md$school_new[str_extract(md$school,'uni_city_college') %in% "uni_city_college"] <- "uni_city_college_of_new_york"
md$school_new[str_extract(md$school,'uni_columbia') %in% "uni_columbia"] <- "uni_columbia_university"
md$school_new[str_extract(md$school,'uni_cuny_graduate') %in% "uni_cuny_graduate"] <- "uni_cuny_graduate_cente"
md$school_new[str_extract(md$school,'hunter') %in% "hunter"] <- "uni_hunter_college"
md$school_new[str_extract(md$school,'duke_university') %in% "duke_university"] <- "uni_duke_university"
md$school_new[str_extract(md$school,'fairleigh_dickinson') %in% "fairleigh_dickinson"] <- "uni_fairleigh_dickinson_university"
md$school_new[str_extract(md$school,'ferkauf') %in% "ferkauf"] <- "uni_ferkauf_graduate_school_of_psychology"
md$school_new[str_extract(md$school,'fordham') %in% "fordham"] <- "uni_fordham_university"
md$school_new[str_extract(md$school,'gestalt') %in% "gestalt"] <- "uni_gestalt_associates_for_psychotherapy"
md$school_new[str_extract(md$school,'graduate_center') %in% "graduate_center"] <- "uni_cuny_graduate_cente"
md$school_new[str_extract(md$school,'harvard') %in% "harvard"] <- "uni_harvard"
md$school_new[str_extract(md$school,'hofstra') %in% "hofstra"] <- "uni_hofstra_university"
md$school_new[str_extract(md$school,'illinois_school') %in% "illinois_school"] <- "uni_illinois_school_of_professional_psychology"
md$school_new[str_extract(md$school,'john_jay') %in% "john_jay"] <- "uni_john_jay_college_of_criminal_justice"
md$school_new[str_extract(md$school,'johns_hopkins') %in% "johns_hopkins"] <- "uni_johns_hopkins_university_nyu_columbia"
md$school_new[str_extract(md$school,'liu') %in% "liu"] <- "uni_long_island_university"
md$school_new[str_extract(md$school,'long_island') %in% "long_island"] <- "uni_long_island_university"
md$school_new[str_extract(md$school,'loyola') %in% "loyola"] <- "uni_loyola_university"
md$school_new[str_extract(md$school,'massachusetts_school') %in% "massachusetts_school"] <- "uni_massachusetts_school_of_psychology"
md$school_new[str_extract(md$school,'mount_sinai') %in% "mount_sinai"] <- "uni_mount_sinai_medical_center"
md$school_new[str_extract(md$school,'yeshiva') %in% "yeshiva"] <- "uni_yeshiva_university"
md$school_new[str_extract(md$school,'psychotherapies') %in% "psychotherapies"] <- "uni_national_institute_for_the_psychotherapie"
md$school_new[str_extract(md$school,'association_for_psychoanaly') %in% "association_for_psychoanaly"] <- "uni_national_psychological_association_for_psychoanalysis"
md$school_new[str_extract(md$school,'for_psychoanaly') %in% "for_psychoanaly"] <- ""
md$school_new[str_extract(md$school,'york_uni') %in% "york_uni"] <- "uni_new_york_university"
md$school_new[str_extract(md$school,'nyu_post') %in% "nyu_post"] <- "uni_new_york_university"
md$school_new[str_extract(md$school,'nyu') %in% "nyu"] <- "uni_new_york_university"
md$school_new[str_extract(md$school,'social_research') %in% "social_research"] <- "uni_new_school_for_social_research"
md$school_new[str_extract(md$school,'new_york_presbyterian') %in% "new_york_presbyterian"] <- "uni_mount_sinai_medical_center"
md$school_new[str_extract(md$school,'new_york_university') %in% "new_york_university"] <- "uni_new_york_university"
md$school_new[str_extract(md$school,'nyack') %in% "nyack"] <- "uni_alliance_graduate_school_of_counseling"
md$school_new[str_extract(md$school,'princeton') %in% "princeton"] <- "uni_princeton"
md$school_new[str_extract(md$school,'queens_college') %in% "queens_college"] <- "uni_queens_college"
md$school_new[str_extract(md$school,'rutgers') %in% "rutgers"] <- "uni_rutgers_university"
md$school_new[str_extract(md$school,'sackler') %in% "sackler"] <- "uni_sackler_school_of_medicine"
md$school_new[str_extract(md$school,'saybrook') %in% "saybrook"] <- "uni_saybrook_univerity"
md$school_new[str_extract(md$school,'silberman') %in% "silberman"] <- "uni_hunter_college"
md$school_new[str_extract(md$school,'simmons') %in% "simmons"] <- "uni_simmons_college"
md$school_new[str_extract(md$school,'smith') %in% "smith"] <- "uni_smith_college"
md$school_new[str_extract(md$school,'st._john') %in% "st._john"] <- "uni_st_johns_university"
md$school_new[str_extract(md$school,'albany') %in% "albany"] <- "uni_suny_albany"
md$school_new[str_extract(md$school,'buffal') %in% "buffal"] <- "uni_suny_buffalo"
md$school_new[str_extract(md$school,'stony_brook') %in% "stony_brook"] <- "uni_suny_stony_brook"
md$school_new[str_extract(md$school,'new_paltz') %in% "new_paltz"] <- "uni_suny_new_paltz"
md$school_new[str_extract(md$school,'brockport') %in% "brockport"] <- "suny_brockport"
md$school_new[str_extract(md$school,'binghamton') %in% "binghamton"] <- "uni_suny_binghamton"
md$school_new[str_extract(md$school,'downstat') %in% "downstat"] <- "uni_suny_downstate"
md$school_new[str_extract(md$school,'teachers_college') %in% "teachers_college"] <- "uni_columbia_university"
md$school_new[str_extract(md$school,'temple') %in% "temple"] <- "uni_temple_university"
md$school_new[str_extract(md$school,'ackerman') %in% "ackerman"] <- "uni_ackerman_institute_for_the_family"
md$school_new[str_extract(md$school,'city_college_of') %in% "city_college_of"] <- "uni_city_college_of_new_york"
md$school_new[str_extract(md$school,'derner') %in% "derner"] <- "uni_adelphi_uni"
md$school_new[str_extract(md$school,'penn') %in% "penn"] <- "uni_university_of_pennsylvania"
md$school_new[str_extract(md$school,'of_michigan') %in% "of_michigan"] <- "uni_univerisity_of_michigan"
md$school_new[str_extract(md$school,'of_texas') %in% "of_texas"] <- "uni_university_of_texas"
md$school_new[str_extract(md$school,'wrigh') %in% "wrigh"] <- "uni_the_wright_institute"
md$school_new[str_extract(md$school,'touro') %in% "touro"] <- "uni_touro_college"
md$school_new[str_extract(md$school,'self_psycholog') %in% "self_psycholog"] <- "uni_training_and_research_institute_for_self_psychology"
md$school_new[str_extract(md$school,'tulane') %in% "tulane"] <- "uni_tulane_university"
md$school_new[str_extract(md$school,'ucla') %in% "ucla"] <- "uni_ucla"
md$school_new[str_extract(md$school,'union') %in% "union"] <- "uni_union_institute_and_university"
md$school_new[str_extract(md$school,'theological') %in% "theological"] <- "uni_union_institute_and_university"
md$school_new[str_extract(md$school,'berkeley') %in% "berkeley"] <- "uni_ucberkley"
md$school_new[str_extract(md$school,'san_francisco_st') %in% "san_francisco_st"] <- "uni_san_francisco_state_university"
md$school_new[str_extract(md$school,'maryland') %in% "maryland"] <- "uni_university_of_maryland"
md$school_new[str_extract(md$school,'of_massachusetts') %in% "of_massachusetts"] <- "uni_university_of_massachusetts"
md$school_new[str_extract(md$school,'rochester') %in% "rochester"] <- "uni_university_of_rochester"
md$school_new[str_extract(md$school,'tennessee') %in% "tennessee"] <- "uni_university_of_tennessee"
md$school_new[str_extract(md$school,'walden') %in% "walden"] <- "uni_walden_university"
md$school_new[str_extract(md$school,'george_washington') %in% "george_washington"] <- "uni_george_washington_university"
md$school_new[str_extract(md$school,'widener') %in% "widener"] <- "uni_widener_university"
md$school_new[str_extract(md$school,'alanson_white') %in% "alanson_white"] <- "uni_william_alanson_white_institute"
md$school_new[str_extract(md$school,'wurzweiler') %in% "wurzweiler"] <- "uni_yeshiva_university"
md$school_new[str_extract(md$school,'yale') %in% "yale"] <- "uni_yale"
md$school_new[str_extract(md$school,'wurzweilier') %in% "wurzweilier"] <- "uni_yeshiva_university"
md$school_new[str_extract(md$school,'wurtzweiler') %in% "wurtzweiler"] <- "uni_yeshiva_university"
md$school_new[md$school_new %in% ''] <- NA

# specialties
md$school_new[md$school_new %in% 'xNotListed'] <- "spec_NotListed"

# titles
# table(md$titles, useNA = 'ifany')
md$titles[md$titles %in% ''] <- NA
md$titles <- str_replace(md$titles, 'Clinical_Social_Work_Therapist, LCSW_R', 
                         'Clinical_Social_Work_Therapist, LCSW') 
md$titles <- str_replace(md$titles, '^', 'ttl_')
md$titles <- str_replace_all(md$titles, ', ', ', ttl_')

# year graduated:
table(md$yeargrad, useNA = 'ifany')
str(md$yeargrad)

# years_in_practice
table(md$years_in_practice, useNA = 'ifany')
sort(unique(md$years_in_practice))
md$years_in_practice <- str_replace(md$years_in_practice, '< 1 Year', '1')
md$years_in_practice <- str_replace(md$years_in_practice, '< 2', '1.5')
md$years_in_practice <- str_replace(md$years_in_practice, '1 Year', '1')

md$years_in_practice <- str_replace(md$years_in_practice, '10\\+', '11')
md$years_in_practice <- str_replace(md$years_in_practice, '15\\+', '17')
md$years_in_practice <- str_replace(md$years_in_practice, '20\\+', '25')
md$years_in_practice <- str_replace(md$years_in_practice, '30\\+', '37')

md$years_in_practice[md$years_in_practice %in% ''] <- NA
table(md$years_in_practice, useNA = 'ifany')
str(md$years_in_practice)
md$years_in_practice <- as.numeric(md$years_in_practice)

md$zipcode <- paste0("US", as.character(md$zipcode))
length(unique(md$zipcode))   # I have 230 zipcodes in my data

#------------------------------------------------------
# Function that spreads strings of one column
# and creates dummy variable for each string
# With string being the name of the new variable
#------------------------------------------------------

# Need MY function, not this one:
myspread = function(data, variable){
  library(stringr)
  df = data[variable]
  split_list <- str_split(df[[variable]], ", ")
  split_list  # the result is a list of strings
  
  # Grab unique values of all strings:
  unique_strings <- sort(unique(unlist(split_list)))
  unique_strings
  
  # For each string in unique_strings create a variable with zeros:
  df[unique_strings] <- 0
  
  # Replace a zero with a 1 in a column if that row contains that string:
  for (row in 1:nrow(df)) {             # loop through rows
    for (string in split_list[[row]]) { # split a string; populate relevant columns
      df[row, string] <- 1
    }
  }
  df[-1]
}


View((myspread(md, 'specialties')))
View(md)
