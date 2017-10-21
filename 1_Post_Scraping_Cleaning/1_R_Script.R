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


#-----------------------------------------------------------------------
# Defining a function that checks if each
# string in a vector contains an element with a "bad" substring in it.
# If it does, that whole substring is replaced with newstring:
library(stringr)
string_replace = function(strings_vector, badstring, newstring){
  with_string <- grepl(badstring, strings_vector)
  mysplits <- str_split(string = strings_vector[with_string], pattern = ', ')
  for (i in 1:length(mysplits)) {
    allstrings <- mysplits[[i]]
    for (ii in 1:length(allstrings)) {
      if (grepl(badstring, allstrings[ii])) mysplits[[i]][ii] <- newstring
    }
  }
  for (i in seq_along(mysplits)) { # i = 1
    mysplits[[i]] <- paste(mysplits[[i]], collapse = ", ")
  }
  strings_vector[with_string] <- unlist(mysplits)
  return(strings_vector)
}
#-----------------------------------------------------------------------
#
# Replace certain characters in approaches
library(stringr)

md$approaches <- str_replace_all(md$approaches, 'appr_10001', '')
md$approaches <- str_replace_all(md$approaches, 'appr_10005', '')
md$approaches <- str_replace_all(md$approaches, 'appr_10011', '')
md$approaches <- str_replace_all(md$approaches, 'appr_10012', '')
md$approaches <- str_replace_all(md$approaches, 'appr_10013', '')
md$approaches <- str_replace_all(md$approaches, 'appr_10018', '')
md$approaches <- str_replace_all(md$approaches, 'appr_10025', '')
md$approaches <- str_replace_all(md$approaches, 'appr_10026', '')
md$approaches <- str_replace_all(md$approaches, 'appr_10027', '')
md$approaches <- str_replace_all(md$approaches, 'appr_10038', '')
md$approaches <- str_replace_all(md$approaches, 'appr_10039', '')
md$approaches <- str_replace_all(md$approaches, 'appr_10040', '')
md$approaches <- str_replace_all(md$approaches, 'appr_11021', '')
md$approaches <- str_replace_all(md$approaches, 'appr_11022', '')
md$approaches <- str_replace_all(md$approaches, 'appr_11023', '')
md$approaches <- str_replace_all(md$approaches, 'appr_11208', '')
md$approaches <- str_replace_all(md$approaches, 'appr_11209', '')

md$approaches <- str_replace_all(md$approaches, ';_', ', ')
md$approaches <- str_replace_all(md$approaches, '; ', ', ')
md$approaches <- str_replace_all(md$approaches, "12_step", "appr_12_steps")
md$approaches <- str_replace_all(md$approaches, '_&_', '_')
md$approaches <- str_replace_all(md$approaches, '&', '_and_')
md$approaches <- str_replace_all(md$approaches, ',_', ', ')
md$approaches <- str_replace_all(md$approaches, ':', '_')
md$approaches <- str_replace_all(md$approaches, 'xNotListed', 'appr_NotListed')
md$approaches <- str_replace_all(md$approaches, ';$', '')
md$approaches <- str_replace_all(md$approaches, ',$', '')

md$approaches <- str_replace_all(md$approaches, 'act', 'appr_act')
md$approaches <- str_replace_all(md$approaches, 'acupressure', 'appr_acupressure')
md$approaches <- str_replace_all(md$approaches, 'adhd', 'appr_adhd')
md$approaches <- str_replace_all(md$approaches, 'adolescent_and_adult_treatment', 'appr_teens')
md$approaches <- str_replace_all(md$approaches, 'aedp', 'appr_aedp')
md$approaches <- str_replace_all(md$approaches, 'afot', 'appr_afot')
md$approaches <- str_replace_all(md$approaches, 'age_regression', 'appr_age_regression')
md$approaches <- str_replace_all(md$approaches, 'alpha_stim', 'appr_alpha_stim')
md$approaches <- str_replace_all(md$approaches, 'and_cbt_used_to_manage_symptoms', 'appr_cbt')
md$approaches <- str_replace_all(md$approaches, 'and_entrepreneur_coaching', 'appr_coaching')
md$approaches <- str_replace_all(md$approaches, 'anxiety_disorders', 'appr_anxiety')
md$approaches <- str_replace_all(md$approaches, 'anxiety_phobia_relief', 'appr_anxiety')

badstrings <- c('ask_receive','becoming_badass','bereavement','bionian','board_certified','breathing_techniques','business','cam','career_counseling','caregiving','carl_rogers','certified','chronic_illness','co_active_coaching','counseling_for_women','couples_children','customized_to_the_client','dbt','death_and_bereavement_work','decision_making','deep_relaxation','depression','developed_by_david_burns','developmental_issues','diplomat_act','dream_work','dying_bereavement_counselor','eating_disorde','eating_disorders,weight_problems','eclectic','eft','ego_state_therapy','emotionally_focused_therapy_for_couples','employee_assistance','energy_focused_treatment','etc','existential','experiential','expert_witness','family_therapy','fertility_issues','floortime','focusing','former_patients,and','fsap','growth_oriented_treatment_informed_by','habit_control','harm_reduction','home_school_behavior_mgmt','humanistic','iep','ifs','inc_parenting','internal_family_systems','interpersonal','it_is_hard_to','lcsw','lifestyle_wellness','md','meditation','memory','mind_body_focus','mindfulness_and_meditation','motivation','moving_forward','music','network_therapy','nlp','nutrition','object_relations_human_needs_psychology','oppositional_defiant_tx','organizational_consulting','oriental_medicine_herbs','panic_disorders','pastoral','plant_spirit_medicine','play','positive_psychology','post_treatment','prolonged_exposure_therapy','psycho_spiritual','psychoeducation','ptsd','ptt','realizational','reiki','revenue_your_life','ritualistic_abuse','rosen_method','sensorimotor_psychotherapy','sex_addiction','sexism','sexual_abuse_trauma','short_term_psychodynamic','skype_for_housebound_client','smoking_cesation','sociometry_group_psychotherapy','spiritual_life_coaching','spiritually_oriented','stress_reduction','supportive','systems_theory','talk','tension_headaches','test_anxiety','tft,energy_psychotherapies','therapy','trauma','weight_loss','worry','writing','young_adult')
for (i in badstrings) {
  md$approaches <- str_replace_all(md$approaches, i, paste0("appr_", i))
}

badstrings <- c('co_appr_active_coaching','death_and_appr_bereavement_work','diplomat_appr_act','dying_appr_bereavement_counselor','mindfulness_and_appr_meditation')
for (i in badstrings) {
  md$approaches <- str_replace_all(md$approaches, i, paste0("appr_", i))
}

md$approaches <- string_replace(md$approaches, 'mindfulness', 'appr_mindfulness_based')
md$approaches <- string_replace(md$approaches, 'meditation', 'appr_meditation')
md$approaches <- string_replace(md$approaches, '_somatic', 'appr_somatic')
md$approaches <- string_replace(md$approaches, 'cognitive', 'appr_cognitive_behavioral')

#-------------------------------------------------------------
# Replace certain characters in client_ethnicities

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

#-------------------------------------------------------------
# Replace certain characters in client_languages

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

#-------------------------------------------------------------
# Replace certain characters in client_religion

# unique(md$client_religion)
md$client_religion <- str_replace(md$client_religion, 
                                   'Other Spiritual or Religious Affiliations', 'X')
md$client_religion <- str_replace(md$client_religion, '^', 'rel_')
md$client_religion <- str_replace_all(md$client_religion, ', ', ', rel_')
# unique(md$client_religion)

#-------------------------------------------------------------
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

#-------------------------------------------------------------
# Payment methods:
md$payment_methods <- str_replace(md$payment_methods, 
                                  'xNotListed', 'paym_notlisted')

#-------------------------------------------------------------
# Fee per session:
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
md$fee_to[is.na(md$fee_to)] <- md$fee_from[is.na(md$fee_to)] 
str(md$fee_from)
str(md$fee_to)
# md[1:20, c("per_session", "fee_from", "fee_to")]

#-------------------------------------------------------------
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
md$plans <- string_replace(md$plans, 'affinity', 'ins_affintiy')
md$plans <- string_replace(md$plans, 'aetna', 'ins_aetna')
md$plans <- string_replace(md$plans, 'aarp', 'ins_aarp')
md$plans <- string_replace(md$plans, '_1199', 'ins_1199')
md$plans <- string_replace(md$plans, 'out_of_network', 'ins_out_of_network')
md$plans <- string_replace(md$plans, 'amida', 'ins_amidacare')
md$plans <- string_replace(md$plans, 'anthem', 'ins_anthem')
md$plans <- string_replace(md$plans, 'bc_bs', 'ins_bcbs')
md$plans <- string_replace(md$plans, 'beacon', 'ins_beacon')
md$plans <- string_replace(md$plans, 'blue_cross_blue_shield', 'ins_blue_cross_blue_shield')
md$plans <- string_replace(md$plans, 'blue_shield', 'ins_blue_cross_blue_shield')
md$plans <- string_replace(md$plans, 'chp', 'ins_chp')
md$plans <- string_replace(md$plans, 'cigna', 'ins_cigna')
md$plans <- string_replace(md$plans, 'compsych', 'ins_compsych')
md$plans <- string_replace(md$plans, 'consolidated_health', 'ins_consolidated_health')
md$plans <- string_replace(md$plans, 'devon', 'ins_ins_devon_health')
md$plans <- string_replace(md$plans, 'eap', 'ins_eap')
md$plans <- string_replace(md$plans, 'emblem_health', 'ins_emblem_health')
md$plans <- string_replace(md$plans, 'empire_bcbs', 'ins_empire_bcbs')
md$plans <- string_replace(md$plans, 'empire_bluecross', 'ins_empire_bcbs')
md$plans <- string_replace(md$plans, 'fdny', 'ins_fdny')
md$plans <- string_replace(md$plans, 'fidelis', 'ins_fidelis')
md$plans <- string_replace(md$plans, 'galaxy', 'ins_galaxy')
md$plans <- string_replace(md$plans, '_ghi_', 'ins_ghi')
md$plans <- string_replace(md$plans, 'guardian', 'ins_guardian')
md$plans <- string_replace(md$plans, 'healthfirst', 'ins_healthfirst')
md$plans <- string_replace(md$plans, 'healthplus', 'ins_healthplus')
md$plans <- string_replace(md$plans, 'magellan', 'ins_magellan_behavioral_health')

#-------------------------------------------------------------
# Creating a new variable insurance that is = 1 only
# if a person accepts more than one insurance or
# if it is just one insurance, then it's not 'ins_out_of_network'
# and NOT 'ins_notlisted'
more_than_one_insurance <- sapply(str_split(md$plans, pattern = ", "), length) > 1 
only_one_insurance <- sapply(str_split(md$plans, pattern = ", "), length) == 1 
out_of_network_instring <- str_extract(md$plans,'ins_out_of_network') %in% "ins_out_of_network"
notlisted <- str_extract(md$plans,'ins_notlisted') %in% "ins_notlisted"

md$insurance <- 0
md$insurance[more_than_one_insurance] <- 1
md$insurance[only_one_insurance & !out_of_network_instring & !notlisted] <- 1
count(md, insurance)

#-------------------------------------------------------------
# Cleaning schools they went to

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

# View(sort(table(md$school_new, useNA = 'ifany')))

#-------------------------------------------------------------
# specialties
md$specialties[md$specialties %in% 'xNotListed'] <- "spec_NotListed"

#-------------------------------------------------------------
# titles
# table(md$titles, useNA = 'ifany')
md$titles[md$titles %in% ''] <- NA
md$titles <- str_replace(md$titles, 'Clinical_Social_Work_Therapist, LCSW_R', 
                         'Clinical_Social_Work_Therapist, LCSW') 
md$titles <- str_replace(md$titles, '^', 'ttl_')
md$titles <- str_replace_all(md$titles, ', ', ', ttl_')

#-------------------------------------------------------------
# year graduated:
table(md$yeargrad, useNA = 'ifany')
str(md$yeargrad)

#-------------------------------------------------------------
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

#-------------------------------------------------------------
md$zipcode <- paste0("US", as.character(md$zipcode))
length(unique(md$zipcode))   # I have 230 zipcodes in my data

#------------------------------------------------------
# 2 Functions that spread strings in rows of one column
# and creates dummy variable for each string
# With string itself becoming the nxame of the new variable
#------------------------------------------------------

# Function 1:
myspread = function(mydata, variable){
  library(stringr)
  df <- mydata[variable]
  split_list <- str_split(df[[variable]], ", ") 
  # the result is a list of strings
  
  # Grab unique values of all strings:
  unique_strings <- sort(unique(unlist(split_list)))
  unique_strings <- unique_strings[!unique_strings %in% '']
  unique_strings <- str_replace_all(unique_strings, ';$', '')
  unique_strings <- str_replace_all(unique_strings, ',$', '')
  unique_strings <- str_replace_all(unique_strings, '\\.', '')
  unique_strings <- str_replace_all(unique_strings, '\\\\', '')
  unique_strings <- str_replace_all(unique_strings, '"', '')
  unique_strings <- str_replace_all(unique_strings, '\"', '')
  unique_strings <- str_replace_all(unique_strings, '\\"', '')
  unique_strings <- str_replace_all(unique_strings, '_$', '')
  unique_strings <- str_replace_all(unique_strings, '\'', '')
  unique_strings <- str_replace_all(unique_strings, ',_', '_')
  unique_strings <- str_replace_all(unique_strings, '\\,_', '_')
  unique_strings <- str_replace_all(unique_strings, '_,', '_')
  unique_strings <- str_replace_all(unique_strings, '_\\,', '_')
  unique_strings <- str_replace_all(unique_strings, ':$', '')
  unique_strings <- str_replace_all(unique_strings, '\\*', '')
  unique_strings <- str_replace_all(unique_strings, '%', '')
  unique_strings <- str_replace_all(unique_strings, '\\(', '')
  unique_strings <- str_replace_all(unique_strings, '\\)', '')
  unique_strings <- str_replace_all(unique_strings, '$', '')  
  unique_strings <- str_replace_all(unique_strings, '___', '_')
  unique_strings <- str_replace_all(unique_strings, '__', '_')
  unique_strings <- unique_strings[!unique_strings %in% '']
  unique_strings <- unique_strings[!is.na(unique_strings)]

  # For each string in unique_strings create a variable with zeros:
  df[unique_strings] <- 0

  # Replace a zero with a 1 in a column if that row contains that string:
  for (row in 1:nrow(df)) {         # loop through rows row=1 i
    for (i in split_list[[row]]) {  # split a string; populate relevant columns
      # print(i)
      if (i %in% '') next
      df[row, i] <- 1
    }
  }
  df <- df[-1]
  temp <- (colSums(df)/nrow(df) * 100) > 4  # I want only those with > 4%
  temp[is.na(temp)] <- FALSE
  df <- df[,temp]
  df
}

# Function 2
myspread_ethn = function(mydata, variable){
  library(stringr)
  df <- mydata[variable]
  split_list <- str_split(df[[variable]], ", ") 
  # the result is a list of strings
  
  # Grab unique values of all strings:
  unique_strings <- sort(unique(unlist(split_list)))
  unique_strings <- unique_strings[!unique_strings %in% '']
  unique_strings <- str_replace_all(unique_strings, ';$', '')
  unique_strings <- str_replace_all(unique_strings, ',$', '')
  unique_strings <- str_replace_all(unique_strings, '\\.', '')
  unique_strings <- str_replace_all(unique_strings, '\\\\', '')
  unique_strings <- str_replace_all(unique_strings, '"', '')
  unique_strings <- str_replace_all(unique_strings, '\"', '')
  unique_strings <- str_replace_all(unique_strings, '\\"', '')
  unique_strings <- str_replace_all(unique_strings, '_$', '')
  unique_strings <- str_replace_all(unique_strings, '\'', '')
  unique_strings <- str_replace_all(unique_strings, ',_', '_')
  unique_strings <- str_replace_all(unique_strings, '\\,_', '_')
  unique_strings <- str_replace_all(unique_strings, '_,', '_')
  unique_strings <- str_replace_all(unique_strings, '_\\,', '_')
  unique_strings <- str_replace_all(unique_strings, ':$', '')
  unique_strings <- str_replace_all(unique_strings, '\\*', '')
  unique_strings <- str_replace_all(unique_strings, '%', '')
  unique_strings <- str_replace_all(unique_strings, '\\(', '')
  unique_strings <- str_replace_all(unique_strings, '\\)', '')
  unique_strings <- str_replace_all(unique_strings, '$', '')  
  unique_strings <- str_replace_all(unique_strings, '\\.', '')
  unique_strings <- str_replace_all(unique_strings, '/', '_')
  unique_strings <- unique_strings[!unique_strings %in% '']
  unique_strings <- str_replace_all(unique_strings, '___', '_')
  unique_strings <- str_replace_all(unique_strings, '__', '_')
  unique_strings <- unique_strings[!is.na(unique_strings)]
    # For each string in unique_strings create a variable with zeros:
  df[unique_strings] <- 0
  # Replace a zero with a 1 in a column if that row contains that string:
  for (row in 1:nrow(df)) {         # loop through rows row=1 i
    for (i in split_list[[row]]) {  # split a string; populate relevant columns
      # print(i)
      if (i %in% '') next
      if (is.na(i)) next
      df[row, i] <- 1
    }
  }
  df <- df[-1]
  df
}

#------------------------------------------------------

# Casting approaches, specialties, etc. and adding new columns to my df:

# Approaches:
md <- cbind(md, myspread(md, 'approaches'))

# Client ethnicities:
temp <- myspread_ethn(md, 'client_ethnicities')
temp <- select(temp, -ethn_X)
md <- cbind(md, temp)

# Client languages:
temp <- myspread_ethn(md, 'client_languages')
temp <- select(temp, -lng_X)
colsums <- (colSums(temp)/nrow(temp) * 100) > 0.05  # I want only those with > 0.5%
colsums[is.na(colsums)] <- FALSE
temp <- temp[,colsums]
md <- cbind(md, temp)

# Client religions:
temp <- myspread_ethn(md, 'client_religion')
temp <- select(temp, -rel_X)
md <- cbind(md, temp)

# Client Insurance plans:
temp <- myspread_ethn(md, 'plans')
temp <- select(temp, -ins_out_of_network, -ins_notlisted)
colsums <- (colSums(temp)/nrow(temp) * 100) > 0.5  # I want only those with > 0.5%
colsums[is.na(colsums)] <- FALSE
temp <- temp[,colsums]
md <- cbind(md, temp)

# Titles:
temp <- myspread_ethn(md, 'titles')
names(temp)

temp$ttl_LCSW <- temp$ttl_lcsw + temp$ttl_Lcsw + temp$ttl_LCSw + temp$ttl_LCSW +
                 temp$ttl_LCSWNJ + temp$ttl_LCSW.1 + temp$ttl_LCSW_C +
                 temp$ttl_LCSW_PR + temp$ttl_lcsw_R + temp$ttl_LCSW_R +
                 temp$ttl_LCSW_S + temp$ttl_LCSWBCD + temp$ttl_lcswr +
                 temp$ttl_LCSWR

temp$ttl_lcsw <- NULL
temp$ttl_Lcsw <- NULL
temp$ttl_LCSw <- NULL
temp$ttl_LCSWNJ <- NULL
temp$ttl_LCSW.1 <- NULL
temp$ttl_LCSW_C <- NULL
temp$ttl_LCSW_PR <- NULL
temp$ttl_lcsw_R <- NULL
temp$ttl_LCSW_R <- NULL
temp$ttl_LCSW_S <- NULL
temp$ttl_LCSWBCD <- NULL
temp$ttl_lcswr <- NULL
temp$ttl_LCSWR <- NULL

temp$ttl_CASAC <- temp$ttl_CASAC + temp$ttl_CASAC_T
temp$ttl_CASAC_T <- NULL

temp$ttl_PhD <- temp$ttl_PhD + temp$ttl_PHD
temp$ttl_PHD <- NULL

temp$ttl_Psychiatric_Nurse <- temp$ttl_Psychiatric_Nurse + temp$ttl_Psychiatric_Nurse_Practitioner
temp$ttl_Psychiatric_Nurse_Practitioner <- NULL

colsums <- (colSums(temp)/nrow(temp) * 100) > 0.3  # I want only those with > 0.4
colsums[is.na(colsums)] <- FALSE
temp <- temp[,colsums]
colsums[is.na(colsums)] <- FALSE
md <- cbind(md, temp)

# Specialties
temp <- myspread_ethn(md, 'specialties')
temp <- select(temp, -spec_NotListed)
colsums <- (colSums(temp)/nrow(temp) * 100) > 0.1  # I want only those with > 0.1%
colsums[is.na(colsums)] <- FALSE
temp <- temp[,colsums]
md <- cbind(md, temp)

# Issues
temp <- myspread_ethn(md, 'issues')
temp <- select(temp, -xNotListed)
colsums <- (colSums(temp)/nrow(temp) * 100) > 0.1  # I want only those with > 0.1%
colsums[is.na(colsums)] <- FALSE
temp <- temp[,colsums]
md <- cbind(md, temp)
rm(temp)

# Adding variable years since graduation:
md$years_since_graduation <- 2017 - md$yeargrad

# Fixing approach variables:
library(stringr)
md$appr_NotListed <- NULL
appr_appr_vars <- names(md)[grep('^appr_appr', names(md))]
names(md)[names(md) %in% appr_appr_vars] <- 
  str_replace_all(appr_appr_vars, 'appr_appr_', 'appr_')
names(md)[names(md) %in% "appr_eappr_mdr"] <- "appr_emdr"
names(md)[names(md) %in% "appr_art_appr_therapy"] <- "appr_art_therapy"
names(md)[names(md) %in% "appr_hypnoappr_therapy"] <- "appr_hypnotherapy"
names(md)[names(md) %in% "appr_acceptance_and_commitment_appr_therapy"] <- 
         "appr_acceptance_and_commitment_therapy"
names(md)[names(md) %in% "appr_play_appr_therapy"] <- "appr_play_therapy"
appr_vars <- names(md)[grep('^appr_', names(md))]

# Removing columns we don't need anymore:
library(tidyverse)
dim(md)
md <- md %>% select(-approaches, -atherapist, -client_ethnicities,
                    -client_languages, -client_religion, -issues,
                    -license, -payment_methods, -per_session, -plans,
                    -school, -specialties, -titles, -yeargrad, -iss_other)
dim(md)

write.csv(md, "x Scraped Data Cleaned.csv", row.names = F, na = '')
