
# Packages
library(tidyverse)
library(readxl)

d <- read_excel("Jan2025-US-Cyberattacks_share.xlsx")
d <- d[-1, ]
#### Data Cleaning ####

# Sex
d$sex <- as.factor(d$Sex)
summary(d$sex)

d$gender <- as.factor(d$Gender)
summary(d$gender) ### No non-binary, only one changed from female to male

# age
d$Q585 <- as.numeric(d$Q585)
d <- d %>% mutate(Q585 = ifelse(Q585 < 1900, NA, Q585))
summary(d$Q585)
d <- d %>% mutate(age = 2024 - d$Q585)
d <- d %>% drop_na(age)
summary(d$age)

#race
d$Q4_3
d$Q4_6
d$Q4_7
d$Q4_8
d$Q4_9
d$Q4_10
d <- d %>%
  mutate(race = coalesce(Q4_3, Q4_6, Q4_7, Q4_8, Q4_9, Q4_10))
d$race <- as.factor(d$race)
summary(d$race)
d$race_b <- ifelse(d$race == "White", "White", "Non-White")

d$race_b <- factor(d$race_b)

summary(d$race_b)

d <- d %>%
  mutate(race_cat = case_when(
    Q4_3 == "White" ~ "White",
    Q4_6 == "Black or African American" ~ "Black",
    Q4_7 == "Hispanic" ~ "Hispanic",
    Q4_8 == "Asian" ~ "Other",
    Q4_9 == "American Indian, Alaska Native, or Pacific Islander" | Q4_10 == "Other" ~ "Other",
    TRUE ~ NA_character_
  )) 
d$race_cat <- as.factor(d$race_cat)
summary(d$race_cat)
# education
d$edu <- d$Q5...384 
d$edu <- as.factor(d$edu)
summary(d$edu)
d <- d %>%
  mutate(edu_binary = case_when(
    edu %in% c("Bachelor’s degree", "Master’s degree", "Doctoral degree", 
               "Associate degree", "Professional Degree (JD, MD)") ~ 1,  # College and above
    edu %in% c("High School", "Middle/Junior High School", "No schooling", 
               "Some college, no degree", "Trade School") ~ 0,  # High school and below
    TRUE ~ NA_real_  # Ensure NA for any unexpected values
  ))
summary(d$edu_binary)


# political ideology
summary(d$Q653)
d$ideo <- as.factor(d$Q653)
summary(d$ideo)
d <- d %>%
  mutate(ideo3 = case_when(
    ideo %in% c("Very progressive", "Somewhat progressive") ~ "Progressive",
    ideo == "Moderate" ~ "Moderate",
    ideo %in% c("Very conservative", "Somewhat conservative") ~ "Conservative",
    TRUE ~ NA_character_
  ))
d$ideo3 <- as.factor(d$ideo3)
summary(d$ideo3)

#income
d$income <- as.factor(d$Q8...387)
summary(d$income)

d <- d %>%
  mutate(income_category = case_when(
    income %in% c("less than $5,000", "$5,000 to $7,499", "$7,500 to $9,999",
                  "$10,000 to $12,499", "$12,500 to $14,999", "$15,000 to $17,499", 
                  "$17,500 to $19,999", "$20,000 to $24,499", "$25,000 to $29,999", 
                  "$30,000 to $34,999", "$35,000 to $39,999") ~ "$0–$39,999",
    
    income %in% c("$40,000 to $49,999", "$50,000 to $74,999", "$75,000 to $99,999",
                  "$100,000 to $149,999") ~ "$40,000–$149,999",
    
    income %in% c("$150,000 to $199,999", 
                  "$200,000 or more") ~ "$149,999 and above",
  ))

d <- d %>%
  mutate(income_category = case_when(
    income %in% c("less than $5,000", "$5,000 to $7,499", "$7,500 to $9,999",
                  "$10,000 to $12,499", "$12,500 to $14,999", "$15,000 to $17,499", 
                  "$17,500 to $19,999", "$20,000 to $24,499", "$25,000 to $29,999", 
                  "$30,000 to $34,999", "$35,000 to $39,999") ~ "$0–$39,999",
    
    income %in% c("$40,000 to $49,999", "$50,000 to $74,999", "$75,000 to $99,999") ~ "$40,000–$99,999",
    
    income %in% c("$100,000 to $149,999", "$150,000 to $199,999", 
                  "$200,000 or more") ~ "$100,000 and above",
  ))

d$income_category <- as.factor(d$income_category)
summary(d$income_category)


#citizenship
d$citizen <- as.factor(d$Q6...385)
summary(d$citizen)
d <- d %>%
  mutate(citizen_binary = case_when(
    citizen %in% c("Native-born U.S. citizen", "Foreign-born U.S. citizen") ~ 1,  # Citizen
    citizen %in% c("Non-U.S. citizen", "Permanent resident in the U.S. (Green Card Holder)", 
                   "U.S. Visa Holder (e.g., F1, J1, H1B)") ~ 0,  # Non-Citizen
    TRUE ~ NA_real_
  ))
summary(d$citizen_binary)

#### Suitable Target
# How Often use Internet
d$internet_use <- d$Q1 %>% as.factor()
summary(d$internet_use)
d$internet_use_num <- as.numeric(factor(d$internet_use, 
                                        levels = c("Never", "Rarely", "Sometimes", "Often", "Always"), 
                                        labels = c(1, 2, 3, 4, 5)))


d$internet_always <- ifelse(d$internet_use == "Always", "Always", "Not Always")
d$internet_always <- d$internet_always %>% as.factor()
summary(d$internet_always)
summary(d$internet_use)
d$regularly_internet <- ifelse(d$internet_use %in% c("Always", "Often"), "Yes", "No")

# Convert to factor if needed
d$regularly_internet <- factor(d$regularly_internet)

# Check updated summary
summary(d$regularly_internet)

# Use online Banking
d$online_banking <- d$Q4_19 %>% as.factor()
d$online_banking <- ifelse(is.na(d$online_banking), "Yes", "No")
d$online_banking <- d$online_banking %>% as.factor()
summary(d$online_banking)

d$bank_often <- d$Q5...305 %>% as.factor()
summary(d$bank_often)
d$bank_often_num <- as.numeric(factor(d$bank_often, 
                                      levels = c("A few times a month", "Sometimes a week", 
                                                 "Once a day", "Always"), 
                                      labels = c(2, 3, 4, 5)))

d <- d %>%
  mutate(bank_often_binary = case_when(
    bank_often %in% c("Always", "Once a day") ~ "Often",
    TRUE ~ "Not Often"  # Includes "A few times a month" and NA
  ))
d$bank_often_binary <- as.factor(d$bank_often_binary)
summary(d$bank_often_binary)
#Use online shopping 
d$online_shopping <- d$Q6_1 %>% as.factor()
d$online_shopping <- ifelse(is.na(d$online_shopping), "Yes", "No")
d$online_shopping <- as.factor(d$online_shopping)
summary(d$online_shopping)

d$shopping_often <- d$Q7...313 %>% as.factor()
summary(d$shopping_often)
d <- d %>%
  mutate(shopping_often = case_when(
    shopping_often %in% c("Always", "Once a day") ~ "Often",
    TRUE ~ "Not Often"  # Includes "A few times a month" and NA
  ))
d$shopping_often <- d$shopping_often %>% as.factor()
summary(d$shopping_often)
#Use Social Media
d$Q8...314
d$socmedia<- d$Q8...314 %>% as.factor()
summary(d$socmedia)

d$socmedia_often <- d$Q11...334 %>% as.factor()
summary(d$socmedia_often)
d$socmedia_num <- as.numeric(factor(d$socmedia_often, 
                                    levels = c("A few times a month", "Sometimes a week", 
                                               "Once a day", "Always"), 
                                    labels = c(2, 3, 4, 5)))

d <- d %>%
  mutate(socmedia_often = case_when(
    socmedia_often %in% c("Always", "Once a day") ~ "Often",
    TRUE ~ "Not Often"  # Includes "A few times a month" and NA
  ))
d$socmedia_often <- d$socmedia_often %>% as.factor()
summary(d$socmedia_often)
#Separate Email for Work
d$work_email <- d$Q13...336 %>% as.factor()
summary(d$work_email)

#Separate Email for Personal Use
d$Q14...338
d$personal_email <- as.factor(d$Q14...338)
summary(d$personal_email)
#Online Gaming
d$Q18
d$online_gaming <- as.factor(d$Q18)
summary(d$online_gaming)
#### Capable Guardianship ####
# Password
d$password <- d$Q2...175 %>% as.factor()
summary(d$password)
d <- d %>%
  mutate(password_binary = case_when(
    password %in% c("At least once a week", "Every few weeks", "Maybe once every few months") ~ 1,  # Often
    password %in% c("At least once a year", "Never") ~ 0,  # Not Often
    TRUE ~ NA_real_  # Catch any unexpected values
  ))
d$password_binary <- as.factor(d$password_binary)
summary(d$password_binary)

d$password <- as.numeric(factor(d$password, 
                                levels = c("Never", "At least once a year", "Maybe once every few months", 
                                           "Every few weeks", "At least once a week"), 
                                labels = c(1, 2, 3, 4, 5)))

summary(d$password)
# Browser Block
d$block <- d$Q7...184 %>% as.factor()
summary(d$block)
d <- d %>%
  mutate(block_b = case_when(
    block %in% c("Always", "Often") ~ 1,  # Often
    block %in% c("Sometimes", "Rarely", "Never") ~ 0,  # Not Often
    TRUE ~ NA_real_  # Catch any unexpected values
  ))
summary(d$block_b)

d$block <- as.numeric(factor(d$block, levels = c("Never", "Rarely", "Sometimes", "Often", "Always"), 
                             labels = c(1, 2, 3, 4, 5)))

# Anti Virus
d$antivirus <- d$Q10...187 %>% as.factor()
d$antivirus <- ifelse(d$antivirus == "Yes", "Yes", "No") %>% as.factor()
summary(d$antivirus)
# 2-factor
d$twofactor <- d$Q14...191 %>% as.factor()
d$twofactor <- ifelse(d$twofactor == "Yes", "Yes", "No") %>% as.factor()
summary(d$twofactor)

#### Privacy Scale ####
d$privacy_1
d$privacy_2
d$privacy_3
d$privacy_4
d$privacy_5
d$privacy_6
d$privacy_7
d$privacy_8
d$privacy_9
d <- d %>%
  mutate(
    privacy_1 = as.numeric(str_extract(privacy_1, "\\d+")),
    privacy_2 = as.numeric(str_extract(privacy_2, "\\d+")),
    privacy_3 = as.numeric(str_extract(privacy_3, "\\d+")),
    privacy_4 = as.numeric(str_extract(privacy_4, "\\d+")),
    privacy_5 = as.numeric(str_extract(privacy_5, "\\d+")),
    privacy_6 = as.numeric(str_extract(privacy_6, "\\d+")),
    privacy_7 = as.numeric(str_extract(privacy_7, "\\d+")),
    privacy_8 = as.numeric(str_extract(privacy_8, "\\d+")),
    privacy_9 = as.numeric(str_extract(privacy_9, "\\d+")),
    privacy_10 = as.numeric(str_extract(Q3_3, "\\d+")),
    privacy_11 = as.numeric(str_extract(Q3_4, "\\d+")),
    privacy_12 = as.numeric(str_extract(Q3_5, "\\d+")),
    privacy_13 = as.numeric(str_extract(Q3_6, "\\d+")),
    privacy_14 = as.numeric(str_extract(Q3_7, "\\d+")),
    privacy_15 = as.numeric(str_extract(Q3_8, "\\d+")),
  )

summary(d$Q3_8)
d <- d %>%
  mutate(privacy_10 = case_when(
    privacy_10 == 5 ~ 1,
    privacy_10 == 4 ~ 2,
    privacy_10 == 3 ~ 3,
    privacy_10 == 2 ~ 4,
    privacy_10 == 1 ~ 5
  ))
d <- d %>%
  mutate(privacy_8 = case_when(
    privacy_8 == 5 ~ 1,
    privacy_8 == 4 ~ 2,
    privacy_8 == 3 ~ 3,
    privacy_8 == 2 ~ 4,
    privacy_8 == 1 ~ 5,
    TRUE ~ NA_real_  # Keep NA values
  ))

summary(d$privacy_1)
summary(d$privacy_2)
summary(d$privacy_3)
summary(d$privacy_4)
summary(d$privacy_8)
summary(d$privacy_5)
summary(d$privacy_10)
summary(d$privacy_8)

d <- d %>% mutate(privacy_sum = 
                    rowSums(across(c(privacy_1, privacy_2, 
                                     privacy_3, privacy_4, privacy_5, 
                                     privacy_6, privacy_7, 
                                     privacy_9, privacy_10, privacy_11, 
                                     privacy_12, privacy_13, privacy_14, 
                                     privacy_15))))

summary(d$privacy_sum)

#### Informed Risk ####
informed_vars <- paste0("informed_", 1:13)

d <- d %>%
  mutate(across(all_of(informed_vars), ~ case_when(
    . == "Not at all informed" ~ 1,
    . == "Not very well informed" ~ 2,
    . == "Neutral" ~ 3,
    . == "Fairly well informed" ~ 4,
    . == "Very well informed" ~ 5,
    TRUE ~ NA_real_  # Keep NA if present
  )))

summary(d$informed_1)
d <- d %>%
  mutate(total_informed_score = rowSums(across(all_of(informed_vars)), na.rm = TRUE))

summary(d$total_informed_score)
#### Dependent Variable ####
# Ransomware 
d$ransomware <- d$Q616 %>% as.factor()
d$ransomware <- ifelse(d$ransomware == "Yes", "Yes", "No") %>% as.factor()
summary(d$ransomware)

# DDos
d$ddos <- d$ddos_vic %>% as.factor()
d$ddos <- ifelse(d$ddos == "Yes", "Yes", "No") %>% as.factor()
summary(d$ddos)

# Crypto Cyberattack
d$cryptoattack <- d$crycyvic %>% as.factor()
d$cryptoattack <- ifelse(d$cryptoattack == "Yes", "Yes", "No") %>% as.factor()
summary(d$cryptoattack)

#Crypto Fraud
d$cryptofraud <- d$Q638 %>% as.factor()
d$cryptofraud <- ifelse(d$cryptofraud == "Yes", "Yes", "No") %>% as.factor()
summary(d$cryptofraud)


# Comfort Technology
d$comftech_1
d$comftech_2
d$comftech_3
d$comftech_4
d$comftech_5
d$comftech_6
table(d$comftech_1)

d <- d %>%
  mutate(across(starts_with("comftech_"), ~ case_when(
    . == "Extremely comfortable" ~ 5,
    . == "Fairly comfortable" ~ 4,
    . == "Neutral" ~ 3,
    . == "Slightly comfortable" ~ 2,
    . == "Not comfortable at all" ~ 1,
    TRUE ~ NA_real_
  )))
summary(d$comftech_1)
summary(d$comftech_2)
d$comftech_sum <- rowSums(d[, c("comftech_1", "comftech_2", "comftech_3", 
                                "comftech_4", "comftech_5", "comftech_6")], 
                          na.rm = TRUE)
d$comftech_sum

# Prior Work Experience in IT
d <- d[!is.na(d$Q9...390), ]
d$priorit <- as.factor(d$Q9...390)
summary(d$priorit)


# Block Social Media
summary(as.factor(d$Q9...186))
d$soc_block <- as.numeric(factor(d$Q9...186, levels = c("Never", "Rarely", "Sometimes", "Often", "Always"), 
                             labels = c(1, 2, 3, 4, 5)))

summary(d$soc_block)
