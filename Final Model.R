options(scipen = 999)
options(digits = 5)


#### Descriptive Statistics ####
summary(d$ransomware)
summary(d$ddos)
summary(d$cryptoattack)
summary(d$cryptofraud)

summary(d$age)
sd(d$age)

# Age #

age_quartiles <- cut(
  d$age,
  breaks = c(18, 32, 46, 65, 87),
  labels = c("Q1: 18–32", "Q2: 33–46", "Q3: 47–65", "Q4: 66–87"),
  include.lowest = TRUE,
  right = TRUE
)

d$age_quartile <- age_quartiles

table(d$age_quartile)

# Gender
summary(d$gender)
summary(d$race_b)

summary(as.factor(d$edu_binary))
summary(d$edu)

summary(d$ideo3)
summary(d$income_category)

summary(as.factor(d$citizen_binary))

summary(d$comftech_sum)
sd(d$comftech_sum)

d$tech_z <- scale(d$comftech_sum)
summary(d$tech_z)

summary(d$privacy_sum)
sd(d$privacy_sum)

d$privacy_z <- scale(d$privacy_sum)
summary(d$privacy_z)

summary(d$password)
sd(d$password)

summary(d$block)
sd(d$block)

summary(d$antivirus)

summary(d$twofactor)

summary(d$internet_use_num)
sd(d$internet_use_num)

summary(d$bank_often_binary)

summary(d$online_shopping)

summary(d$socmedia_num)
sd(d$socmedia_num)

summary(d$work_email)
summary(d$personal_email)

summary(d$online_gaming)

table(d$cryptoattack, d$race_cat)
summary(d$race_cat)
d$race_cat <- relevel(d$race_cat, ref = "White")

summary(d$soc_block)
sd(d$soc_block)
#### Ransomware ####
ransom_vic_final <- glm(ransomware ~ age + gender + race_cat + edu_binary + ideo3 +
                     income_category + citizen_binary + priorit + 
                     tech_z + privacy_z + password +
                     block + antivirus + twofactor + internet_use_num + bank_often_binary + 
                     online_shopping + socmedia_num + work_email + 
                     personal_email + online_gaming,
                   data = d,
                   family = binomial)
summary(ransom_vic_final)

ransom_vic_final_cat <- glm(ransomware ~ age_quartile + gender + race_cat + edu_binary + ideo3 +
                          income_category + citizen_binary + priorit + 
                          tech_z + privacy_z + password +
                          block + antivirus + twofactor + internet_use_num + bank_often_binary + 
                          online_shopping + socmedia_num + work_email + 
                          personal_email + online_gaming,
                        data = d,
                        family = binomial)
summary(ransom_vic_final_cat)

ransom_vic_final_quad <- glm(ransomware ~ age + I(age^2) + gender + race_cat + edu_binary + ideo3 +
                          income_category + citizen_binary + priorit + 
                          tech_z + privacy_z + password +
                          block + antivirus + twofactor + internet_use_num + bank_often_binary + 
                          online_shopping + socmedia_num + work_email + 
                          personal_email + online_gaming,
                        data = d,
                        family = binomial)
summary(ransom_vic_final_quad)

ransom_vic_final_cub <- glm(ransomware ~ age+ I(age^3) + gender + race_cat + edu_binary + ideo3 +
                          income_category + citizen_binary + priorit + 
                          tech_z + privacy_z + password +
                          block + antivirus + twofactor + internet_use_num + bank_often_binary + 
                          online_shopping + socmedia_num + work_email + 
                          personal_email + online_gaming,
                        data = d,
                        family = binomial)
summary(ransom_vic_final_cub)

BIC(ransom_vic_final, ransom_vic_final_cat, 
    ransom_vic_final_quad, ransom_vic_final_cub)


#### DDos####
ddos_vic_final <- glm(ddos ~ age + gender + race_cat + edu_binary + ideo3 +
                        income_category + citizen_binary + priorit + 
                        tech_z + privacy_z + password +
                        block + antivirus + twofactor + internet_use_num + bank_often_binary + 
                        online_shopping + socmedia_num + work_email + 
                        personal_email + online_gaming,
                      data = d,
                      family = binomial)
summary(ddos_vic_final)

ddos_vic_final_cat <- glm(ddos ~ age_quartile + gender + race_cat + edu_binary + ideo3 +
                        income_category + citizen_binary + priorit + 
                        tech_z + privacy_z + password +
                        block + antivirus + twofactor + internet_use_num + bank_often_binary + 
                        online_shopping + socmedia_num + work_email + 
                        personal_email + online_gaming,
                      data = d,
                      family = binomial)
summary(ddos_vic_final_cat)

ddos_vic_final_quad <- glm(ddos ~ age + I(age^2) + gender + race_cat + edu_binary + ideo3 +
                        income_category + citizen_binary + priorit + 
                        tech_z + privacy_z + password +
                        block + antivirus + twofactor + internet_use_num + bank_often_binary + 
                        online_shopping + socmedia_num + work_email + 
                        personal_email + online_gaming,
                      data = d,
                      family = binomial)
summary(ddos_vic_final_quad)

ddos_vic_final_cub <- glm(ddos ~ age + I(age^3) + gender + race_cat + edu_binary + ideo3 +
                        income_category + citizen_binary + priorit + 
                        tech_z + privacy_z + password +
                        block + antivirus + twofactor + internet_use_num + bank_often_binary + 
                        online_shopping + socmedia_num + work_email + 
                        personal_email + online_gaming,
                      data = d,
                      family = binomial)
summary(ddos_vic_final_cub)

BIC(ddos_vic_final, ddos_vic_final_cat, 
    ddos_vic_final_quad, ddos_vic_final_cub)

#### Cryptoattck ####
cryptoattack_mod_final <- glm(cryptoattack ~ age + gender + race_cat + edu_binary + ideo3 +
                                income_category + citizen_binary + priorit + 
                                tech_z + privacy_z + password +
                                block + antivirus + twofactor + internet_use_num + bank_often_binary + 
                                online_shopping + socmedia_num + work_email + 
                                personal_email + online_gaming,
                              data = d,
                              family = binomial)
summary(cryptoattack_mod_final)

cryptoattack_mod_cat <- glm(cryptoattack ~ age_quartile + gender + race_cat + edu_binary + ideo3 +
                                income_category + citizen_binary + priorit + 
                                tech_z + privacy_z + password +
                                block + antivirus + twofactor + internet_use_num + bank_often_binary + 
                                online_shopping + socmedia_num + work_email + 
                                personal_email + online_gaming,
                              data = d,
                              family = binomial)
summary(cryptoattack_mod_cat)

cryptoattack_mod_quad <- glm(cryptoattack ~ age + I(age^2) + gender + race_cat + edu_binary + ideo3 +
                                income_category + citizen_binary + priorit + 
                                tech_z + privacy_z + password +
                                block + antivirus + twofactor + internet_use_num + bank_often_binary + 
                                online_shopping + socmedia_num + work_email + 
                                personal_email + online_gaming,
                              data = d,
                              family = binomial)
summary(cryptoattack_mod_quad)

cryptoattack_mod_cub <- glm(cryptoattack ~ age + I(age^3) + gender + race_cat + edu_binary + ideo3 +
                                income_category + citizen_binary + priorit + 
                                tech_z + privacy_z + password +
                                block + antivirus + twofactor + internet_use_num + bank_often_binary + 
                                online_shopping + socmedia_num + work_email + 
                                personal_email + online_gaming,
                              data = d,
                              family = binomial)
summary(cryptoattack_mod_cub)

BIC(cryptoattack_mod_final, cryptoattack_mod_cat, 
    cryptoattack_mod_quad, cryptoattack_mod_cub)





#### Cryptofraud ####

cryptofraud_mod_final <- glm(cryptofraud ~ age + gender + race_cat + edu_binary + ideo3 +
                               income_category + citizen_binary + priorit + 
                               tech_z + privacy_z + password +
                               block + antivirus + twofactor + internet_use_num + bank_often_binary + 
                               online_shopping + socmedia_num + work_email + 
                               personal_email + online_gaming,
                             data = d,
                             family = binomial)
summary(cryptofraud_mod_final)
cryptofraud_mod_cat <- glm(cryptofraud ~ age_quartile + gender + race_cat + edu_binary + ideo3 +
                               income_category + citizen_binary + priorit + 
                               tech_z + privacy_z + password +
                               block + antivirus + twofactor + internet_use_num + bank_often_binary + 
                               online_shopping + socmedia_num + work_email + 
                               personal_email + online_gaming,
                             data = d,
                             family = binomial)
summary(cryptofraud_mod_cat)

cryptofraud_mod_quad <- glm(cryptofraud ~ age + I(age^2) + gender + race_cat + edu_binary + ideo3 +
                               income_category + citizen_binary + priorit + 
                               tech_z + privacy_z + password +
                               block + antivirus + twofactor + internet_use_num + bank_often_binary + 
                               online_shopping + socmedia_num + work_email + 
                               personal_email + online_gaming,
                             data = d,
                             family = binomial)
summary(cryptofraud_mod_quad)

cryptofraud_mod_cub <- glm(cryptofraud ~ age + I(age^3) + gender + race_cat + edu_binary + ideo3 +
                              income_category + citizen_binary + priorit + 
                              tech_z + privacy_z + password +
                              block + antivirus + twofactor + internet_use_num + bank_often_binary + 
                              online_shopping + socmedia_num + work_email + 
                              personal_email + online_gaming,
                            data = d,
                            family = binomial)
summary(cryptofraud_mod_cub)

BIC(cryptofraud_mod_final, cryptofraud_mod_cat, 
    cryptofraud_mod_quad, cryptofraud_mod_cub)

#### Model Comparison ####
BIC(ransom_vic_final, ransom_vic_final_cat, 
    ransom_vic_final_quad, ransom_vic_final_cub)
BIC(ddos_vic_final, ddos_vic_final_cat, 
    ddos_vic_final_quad, ddos_vic_final_cub)
BIC(cryptoattack_mod_final, cryptoattack_mod_cat, 
    cryptoattack_mod_quad, cryptoattack_mod_cub)
BIC(cryptofraud_mod_final, cryptofraud_mod_cat, 
    cryptofraud_mod_quad, cryptofraud_mod_cub)
#### Final Model ####
summary(d$race_cat)
ransom_vic_final <- glm(ransomware ~ age + gender + race_cat + edu_binary + ideo3 +
                          income_category + priorit + 
                          tech_z + privacy_z + password +
                          antivirus + twofactor + internet_use_num + bank_often_binary + 
                          online_shopping + socmedia_num + work_email + 
                          personal_email + online_gaming + soc_block,
                        data = d,
                        family = binomial)
summary(ransom_vic_final)
# Odds Ratio
exp(coef(ransom_vic_final))

# Confidence intervals
exp(confint(ransom_vic_final))


ddos_vic_final <- glm(ddos ~ age + gender + race_cat + edu_binary + ideo3 +
                        income_category + priorit + 
                        tech_z + privacy_z + password +
                        antivirus + twofactor + internet_use_num + bank_often_binary + 
                        online_shopping + socmedia_num + work_email + 
                        personal_email + online_gaming + soc_block,
                      data = d,
                      family = binomial)
summary(ddos_vic_final)

# Odds Ratio
exp(coef(ddos_vic_final))

# Confidence intervals
exp(confint(ddos_vic_final))

cryptoattack_mod_quad <- glm(cryptoattack ~ age + gender + race_cat + edu_binary + ideo3 +
                               income_category + priorit + 
                               tech_z + privacy_z + password +
                               antivirus + twofactor + internet_use_num + bank_often_binary + 
                               online_shopping + socmedia_num + work_email + 
                               personal_email + online_gaming + soc_block,
                             data = d,
                             family = binomial)
summary(cryptoattack_mod_quad)

# Odds Ratio
exp(coef(cryptoattack_mod_quad))

# Confidence intervals
exp(confint(cryptoattack_mod_quad))

# Cryptofraud
cryptofraud_mod_final <- glm(cryptofraud ~ age + gender + race_cat + edu_binary + ideo3 +
                               income_category + priorit + 
                               tech_z + privacy_z + password +
                               antivirus + twofactor + internet_use_num + bank_often_binary + 
                               online_shopping + socmedia_num + work_email + 
                               personal_email + online_gaming + soc_block,
                             data = d,
                             family = binomial)
summary(cryptofraud_mod_final)
# Odds Ratio
exp(coef(cryptofraud_mod_final))

# Confidence intervals
exp(confint(cryptofraud_mod_final))