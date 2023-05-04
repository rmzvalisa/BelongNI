
lapply(c("car", "foreign", "haven", "dplyr", "mice", "LittleHelpers", "MplusAutomation",
         "readxl", "gdata", "kableExtra", "sna", "forcats", "purrr"),
       require, character.only = TRUE)  

# Upload two functions written for the current analysis
source("/Users/alisa/Desktop/Research/Religiosity all/WVS7/Belonging/BelongNI/01_Scripts/02_AnalysisScripts/AddFunctionsBelong.R")

# WVS 7 + EVS 5

# Religiosity indicators:
## A006 - Important in life: Religion
## E069_01 - Confidence: Churches
## F025 - Religious denomination (Do not belong to a denomination or other)
## F028 - How often do you attend religious services
## F028B_WVS7 - How often do you pray (WVS7)
### F066_EVS5 - Pray to God outside of religious services (EVS5)
## F034 - Religious person

## A065 - Member: Belong to religious organization - no reverse recoding necessary
## F050 - Believe in: God - no reverse recoding necessary
## F053 - Believe in: hell - no reverse recoding necessary
## F063 - How important is God in your life - no reverse recoding necessary

# Missing data correlates:
## X001 - Gender: Male Female 
## X003R - Age recoded (6 intervals) - no reverse recoding necessary
### WVS 7: You are ____ years old; EVS 5: Source variable: year of birth
### didn't use simple X003 age because there is the category "82 and older" anyway
## X047_WVS7 - Scale of incomes (WVS7) - no reverse recoding necessary
### X047E_EVS5 - Scale of incomes (EVS5)
## X025A_01 - Highest educational level attained - Respondent: ISCED code one digit

# Other variables
## cntry - country name
## reg_iso - country-specific region (necessary for Germany)
## study - survey name
## year - survey year

# Read country codes
codes <- read_excel("/Users/alisa/Desktop/Research/Religiosity all/WVS7/Belonging/BelongNI/02_Data/01_InputData/CountryInfoWVS_EVS.xlsx", 
                    sheet = "Country codes")

# Read data, set the same country names in WVS7 as in WVS6, and merge with country codes
WVS7_EVS5 <- read.spss("/Users/alisa/Desktop/Research/Data/EVS_WVS_Joint_Spss_v4_0.sav", 
                       use.value.labels = T, to.data.frame = T, use.missings = T) %>%
  # Set the same country names in WVS7 as in WVS6
  mutate(cntry = fct_recode(cntry, "Taiwan" = "Taiwan ROC")) %>%
  mutate(cntry = fct_recode(cntry, "Hong Kong" = "Hong Kong SAR")) %>%
  # Merge with country codes
  merge(codes, by.x = c("cntry"), by.y = c("country"),  all.x = T)

# Select only necessary variables and recode
# Set the following column names:
## Importance of religion = imprel
## Membership in a religious organisation = member
## Confidence in institutions = confidence
## Belonging to a denomination = belong
## Frequency of attendance = attend
## Frequency of praying = pray_WVS and pray_EVS
## Identification as religious person = person
## Belief in God = bgod
## Belief in hell = bhell
## Importance of God = impgod

## Income = income_WVS and income_EVS
## Gender = gender
## Age = age
## Education = education
## Region = region
rel_data <- WVS7_EVS5 %>%
  select(cntry, reg_iso, study, year, A006, A065, E069_01, F025, F028, F028B_WVS7, F066_EVS5, F034, 
         F050, F053, F063, X001, X003R, X047_WVS7, X047E_EVS5, X025A_01, code) %>%
  rename(country = cntry, region = reg_iso, survey = study, imprel = A006, member = A065, 
         confidence = E069_01, belong = F025, attend = F028, pray_WVS = F028B_WVS7,
         pray_EVS = F066_EVS5, person = F034, bgod = F050, bhell = F053, impgod = F063, 
         gender = X001, age = X003R, income_WVS = X047_WVS7, income_EVS = X047E_EVS5, education = X025A_01)

rel_data <- subset(rel_data, 
                   subset = !((rel_data$country %in% 
                                 c("Armenia", "Czechia", "Germany", "Great Britain", "Netherlands", 
                                   "Romania", "Russia", "Serbia", "Slovakia", "Ukraine")) & 
                                rel_data$survey == "EVS"))

# Make two separate samples for Germany: East and West
## Recode and drop Berlin
Germany <- rel_data %>%
  filter(country == "Germany") %>%
  mutate(country = case_when(
    region %in% c("DE-MV Mecklenburg-Western Pomerania", "DE-BB Brandenburg",
                  "DE-SN Saxony", "DE-ST Saxony-Anhalt", "DE-TH Thuringia") ~ "Germany East",
    region == "DE-BE Berlin" ~ NA_character_,
    TRUE ~ "Germany West"
  )) %>%
  mutate(country = as.factor(country)) %>%
  filter(!is.na(country))

rel_data <- rel_data  %>%
  rbind(Germany) %>%
  subset(country != "Germany") %>% 
  select(-region)

rm(Germany)

levels(rel_data$country)
# 91 countries - do not count "Germany" - this factor level will be dropped later

# -----------------------

# WVS 6

# Religiosity indicators:
## V9 - For each of the following, indicate how important it is in your life: Religion
## V108 - For each one, could you tell me how much confidence you have in them. The Churches
## V144 - Do you belong to a religion or religious denomination? 
## V145 - Apart from weddings and funerals, about how often do you attend religious services these days?
## V146 - Apart from weddings and funerals, about how often do you pray?
## V147 - Would you say you are... religious / not a religious person / an atheist

## V25 - For each organization, could you tell me whether you are….? 
### active / inactive member / do not belong. Church or religious organization - no reverse recoding necessary
## V148 - Do you believe in God?
## V149 - Do you believe in hell? 
## V152 - How important is God in your life? - no reverse recoding necessary

# Missing data correlates:
# V240 - Gender: Male Female 
# V242 - You are ____ years old 
# V239 - On this card is an income scale on which 1 indicates the lowest income group and 10 the highest
## income group in your country. We would like to know in what group your household is. 
## from Lower step to Tenth step
# V248 - What is the highest educational level that you have attained?
## from No formal education to University - level education, with degree 

# Read the survey year
WVS6_year <- read_excel("/Users/alisa/Desktop/Research/Religiosity all/WVS7/Belonging/BelongNI/02_Data/01_InputData/CountryInfoWVS_EVS.xlsx", 
                        sheet = "Survey year")

# Read the data and merge with country codes and survey year
WVS6 <- read.spss("/Users/alisa/Desktop/Research/Data/WV6_Data_Spss_v20180912.sav",
                  use.value.labels = T, to.data.frame = T, use.missings = T) %>%
  merge(WVS6_year, by.x = c("V2"), by.y = c("country"), all.x = T) %>%
  merge(codes, by.x = c("V2"), by.y = c("country"), all.x = T)

# Select only necessary variables and rename them
rel_data_WVS6 <- WVS6 %>%
  select(c(V2, V9, V25, V108, V144:V149, V152, V240, V242, V239, V248, year, code)) %>%
  rename(country = V2, imprel = V9, member = V25, confidence = V108, belong = V144, 
         attend = V145, pray = V146, person = V147, bgod = V148, bhell = V149, impgod = V152, 
         gender = V240, age = V242, income = V239, education = V248)

# Subset countries that did not participate in WVS 7 and EVS 5
rel_data_WVS6 <- subset(rel_data_WVS6, subset = 
                          !(rel_data_WVS6$country %in% levels(rel_data$country)))
rel_data_WVS6$country <- droplevels(rel_data_WVS6$country)
levels(rel_data_WVS6$country)
# 12 countries

# Create a column with survey name as in WVS_EVS
rel_data_WVS6$survey <- "WVS 6"

# -----------------------

# Recode variables: WVS7 and EVS5
## income, attendance, praying, person, survey year, age

# INCOME. Combining two surveys
## 1. Recode from factor into numeric the two income variables
## 2. Combine two variables into new variable
## 3. Drop survey-specific variables
rel_data <- rel_data %>%
  mutate(across(c(income_WVS, income_EVS), as.numeric)) %>%
  mutate(income = ifelse(survey == "WVS", income_WVS, income_EVS)) %>%
  select(-c(income_WVS, income_EVS))

# ATTENDANCE. Recoding
## 1. Exclude "Other specific holy days" category from (probably a typo) - 0 observations
rel_data$attend <- droplevels(rel_data$attend)

## 2. Recode the "Only on special holy days/Christmas/Easter days" category as in WVS6
rel_data <- rel_data %>%
  mutate(attend = fct_recode(attend, "Only on special holy days" = 
                               "Only on special holy days/Christmas/Easter days"))

# PERSON. Recoding
## Recode the "A convinced atheist" category as in WVS6
rel_data <- rel_data %>%
  mutate(person = fct_recode(person, "An atheist" = "A convinced atheist"))


# PRAYING. Combining two surveys
## 1. Unite response options for praying in WVS (for similar recoding guidance, see Onbound project)
### Bechert, Insa, May, Antonia, Quandt, Markus and Werhan, Katharina.2020. 
### ONBound - Old and new boundaries: National Identities and Religion. 
### Customized dataset. GESIS Data Archive, Cologne. http://onbound.gesis.org/wizard.

# According to the Onbound project:  
## WVS7: 
### Several times a day + Once a day + Several times each week = Once or several times a week
### Only when attending religious services
### Only on special holy days = Several times a year, only on special holidays
### Once a year + Less often = Once a year or less frequently
### Never, practically never = Never, practically never

## EVS:
### Every day + More than once week + Once a week = Once or several times a week
### At least once a month = One to three times a month
### At least once a month + Several times a year = Several times a year, only on special holidays
### Less often = Once a year or less frequently
### Never = Never, practically never

# Modify these five categories as follows:
## WVS7
### Several times a day + Once a day = Once or several times a day
### Several times each week + Only when attending religious services = Once or several times a month 
### Only on special holy days = Several times a year, only on special holidays
### Once a year + Less often = Once a year or less often
### Never, practically never = Never, practically never

## EVS5:
### Every day = Once or several times a day
### Once a week + More than once week + At least once a month = Once or several times a month 
### Several times a year = Several times a year, only on special holidays
### Less often = Once a year or less often
### Never = Never, practically never

## 2. Reverse code
## 3. Combine two variables into new variable 
## 4. Drop survey-specific variables

rel_data <- rel_data %>%
  mutate(pray_WVS = fct_recode(pray_WVS, 
                             "Once or several times a day" = "Several times a day",
                             "Once or several times a day" = "Once a day",
                             "Once or several times a month" = "Several times each week",
                             "Once or several times a month" = "Only when attending religious services",
                             "Several times a year, only on special holidays" = "Only on special holy days",
                             "Once a year or less often" = "Once a year",
                             "Once a year or less often" = "Less often")) %>%
  mutate(pray_EVS = fct_recode(pray_EVS, 
                               "Once or several times a day" = "Every day",
                               "Once or several times a month" = "More than once week",
                               "Once or several times a month" = "Once a week",
                               "Once or several times a month" = "At least once a month",
                               "Several times a year, only on special holidays" = "Several times a year",
                               "Once a year or less often" = "Less often",
                               "Never, practically never" = "Never"))  %>%
  mutate(across(c(pray_WVS, pray_EVS), ~ fct_relevel(., rev))) %>%
  mutate(pray = if_else(survey == "WVS", pray_WVS, pray_EVS)) %>%
  select(-c(pray_WVS, pray_EVS))


# SURVEY YEAR. Recoding
## Recode into numeric
rel_data$year <- as.numeric(as.character(rel_data$year))


# AGE. Recoding
## Recode into numeric
rel_data$age <- as.numeric(rel_data$age)

# -----------------------

# Recode variables: WVS6
## education, praying, member, age, belief in God and in hell

# EDUCATION
# WVS_EVS:
## Less than primary = Less than primary
## Primary = Primary
## Lower secondary = Lower secondary
## Upper secondary + Post-secondary non tertiary + Short-cycle tertiary =
### Upper secondary / tertiary without degree
## Bachelor or equivalent + Master or equivalent + Doctoral or equivalent = 
### University - level education, with degree

# WVS 6:
## No formal education + Incomplete primary school = Less than primary
## Complete primary school = Primary
## Incomplete secondary school: technical/ vocational type + 
### Incomplete secondary school: university-preparatory type = Lower secondary	
## Complete secondary school: technical/ vocational type + 
### Complete secondary school: university-preparatory type +
### Some university-level education, without degree = Upper secondary / tertiary without degree
## University - level education, with degree = University - level education, with degree

rel_data <- rel_data %>%
  mutate(education = fct_recode(education, 
                                "Upper secondary / tertiary without degree" = "Upper secondary",
                                "Upper secondary / tertiary without degree" = "Post-secondary non tertiary",
                                "Upper secondary / tertiary without degree" = "Short-cycle tertiary",
                                "University - level education, with degree" = "Bachelor or equivalent",
                                "University - level education, with degree" = "Master or equivalent",
                                "University - level education, with degree" = "Doctoral or equivalent"))

rel_data_WVS6 <- rel_data_WVS6 %>%
  mutate(education = fct_recode(education, 
                                "Less than primary" = "No formal education",
                                "Less than primary" = "Incomplete primary school",
                                "Primary" = "Complete primary school",
                                "Lower secondary" = "Incomplete secondary school: technical/ vocational type",
                                "Lower secondary" = "Incomplete secondary school: university-preparatory type",
                                "Upper secondary / tertiary without degree" = "Complete secondary school: technical/ vocational type",
                                "Upper secondary / tertiary without degree" = "Complete secondary school: university-preparatory type",
                                "Upper secondary / tertiary without degree" = "Some university-level education, without degree"))


# MEMBER. Recoding
## Make the same options as in WVS_EVS
rel_data_WVS6 <- rel_data_WVS6 %>%
  mutate(member = fct_recode(member, 
                             "Not mentioned" = "Not a member",
                             "Mentioned" = "Inactive member",
                             "Mentioned" = "Active member"))

# PRAYING. Recoding
## Make the same options as in WVS_EVS
rel_data_WVS6 <- rel_data_WVS6 %>%
  mutate(pray = fct_recode(pray, 
                               "Once or several times a day" = "Several times a day",
                               "Once or several times a day" = "Once a day",
                               "Once or several times a month" = "Several times each week",
                               "Once or several times a month" = "Only when attending religious services",
                               "Several times a year, only on special holidays" = "Only on special holy days",
                               "Once a year or less often" = "Once a year",
                               "Once a year or less often" = "Less often than once a year"))  %>%
  mutate(pray = fct_relevel(pray, rev))


# BELONGING. Recoding
## 1. Recode into numeric for each survey because of the different coding of levels (too many levels)
## 2. Set the new coding and reverse code
rel_data <- rel_data %>%
  mutate(belong = as.numeric(belong)) %>%
  mutate(belong = ifelse(belong == "1", "Do not belong to a denomination", "Belong to a denomination")) %>%
  mutate(belong = as.factor(belong)) %>%
  mutate(belong = fct_relevel(belong, rev))

rel_data_WVS6 <- rel_data_WVS6 %>%
  mutate(belong = as.numeric(belong)) %>%
  mutate(belong = ifelse(belong == "1", "Do not belong to a denomination", "Belong to a denomination")) %>%
  mutate(belong = as.factor(belong)) %>%
  mutate(belong = fct_relevel(belong, rev))

# AGE. Recoding
# Recode into numeric (survey year is already numeric) and collapse into categories
rel_data_WVS6 <- rel_data_WVS6 %>%
  mutate(age = as.numeric(as.character(age))) %>%
  mutate(age = case_when(
    between(age, 16, 24) ~ 1,
    between(age, 25, 34) ~ 2,
    between(age, 35, 44) ~ 3,
    between(age, 45, 54) ~ 4,
    between(age, 55, 64) ~ 5,
    between(age, 65, 102) ~ 6,
    is.na(age) ~ NA_real_
  ))

# BGOD AND BHELL. Recoding
# Reverse code
rel_data_WVS6 <- rel_data_WVS6 %>%
  mutate(across(c(bgod, bhell), ~ fct_relevel(., rev))) 

# INCOME. Recoding
# Recode into numeric class
rel_data_WVS6$income <- as.numeric(rel_data_WVS6$income)

# Set the same order of columns in WVS_EVS as in WVS6
rel_data <- rel_data[colnames(rel_data_WVS6)]

# Combine two datasets
rel_data <- rbind(rel_data, rel_data_WVS6)
rel_data$country <- droplevels(rel_data$country)
levels(rel_data$country)
# 103 territories in total (two samples for Germany -> 102 countries)

# -----------------------

# Make person binary and reverse code
rel_data <- rel_data %>%
  mutate(person = fct_recode(person, "Not a religious person" = "An atheist")) %>%
  mutate(person = fct_relevel(person, rev))

# Reverse recode imprel and confidence
rel_data <- rel_data %>%
  mutate(across(c(imprel, confidence), ~ fct_relevel(., rev)))

# Recode the remaining variables into numeric
rel_data <- rel_data %>%
  mutate(across(c(attend, pray, impgod, education), as.numeric))

# Reverse recode attendance
rel_data$attend <- Recode(rel_data$attend, rec = "1=7; 2=6; 3=5; 4=4; 5=3; 6=2; 7=1; else=NA")


# impgod and member do not need to be recoded: 
## higher scores already correspond to higher religiosity

# praying, bgod, and bhell are already recoded


#===================================================================================================


rel_data <- rel_data %>%
  filter(!country %in% 
           c("Bangladesh", "Indonesia", "Iraq", "Jordan", "Lebanon", "Pakistan", "Thailand",
             "Egypt", "Palestine", "Algeria", "Haiti", "Yemen", "Libya", "Kuwait", "Qatar", "Ethiopia")) %>%
  mutate(country = droplevels(country))
# 87 countries

# Also drop Hong Kong, Macau SAR, and Puerto Rico because 
## there are no RRI and RLI country-level measures
rel_data <- rel_data %>%
  filter(!country %in% c("Hong Kong", "Macau SAR", "Puerto Rico")) %>%
  mutate(country = droplevels(country))

#===================================================================================================

# IMPUTATION
## Imputation process for all countries is time-consuming
## There is code to load the imputed dataset at the end of the section

# Specify prediction method for each indicator
pred_matrix <- make.predictorMatrix(data = rel_data)
pred_matrix[, c("country", "code", "year", "survey")] <- 0 # exclude these variables
pred_matrix[c("country", "code", "year", "survey"), ] <- 0

imp_method <- make.method(data = rel_data)
imp_method[c("person", "belong", "member", "gender", "bgod", "bhell")] <- "logreg" # binary variables
imp_method[c("confidence", "imprel")] <- "polr" # ordered categorical variables with > 2 categories
imp_method[c("attend", "pray", "impgod", "age", "income", "education")] <- "pmm" # continuous variables

# Make factor religiosity indicators as they appear in the usual analysis - numbers, not factor labels
rel_data <- rel_data %>%
  mutate(across(c(person, belong, member, bgod, bhell, confidence, imprel), as.numeric)) %>%
  mutate(across(c(person, belong, member, bgod, bhell, confidence, imprel), as.factor))

# Drop Russia, Tunisia, Turkey, Georgia, and Maldives
# the corresponding data will be imputed separately due to the convergence issues
imp_data <- rel_data %>%
  filter(!country %in% c("Russia", "Tunisia", "Turkey", "Georgia", "Maldives")) %>%
  mutate(country = droplevels(country))

# Split into country-specific datasets
imp_data <- split(imp_data, imp_data$country)

# There are no missing values in: 
## Rwanda, Ghana, Venezuela, Nicaragua, Morocco, South Korea, Colombia, Canada, and Myanmar
## Drop these countries from the imputation; they will be added later
## lapply(imp_data, function(x)
##  sum(is.na(x[, c("person", "belong", "confidence", "imprel", "attend", "pray")])))
imp_data <- imp_data %>%
  subset(!names(.) %in% c("Rwanda", "Ghana", "Venezuela", "Nicaragua", "Morocco", 
                          "South Korea", "Colombia", "Canada", "Myanmar"))

for (i in 1:length(imp_data)){
  imp_data[[i]] <- mice(data = imp_data[[i]], m = 5, #5 imputed datasets
                        maxit = 100, #100 iterations
                        meth = imp_method, predictorMatrix = pred_matrix, 
                        seed = 12345, #to replicate the results
                        print = F)
}


# Ignore the warnings - logged events (for constant variables):
## Kyrgyzstan - constant bhell
## Portugal - constant income (not asked)

# diagnostics
plot(imp_data$Andorra)
# for continious variables
densityplot(imp_data$Andorra)
# or 
bwplot(imp_data$Andorra)

# make five imputed datasets for each country in a list
for (i in 1:length(imp_data)){
  imp_data[[i]] <- complete(imp_data[[i]], action = "all") 
}

# -----------------------

# Imputation for Russia and Georgia
## Impute without bgod due to its high correlation with person (>0.9)
## in Russia - bgod is collinear (mice warning)
## correlations computed with lavCor function, "pairwise" deletion of missings
## e.g., subset Georgia and compute
## lavCor(Georgia[, 2:10], ordered = c("person", "belong", "bgod", "bhell", 
## "confidence", "member", "imprel"), 
## missing = "pairwise")

imp_data_RG <- rel_data %>% 
  filter(country %in% c("Russia", "Georgia")) %>%
  mutate(country = droplevels(country), bgod = NULL) %>%
  split(., .$country)

pred_matrix <- make.predictorMatrix(data = imp_data_RG$Russia)
pred_matrix[, c("country", "code", "year", "survey")] <- 0
pred_matrix[c("country", "code", "year", "survey"), ] <- 0

imp_method <- make.method(data = imp_data_RG$Russia)
imp_method[c("person", "belong", "member", "gender", "bhell")] <- "logreg"
imp_method[c("confidence", "imprel")] <- "polr"
imp_method[c("attend", "pray", "impgod", "age", "income", "education")] <- "pmm"

for (i in 1:length(imp_data_RG)){
  imp_data_RG[[i]] <- mice(data = imp_data_RG[[i]], m = 5, #5 imputed datasets
                           maxit = 100, #100 iterations
                           meth = imp_method, predictorMatrix = pred_matrix, 
                           seed = 12345, #to replicate the results
                           print = F)
  
}

# Diagnostics
plot(imp_data_RG$Georgia)
# for continious variables
densityplot(imp_data_RG$Georgia)
# or 
bwplot(imp_data_RG$Georgia)

# Make five imputed datasets for each country in a list
for (i in 1:length(imp_data_RG)){
  imp_data_RG[[i]] <- complete(imp_data_RG[[i]], action = "all")
}

# Add to the list with the remaining countries
imp_data$Russia <- imp_data_RG$Russia
imp_data$Georgia <- imp_data_RG$Georgia

# -----------------------

# Imputation for Tunisia
## Impute without bhell due to the convergence issues when predicting bgod (mice warning)
## high correlation with bgod (>0.9)
Tunisia <- rel_data %>%
  filter(country == "Tunisia")  %>%
  mutate(country = droplevels(country), bhell = NULL)

pred_matrix <- make.predictorMatrix(data = Tunisia)
pred_matrix[, c("country", "code", "year", "survey")] <- 0
pred_matrix[c("country", "code", "year", "survey"), ] <- 0

imp_method <- make.method(data = Tunisia)
imp_method[c("person", "belong", "member", "gender", "bgod")] <- "logreg"
imp_method[c("confidence", "imprel")] <- "polr"
imp_method[c("attend", "pray", "impgod", "age", "income", "education")] <- "pmm"

Tunisia <- mice(data = Tunisia, m = 5, #5 imputed datasets
                maxit = 100, #100 iterations
                meth = imp_method, predictorMatrix = pred_matrix, 
                seed = 12345, #to replicate the results
                print = F)

# Diagnostics
plot(Tunisia)
# for continuous variables
densityplot(Tunisia)
# or 
bwplot(Tunisia)

# Make five imputed datasets and add to the list with the remaining countries
Tunisia <- complete(Tunisia, action = "all")
imp_data$Tunisia <- Tunisia

# -----------------------

# Imputation for Turkey
## Impute without belong due to the convergence issues when predicting attendance (mice warning)
Turkey <- rel_data %>%
  filter(country == "Turkey")  %>%
  mutate(country = droplevels(country), belong = NULL)

pred_matrix <- make.predictorMatrix(data = Turkey)
pred_matrix[, c("country", "code", "year", "survey")] <- 0
pred_matrix[c("country", "code", "year", "survey"), ] <- 0

imp_method <- make.method(data = Turkey)
imp_method[c("person", "gender", "member", "bgod", "bhell")] <- "logreg"
imp_method[c("confidence", "imprel")] <- "polr"
imp_method[c("attend", "pray", "impgod", "age", "income", "education")] <- "pmm"

Turkey <- mice(data = Turkey, m = 5, #5 imputed datasets
               maxit = 100, #100 iterations
               meth = imp_method, predictorMatrix = pred_matrix, 
               seed = 12345, #to replicate the results
               print = F)

# Diagnostics
plot(Turkey)
# for continuous variables
densityplot(Turkey)
# or 
bwplot(Turkey)

# Make five imputed datasets and add to the list with the remaining countries
Turkey <- complete(Turkey, action = "all")
imp_data$Turkey <- Turkey


# Impute belong without attendance
Turkey <- rel_data %>%
  filter(country == "Turkey")  %>%
  mutate(country = droplevels(country), attend = NULL)

pred_matrix <- make.predictorMatrix(data = Turkey)
pred_matrix[, c("country", "code", "year", "survey")] <- 0
pred_matrix[c("country", "code", "year", "survey"), ] <- 0

imp_method <- make.method(data = Turkey)
imp_method[c("person", "belong", "member", "gender", "bgod", "bhell")] <- "logreg"
imp_method[c("confidence", "imprel")] <- "polr"
imp_method[c("pray", "impgod", "age", "income", "education")] <- "pmm"

Turkey <- mice(data = Turkey, m = 5, #5 imputed datasets
               maxit = 100, #100 iterations
               meth = imp_method, predictorMatrix = pred_matrix, 
               seed = 12345, #to replicate the results
               print = F)

# Diagnostics
plot(Turkey)
# for continuous variables
densityplot(Turkey)
# or 
bwplot(Turkey)

## Add belong to the imputed datasets
Turkey <- complete(Turkey, action = "all")
for (i in 1:length(imp_data$Turkey)){
  imp_data$Turkey[[i]]$belong <- Turkey[[i]]$belong
}

# -----------------------

# Imputation for Maldives
## Impute without bgod due to its high correlation with confidence (>0.9)
Maldives <- rel_data %>%
  filter(country == "Maldives")  %>%
  mutate(country = droplevels(country), bgod = NULL)

pred_matrix <- make.predictorMatrix(data = Maldives)
pred_matrix[, c("country", "code", "year", "survey")] <- 0
pred_matrix[c("country", "code", "year", "survey"), ] <- 0

imp_method <- make.method(data = Maldives)
imp_method[c("person", "gender", "member", "belong", "bhell")] <- "logreg"
imp_method[c("confidence", "imprel")] <- "polr"
imp_method[c("attend", "pray", "impgod", "age", "income", "education")] <- "pmm"

Maldives <- mice(data = Maldives, m = 5, #5 imputed datasets
                 maxit = 100, #100 iterations
                 meth = imp_method, predictorMatrix = pred_matrix, 
                 seed = 12345, #to replicate the results
                 print = F)

# Diagnostics
plot(Maldives)
# for continuous variables
densityplot(Maldives)
# or 
bwplot(Maldives)

# Make five imputed datasets and add to the list with the remaining countries
Maldives <- complete(Maldives, action = "all")
imp_data$Maldives <- Maldives

# -----------------------

# Make five datasets for each country that had no missing values
imp_data_nomis <- rel_data %>%
  filter(country %in% 
           c("Rwanda", "Ghana", "Venezuela", "Nicaragua", "Morocco", "South Korea", 
             "Colombia", "Canada", "Myanmar")) %>%
  mutate(country = droplevels(country)) %>%
  split(., .$country) %>%
  lapply(function(x) rep(list(x), 5))

# Combine datasets
imp_data <- append(imp_data, imp_data_nomis)


setwd("/Users/alisa/Desktop/untitled folder 2")
load("imp_data.RData")

# -----------------------

# Final imputed datasets

imp_data <- imp_data %>%
  # Drop all variables used for imputation only
  # Specify the same order of columns for all datasets
  map(~ map(.x, select, country, imprel, confidence, belong, attend, pray, person, year, code, survey)) %>%
  map(~ map(.x, ~ mutate(.x, country = droplevels(country)))) %>%
  # Recode factor variables into numeric
  map(~ map(.x, ~ mutate_at(.x, vars(person, belong, confidence, imprel), as.numeric)))

imp_data <- imp_data[base::order(names(imp_data))]
# 84 countries

# save or download the imputed dataset 
## setwd("/Users/alisa/Desktop/Research/Religiosity all/WVS7/Belonging/BelongNI/02_Data/02_AnalysisData")
## save(imp_data, file = "imp_data_WVS_EVS.RData")
## load("imp_data_WVS_EVS.RData")

## lapply(imp_data, function(x)
##  lapply(x, function(y)
##   sum(is.na(y))))
## no missings

rm(Tunisia, Turkey, Maldives, imp_data_RG, imp_data_nomis)

# ========

# Compute correlations between variables
## and drop the countries with negative correlations or collinear variables
cor_imp <- imp_data %>%
  map(~ map(.x, select, imprel, confidence, belong, attend, pray, person)) %>%
  map(~ map(.x, ~ lavCor(as.data.frame(.), ordered = ord_items, output = "cor"))) %>%
  map(~ map(.x, diag.remove, remove.val = NA)) %>%
  map(~ map(.x, ~ sum(!is.na(ifelse(. < 0, ., NA)))))




cor_imp <- imp_data
for (i in 1:length(cor_imp)) {
  
  for (a in 1:length(cor_imp[[i]])) { 
    # Compute correlations that are used in CFA analyses
    cor_imp[[i]][[a]] <- cor_imp[[i]][[a]][, c("person", "belong", "confidence", "imprel", 
                                               "pray", "attend")]
    
    cor_imp[[i]][[a]] <- lavCor(as.data.frame(cor_imp[[i]][[a]]), 
                                ordered = ord_items, output = "cor")
    
    # Remove diagonal variances
    cor_imp[[i]][[a]] <- diag.remove(cor_imp[[i]][[a]], remove.val = NA)
    
    # Find negative correlations
    cor_imp[[i]][[a]] <- ifelse(cor_imp[[i]][[a]] < 0, cor_imp[[i]][[a]], NA)
    cor_imp[[i]][[a]] <- sum(!is.na(cor_imp[[i]][[a]]))
    
  }
  
}
















# ========

Brazil <- rel_data[rel_data$country=="Brazil", ]
for (item in c("person", "confidence", "imprel", "belong")) {
  Brazil[, item] <- as.numeric(Brazil[, item])
}
for (item in c("person", "confidence", "imprel", "belong")) {
  Brazil[, item] <- as.factor(Brazil[, item])
}

Brazil <- mice(data = Brazil, m = 5, #5 imputed datasets
                maxit = 100, #100 iterations
                meth = imp_method, predictorMatrix = pred_matrix, 
                seed = 12345, #to replicate the results
                print = F)
Brazil <- complete(Brazil, action = "all")

table(Brazil[[1]]$attend)

imp_method


## попробовать факторные 4 переменные сделать numeric потом перед импутацией factor...



sapply(c("imprel", "confidence", "belong", "attend", "pray", "person",
         "member", "impgod", "bgod", "bhell",
         "income", "education", "gender", "age"), function(x) {
           crosstab("country", x, rel_data,  margin = "row")
         }) 

