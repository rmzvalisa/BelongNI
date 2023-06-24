
# DATA PREPARATION

lapply(c("car", "foreign", "haven", "dplyr", "mice", "LittleHelpers", "MplusAutomation",
         "readxl", "gdata", "kableExtra", "sna", "forcats", "purrr"),
       require, character.only = TRUE)  

# Upload two functions written for the current analysis
source("./01_Scripts/02_AnalysisScripts/AddFunctionsBelong.R")

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
codes <- read_excel("./02_Data/01_InputData/CountryInfoWVS_EVS.xlsx", 
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
WVS6_year <- read_excel("./02_Data/01_InputData/CountryInfoWVS_EVS.xlsx", 
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

# There are empty categories / omitted questions in the following countries:
sapply(c("imprel", "confidence", "belong", "attend", "pray", "person",
         "member", "impgod", "bgod", "bhell",
         "income", "education", "gender", "age"), function(x) {
           crosstab("country", x, rel_data,  margin = "row")
         }) 

# Omitted questions
## praying: Kuwait, Qatar
## attendance: Kuwait, Qatar
## belonging: Libya, Kuwait, Qatar

# Empty response categories of categorical indicators:  
## belonging: Bangladesh, Indonesia, Iraq, Jordan, Lebanon, Pakistan, Thailand,
### Egypt, Palestine, Algeria, Haiti, Yemen
## confidence: Bangladesh
## importance of religion: Ethiopia, Egypt, Libya, Qatar

# For imputation (aka logged events)
## Omitted questions
### bhell: Iraq, Kyrgyzstan, Palestine, Kuwait, Qatar, Yemen
### bgod: Iraq, Egypt, Palestine, Kuwait, Qatar, Yemen
### impgod: Iraq, Kuwait
### income: Portugal

## Empty response categories of categorical indicators: 
### bgod: Algeria

# Drop Bangladesh, Indonesia, Iraq, Jordan, Lebanon, Pakistan, Thailand,
## Egypt, Palestine, Algeria, Haiti, Yemen, Libya, Kuwait, Qatar, and Ethiopia 
## due to the absence/empty response options of the core indicators
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

levels(rel_data$country)
# 84 countries for the further analysis

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
## save(imp_data, file = "imp_data_WVS_EVS.RData")
## load("./02_Data/02_AnalysisData/imp_data_WVS_EVS.RData")

## lapply(imp_data, function(x)
##  lapply(x, function(y)
##   sum(is.na(y))))
## no missings

rm(Tunisia, Turkey, Maldives, imp_data_RG, imp_data_nomis)

#===================================================================================================

# CFA----

# ordered indicators
ord_items <- c("person", "imprel", "confidence", "belong")

# Compute correlations between variables
## and drop the countries with negative correlations or collinear variables
cor_imp <- imp_data %>%
  map(~ map(.x, select, imprel, confidence, belong, attend, pray, person)) %>%
  map(~ map(.x, ~ lavCor(as.data.frame(.), ordered = ord_items, output = "cor"))) %>%
  map(~ map(.x, sna::diag.remove, remove.val = NA)) %>%
  map(~ map(.x, ~ sum(!is.na(ifelse(. < 0, ., NA)))))

## ignore the warnings

lapply(cor_imp, function(x) 
  lapply(x, function(y)
    y == 0))

# There are negative correlations in:
## China, Kenya, Maldives, Myanmar, Nigeria, the Philippines, Rwanda, Tunisia, and Zimbabwe
## additionally drop these countries
imp_data <- imp_data %>%
  subset(!names(.) %in% c("China", "Kenya", "Maldives", "Myanmar", "Nigeria", "Philippines", "Rwanda",
                          "Tunisia", "Zimbabwe"))

# One-factor model - Model 2 from 
## Remizova, Alisa, Maksim Rudnev, and Eldad Davidov. 2022. 
## In search of a comparable measure of generalized individual religiosity 
## in the World Values Survey. Sociological Methods and Research:1-33.

## Add the second residual covariance: imprel ~~ confidence 

model <- 'Religiosity =~ imprel + person + confidence + pray + attend + belong;
pray ~~ attend;
imprel ~~ confidence;
'
# save only the countries with acceptable model fit
cfa_model <- groupwiseCFA.mi.sep(model, data = imp_data, ordered = ord_items, 
                                 estimator = "WLSMV", out = c("goodfit"))

## ignore the warnings

# 50 countries
## Asian: India, Japan, Malaysia, Singapore, Taiwan, Vietnam
## Muslim: Cyprus, Iran, Kazakhstan, Kyrgyzstan, 

# Ignore the warnings, they do not affect the sample with acceptable model fit
## Verify: 
## imp_data_belong <- subset(imp_data, (names(imp_data)  %in% rownames(cfa_model)))
## cfa_model_belong <- groupwiseCFA.mi.sep(model, data = imp_data_belong, ordered = ord_items, 
##                                         estimator = "WLSMV", out = c("goodfit"))
## There are no warnings
## rm(imp_data_belong, cfa_model_belong)

# Make five final imputed datasets
data1 <- lapply(imp_data, function(x) x[[1]]) %>% do.call("rbind", .) 
data2 <- lapply(imp_data, function(x) x[[2]]) %>% do.call("rbind", .) 
data3 <- lapply(imp_data, function(x) x[[3]]) %>% do.call("rbind", .) 
data4 <- lapply(imp_data, function(x) x[[4]]) %>% do.call("rbind", .) 
data5 <- lapply(imp_data, function(x) x[[5]]) %>% do.call("rbind", .) 

data_list <- list(data1, data2, data3, data4, data5)

# Model sample - select 50 countries
mlsem_dat <- map(data_list, ~subset(.x, subset = .x$country %in% rownames(cfa_model)) %>% 
                   {.$country <- droplevels(.$country); .})

# MGCFA to ensure the similarity of the factor structure
mgcfa_model <- globalMI.mi.sep(model, data = mlsem_dat, ordered = ord_items, 
                               estimator = "WLSMV", group = "country")

rm(data1, data2, data3, data4, data5)

#===================================================================================================

# COUNTRY-LEVEL INDICATORS

# Religious composition
# Data: Brown, D., & James, P. Religious Characteristics of States Dataset Project
## Demographics v. 2.0 (RCS-Dem 2.0), COUNTRIES ONLY.

# WVS6: select observations corresponding to the country-specific year of WVS data collection
# WVS/EVS: select observations for 2015 year as the most recent data

# The following variables are selected:
## NUMISO - Country Code assigned by International Standards Organization

## Christians:
## CHRPC - Percentage of Christians (All denominations combined; incl. liminal but not syncretic)
## CSYNPC - Percentage of Christian Syncretics (Mostly African and New World African spiritist and spiritualist denominations)

## Muslims:
### MUSPC - Percentage of Muslims
### MSYNPC - Percentage of Muslim Syncretics 

## Asian Religions:
## HINPC - Percentage of Hindus
## Buddhists:
### BUDPC - Percentage of Buddhists
### BSYNPC - Percentage of Buddhist Syncretics 
## Other Asian religions:
### JAIPC - Percentage of Jains
### SHNPC - Percentage of Shintoists
### CNFPC - Percentage of Confucianists
### TAOPC - Percentage of Taoists
### CHFPC - Percentage of Chinese Folk Religionists

## NREPC - Percentage of Not Religious
## UNKPC - Percentage of Unknown

## Other religions not included in the analysis:
### JEWPC - Percentage of Jews
### MANPC - Percentage of Mandaeans
### ZORPC - Percentage of Zoroastrians
### BAHPC - Percentage of Bahais
### SIKPC - Percentage of Sikhs
### INDPC - Percentage of Indigenous Religionists (Ethnoreligionists)
### NEWPC - Percentage of New Age Religionists
### OREPC - Percentage of Other Religionists

# Reading data
rcs_data <- read.spss("https://osf.io/xtvf5/download", use.value.labels = T, to.data.frame = T) %>%
  # Change country code for Yugoslavia, as Serbia
  mutate(NUMISO = ifelse(NUMISO == "688", "891", NUMISO))

# Subset WVS/EVS countries and the most recent available 2015 year and select variables
rel_compos_WVS_EVS <- rcs_data  %>% 
  rename(code = NUMISO, year = YEAR) %>%
  filter(code %in% WVS7_EVS5$code, year == 2015) %>%
  select(code, year, CHRPC, CSYNPC, MUSPC, MSYNPC, 
         HINPC, BUDPC, BSYNPC, SHNPC, CNFPC, TAOPC, CHFPC, JAIPC,
         NREPC, UNKPC, 
         JEWPC, MANPC, ZORPC, BAHPC, SIKPC, INDPC, NEWPC, OREPC)

# Subset WVS 6 countries and the year corresponding to the data collection and select variables
rel_compos_WVS6 <- rcs_data %>% 
  rename(code = NUMISO, year = YEAR) %>%
  merge(rel_data_WVS6[, c("code", "year")], by = c("code", "year")) %>%
  distinct(code, .keep_all = TRUE) %>%
  select(code, year, CHRPC, CSYNPC, MUSPC, MSYNPC, 
         HINPC, BUDPC, BSYNPC, SHNPC, CNFPC, TAOPC, CHFPC, JAIPC,
         NREPC, UNKPC, 
         JEWPC, MANPC, ZORPC, BAHPC, SIKPC, INDPC, NEWPC, OREPC)
# no Haiti 332???

rel_compos <- rbind(rel_compos_WVS6, rel_compos_WVS_EVS)
rm(rel_compos_WVS6, rel_compos_WVS_EVS)

# Remove countries with empty response options of categorical indicators or omitted questions of religiosity
rel_compos <- subset(rel_compos, subset = rel_compos$code %in% rel_data$code)
# 82 countries. The Northern Ireland will be added later; Germany will be divided latter

# ----------------------------

# Calculate final country-level predictors
# rel_compos <- rel_compos %>%
#   mutate(RCCHR = rowSums(select(., CHRPC, CSYNPC), na.rm = TRUE), # Christians
#          RCASIAN = rowSums(select(., SHNPC, CNFPC, TAOPC, CHFPC, JAIPC, 
#                                   HINPC, BUDPC, BSYNPC), na.rm = TRUE), # Asian Religions
#          RCMUSLIM = rowSums(select(., MUSPC, MSYNPC), na.rm = TRUE), # Muslims
#          RCOTHER = rowSums(select(., NREPC, UNKPC,
#                                     JEWPC, MANPC, ZORPC, BAHPC, SIKPC, INDPC, NEWPC, OREPC), 
#                              na.rm = TRUE) # All other categories
#   )  %>%
#   mutate(RCABR = rowSums(select(., RCCHR, RCMUSLIM), na.rm = TRUE) # Abrahamic religions
#          )

## RCOTHER without NREPC
rel_compos <- rel_compos %>%
  mutate(RCCHR = rowSums(select(., CHRPC, CSYNPC), na.rm = TRUE), # Christians
         RCASIAN = rowSums(select(., SHNPC, CNFPC, TAOPC, CHFPC, JAIPC, 
                                  HINPC, BUDPC, BSYNPC), na.rm = TRUE), # Asian Religions
         RCMUSLIM = rowSums(select(., MUSPC, MSYNPC), na.rm = TRUE), # Muslims
         RCOTHER = rowSums(select(., UNKPC,
                                  JEWPC, MANPC, ZORPC, BAHPC, SIKPC, INDPC, NEWPC, OREPC), 
                           na.rm = TRUE) # All other categories except NREPC
  )  %>%
  mutate(RCABR = rowSums(select(., RCCHR, RCMUSLIM), na.rm = TRUE) # Abrahamic religions
  ) %>%
  rename(RCNREL = NREPC)


# Add country names and Northern Ireland with two samples for Germany
rel_compos <- rel_compos %>% 
  merge(rel_data[, c("code", "country")], by = c("code")) %>%
  distinct(country, .keep_all = TRUE)

# Calculate the total percentage to see the discrepancy in estimates
## rel_compos$SUM <- rowSums(rel_compos[,c("RCABR", "RCASIAN", "RCOTHER", "RCNREL")], na.rm = T)
## df_to_viewer(rel_compos[, c("country", "SUM")], rownames = F)

# ----------

# Adjust the variables in Maldives and Hong Kong:
# RCOTHER is negative
## Hong Kong: OREPC = -0.30437088
## Maldives: OREPC = -0.07376294
## -> subtract proportionally from all variables 
rel_compos <- rel_compos %>%
  mutate(DIF = if_else(country %in% c("Hong Kong", "Maldives"), 
                       RCOTHER/3, 0)) %>%
  mutate_at(vars(RCABR, RCASIAN, RCNREL), 
            ~ if_else(country %in% c("Hong Kong", "Maldives"), . + DIF, .)) %>%
  mutate(RCOTHER = if_else(country %in% c("Hong Kong", "Maldives"), 0, RCOTHER))


# Adjust the variables in South Korea and Netherlands:
## the total percentage of adherents of all religions is a bit > 100%
## -> subtract the extra % from all categories proportionally to the number of these categories
# rel_compos <- rel_compos %>%
#  mutate(SUM = rowSums(.[c("RCABR", "RCASIAN", "RCOTHER")], na.rm = TRUE)) %>%
#  mutate(DIF = if_else(country %in% c("South Korea", "Netherlands"), (SUM - 100)/3, 0)) %>%
#  mutate_at(vars(RCABR, RCASIAN, RCOTHER), ~ if_else(country %in% c("South Korea", "Netherlands"), . - DIF, .))


# Because RCOTHER is too small in the Netherlands, drop it
rel_compos <- rel_compos %>%
  mutate(SUM = rowSums(.[c("RCABR", "RCASIAN", "RCNREL", "RCOTHER")], na.rm = TRUE)) %>%
  mutate(DIF = if_else(country %in% c("South Korea"), (SUM - 100)/4, 0)) %>%
  mutate(DIF1 = if_else(country %in% c("Netherlands"), (SUM - 100)/3, 0)) %>%
  mutate_at(vars(RCABR, RCASIAN, RCNREL, RCOTHER), ~ if_else(country %in% c("South Korea"), . - DIF, .)) %>%
  mutate_at(vars(RCABR, RCASIAN, RCNREL), ~ if_else(country %in% c("Netherlands"), . - DIF1, .))

rel_compos <- trim(rel_compos)
rel_compos <- rel_compos[order(rel_compos$country), ] 

#----------------------------------------------------------------------------------------

# Cultural zones
## ZAFRICA - sub-Saharan Africa
## ZINDIC	- Indic East
## ZLA - Latin America
## ZISLAM - Islamic East
## ZNWEST	- New West
## ZOLDWEST	- Old West
## ZORT	- Orthodox East
## ZREFWEST	- Reformed West
## ZRETWEST	- Returned West
## ZSINIC	- Sinic East

# Read the data
zones <- read_excel("./02_Data/01_InputData/CountryInfoWVS_EVS.xlsx", 
                    sheet = "Predictors") %>%
  select(-c(COMMALL, COMMFORM,	COMMOTHR, TAX)) %>%
  replace(is.na(.), 0)

#----------------------------------------------------------------------------------------

# Communist legacy
## COMMALL - all countries that experienced/experience the communist regime

# Read the data
communism <- read_excel("./02_Data/01_InputData/CountryInfoWVS_EVS.xlsx",
                        sheet = "Predictors") %>%
  select(c(country, COMMALL, COMMFORM,	COMMOTHR)) %>%
  replace(is.na(.), 0)

#-----------------------------------------------------------------------------------------------

# Religious taxes

# Read the data
taxes <- read_excel("./02_Data/01_InputData/CountryInfoWVS_EVS.xlsx",
                    sheet = "Predictors") %>%
  select(c(country, TAX)) %>%
  replace(is.na(.), 0)

#----------------------------------------------------------------------------------------

# Religious regulations

# Religion and State: Round 3
## NXX2010-2014 - Religious Regulation [annually; 1990-2014]
### Regulation of and Restrictions on the Majority Religion or All Religions: INDEX
## LXX2010-2014 - Religious Legislation [annually; 1990-2014]
### Specific Types of Religious Support: INDEX

# Read the data from online repository or download from the InputData folder
rands_data <- read.spss("https://osf.io/mq2kt/download", use.value.labels = T, to.data.frame = T)

# Select variables
rel_regulation <- rands_data %>%
  select(c(COUNTRY, NUMISO,
           NXX2010, NXX2011, NXX2012, NXX2013, NXX2014,
           LXX2010, LXX2011, LXX2012, LXX2013, LXX2014)) %>% 
  
  # Change country code for Yugoslavia, as Serbia
  mutate(NUMISO = ifelse(NUMISO == "688", "891", NUMISO)) %>% 
  
  # Reshape data to the long format
  reshape(direction = "long",
          varying = list(NXX = c("NXX2010", "NXX2011", "NXX2012", "NXX2013", "NXX2014"),
                         LXX = c("LXX2010", "LXX2011", "LXX2012", "LXX2013", "LXX2014")),
          timevar = c("year"),
          times = c("2010", "2011", "2012", "2013", "2014"),
          idvar = "COUNTRY",
          sep = "",
          v.names = c("NXX", "LXX"))

# OR
# Select variables
# rel_regulation <- rands_data %>%
#  select(c(COUNTRY, NUMISO,
#          NXX2010, NXX2011, NXX2012, NXX2013, NXX2014,
#           LXX2010, LXX2011, LXX2012, LXX2013, LXX2014,
#           LX31X2010, LX31X2011, LX31X2012, LX31X2013, LX31X2014)) %>% 
#  
#  # Change country code for Yugoslavia, as Serbia
#  mutate(NUMISO = ifelse(NUMISO == "688", "891", NUMISO)) %>% 
#  
#  # Reshape data to the long format
#  reshape(direction = "long",
#          varying = list(NXX = c("NXX2010", "NXX2011", "NXX2012", "NXX2013", "NXX2014"),
#                         LXX = c("LXX2010", "LXX2011", "LXX2012", "LXX2013", "LXX2014"),
#                         TAX = c("LX31X2010", "LX31X2011", "LX31X2012", "LX31X2013", "LX31X2014")),
#          timevar = c("year"),
#          times = c("2010", "2011", "2012", "2013", "2014"),
#          idvar = "COUNTRY",
#          sep = "",
#          v.names = c("NXX", "LXX", "TAX"))


# Subset WVS/EVS countries and the most recent available 2014 year
rel_regulation_WVS_EVS <- rel_regulation %>%
  filter(NUMISO %in% WVS7_EVS5$code, year == 2014)

# Subset WVS 6 countries and the year corresponding to the data collection year
rel_regulation_WVS6 <- rel_regulation  %>%
  merge(rel_data_WVS6[, c("code", "year")], by.x = c("NUMISO", "year"), 
        by.y = c("code", "year")) %>%
  distinct(COUNTRY, .keep_all = TRUE)

# Set the same order of columns in WVS_EVS as in WVS6
rel_regulation_WVS6 <- rel_regulation_WVS6[colnames(rel_regulation_WVS_EVS)]

# Combine datasets
rel_regulation <- rbind(rel_regulation_WVS6, rel_regulation_WVS_EVS) %>%
  rename(RRI = NXX, RLI = LXX, code = NUMISO) %>%
  merge(rel_data[, c("country", "code")], by = "code") %>%
  distinct(country, .keep_all = TRUE) %>%
  select(-COUNTRY)

rel_regulation <- trim(rel_regulation)
rel_regulation <- rel_regulation[order(rel_regulation$country), ]

rm(rel_regulation_WVS6, rel_regulation_WVS_EVS)


# lx31: Government collects taxes on behalf of religious organizations (religious
## taxes).
# Tax: Austria, Denmark, Finland, Germany East, Germany West, Hungary, Iceland, Italy, 
## Malaysia, Portugal, Slovakia, Slovenia, Spain, Sweden, and Switzerland
# Slovakia and Slovenia - no taxes??

# names(rel_regulation)[5] <- "TAXALL"
# rel_regulation$TAXALL <-ifelse(rel_regulation$TAXALL == "Yes", 1, 0)

#-----------------------------------------------------------------------------------------------

# HDI

# No data fo Taiwan
## retrieved from Directorate General of Budget, Accounting and Statistics, Executive Yuan, Taiwan (ROC)
## https://www.dgbas.gov.tw/public/Data/112116036FDX2D8F3.pdf
## HDI = 0.916

# No data for Macau SAR
## retrieved from Macau Statistics and Census Service https://www.dsec.gov.mo/en-US/
## https://www.dsec.gov.mo/ts/#!/step2/Latest5Indicator/en-US/19001
## HDI = 0.922

# No data for Puerto Rico
## retriewed for 2015 instead of 2018 from 
## Fuentes-Ramirez, R. (2017) Human Development Index Trends and Inequality in Puerto Rico 2010-2015
## HDI = 0.845
## But for merging set the year as 2018

hdi <- read.csv(file = "https://hdr.undp.org/sites/default/files/2021-22_HDR/HDR21-22_Composite_indices_complete_time_series.csv") %>%
  select(country, paste0("hdi_", 2010:2021)) %>%
  reshape(direction = "long",
          varying = c(paste0("hdi_", 2010:2021)),
          v.names = "HDI",
          idvar = "country",
          timevar = "Year",
          times = 2010:2021) %>%
  trim() %>%
  # Set names as in WVS
  mutate(country = case_when(
    country == "Bolivia (Plurinational State of)" ~ "Bolivia",
    country == "Russian Federation" ~ "Russia",
    country == "Palestine, State of" ~ "Palestine",
    country == "Korea (Republic of)" ~ "South Korea",
    country == "Iran (Islamic Republic of)" ~ "Iran",
    country == "Hong Kong, China (SAR)" ~ "Hong Kong",
    country == "Viet Nam" ~ "Vietnam",
    country == "Venezuela (Bolivarian Republic of)" ~ "Venezuela",
    country == "United Kingdom" ~ "Great Britain",
    country == "Germany" ~ "Germany East",
    TRUE ~ country
  )) %>%
  rbind(c("Taiwan", "2019", "0.916"), 
        c("Macau SAR", "2019", "0.922"),
        c("Puerto Rico", "2018", "0.845")) %>%
  # Take the 2021 estimates for countries with 2022 year of data collection
  merge(transform(rel_data[, c("country", "year")], 
                  Merge_Year = ifelse(year == 2022, 2021, year)),
        by.x = c("country", "Year"),
        by.y = c("country", "Merge_Year")) %>%
  mutate(HDI = as.numeric(HDI)) %>%
  unique()  %>%
  select(-Year)

# Duplicate Germany East for Germany West and Great Britain for the Northern Ireland
hdi <- rbind(hdi, hdi[hdi$country %in% c("Germany East", "Great Britain"), ])
hdi$country[83] <- "Germany West"
hdi$country[84] <- "Northern Ireland"

# Rename hdi for furhter merging
names(hdi)[3] <- "Year"


#-----------------------------------------------------------------------------------------------

# Merge imputed data with country-level predictors
mlsem_dat <- mlsem_dat %>% 
  lapply(function(y) Reduce(function(x, z) merge(x, z, by = "country", all.x = FALSE),
                            list(y, rel_compos[, c("country", "RCABR", "RCASIAN", "RCOTHER",
                                                   "RCNREL")], 
                                 communism[, c("country", "COMMALL", "COMMFORM",	"COMMOTHR")], 
                                 taxes[, c("country", "TAX")], 
                                 hdi[, c("country", "HDI")], 
                                 rel_regulation[, c("country", "RRI", "RLI")],
                                 zones[, c("country", "ZAFRICA", "ZLA", "ZINDIC", "ZSINIC",
                                           "ZNWEST", "ZISLAM", "ZORT", "ZOLDWEST", "ZREFWEST", 
                                           "ZRETWEST")])
  )) %>% 
  map(~ mutate(., country = droplevels(country)))

# Checking for multicollinearity
# modelmult <- lm(mlsem_dat[[1]]$imprel ~ 
#                   mlsem_dat[[1]]$RCASIAN + mlsem_dat[[1]]$RCOTHER +  mlsem_dat[[1]]$RCNREL +
#                   mlsem_dat[[1]]$COMMALL + mlsem_dat[[1]]$TAX + 
#                   mlsem_dat[[1]]$RRI + mlsem_dat[[1]]$RLI + 
#                   mlsem_dat[[1]]$HDI) %>%
#   car::vif()
# modelmult
# No milticollinearity


# Save data for MLSEM
for (i in 1:length(mlsem_dat)) {
  prepareMplusData(mlsem_dat[[i]], filename = paste0("mlsem_dat", i, ".dat"))
}

# You can run all models with the following command:
runmodels("", 
          recursive = T)

# You can check the traceplots with the following command:
traceplots_mplus("",
                 is.file = T)

# ===================================================================================================

# APPENDIX

# Table 2. Sample size and survey wave, by country
n_tab <- fct_count(rel_data$country) %>%
  merge(rel_data[, c("country", "survey")], by = 1) %>%
  distinct() %>%
  mutate(survey = ifelse(survey == "WVS 6", "WVS 6", 
                         ifelse(survey == "EVS", "EVS 5", "WVS 7")))

colnames(n_tab) <- c("Country", "N", "Wave")
df_to_viewer(n_tab, rownames = F)

# ----------------------------


# Table ... Percentage of missing observations for each religiosity indicator, by country

mis_tab <- rel_data %>% 
  mutate(across(c(imprel, confidence, belong, attend, pray, person), as.numeric)) %>% 
  group_by(country) %>% 
  
  # Means for each variable in each country
  summarize(across(c(imprel, confidence, belong, attend, pray, person), 
                    ~ mean(is.na(.))*100)) %>%
  
  # Means across variables in each country
  mutate(Mean = rowMeans(.[, 2:7], na.rm = TRUE)) %>% 
  trim() %>% 
  arrange(country) %>% 
  bind_rows(summarize_all(., ~ if(is.numeric(.)) mean(., na.rm = TRUE) else "Mean")) %>% 
  
  # Means across countries for each variable
  mutate(across(c(imprel, confidence, belong, attend, pray, person, Mean), 
                ~ifelse(is.na(.), "--", round(., 0)))) %>% 
  rename(`Importance of religion` = imprel, 
         `Confidence in institutions` = confidence,
         `Belonging to a denomination` = belong,
         `Frequency of a religious attendance` = attend,
         `Frequency of praying` = pray,
         `Identification as religious person` = person, 
         Country = country)

df_to_viewer(mis_tab, rownames = F)


# ----------------------------

# Table ... Fit measures of the factor model across five imputed datasets, by country
cfa_model_all <- groupwiseCFA.mi.sep(model, data = imp_data, ordered = ord_items, 
                                     estimator = "WLSMV", out = c("fit"))

# ----------------------------

# Table ... Fit measures for configural model across five imputed datasets
df_to_viewer(mgcfa_model, rownames = F, digits = 3)

# ----------------------------

# Table ... Denominational composition, by country (%)
df_to_viewer(
  rel_compos[, c("country", "year", "RCABR", "RCASIAN", "RCOTHER", "RCNREL")], 
  rownames = F, 
  digits = 0)

# ----------------------------

# Table ... Country-level estimates, by country
tab_predict <- Reduce(function(x, y) 
  merge(x, y, all = F), 
  list(zones[, c("country", "Zone")], 
       communism[, c("country", "COMMFORM",	"COMMOTHR")],
       taxes[, c("country", "TAX")],
       hdi[, c("country", "HDI", "Year")],
       rel_regulation[, c("country", "RRI", "RLI", "year")]))

colnames(tab_predict) <- c("Country", "Zone", "Former Communist", "Other Communist", 
                           "Tax", "HDI", "Year", "RRI", "RLI", "Year")

# Recode the name of predictors to full and more intuitive ones
tab_predict$Zone[tab_predict$Zone == "ZAFRICA"] <- "Sub-Saharan Africa"
tab_predict$Zone[tab_predict$Zone == "ZLA"] <- "Latin America"
tab_predict$Zone[tab_predict$Zone == "ZINDIC"] <- "Indic East"
tab_predict$Zone[tab_predict$Zone == "ZSINIC"] <- "Sinic East"
tab_predict$Zone[tab_predict$Zone == "ZISLAM"] <- "Islamic East"
tab_predict$Zone[tab_predict$Zone == "ZNWEST"] <- "New West"
tab_predict$Zone[tab_predict$Zone == "ZOLDWEST"] <- "Old West"
tab_predict$Zone[tab_predict$Zone == "ZRETWEST"] <- "Returned West"
tab_predict$Zone[tab_predict$Zone == "ZREFWEST"] <- "Reformed West"
tab_predict$Zone[tab_predict$Zone == "ZORT"] <- "Orthodox East"

# Set "+" for (ex-)communist countries
tab_predict$`Former Communist` <- ifelse(
  tab_predict$`Former Communist` == 1, "+", 
  ""
)

tab_predict$`Other Communist` <- ifelse(
  tab_predict$`Other Communist` == 1, "+", 
  ""
)

# Set "+" for countries with religious taxes
tab_predict$Tax <- ifelse(
  tab_predict$Tax == 1, "+", 
  ""
)

df_to_viewer(tab_predict, rownames = F)

# ----------------------------

# Table ... Mean (sd) for continuous country-level predictors
cont_tab <- mlsem_dat[[1]][!duplicated(mlsem_dat[[1]]$country), ] %>%
  select(., c("RCABR", "RCASIAN", "RCOTHER", "RCNREL", "RRI", "RLI", "HDI")) %>%
  sapply(., function(x)
    paste0(
      round(
        mean(x, na.rm = T), 2), " (", round(
          sd(x, na.rm = T), 2), ")"
    )) %>%
  as.data.frame()

cont_tab <- as.data.frame(cont_tab)
rownames(cont_tab) <- c("Followers of Abrahamic religions", 
                        "Followers of Asian religions", "Others", 
                        "Non-affiliated",
                         "RRI", "RLI", "HDI")
cont_tab <- cbind(rownames(cont_tab), cont_tab)

colnames(cont_tab) <- c("Predictor", "Mean (SD)")

kable(cont_tab, row.names = FALSE) %>%
  group_rows("Religious composition", 1, 4) %>%
  group_rows("Regulation of religion", 5, 6) %>%
  group_rows("HDI", 6, 6) %>%
  footnote(
    general = "Followers of Abrahamic religions = the sum percentage of Christians and Muslims;
    Others = the sum percentage of individuals with unknown classification, 
    and the followers of all 'other' religions: Jews, Mandaeans, Zoroastrians, Bahais, Sikhs, 
    indigenous religionists (Ethnoreligionists), New Age religionists, and other religionists.
    The descriptive statistics were computed for the sample of 50 countries."
  )

# ----------------------------

# Table ... Number (percentage) of countries for binary country-level predictors
bin_tab <- mlsem_dat[[1]][!duplicated(mlsem_dat[[1]]$country), ] %>%
  select(., c("ZAFRICA", "ZLA", "ZINDIC", "ZSINIC", "ZISLAM", "ZNWEST", 
              "ZORT", "ZOLDWEST", "ZRETWEST", "ZREFWEST",
              "COMMFORM",	"COMMOTHR", "TAX")) %>%
  sapply(., function(x)
    paste0(length(which(x==1)), " (", 
           round(
             length(which(x==1))/length(x)*100, 0), 
           "%)"))

bin_tab <- as.data.frame(bin_tab)

rownames(bin_tab) <- c("Sub-Saharan Africa", "Latin America", "Indic East",
                       "Sinic East", "Islamic East", "New West", 
                       "Orthodox East", "Old West", "Returned West", "Reformed West",
                       "Communist", "Taxes")
bin_tab <- cbind(rownames(bin_tab), bin_tab)

colnames(bin_tab) <- c("Predictor", "Number (share)")

kable(bin_tab, row.names = FALSE) %>%
  footnote(
    general = "Taxes = countries with mandatory religious taxes. 
    The descriptive statistics were computed for the sample of 50 countries."
  )


# ----------------------------

# Table ... Correlations of country-level predictors

# Correlations of continuous indicators - Pearson
cont_tab_cor <-  mlsem_dat[[1]] %>%
  distinct(country, .keep_all = TRUE) %>%
  select(., c("RCABR", "RCASIAN", "RCOTHER", "RRI", "RLI", "HDI")) %>%
  cor(., method = "pearson", use = "pairwise.complete.obs") %>%
  sna::diag.remove(., remove.val = NA) # Delete invalid pairs

cont_tab_cor <- as.data.frame(as.table(cont_tab_cor)) %>%
  na.omit(.) %>% # Delete NAs
  distinct(Freq, .keep_all = TRUE) %>% # Delete duplicates
  mutate(Freq = round(Freq, 2)) %>%
  rename(Indicator1 = Var1, Indicator2 = Var2, Correlation = Freq)
  

# Correlations of continuous indicators with Communism - Spearman
bin_tab_cor <-  mlsem_dat[[1]] %>%
  distinct(country, .keep_all = TRUE) %>%
  select(., c("COMMFORM",	"COMMOTHR",
              "RCABR", "RCASIAN", "RCOTHER", "RRI", "RLI", "HDI"))

bin_tab_cor <- cor(bin_tab_cor[, 1:2], bin_tab_cor[, -1], 
                   method = "spearman", use = "pairwise.complete.obs")

bin_tab_cor <-  as.data.frame(as.table(bin_tab_cor)) %>%
  na.omit(.) %>% # Delete NAs
  distinct(Freq, .keep_all = TRUE) %>% # Delete duplicates
  filter(Freq != 1) %>%
  mutate(Freq = round(Freq, 2)) %>%
  rename(Indicator1 = Var1, Indicator2 = Var2, Correlation = Freq)

corr_tab <- rbind(cont_tab_cor, bin_tab_cor)
corr_tab <- trim(corr_tab)

corr_tab <- corr_tab %>%
  mutate(Indicator1 = fct_recode(Indicator1, "Former Communist" = "COMMFORM",
                                 "Other Communist" = "COMMOTHR",
                                 "Followers of Abrahamic religions" = "RCABR",
                                 "Followers of Asian religions" = "RCASIAN",
                                 "Others" = "RCOTHER")) %>%
  mutate(Indicator2 = fct_recode(Indicator2, "Other Communist" = "COMMOTHR",
                                 "Followers of Abrahamic religions" = "RCABR",
                                 "Followers of Asian religions" = "RCASIAN",
                                 "Others" = "RCOTHER"))

kable(corr_tab, row.names = FALSE) %>%
  footnote(
    general = "Followers of Abrahamic religions = the sum percentage of Christians and Muslims; 
    Others = the sum percentage of not religious, individuals with unknown classification, and 
    the followers of all “other” religions: Jews, Mandaeans, Zoroastrians, Bahais, Sikhs, 
    indigenous religionists (Ethnoreligionists), New Age religionists, and other religionists.
    Correlations were computed for the sample of 50 countries. 
    Pearson correlations were calculated for all pairs of predictors except those with 
    Communist groups, for which Spearman correlations were used."
  )

rm(cont_tab_cor, bin_tab_cor)

# ----------------------------
# Table ... Means of parameters in normal priors for Bayesian MLSEM

# Retrieve priors for thresholds of the two categorical indicators
priors <- readModels(
  target = "/Users/alisa/Desktop/Research/Religiosity all/WVS7/Belonging/BelongNI/01_Scripts/02_AnalysisScripts/Mplus/01_Priors/02_MLR_StV", 
  recursive = TRUE, what = "parameters"
) %>%
  
  # Select estimates only for thresholds
  map(~ {
    x <- .x$parameters$unstandardized
    x <- subset(x, param %in% 
                  c("IMPREL$1", "IMPREL$2", "IMPREL$3", "CONFIDEN$1", "CONFIDEN$2", "CONFIDEN$3"), 
                select = c("param", "est"))
    as.data.frame(x)
  }) %>%
  setNames(1:length(.)) %>%
  
  # Merge datasets to a single table
  reduce(function(x, y) merge(x, y, by = "param", all = TRUE)) %>%
  setNames(c("Parameter", "Data 1", "Data 2", "Data 3", "Data 4", "Data 5")) %>%
  mutate(Parameter = fct_recode(as.factor(Parameter),
                            'Confidence in institutions - t1' = 'CONFIDEN$1',
                            'Confidence in institutions - t2' = 'CONFIDEN$2',
                            'Confidence in institutions - t3' = 'CONFIDEN$3',
                            'Importance of religion - t1' = 'IMPREL$1',
                            'Importance of religion - t2' = 'IMPREL$2',
                            'Importance of religion - t3' = 'IMPREL$3'))

df_to_viewer(priors, rownames = F, digits = 3)
