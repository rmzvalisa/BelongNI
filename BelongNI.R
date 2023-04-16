
# DATA PREPARATION

library(car)
library(foreign)
library(haven)
library(dplyr)
library(mice)
library(LittleHelpers)
library(MplusAutomation)
library(readxl)
library(gdata)
library(kableExtra) 
library(sna)

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
codes <- read_excel("/Users/alisa/Desktop/Research/Religiosity all/WVS7/Belonging/CountryInfoWVS_EVS.xlsx", 
                    sheet = "Country codes")

# Read data 
WVS7_EVS5 <- read.spss("/Users/alisa/Desktop/Research/Data/EVS_WVS_Joint_Spss_v4_0.sav", 
                       use.value.labels = T, to.data.frame = T, use.missings = T)

# Set the same country names in WVS7 as in WVS6:
levels(WVS7_EVS5$cntry)[levels(WVS7_EVS5$cntry)=="Taiwan ROC"] <- "Taiwan"
levels(WVS7_EVS5$cntry)[levels(WVS7_EVS5$cntry)=="Hong Kong SAR"] <- "Hong Kong"

# Merge with country codes
WVS7_EVS5 <- merge(WVS7_EVS5, codes, by.x = c("cntry"), by.y = c("country"),  all.x = T)

# Select only necessary variables
rel_data <- select(WVS7_EVS5, c(cntry, reg_iso, study, year, 
                           A006, A065, E069_01, F025, F028, 
                           F028B_WVS7, F066_EVS5, F034, F050, F053, F063,
                           X001, X003R, X047_WVS7, X047E_EVS5, X025A_01, code))

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
colnames(rel_data) <- c("country", "region", "survey", "year", 
                        "imprel", "member", "confidence", "belong", "attend",
                        "pray_WVS", "pray_EVS", "person", "bgod", "bhell", "impgod", 
                        "gender", "age", "income_WVS", "income_EVS", "education", "code")

# For 10 countries that participated in both EVS and WVS, select WVS observations
rel_data <- subset(rel_data, 
              subset = !((rel_data$country %in% 
                           c("Armenia", "Czechia", "Germany", "Great Britain", "Netherlands", 
                             "Romania", "Russia", "Serbia", "Slovakia", "Ukraine")) & 
                           rel_data$survey == "EVS"))

# Make two separate samples for Germany: East and West
Germany <- rel_data[rel_data$country == "Germany", ]
Germany$region <- droplevels(Germany$region)
Germany$country <- droplevels(Germany$country)

# Recode and drop Berlin
Germany$country <- Recode(
  Germany$region,
  recodes = "'DE-MV Mecklenburg-Western Pomerania' = 'Germany East';
  'DE-BB Brandenburg' = 'Germany East';
  'DE-SN Saxony' = 'Germany East';
  'DE-ST Saxony-Anhalt' = 'Germany East';
  'DE-TH Thuringia' = 'Germany East';
  'DE-BE Berlin' = NA;
  else = 'Germany West'",
  as.factor = T)

Germany <- Germany[!is.na(Germany$country), ]

rel_data <- rbind(rel_data, Germany)
rel_data <- subset(
  rel_data, subset = !(rel_data$country == "Germany"))
rel_data$region <- NULL
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

WVS6 <- read.spss("/Users/alisa/Desktop/Research/Data/WV6_Data_Spss_v20180912.sav",
                  use.value.labels = T, to.data.frame = T, use.missings = T)

# Read the survey year
WVS6_year <- read_excel("/Users/alisa/Desktop/Research/Religiosity all/WVS7/Belonging/CountryInfoWVS_EVS.xlsx", 
                        sheet = "Survey year")

# Merge with country codes and survey year
WVS6 <- merge(WVS6, WVS6_year, by.x = c("V2"), by.y = c("country"), all.x = T)
WVS6 <- merge(WVS6, codes, by.x = c("V2"), by.y = c("country"),  all.x = T)

# Select only necessary variables
rel_data_WVS6 <- select(WVS6, c(V2, V9, V25, V108, V144:V149, V152, 
                                V240, V242, V239, V248, year, code))

colnames(rel_data_WVS6) <- c("country", "imprel", "member", "confidence", "belong", "attend",
                             "pray", "person", "bgod", "bhell", "impgod", 
                             "gender", "age", "income", "education", "year", "code")

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
for (item in c("income_WVS", "income_EVS")) {
  rel_data[, item] <- as.numeric(rel_data[, item])
}

## 2. Combine two variables into new variable
rel_data$income <- ifelse(rel_data$survey == "WVS", rel_data$income_WVS, rel_data$income_EVS)

## 3. Drop survey-specific variables
rel_data[, c("income_EVS", "income_WVS")] <- NULL


# ATTENDANCE. Recoding
## 1. Exclude "Other specific holy days" category from (probably a typo) - 0 observations
rel_data$attend <- droplevels(rel_data$attend)

## 2. Recode the "Only on special holy days/Christmas/Easter days" category as in WVS6
levels(rel_data$attend)[
  levels(rel_data$attend) == "Only on special holy days/Christmas/Easter days"] <- 
  "Only on special holy days"


# PERSON. Recoding
## Recode the "A convinced atheist" category as in WVS6
levels(rel_data$person)[
  levels(rel_data$person) == "A convinced atheist"] <- 
  "An atheist"


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

## Otherwise it is not clear what to do with WVS 7 "Only when attending religious services"

rel_data$pray_WVS <- Recode(rel_data$pray_WVS, 
                            recodes = "'Several times a day' = 'Once or several times a day';
                            'Once a day' = 'Once or several times a day';
                            'Several times each week' = 'Once or several times a month';
                            'Only when attending religious services' = 'Once or several times a month';
                            'Only on special holy days' = 'Several times a year, only on special holidays';
                            'Once a year' = 'Once a year or less often';
                            'Less often' = 'Once a year or less often';
                            'Never, practically never' = 'Never, practically never';
                            else = NA",
                            as.factor = T,
                            levels = c("Once or several times a day",
                                       "Once or several times a month",
                                       "Several times a year, only on special holidays",
                                       "Once a year or less often",
                                       "Never, practically never"))

rel_data$pray_EVS <- Recode(rel_data$pray_EVS, 
                            recodes = "'Every day' = 'Once or several times a day';
                            'More than once week' = 'Once or several times a month';
                            'Once a week' = 'Once or several times a month';
                            'At least once a month' = 'Once or several times a month';
                            'Several times a year' = 'Several times a year, only on special holidays';
                            'Less often' = 'Once a year or less often';
                            'Never' = 'Never, practically never';
                            else = NA", as.factor = T,
                            levels = c("Once or several times a day",
                                       "Once or several times a month",
                                       "Several times a year, only on special holidays",
                                       "Once a year or less often",
                                       "Never, practically never"))


## 2. Combine two variables into new variable 
rel_data$pray <- ifelse(rel_data$survey == "WVS", rel_data$pray_WVS, rel_data$pray_EVS)

## 3. Drop survey-specific variables
rel_data[, c("pray_EVS", "pray_WVS")] <- NULL

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

# WVS_EVS
levels(rel_data$education)[levels(rel_data$education)=="Upper secondary"|
                             levels(rel_data$education) =="Post-secondary non tertiary"|
                             levels(rel_data$education) =="Short-cycle tertiary"] <- 
  "Upper secondary / tertiary without degree"
levels(rel_data$education)[levels(rel_data$education)=="Bachelor or equivalent"|
                             levels(rel_data$education) =="Master or equivalent"|
                             levels(rel_data$education) =="Doctoral or equivalent"] <- 
  "University - level education, with degree"
rel_data$education <- droplevels(rel_data$education)

# WVS 6
levels(rel_data_WVS6$education)[levels(rel_data_WVS6$education)=="No formal education"|
                                  levels(rel_data_WVS6$education) =="Incomplete primary school"] <- 
  "Less than primary"
levels(rel_data_WVS6$education)[levels(rel_data_WVS6$education)=="Complete primary school"] <- "Primary"
levels(rel_data_WVS6$education)[levels(rel_data_WVS6$education)=="Incomplete secondary school: technical/ vocational type"|
                                  levels(rel_data_WVS6$education) =="Incomplete secondary school: university-preparatory type"] <- 
  "Lower secondary"
levels(rel_data_WVS6$education)[levels(rel_data_WVS6$education)=="Complete secondary school: technical/ vocational type"|
                                  levels(rel_data_WVS6$education) =="Complete secondary school: university-preparatory type"|
                                  levels(rel_data_WVS6$education) =="Some university-level education, without degree"] <- 
  "Upper secondary / tertiary without degree"


# MEMBER. Recoding
## Make the same options as in WVS_EVS
levels(rel_data_WVS6$member)[levels(rel_data_WVS6$member) == "Not a member"] <- "Not mentioned"
levels(rel_data_WVS6$member)[levels(rel_data_WVS6$member) == "Inactive member"|
                             levels(rel_data_WVS6$member) == "Active member"] <- 
  "Mentioned"


# PRAYING. Recoding
## Make the same options as in WVS_EVS
rel_data_WVS6$pray <- Recode(rel_data_WVS6$pray, 
                            recodes = "'Several times a day' = 'Once or several times a day';
                            'Once a day' = 'Once or several times a day';
                            'Several times each week' = 'Once or several times a month';
                            'Only when attending religious services' = 'Once or several times a month';
                            'Only on special holy days' = 'Several times a year, only on special holidays';
                            'Once a year' = 'Once a year or less often';
                            'Less often than once a year' = 'Once a year or less often';
                            'Never, practically never' = 'Never, practically never';
                            else = NA",
                            as.factor = T,
                            levels = c("Once or several times a day",
                                       "Once or several times a month",
                                       "Several times a year, only on special holidays",
                                       "Once a year or less often",
                                       "Never, practically never"))

rel_data_WVS6$pray <- as.numeric(rel_data_WVS6$pray)


# BELONGING. Recoding
## Recode into numeric for each survey because of the different coding of levels
rel_data$belong <- as.numeric(rel_data$belong)
rel_data_WVS6$belong <- as.numeric(rel_data_WVS6$belong)


# AGE. Recoding
# Recode into numeric (survey year is already numeric) and collapse into categories
rel_data_WVS6$age <- as.numeric(as.character(rel_data_WVS6$age))
rel_data_WVS6$age <- Recode(rel_data_WVS6$age, 
                            recodes = "16:24 = 1;
                            25:34 = 2; 35:44 = 3; 45:54 = 4; 55:64 = 5; 65:102 = 6; else = NA", 
                            as.factor = F)


# BGOD AND BHELL. Recoding
# Reverse code
for (item in c("bgod", "bhell")) {
  rel_data_WVS6[, item] <- ordered(rel_data_WVS6[, item],
                                  levels = c("No", "Yes"))
}


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

# Recode the remaining variables into numeric
for (item in c("imprel", "confidence", "attend", "person", "impgod", 
               "education")) {
  rel_data[, item] <- as.numeric(rel_data[, item])
}

# Reverse recode indicators
for (item in c("imprel", "confidence")) {
  rel_data[,item] <- Recode(rel_data[,item], rec= "1=4; 2=3; 3=2; 4=1; else=NA")
}

rel_data$attend <- Recode(rel_data$attend, rec = "1=7; 2=6; 3=5; 4=4; 5=3; 6=2; 7=1; else=NA")
rel_data$pray <- Recode(rel_data$pray, rec = "1=5; 2=4; 3=3; 4=2; 5=1; else=NA")

# Make person binary
rel_data$person <- Recode(rel_data$person, rec = "1=2; 2=1; 3=1; else=NA")

# Make belonging binary
rel_data$belong <- ifelse(rel_data$belong == "1", 1, 2)

# impgod, member, bgod, and bhell do not need to be recoded: 
## higher scores already correspond to higher religiosity

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
rel_data <- subset(
  rel_data, subset = !(rel_data$country %in% 
                         c("Bangladesh", "Indonesia", "Iraq", "Jordan", "Lebanon", "Pakistan", "Thailand",
                           "Egypt", "Palestine", "Algeria", "Haiti", "Yemen", "Libya", "Kuwait", "Qatar",
                           "Ethiopia")))
rel_data$country <- droplevels(rel_data$country)
levels(rel_data$country)
# 87 countries

# Also drop Hong Kong, Macau SAR, and Puerto Rico because 
## there are no RRI and RLI country-level measures
rel_data <- subset(
  rel_data, subset = !(rel_data$country %in% c("Hong Kong", "Macau SAR", "Puerto Rico")))

rel_data$country <- droplevels(rel_data$country)
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

# drop Russia, Tunisia, Turkey, Georgia, and Maldives
# the corresponding data will be imputed separately due to the convergence issues
imp_data <- subset(
  rel_data, subset = !(rel_data$country %in% 
                         c("Russia", "Tunisia", "Turkey", "Georgia", "Maldives")))
imp_data$country <- droplevels(imp_data$country)
imp_data <- split(imp_data, imp_data$country)

# There are no missing values in: 
## Rwanda, Ghana, Venezuela, Nicaragua, Morocco, South Korea, Colombia, Canada, and Myanmar
## Drop these countries from the imputation; they will be added later
## lapply(imp_data, function(x)
##  sum(is.na(x[, c("person", "belong", "confidence", "imprel", "attend", "pray")])))

imp_data <- subset(imp_data, subset = !(names(imp_data) %in% 
                                          c("Rwanda", "Ghana", "Venezuela", "Nicaragua", 
                                            "Morocco", "South Korea", "Colombia", "Canada", "Myanmar")))

for (i in 1:length(imp_data)){
  for (item in c("person", "belong", "confidence", "imprel")) {
    imp_data[[i]][, item] <- as.factor(imp_data[[i]][, item])
  } # member, bgod, bhell, and gender are already a factor
    
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

imp_data_RG <- subset(rel_data, rel_data$country %in% c("Russia", "Georgia"))
imp_data_RG$country <- droplevels(imp_data_RG$country)
imp_data_RG$bgod <- NULL
imp_data_RG <- split(imp_data_RG, imp_data_RG$country)

pred_matrix <- make.predictorMatrix(data = imp_data_RG$Russia)
pred_matrix[, c("country", "code", "year", "survey")] <- 0
pred_matrix[c("country", "code", "year", "survey"), ] <- 0

imp_method <- make.method(data = imp_data_RG$Russia)
imp_method[c("person", "belong", "gender", "bhell")] <- "logreg"
imp_method[c("confidence", "member", "imprel")] <- "polr"
imp_method[c("attend", "pray", "impgod", "age", "income", "education")] <- "pmm"

for (i in 1:length(imp_data_RG)){
  for (item in c("person", "belong", "confidence", "imprel")) {
    imp_data_RG[[i]][, item] <- as.factor(imp_data_RG[[i]][, item])
  }
  
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
Tunisia <- rel_data[rel_data$country=="Tunisia", ]
Tunisia$bhell <- NULL
pred_matrix <- make.predictorMatrix(data = Tunisia)
pred_matrix[, c("country", "code", "year", "survey")] <- 0
pred_matrix[c("country", "code", "year", "survey"), ] <- 0

imp_method <- make.method(data = Tunisia)
imp_method[c("person", "belong", "gender", "bgod")] <- "logreg"
imp_method[c("confidence", "member", "imprel")] <- "polr"
imp_method[c("attend", "pray", "impgod", "age", "income", "education")] <- "pmm"

for (item in c("person", "belong", "confidence", "imprel")) {
  Tunisia[, item] <- as.factor(Tunisia[, item])
}

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
Turkey <- rel_data[rel_data$country=="Turkey",]
Turkey$belong <- NULL
pred_matrix <- make.predictorMatrix(data = Turkey)
pred_matrix[, c("country", "code", "year", "survey")] <- 0
pred_matrix[c("country", "code", "year", "survey"), ] <- 0

imp_method <- make.method(data = Turkey)
imp_method[c("person", "gender", "bgod", "bhell")] <- "logreg"
imp_method[c("confidence", "member", "imprel")] <- "polr"
imp_method[c("attend", "pray", "impgod", "age", "income", "education")] <- "pmm"

for (item in c("person", "confidence", "imprel")) {
  Turkey[, item] <- as.factor(Turkey[, item])
}

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

## Impute belong without attendance
Turkey <- rel_data[rel_data$country=="Turkey",]
Turkey$attend <- NULL
pred_matrix <- make.predictorMatrix(data = Turkey)
pred_matrix[, c("country", "code", "year", "survey")] <- 0
pred_matrix[c("country", "code", "year", "survey"), ] <- 0

imp_method <- make.method(data = Turkey)
imp_method[c("person", "belong", "gender", "bgod", "bhell")] <- "logreg"
imp_method[c("confidence", "member", "imprel")] <- "polr"
imp_method[c("pray", "impgod", "age", "income", "education")] <- "pmm"

for (item in c("person", "belong", "confidence", "imprel")) {
  Turkey[, item] <- as.factor(Turkey[, item])
}

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
Maldives <- rel_data[rel_data$country=="Maldives", ]
Maldives$bgod <- NULL
pred_matrix <- make.predictorMatrix(data = Maldives)
pred_matrix[, c("country", "code", "year", "survey")] <- 0
pred_matrix[c("country", "code", "year", "survey"), ] <- 0

imp_method <- make.method(data = Maldives)
imp_method[c("person", "gender", "belong", "bhell")] <- "logreg"
imp_method[c("confidence", "member", "imprel")] <- "polr"
imp_method[c("attend", "pray", "impgod", "age", "income", "education")] <- "pmm"

for (item in c("person", "confidence", "imprel", "belong")) {
  Maldives[, item] <- as.factor(Maldives[, item])
}


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
imp_data_nomis <- subset(rel_data, 
                         subset = (rel_data$country %in%
                                     c("Rwanda", "Ghana", "Venezuela", "Nicaragua", 
                                       "Morocco", "South Korea", "Colombia", "Canada", "Myanmar")))
imp_data_nomis$country <- droplevels(imp_data_nomis$country)
imp_data_nomis <- split(imp_data_nomis, imp_data_nomis$country)

imp_data_nomis <- lapply(imp_data_nomis, function(x)
  rep(list(x), 5))

# Combine datasets
imp_data <- append(imp_data, imp_data_nomis)

# -----------------------

# Final imputed datasets

for (i in 1:length(imp_data)){
  
  for (a in 1:length(imp_data[[i]])){
    # Drop all variables used for imputation only
    # Specify the same order of columns for all datasets
    imp_data[[i]][[a]] <- imp_data[[i]][[a]][, c("country", "imprel", "confidence", "belong", "attend",
                                                 "pray", "person", "year", "code", "survey")]
    
    imp_data[[i]][[a]]$country <- droplevels(imp_data[[i]][[a]]$country)
    
    # Recode factor variables into numeric
    for (item in c("person", "belong", "confidence", "imprel")) {
      imp_data[[i]][[a]][, item] <- as.numeric(imp_data[[i]][[a]][, item])
    }
    
  }
  
}

imp_data <- imp_data[base::order(names(imp_data))]
# 84 countries

# save or download the imputed dataset 
## setwd("/Users/alisa/Desktop/Research/Religiosity all/WVS7/Belonging/BelongNI/02_Data/02_AnalysisData")
## save(imp_data, file = "imp_data_full_WVS_EVS.RData")
## load("imp_data_full_WVS_EVS.RData")

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

lapply(cor_imp, function(x) 
  lapply(x, function(y)
    y == 0))

# There are negative correlations in:
## China, Kenya, Maldives, Myanmar, Nigeria, the Philippines, Rwanda, Tunisia, and Zimbabwe
## additionally drop these countries
imp_data <- subset(imp_data, 
                   subset = !(names(imp_data) %in%
                                c("China", "Kenya", "Maldives", "Myanmar", "Nigeria", "Philippines", "Rwanda",
                                  "Tunisia", "Zimbabwe")))

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
mlsem_dat <- lapply(data_list, function(x) 
  subset(x, subset = x$country %in% rownames(cfa_model)))

for (i in 1:length(mlsem_dat)) {
  mlsem_dat[[i]]$country <- droplevels(mlsem_dat[[i]]$country)
}

# MGCFA to ensure the similarity of the factor structure
mgcfa_model <- globalMI.mi.sep(model, data = mlsem_dat, ordered = ord_items, 
                               estimator = "WLSMV", group = "country")

rm(data1, data2, data3, data4, data5)

#===================================================================================================

# COUNTRY-LEVEL INDICATORS

# Religious composition
# Data: Brown, D., & James, P. Religious Characteristics of States Dataset Project
## Demographics v. 2.0 (RCS-Dem 2.0), COUNTRIES ONLY.

# Reading data
rcs_data <- read.spss("https://osf.io/xtvf5/download", use.value.labels = T, to.data.frame=T)
# WVS6: select observations corresponding to the country-specific year of WVS data collection
# WVS/EVS: select observations for 2015 year as the most recent data

# The following variables are selected:
## NUMISO - Country Code assigned by International Standards Organization

## Christians:
### CATPC - Percentage of Catholics
### XPRTPC - Percentage of Extended Protestants (Combined Protestant, Anglican, Pentecostal)
### ORTPC - Percentage of Orthodox
## UPRTPC - Percentage of unspecified Protestants - to drop it for South Korea
## Other Christians:
### OCHRPC - Percentage of Other Non-Liminal Christians
### CSYNPC - Percentage of Christian Syncretics (Mostly African and New World African spiritist and spiritualist denominations)
### Percentage of Liminal Christians:
#### ULCHPC - Percentage of unspecified Liminal Christians
#### ECCPC - Percentage of Extra-Canonical Christians 
#### LDSPC - Percentage of Latter-Day Saints (Mormons) 
#### OECCPC - Percentage of Other Extra-Canonical Christians 
#### NTCPC - Percentage of Non-Trinitarian Christians 
#### JWTPC - Percentage of Jehovah’s Witnesses 
#### UNIPC - Percentage of Unitarians
#### ONTCPC - Percentage of Other Non-Trinitarian Christians
#### OLCHPC - Percentage of Other Liminal Christians 

## Muslims:
### MUSPC - Percentage of Muslims
### MSYNPC - Percentage of Muslim Syncretics 

## Asian Religions:
## HINPC - Percentage of Hindus
## Buddhists:
### BUDPC - Percentage of Buddhists
### BSYNPC - Percentage of Buddhist Syncretics 
## Other religions:
### East Asian Complex:
#### JAIPC - Percentage of Jains
#### SHNPC - Percentage of Shintoists
#### CNFPC - Percentage of Confucianists
#### TAOPC - Percentage of Taoists
#### CHFPC - Percentage of Chinese Folk Religionists

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

## UCHRPC - Percentage of unspecified Christians
### All known Christians of unknown classification;
### also used when sum of branches departs from best estimate of total Christians; 
### negative number denotes double-affiliates

# Change country code for Yugoslavia, as Serbia
rcs_data$NUMISO[rcs_data$NUMISO =="688"] <- "891"

# Subset WVS/EVS countries and the most recent available 2015 year
rel_compos_WVS_EVS <- subset(rcs_data, subset = ((rcs_data$NUMISO %in% WVS7_EVS5$code) & 
                                                rcs_data$YEAR == 2015))

# subset WVS 6 countries and the year corresponding to the data collection year
rel_compos_WVS6 <- subset(rcs_data, subset = ((rcs_data$NUMISO %in% rel_data_WVS6$code) & 
                                                (rcs_data$YEAR %in% rel_data_WVS6$year))) 

# subset only 12 countries that did not participate in WVS/EVS
rel_compos_WVS6 <- merge(rel_data_WVS6[ ,c("code", "year")], 
                         rel_compos_WVS6, by.x = c("code", "year"), 
                         by.y = c("NUMISO", "YEAR"))
rel_compos_WVS6 <- rel_compos_WVS6[!duplicated(rel_compos_WVS6$code), ]
# no Haiti 332???

# Select variables
rel_compos_WVS_EVS <- select(rel_compos_WVS_EVS, c(NUMISO, YEAR, CATPC, XPRTPC, ORTPC, 
                                             OCHRPC, CSYNPC, UPRTPC, 
                                             ULCHPC, ECCPC, LDSPC, OECCPC, NTCPC, JWTPC, UNIPC, ONTCPC, OLCHPC,
                                             MUSPC, MSYNPC, 
                                             HINPC,  
                                             BUDPC, BSYNPC,
                                             SHNPC, CNFPC, TAOPC, CHFPC, 
                                             NREPC, UNKPC, 
                                             JEWPC, MANPC, ZORPC, BAHPC, JAIPC, SIKPC, INDPC, NEWPC, OREPC,
                                             UCHRPC))  
names(rel_compos_WVS_EVS)[names(rel_compos_WVS_EVS) == "NUMISO"] <- "code"
names(rel_compos_WVS_EVS)[names(rel_compos_WVS_EVS) == "YEAR"] <- "year"

rel_compos_WVS6 <- select(rel_compos_WVS6, c(code, year, CATPC, XPRTPC, ORTPC, 
                                             OCHRPC, CSYNPC, UPRTPC, 
                                             ULCHPC, ECCPC, LDSPC, OECCPC, NTCPC, JWTPC, UNIPC, ONTCPC, OLCHPC,
                                             MUSPC, MSYNPC, 
                                             HINPC,  
                                             BUDPC, BSYNPC,
                                             SHNPC, CNFPC, TAOPC, CHFPC, 
                                             NREPC, UNKPC, 
                                             JEWPC, MANPC, ZORPC, BAHPC, JAIPC, SIKPC, INDPC, NEWPC, OREPC,
                                             UCHRPC)) 

rel_compos <- rbind(rel_compos_WVS6, rel_compos_WVS_EVS)
rm(rel_compos_WVS6, rel_compos_WVS_EVS)

# Remove countries with empty response options of categorical indicators or omitted questions of religiosity
rel_compos <- subset(rel_compos, subset = rel_compos$code %in% rel_data$code)
# 82 countries. The Northern Ireland will be added later; Germany will be divided latter

# ----------------------------

# Calculate final country-level predictors
## Followers of Asian Religions
rel_compos$RCASIAN <- rowSums(
  rel_compos[, c("SHNPC", "CNFPC", "TAOPC", "CHFPC", "JAIPC", ## other Asian religions
                 "HINPC", 
                 "BUDPC", "BSYNPC" ## Buddhists
  )], na.rm = T)


## Christians
## for South Korea add negative UPRTPC to XPRTPC
rel_compos$XPRTPC <- ifelse((rel_compos$code == "410"), 
                            (rel_compos$XPRTP + rel_compos$UPRTPC), 
                            rel_compos$XPRTPC)

rel_compos$RCCHR <- rowSums(
  rel_compos[,c("CATPC", "XPRTPC", "ORTPC", 
                "ULCHPC", "ECCPC", "LDSPC", "OECCPC", "NTCPC", 
                "JWTPC", "UNIPC", "ONTCPC", "OLCHPC", ## Liminal Christians
                "OCHRPC", "CSYNPC" ## other Christians
  )], na.rm = T)

## Muslims
rel_compos$RCMUSLIM <- rowSums(rel_compos[,c("MUSPC", "MSYNPC")], na.rm = T)

## Followers of Abrahamic religions
rel_compos$RCABR <- rowSums(rel_compos[,c("RCCHR", "RCMUSLIM")], na.rm = T)

## Variable with all other categories
rel_compos$RCOTHER <- rowSums(
  rel_compos[,c("NREPC", "UNKPC", 
                "JEWPC", "MANPC", "ZORPC", "BAHPC", "SIKPC",
                "INDPC", "NEWPC", "OREPC" ## other religions
  )], na.rm = T)

# ----------------------------

# Adjust variables

# Add country names
rel_compos <- merge(rel_compos, rel_data[, c(1, 17)], by = c("code"))
rel_compos <- rel_compos[!duplicated(rel_compos$code), ]

# Calculate the total percentage to see the discrepancy in estimates
## rel_compos$SUM <- rowSums(rel_compos[,c("RCABR", "RCASIAN", "RCOTHER")], na.rm = T)
## df_to_viewer(rel_compos[, c("country", "SUM", "UCHRPC")], rownames = F)

# Add percentage of unspecified Christians to Abrahamic religions variable in:
## France, Iceland, India, Malaysia, Mongolia, New Zealand, Rwanda, South Africa, 
## United States
## UCHRPC is positive + less than 100% of religious adherents in total for these countries 
## -> additional observations are needed

## Argentina, Bolivia, Brazil, Cyprus, 
## Guatemala, Japan, Kenya, Mexico, Nicaragua, Nigeria, 
## Philippines, Serbia, Singapore, Vietnam, Zimbabwe, Netherlands, Great Britain
## Spain, Switzerland, Montenegro, Bosnia and Herzegovina, Denmark
## UCHRPC is negative + more than 100% of religious adherents in total 
## -> possibly double-affiliated Christians
rel_compos$RCABR <- ifelse((rel_compos$country == "France"|
                              rel_compos$country == "Iceland"|
                              rel_compos$country == "India"|
                              rel_compos$country == "Malaysia"|
                              rel_compos$country == "Mongolia"|
                              rel_compos$country == "New Zealand"|
                              rel_compos$country == "Rwanda"|
                              rel_compos$country == "South Africa"|
                              rel_compos$country == "United States"|
                              rel_compos$country == "Argentina"|
                              rel_compos$country == "Bolivia"|
                              rel_compos$country == "Brazil"|
                              rel_compos$country == "Cyprus"|
                              rel_compos$country == "Guatemala"|
                              rel_compos$country == "Japan"|
                              rel_compos$country == "Kenya"|
                              rel_compos$country == "Mexico"|
                              rel_compos$country == "Nicaragua"|
                              rel_compos$country == "Nigeria"|
                              rel_compos$country == "Philippines"|
                              rel_compos$country == "Serbia"|
                              rel_compos$country == "Singapore"|
                              rel_compos$country == "Vietnam"|
                              rel_compos$country == "Zimbabwe"|
                              rel_compos$country == "Netherlands"|
                              rel_compos$country == "Great Britain"|
                              rel_compos$country == "Spain"|
                              rel_compos$country == "Switzerland"|
                              rel_compos$country == "Montenegro"|
                              rel_compos$country == "Bosnia and Herzegovina"|
                              rel_compos$country == "Denmark"),
                           rowSums(rel_compos[,c("RCABR", "UCHRPC")], na.rm = T), rel_compos$RCABR)


# If the total percentage of adherents of all religions is >100%, subtract the extra %
## from all categories (if a category is higher than extra %)
## proportionally to the number of variables in total: 
rel_compos[is.na(rel_compos)] <- 0
rel_compos$SUM <- rowSums(
  rel_compos[,c("RCABR", "RCASIAN", "RCOTHER")], na.rm = T)
rel_compos$DIF <- (100 - rel_compos$SUM)

# Number of non-zero elements (as we cannot subtract the difference from religions with zero %)
## It is not possible to subtract from all three categories in, for example, Serbia or Albania
rel_compos$NMISS <- rowSums(
  rel_compos[,c("RCABR", "RCASIAN", "RCOTHER")] != 0)

# Calculate the new difference to further subtraction based on non-zero elements
## because the difference is negative - subtract from 0
rel_compos$DIF1 <- (0 - rel_compos$DIF/rel_compos$NMISS)

# We cannot subtract from cells with values lower than the potential subtraction itself
# -> count the number of cells where we can subtract 
rel_compos$NMISS1 <- rowSums(
  rel_compos[,c("RCABR", "RCASIAN", "RCOTHER")] > rel_compos$DIF1)

# Repeat the procedure - the subtraction value changes 
# -> the number of cells where we can subtract changes as well
rel_compos$DIF1  <- (0 - rel_compos$DIF/rel_compos$NMISS1)
rel_compos$NMISS1 <- rowSums(
  rel_compos[,c("RCABR", "RCASIAN", "RCOTHER")] > rel_compos$DIF1)

# Final differences to subtract
rel_compos$DIF1  <- (0 - rel_compos$DIF/rel_compos$NMISS1)

# Subtract 
for (item in c("RCABR", "RCASIAN", "RCOTHER")) {
  rel_compos[, item] <- ifelse((rel_compos[, item] > rel_compos$DIF1), 
                               (rel_compos[, item] - rel_compos$DIF1), 
                               rel_compos[, item])
}

# If you want to check
## table(rowSums(rel_compos[,c("RCABR", "RCASIAN", "RCOTHER")], na.rm = T))
## all countries have the total percentage = 100

rel_compos <- trim(rel_compos)
rel_compos <- rel_compos[order(rel_compos$country), ] 

# Duplicate Germany for two samples:
rel_compos <- rbind(rel_compos, rel_compos[rel_compos$country == "Germany West", ])
rel_compos[nrow(rel_compos), "country"] <- "Germany East"

# Duplicate the Great Britain for the WVS Northern Ireland
rel_compos <- rbind(rel_compos, rel_compos[rel_compos$country == "Great Britain", ])
rel_compos[nrow(rel_compos), "country"] <- "Northern Ireland"

rel_compos <- rel_compos[order(rel_compos$country), ]

#----------------------------------------------------------------------------------------

# Cultural zones
## ZAFRICA - sub-Saharan Africa
## ZINDIC	- Indic East
## ZLA - Latin America
## ZMUSLIM - Islamic East
## ZNWEST	- New West
## ZOLDWEST	- Old West
## ZORT	- Orthodox East
## ZREFWEST	- Reformed West
## ZRETWEST	- Returned West
## ZSINIC	- Sinic East

# Read the data
zones <- read_excel("/Users/alisa/Desktop/Research/Religiosity all/WVS7/Belonging/BelongNI/02_Data/01_InputData/CountryInfoWVS_EVS.xlsx", 
                    sheet = "Predictors")
zones[, c("COMMALL", "TAX")] <- NULL
zones[is.na(zones)] <- 0

#----------------------------------------------------------------------------------------

# Communist legacy
## COMMALL - all countries that experienced/experience the communist regime

# Read the data
communism <- read_excel("/Users/alisa/Desktop/Research/Religiosity all/WVS7/Belonging/BelongNI/02_Data/01_InputData/CountryInfoWVS_EVS.xlsx",
                        sheet = "Predictors")
communism <- select(communism, c(country, COMMALL)) 
communism[is.na(communism)] <- 0

#----------------------------------------------------------------------------------------

# Religious regulations

# Religion and State: Round 3
## NXX2010-2014 - Religious Regulation [annually; 1990-2014]
### Regulation of and Restrictions on the Majority Religion or All Religions: INDEX
## LXX2010-2014 - Religious Legislation [annually; 1990-2014]
### Specific Types of Religious Support: INDEX

# Read the data from online repository or download from the InputData folder
rands_data <- read.spss("https://osf.io/mq2kt/download", use.value.labels = T, to.data.frame = T)

rel_regulation <- select(rands_data, c(COUNTRY, NUMISO,
                                       NXX2010, NXX2011, NXX2012, NXX2013, NXX2014,
                                       LXX2010, LXX2011, LXX2012, LXX2013, LXX2014))

# Change country code for Yugoslavia, as Serbia
rel_regulation$NUMISO[rel_regulation$NUMISO =="688"] <- "891"

rel_regulation <- merge(
  rel_regulation, rel_data[, c(1,17)], 
  by.x = c("NUMISO"), by.y = c("code"))
rel_regulation <- rel_regulation[!duplicated(rel_regulation$country), ]

# Reshape data to the long format
rel_regulation <- reshape(rel_regulation, direction = "long",
                          varying = list(NXX = c("NXX2010", "NXX2011", "NXX2012", "NXX2013", "NXX2014"),
                                         LXX = c("LXX2010", "LXX2011", "LXX2012", "LXX2013", "LXX2014")),
                          timevar = c("year"),
                          times = c("2010", "2011", "2012", "2013", "2014"),
                          idvar = "country",
                          sep = "",
                          v.names = c("NXX", "LXX"))

# Subset WVS/EVS countries and the most recent available 2014 year
rel_regulation_WVS_EVS <- subset(rel_regulation, subset = ((rel_regulation$NUMISO %in% WVS7_EVS5$code) & 
                                                          rel_regulation$year == 2014))

# Select WVS 6 countries and the year corresponding to the data collection year
rel_regulation_WVS6 <- subset(rel_regulation, subset = ((rel_regulation$NUMISO %in% rel_data_WVS6$code) & 
                                                          (rel_regulation$year %in% rel_data_WVS6$year))) 

rel_regulation_WVS6 <- merge(rel_data_WVS6[,c("code", "year")], rel_regulation_WVS6, by.x = c("code", "year"), 
                             by.y = c("NUMISO", "year"))
rel_regulation_WVS6 <- rel_regulation_WVS6[!duplicated(rel_regulation_WVS6$code), ]

rel_regulation_WVS_EVS$COUNTRY <- NULL
rel_regulation_WVS6$COUNTRY <- NULL

colnames(rel_regulation_WVS_EVS)[1] <- "code"

# Combine datasets
rel_regulation <- rbind(rel_regulation_WVS6, rel_regulation_WVS_EVS)

# Set column names:
## RRI - Religious Regulation Index
## RLI - Religious Legislation Index
names(rel_regulation)[names(rel_regulation) == "NXX"] <- 'RRI'
names(rel_regulation)[names(rel_regulation) == "LXX"] <- 'RLI'

rel_regulation <- trim(rel_regulation)
rel_regulation <- rel_regulation[order(rel_regulation$country), ]

rm(rel_regulation_WVS6, rel_regulation_WVS_EVS)

#-----------------------------------------------------------------------------------------------

# Religious taxes

taxes <- read_excel("/Users/alisa/Desktop/Research/Religiosity all/WVS7/Belonging/BelongNI/02_Data/01_InputData/CountryInfoWVS_EVS.xlsx",
                     sheet = "Predictors")
taxes <- select(taxes, c(country, TAX)) 
taxes[is.na(taxes)] <- 0

#-----------------------------------------------------------------------------------------------

# Merge imputed data with country-level predictors
mlsem_dat <- lapply(mlsem_dat, function(y) 
  Reduce(function(x, z) merge(x, z, by = c("country"), all.x = F),
         list(y, rel_compos[, c("country", "RCABR", "RCASIAN", "RCOTHER")], 
              communism[, c("country", "COMMALL")], 
              taxes[, c("country", "TAX")], 
              rel_regulation[, c("country", "RRI", "RLI")],
              zones[, c("country", "ZAFRICA", "ZLA", "ZINDIC", "ZSINIC",
                        "ZNWEST", "ZISLAM", "ZORT", "ZOLDWEST", "ZREFWEST", "ZRETWEST")])))

for (i in 1:length(mlsem_dat)) {
  mlsem_dat[[i]]$country <- droplevels(mlsem_dat[[i]]$country)  
}

# ===================================================================================================

# APPENDIX

# Table 2. Sample size and survey wave, by country
n_tab <- data.frame(table(rel_data$country))
n_tab <- merge(n_tab, rel_data[, c("country", "survey")], 
               by.y = "country", by.x = "Var1")
colnames(n_tab) <- c("Country", "N", "Wave")
n_tab <- n_tab[!duplicated(n_tab$Country), ]

n_tab$Wave <- ifelse(n_tab$Wave == "WVS 6", "WVS 6", 
                     ifelse(n_tab$Wave == "EVS", "EVS 5", "WVS 7"))

df_to_viewer(n_tab, rownames = F)

# ----------------------------

# Table ... Percentage of missing observations for each religiosity indicator, by country
mis_tab <- aggregate(.~country,
                     rel_data[, c("country", "imprel", "confidence", "belong", "attend",
                                  "pray", "person")], 
                     FUN = function(x) 
                       mean(is.na(x))*100, na.action=NULL)

# For countries with omitted questions
mis_tab[mis_tab==100] <- NA

# Calculate means for countries
mis_tab$Mean <- apply(mis_tab[, 2:7], 1, function(x)
  mean(x, na.rm = T))

# Calculate means for variables
mis_tab[nrow(mis_tab) + 1, ] <- NA
mis_tab[nrow(mis_tab), 2:8] <- apply(mis_tab[, 2:8], 2, function(x)
  mean(x, na.rm = T))

mis_tab[, 2:8] <- round(mis_tab[, 2:8], 0)

mis_tab <- trim(mis_tab)
mis_tab <- mis_tab[order(mis_tab$country), ]
mis_tab[, 2:8][is.na(mis_tab[, 2:8])] <- "--"

colnames(mis_tab) <- c("Country", "Importance of religion", 
                       "Confidence in institutions", "Belonging to a denomination",
                       "Frequency of a religious attendance", "Frequency of praying",
                       "Identification as religious person", "Mean")
df_to_viewer(mis_tab, rownames = F)

# ----------------------------

# Table ... Fit measures of the factor model across five imputed datasets, by country
cfa_model_all <- groupwiseCFA.mi.sep(model, data = imp_data, ordered = ord_items, 
                                     estimator = "WLSMV", out = c("fit"))

## The model did not converge for Ukraine on the second dataset
## probably because correlation between variables belong and person is (nearly) 1.0
## Moreover, there are negqative variances on the fourth dataset

# ----------------------------

# Table ... Fit measures for configural model across five imputed datasets
df_to_viewer(mgcfa_model, rownames = F, digits = 3)

# ----------------------------

# Table ... Denominational composition, by country (%)
df_to_viewer(
  rel_compos[, c("country", "year", "RCABR", "RCASIAN", "RCOTHER")], 
  rownames = F, 
  digits = 0)

# ----------------------------

# Table ... Country-level estimates, by country
tab_predict <- Reduce(function(x, y) 
  merge(x, y, all = F), 
  list(zones[, c("country", "Zone")], 
       communism[, c("country", "COMMALL")],
       taxes[, c("country", "TAX")],
       rel_regulation[, c("country", "RRI", "RLI", "year")]))

colnames(tab_predict) <- c("Country", "Zone", "Communist", "Tax", "RRI", "RLI", "Year")

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
tab_predict$Communist <- ifelse(
  tab_predict$Communist == 1, "+", 
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
  select(., c("RCABR", "RCASIAN", "RCOTHER", "RRI", "RLI")) %>%
  sapply(., function(x)
    paste0(
      round(
        mean(x, na.rm = T), 2), " (", round(
          sd(x, na.rm = T), 2), ")"
    ))

cont_tab <- as.data.frame(cont_tab)
rownames(cont_tab) <- c("Followers of Abrahamic religions", 
                        "Followers of Asian religions", "Others", 
                        "RRI", "RLI")
cont_tab <- cbind(rownames(cont_tab), cont_tab)

colnames(cont_tab) <- c("Predictor", "Mean (SD)")

kable(cont_tab, row.names = FALSE) %>%
  group_rows("Religious composition", 1, 3) %>%
  group_rows("Regulation of religion", 4, 5) %>%
  footnote(
    general = "Followers of Abrahamic religions = the sum percentage of Christians and Muslims;
    Others = the sum percentage of not religious, individuals with unknown classification, 
    and the followers of all 'other' religions: Jews, Mandaeans, Zoroastrians, Bahais, Sikhs, 
    indigenous religionists (Ethnoreligionists), New Age religionists, and other religionists.
    The descriptive statistics were computed for the sample of 50 countries."
  )

# ----------------------------

# Table 12. Number (share) of countries for binary country-level predictors
bin_tab <- mlsem_dat[[1]][!duplicated(mlsem_dat[[1]]$country), ] %>%
  select(., c("ZAFRICA", "ZLA", "ZINDIC", "ZSINIC", "ZISLAM", "ZNWEST", 
              "ZORT", "ZOLDWEST", "ZRETWEST", "ZREFWEST",
              "COMMALL", "TAX")) %>%
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
    general = "The descriptive statistics were computed for the sample of 50 countries."
  )


# ----------------------------




