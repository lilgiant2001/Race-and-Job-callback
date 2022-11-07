---
title: "Influence of Race on Job Callback Rates "
author: "Jiyoung Chang, Kevin Kim, Andy Dao"
date: "11/3/2021"
output: word_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
library(dplyr)
library(ggplot2)
library(leaps)
require(Design)
require(rms)
require(scales)
```


```{r}
resume <- read.csv("~/JiyoungChang/STA-310/resume.csv")


# < DATA CLEANING >
# -----------------------------------------------------------------------------------------------------

## delete id and first name of applicant, as it is unnecessary for the experiment 
resume <-select(resume, -c("job_ad_id", "firstname"))  
# dim(resume)


## changed quantitative variables with binary values (either 0 or 1) to categorical values with "No" and "Yes"
resume <- resume %>%
  mutate(received_callback = ifelse(received_callback == 1, "Yes", "No"),
         job_fed_contractor = ifelse(job_fed_contractor == 1, "Yes", "No"),
         job_equal_opp_employer = ifelse(job_equal_opp_employer == 1, "Yes", "No"),
         job_req_any = ifelse(job_req_any == 1, "Yes", "No"),
         job_req_communication = ifelse(job_req_communication == 1, "Yes", "No"),
         job_req_education = ifelse(job_req_education == 1, "Yes", "No"),
         job_req_computer= ifelse(job_req_computer == 1, "Yes", "No"),
         job_req_organization = ifelse(job_req_organization == 1, "Yes", "No"),
         college_degree = ifelse(college_degree == 1, "Yes", "No"),
         honors = ifelse(honors == 1, "Yes", "No"),
         worked_during_school = ifelse(worked_during_school == 1, "Yes", "No"),
         computer_skills = ifelse(computer_skills == 1, "Yes", "No"),
         special_skills = ifelse(special_skills == 1, "Yes", "No"),
         volunteer = ifelse(volunteer == 1, "Yes", "No"),
         military = ifelse(military == 1, "Yes", "No"),
         employment_holes = ifelse(employment_holes == 1, "Yes", "No"),
         has_email_address = ifelse(has_email_address == 1, "Yes", "No")
         )


## replace NA values with "unknown" to use job_fed_contractor as a three-category variable
resume$job_fed_contractor[is.na(resume$job_fed_contractor)] <- "unknown"

## replace blank values with "unknown" for job_req_min_experience variable
resume$job_req_min_experience[resume$job_req_min_experience == ""] <- NA
resume$job_req_min_experience = factor(resume$job_req_min_experience, levels=c(levels(resume$job_req_min_experience), "unknown"))
resume$job_req_min_experience[is.na(resume$job_req_min_experience)] = "unknown"
# levels(resume$job_req_min_experience)


# modified job_req_min_experience variable so that it's value is either few or many (few = less than 2 or "some", many = 2 or more)
resume <- resume %>%
  mutate(job_req_min_experience = ifelse(job_req_min_experience == "unknown", "unknown", 
                                    ifelse(job_req_min_experience == 0, "few", 
                                    ifelse(job_req_min_experience == 0.5, "few", 
                                           ifelse(job_req_min_experience == 1, "few",
                                                  ifelse(job_req_min_experience == "some", "few", "many"))))))


## change all the other variables as factors (other than years_college and years_experience) to use as categorical variables for logistic regression
resume$job_city <- as.factor(resume$job_city)
resume$job_industry <- as.factor(resume$job_industry)
resume$job_type <- as.factor(resume$job_type)
resume$job_fed_contractor <- as.factor(resume$job_fed_contractor)
resume$job_equal_opp_employer <- as.factor(resume$job_equal_opp_employer)
resume$job_ownership <- as.factor(resume$job_ownership)
resume$job_req_any <- as.factor(resume$job_req_any)
resume$job_req_communication <- as.factor(resume$job_req_communication)
resume$job_req_education <- as.factor(resume$job_req_education)
resume$job_req_min_experience <- as.factor(resume$job_req_min_experience)
resume$job_req_computer <- as.factor(resume$job_req_computer)
resume$job_req_organization <- as.factor(resume$job_req_organization)
resume$job_req_school <- as.factor(resume$job_req_school)
resume$received_callback <- as.factor(resume$received_callback)
resume$race <- as.factor(resume$race)
resume$gender <- as.factor(resume$gender)
resume$college_degree <- as.factor(resume$college_degree)
resume$honors <- as.factor(resume$honors)
resume$worked_during_school <- as.factor(resume$worked_during_school)
resume$computer_skills <- as.factor(resume$computer_skills)
resume$special_skills <- as.factor(resume$special_skills)
resume$volunteer <- as.factor(resume$volunteer)
resume$military <- as.factor(resume$military)
resume$employment_holes <- as.factor(resume$employment_holes)
resume$has_email_address <- as.factor(resume$has_email_address)
resume$resume_quality <- as.factor(resume$resume_quality)


```



```{r}

# model with all variables
resume.all <- glm(resume$received_callback ~ resume$job_city + resume$job_industry + resume$job_type + resume$job_fed_contractor + resume$job_equal_opp_employer + resume$job_ownership + resume$job_req_any + resume$job_req_communication + resume$job_req_education + resume$job_req_min_experience + resume$job_req_computer + resume$job_req_organization + resume$job_req_school + resume$gender + resume$years_college + resume$college_degree + resume$honors + resume$worked_during_school + resume$years_experience + resume$computer_skills + resume$special_skills + resume$volunteer + resume$military + resume$employment_holes + resume$has_email_address + resume$resume_quality, family=binomial)
# summary(resume.all)


# model with the significant variables that we found through calculating the g-statistics for each variable (without gender variable)
resume.top_wo_gender <- glm(resume$received_callback ~ resume$job_city + resume$job_industry + resume$job_type + resume$job_equal_opp_employer + resume$job_ownership + resume$job_req_organization + resume$job_req_school + resume$honors + resume$years_experience + resume$computer_skills + resume$special_skills + resume$employment_holes, family=binomial)
# summary(resume.top_wo_gender)

# decided to keep gender variable bc it is of our interest
resume.top_w_gender <-  glm(resume$received_callback ~ resume$job_city + resume$job_industry + resume$job_type + resume$job_equal_opp_employer + resume$job_ownership + resume$job_req_organization + resume$job_req_school + resume$honors + resume$years_experience + resume$computer_skills + resume$special_skills + resume$employment_holes + resume$gender, family=binomial)



# reduced model with interaction terms (without race variable)
resume.reduced <- glm(resume$received_callback ~ resume$job_city + resume$job_industry + resume$job_type + resume$job_equal_opp_employer + resume$job_ownership + resume$honors + resume$years_experience + resume$special_skills + resume$employment_holes + resume$gender + resume$job_city*resume$gender + resume$job_industry*resume$gender + resume$job_type*resume$gender + resume$job_equal_opp_employer*resume$gender + resume$job_ownership*resume$gender + resume$honors*resume$gender + resume$years_experience*resume$gender + resume$special_skills*resume$gender + resume$employment_holes*resume$gender + resume$job_city*resume$job_type + resume$special_skills*resume$job_type + resume$job_city*resume$special_skills, family=binomial)
# summary(resume.reduced)

# full model(with race variable)
resume.full <- glm(resume$received_callback ~ resume$job_city + resume$job_industry + resume$job_type + resume$job_equal_opp_employer + resume$job_ownership + resume$honors + resume$years_experience + resume$special_skills + resume$employment_holes + resume$gender + resume$job_city*resume$gender + resume$job_industry*resume$gender + resume$job_type*resume$gender + resume$job_equal_opp_employer*resume$gender + resume$job_ownership*resume$gender + resume$honors*resume$gender + resume$years_experience*resume$gender + resume$special_skills*resume$gender + resume$employment_holes*resume$gender + resume$job_city*resume$job_type + resume$special_skills*resume$job_type + resume$job_city*resume$special_skills + resume$race, family=binomial)
# summary(resume.full)

# reduced model with interaction terms
G <- resume.reduced$deviance - resume.full$deviance
# G  # bigger g-statistic
1 - pchisq(G, 1)  # gets a smaller p-value -> better reduced model
```

resume.top_w_gender_full <-  glm(resume$received_callback ~ resume$job_city + resume$job_industry + resume$job_type + resume$job_equal_opp_employer + resume$job_ownership + resume$job_req_organization + resume$job_req_school + resume$honors + resume$years_experience + resume$computer_skills + resume$special_skills + resume$employment_holes + resume$gender + resume$race, family=binomial)


# without interaction terms (just the single significant variables)
Gtwg <- resume.top_w_gender$deviance - resume.top_w_gender_full$deviance
Gtwg
1 - pchisq(Gtwg, 1)

# reduced model with interaction terms
Gred <- resume.reduced$deviance - resume.full$deviance
Gred  # bigger g-statistic
1 - pchisq(Gred, 1)  # gets a smaller p-value -> better reduced model

 
```{r}
## GRAPHS

# percentage of received_callback by race
(ggplot(resume, aes(received_callback, fill=received_callback))
 + geom_bar()
 + geom_text(
     aes(label=paste0(count, ' (', after_stat(round(prop*100)),'%)',sep=''), group=1),
     stat='count',
     nudge_y=0.125,
     size=4
 )
 + facet_wrap('race')
)


# percentage of received_callback by job_city
(ggplot(resume, aes(received_callback, fill=received_callback))
 + geom_bar()
 + geom_text(
     aes(label=paste0(count, ' (', after_stat(round(prop*100)),'%)',sep=''), group=1),
     stat='count',
     nudge_y=0.125,
     size=4
 )
 + facet_wrap('job_city')
)


# percentage of received_callback by job_type
(ggplot(resume, aes(received_callback, fill=received_callback))
 + geom_bar()
 + geom_text(
     aes(label=paste0(count, ' (', after_stat(round(prop*100)),'%)',sep=''), group=1),
     stat='count',
     nudge_y=0.125,
     size=3
 )
 + facet_wrap('job_type')
)
```




```{r}

# calculated g-statistic and corresponding p-value after taking each variable out one at a time to find its significance. 

# model with all explanatory variables
resume.best <- glm(resume$received_callback ~ resume$job_city + resume$job_industry + resume$job_type + resume$job_fed_contractor + resume$job_equal_opp_employer + resume$job_ownership + resume$job_req_any + resume$job_req_communication + resume$job_req_education + resume$job_req_min_experience + resume$job_req_computer + resume$job_req_organization + resume$job_req_school + resume$gender + resume$years_college + resume$college_degree + resume$honors + resume$worked_during_school + resume$years_experience + resume$computer_skills + resume$special_skills + resume$volunteer + resume$military + resume$employment_holes + resume$has_email_address + resume$resume_quality, family=binomial)
# G <- resume.best$null.deviance - resume.best$deviance
# G

resume.best1 <- glm(resume$received_callback ~ resume$job_industry + resume$job_type + resume$job_fed_contractor + resume$job_equal_opp_employer + resume$job_ownership + resume$job_req_any + resume$job_req_communication + resume$job_req_education + resume$job_req_min_experience + resume$job_req_computer + resume$job_req_organization + resume$job_req_school + resume$gender + resume$years_college + resume$college_degree + resume$honors + resume$worked_during_school + resume$years_experience + resume$computer_skills + resume$special_skills + resume$volunteer + resume$military + resume$employment_holes + resume$has_email_address + resume$resume_quality, family=binomial)
G1 <- resume.best1$deviance - resume.best$deviance
#G1
1 - pchisq(G1, 1)

resume.best2 <- glm(resume$received_callback ~ resume$job_city + resume$job_type + resume$job_fed_contractor + resume$job_equal_opp_employer + resume$job_ownership + resume$job_req_any + resume$job_req_communication + resume$job_req_education + resume$job_req_min_experience + resume$job_req_computer + resume$job_req_organization + resume$job_req_school + resume$gender + resume$years_college + resume$college_degree + resume$honors + resume$worked_during_school + resume$years_experience + resume$computer_skills + resume$special_skills + resume$volunteer + resume$military + resume$employment_holes + resume$has_email_address + resume$resume_quality, family=binomial)
G2 <- resume.best2$deviance - resume.best$deviance
# G2
1 - pchisq(G2, 1)
resume.best3 <- glm(resume$received_callback ~ resume$job_city + resume$job_industry + resume$job_fed_contractor + resume$job_equal_opp_employer + resume$job_ownership + resume$job_req_any + resume$job_req_communication + resume$job_req_education + resume$job_req_min_experience + resume$job_req_computer + resume$job_req_organization + resume$job_req_school + resume$gender + resume$years_college + resume$college_degree + resume$honors + resume$worked_during_school + resume$years_experience + resume$computer_skills + resume$special_skills + resume$volunteer + resume$military + resume$employment_holes + resume$has_email_address + resume$resume_quality, family=binomial)
G3 <- resume.best3$deviance - resume.best$deviance
#G3
1 - pchisq(G3, 1)
resume.best4 <- glm(resume$received_callback ~ resume$job_city + resume$job_industry + resume$job_type + resume$job_equal_opp_employer + resume$job_ownership + resume$job_req_any + resume$job_req_communication + resume$job_req_education + resume$job_req_min_experience + resume$job_req_computer + resume$job_req_organization + resume$job_req_school + resume$gender + resume$years_college + resume$college_degree + resume$honors + resume$worked_during_school + resume$years_experience + resume$computer_skills + resume$special_skills + resume$volunteer + resume$military + resume$employment_holes + resume$has_email_address + resume$resume_quality, family=binomial)
G4 <- resume.best4$deviance - resume.best$deviance
#G4
1 - pchisq(G4, 1)
resume.best5 <- glm(resume$received_callback ~ resume$job_city + resume$job_industry + resume$job_type + resume$job_fed_contractor + resume$job_ownership + resume$job_req_any + resume$job_req_communication + resume$job_req_education + resume$job_req_min_experience + resume$job_req_computer + resume$job_req_organization + resume$job_req_school + resume$gender + resume$years_college + resume$college_degree + resume$honors + resume$worked_during_school + resume$years_experience + resume$computer_skills + resume$special_skills + resume$volunteer + resume$military + resume$employment_holes + resume$has_email_address + resume$resume_quality, family=binomial)
G5 <- resume.best5$deviance - resume.best$deviance
#G5
1 - pchisq(G5, 1)
resume.best6 <- glm(resume$received_callback ~ resume$job_city + resume$job_industry + resume$job_type + resume$job_fed_contractor + resume$job_equal_opp_employer + resume$job_req_any + resume$job_req_communication + resume$job_req_education + resume$job_req_min_experience + resume$job_req_computer + resume$job_req_organization + resume$job_req_school + resume$gender + resume$years_college + resume$college_degree + resume$honors + resume$worked_during_school + resume$years_experience + resume$computer_skills + resume$special_skills + resume$volunteer + resume$military + resume$employment_holes + resume$has_email_address + resume$resume_quality, family=binomial)
G6 <- resume.best6$deviance - resume.best$deviance
#G6
1 - pchisq(G6, 1)
resume.best7 <- glm(resume$received_callback ~ resume$job_city + resume$job_industry + resume$job_type + resume$job_fed_contractor + resume$job_equal_opp_employer + resume$job_ownership + resume$job_req_communication + resume$job_req_education + resume$job_req_min_experience + resume$job_req_computer + resume$job_req_organization + resume$job_req_school + resume$gender + resume$years_college + resume$college_degree + resume$honors + resume$worked_during_school + resume$years_experience + resume$computer_skills + resume$special_skills + resume$volunteer + resume$military + resume$employment_holes + resume$has_email_address + resume$resume_quality, family=binomial)
G7 <- resume.best7$deviance - resume.best$deviance
#G7
1 - pchisq(G7, 1)
resume.best8 <- glm(resume$received_callback ~ resume$job_city + resume$job_industry + resume$job_type + resume$job_fed_contractor + resume$job_equal_opp_employer + resume$job_ownership + resume$job_req_any + resume$job_req_education + resume$job_req_min_experience + resume$job_req_computer + resume$job_req_organization + resume$job_req_school + resume$gender + resume$years_college + resume$college_degree + resume$honors + resume$worked_during_school + resume$years_experience + resume$computer_skills + resume$special_skills + resume$volunteer + resume$military + resume$employment_holes + resume$has_email_address + resume$resume_quality, family=binomial)
G8 <- resume.best8$deviance - resume.best$deviance
#G8
1 - pchisq(G8, 1)
resume.best9 <- glm(resume$received_callback ~ resume$job_city + resume$job_industry + resume$job_type + resume$job_fed_contractor + resume$job_equal_opp_employer + resume$job_ownership + resume$job_req_any + resume$job_req_communication + resume$job_req_min_experience + resume$job_req_computer + resume$job_req_organization + resume$job_req_school + resume$gender + resume$years_college + resume$college_degree + resume$honors + resume$worked_during_school + resume$years_experience + resume$computer_skills + resume$special_skills + resume$volunteer + resume$military + resume$employment_holes + resume$has_email_address + resume$resume_quality, family=binomial)
G9 <- resume.best9$deviance - resume.best$deviance
#G9
1 - pchisq(G9, 1)
resume.best10 <- glm(resume$received_callback ~ resume$job_city + resume$job_industry + resume$job_type + resume$job_fed_contractor + resume$job_equal_opp_employer + resume$job_ownership + resume$job_req_any + resume$job_req_communication + resume$job_req_education + resume$job_req_computer + resume$job_req_organization + resume$job_req_school + resume$gender + resume$years_college + resume$college_degree + resume$honors + resume$worked_during_school + resume$years_experience + resume$computer_skills + resume$special_skills + resume$volunteer + resume$military + resume$employment_holes + resume$has_email_address + resume$resume_quality, family=binomial)
G10 <- resume.best10$deviance - resume.best$deviance
#G10
1 - pchisq(G10, 1)
resume.best11 <- glm(resume$received_callback ~ resume$job_city + resume$job_industry + resume$job_type + resume$job_fed_contractor + resume$job_equal_opp_employer + resume$job_ownership + resume$job_req_any + resume$job_req_communication + resume$job_req_education + resume$job_req_min_experience + resume$job_req_organization + resume$job_req_school + resume$gender + resume$years_college + resume$college_degree + resume$honors + resume$worked_during_school + resume$years_experience + resume$computer_skills + resume$special_skills + resume$volunteer + resume$military + resume$employment_holes + resume$has_email_address + resume$resume_quality, family=binomial)
G11 <- resume.best11$deviance - resume.best$deviance
#G11
1 - pchisq(G11, 1)
resume.best12 <- glm(resume$received_callback ~ resume$job_city + resume$job_industry + resume$job_type + resume$job_fed_contractor + resume$job_equal_opp_employer + resume$job_ownership + resume$job_req_any + resume$job_req_communication + resume$job_req_education + resume$job_req_min_experience + resume$job_req_computer +  resume$job_req_school + resume$gender + resume$years_college + resume$college_degree + resume$honors + resume$worked_during_school + resume$years_experience + resume$computer_skills + resume$special_skills + resume$volunteer + resume$military + resume$employment_holes + resume$has_email_address + resume$resume_quality, family=binomial)
G12 <- resume.best12$deviance - resume.best$deviance
#G12
1 - pchisq(G12, 1)
resume.best13 <- glm(resume$received_callback ~ resume$job_city + resume$job_industry + resume$job_type + resume$job_fed_contractor + resume$job_equal_opp_employer + resume$job_ownership + resume$job_req_any + resume$job_req_communication + resume$job_req_education + resume$job_req_min_experience + resume$job_req_computer + resume$job_req_organization  + resume$gender + resume$years_college + resume$college_degree + resume$honors + resume$worked_during_school + resume$years_experience + resume$computer_skills + resume$special_skills + resume$volunteer + resume$military + resume$employment_holes + resume$has_email_address + resume$resume_quality, family=binomial)
G13 <- resume.best13$deviance - resume.best$deviance
#G13
1 - pchisq(G13, 1)
resume.best14 <- glm(resume$received_callback ~ resume$job_city + resume$job_industry + resume$job_type + resume$job_fed_contractor + resume$job_equal_opp_employer + resume$job_ownership + resume$job_req_any + resume$job_req_communication + resume$job_req_education + resume$job_req_min_experience + resume$job_req_computer + resume$job_req_organization + resume$job_req_school + resume$years_college + resume$college_degree + resume$honors + resume$worked_during_school + resume$years_experience + resume$computer_skills + resume$special_skills + resume$volunteer + resume$military + resume$employment_holes + resume$has_email_address + resume$resume_quality, family=binomial)
G14 <- resume.best14$deviance - resume.best$deviance
#G14
1 - pchisq(G14, 1)
resume.best15 <- glm(resume$received_callback ~ resume$job_city + resume$job_industry + resume$job_type + resume$job_fed_contractor + resume$job_equal_opp_employer + resume$job_ownership + resume$job_req_any + resume$job_req_communication + resume$job_req_education + resume$job_req_min_experience + resume$job_req_computer + resume$job_req_organization + resume$job_req_school + resume$gender + resume$college_degree + resume$honors + resume$worked_during_school + resume$years_experience + resume$computer_skills + resume$special_skills + resume$volunteer + resume$military + resume$employment_holes + resume$has_email_address + resume$resume_quality, family=binomial)
G15 <- resume.best15$deviance - resume.best$deviance
#G15
1 - pchisq(G15, 1)
resume.best16 <- glm(resume$received_callback ~ resume$job_city + resume$job_industry + resume$job_type + resume$job_fed_contractor + resume$job_equal_opp_employer + resume$job_ownership + resume$job_req_any + resume$job_req_communication + resume$job_req_education + resume$job_req_min_experience + resume$job_req_computer + resume$job_req_organization + resume$job_req_school + resume$gender + resume$years_college + resume$honors + resume$worked_during_school + resume$years_experience + resume$computer_skills + resume$special_skills + resume$volunteer + resume$military + resume$employment_holes + resume$has_email_address + resume$resume_quality, family=binomial)
G16 <- resume.best16$deviance - resume.best$deviance
#G16
1 - pchisq(G16, 1)
resume.best17 <- glm(resume$received_callback ~ resume$job_city + resume$job_industry + resume$job_type + resume$job_fed_contractor + resume$job_equal_opp_employer + resume$job_ownership + resume$job_req_any + resume$job_req_communication + resume$job_req_education + resume$job_req_min_experience + resume$job_req_computer + resume$job_req_organization + resume$job_req_school + resume$gender + resume$years_college + resume$college_degree + resume$worked_during_school + resume$years_experience + resume$computer_skills + resume$special_skills + resume$volunteer + resume$military + resume$employment_holes + resume$has_email_address + resume$resume_quality, family=binomial)
G17 <- resume.best17$deviance - resume.best$deviance
#G17
1 - pchisq(G17, 1)
resume.best18 <- glm(resume$received_callback ~ resume$job_city + resume$job_industry + resume$job_type + resume$job_fed_contractor + resume$job_equal_opp_employer + resume$job_ownership + resume$job_req_any + resume$job_req_communication + resume$job_req_education + resume$job_req_min_experience + resume$job_req_computer + resume$job_req_organization + resume$job_req_school + resume$gender + resume$years_college + resume$college_degree + resume$honors + resume$years_experience + resume$computer_skills + resume$special_skills + resume$volunteer + resume$military + resume$employment_holes + resume$has_email_address + resume$resume_quality, family=binomial)
G18 <- resume.best18$deviance - resume.best$deviance
#G18
1 - pchisq(G18, 1)
resume.best19 <- glm(resume$received_callback ~ resume$job_city + resume$job_industry + resume$job_type + resume$job_fed_contractor + resume$job_equal_opp_employer + resume$job_ownership + resume$job_req_any + resume$job_req_communication + resume$job_req_education + resume$job_req_min_experience + resume$job_req_computer + resume$job_req_organization + resume$job_req_school + resume$gender + resume$years_college + resume$college_degree + resume$honors + resume$worked_during_school + resume$computer_skills + resume$special_skills + resume$volunteer + resume$military + resume$employment_holes + resume$has_email_address + resume$resume_quality, family=binomial)
G19 <- resume.best19$deviance - resume.best$deviance
#G19
1 - pchisq(G19, 1)
resume.best20 <- glm(resume$received_callback ~ resume$job_city + resume$job_industry + resume$job_type + resume$job_fed_contractor + resume$job_equal_opp_employer + resume$job_ownership + resume$job_req_any + resume$job_req_communication + resume$job_req_education + resume$job_req_min_experience + resume$job_req_computer + resume$job_req_organization + resume$job_req_school + resume$gender + resume$years_college + resume$college_degree + resume$honors + resume$worked_during_school + resume$years_experience + resume$special_skills + resume$volunteer + resume$military + resume$employment_holes + resume$has_email_address + resume$resume_quality, family=binomial)
G20 <- resume.best20$deviance - resume.best$deviance
#G20
1 - pchisq(G20, 1)
resume.best21 <- glm(resume$received_callback ~ resume$job_city + resume$job_industry + resume$job_type + resume$job_fed_contractor + resume$job_equal_opp_employer + resume$job_ownership + resume$job_req_any + resume$job_req_communication + resume$job_req_education + resume$job_req_min_experience + resume$job_req_computer + resume$job_req_organization + resume$job_req_school + resume$gender + resume$years_college + resume$college_degree + resume$honors + resume$worked_during_school + resume$years_experience + resume$computer_skills + resume$volunteer + resume$military + resume$employment_holes + resume$has_email_address + resume$resume_quality, family=binomial)
G21 <- resume.best21$deviance - resume.best$deviance
#G21
1 - pchisq(G21, 1)
resume.best22 <- glm(resume$received_callback ~ resume$job_city + resume$job_industry + resume$job_type + resume$job_fed_contractor + resume$job_equal_opp_employer + resume$job_ownership + resume$job_req_any + resume$job_req_communication + resume$job_req_education + resume$job_req_min_experience + resume$job_req_computer + resume$job_req_organization + resume$job_req_school + resume$gender + resume$years_college + resume$college_degree + resume$honors + resume$worked_during_school + resume$years_experience + resume$computer_skills + resume$special_skills + resume$military + resume$employment_holes + resume$has_email_address + resume$resume_quality, family=binomial)
G22 <- resume.best22$deviance - resume.best$deviance
#G22
1 - pchisq(G22, 1)
resume.best23 <- glm(resume$received_callback ~ resume$job_city + resume$job_industry + resume$job_type + resume$job_fed_contractor + resume$job_equal_opp_employer + resume$job_ownership + resume$job_req_any + resume$job_req_communication + resume$job_req_education + resume$job_req_min_experience + resume$job_req_computer + resume$job_req_organization + resume$job_req_school + resume$gender + resume$years_college + resume$college_degree + resume$honors + resume$worked_during_school + resume$years_experience + resume$computer_skills + resume$special_skills + resume$volunteer + resume$employment_holes + resume$has_email_address + resume$resume_quality, family=binomial)
G23 <- resume.best23$deviance - resume.best$deviance
#G23
1 - pchisq(G23, 1)
resume.best24 <- glm(resume$received_callback ~ resume$job_city + resume$job_industry + resume$job_type + resume$job_fed_contractor + resume$job_equal_opp_employer + resume$job_ownership + resume$job_req_any + resume$job_req_communication + resume$job_req_education + resume$job_req_min_experience + resume$job_req_computer + resume$job_req_organization + resume$job_req_school + resume$gender + resume$years_college + resume$college_degree + resume$honors + resume$worked_during_school + resume$years_experience + resume$computer_skills + resume$special_skills + resume$volunteer + resume$military + resume$has_email_address + resume$resume_quality, family=binomial)
G24 <- resume.best24$deviance - resume.best$deviance
#G24
1 - pchisq(G24, 1)

resume.best25 <- glm(resume$received_callback ~ resume$job_city + resume$job_industry + resume$job_type + resume$job_fed_contractor + resume$job_equal_opp_employer + resume$job_ownership + resume$job_req_any + resume$job_req_communication + resume$job_req_education + resume$job_req_min_experience + resume$job_req_computer + resume$job_req_organization + resume$job_req_school + resume$gender + resume$years_college + resume$college_degree + resume$honors + resume$worked_during_school + resume$years_experience + resume$computer_skills + resume$special_skills + resume$volunteer + resume$military + resume$employment_holes + resume$resume_quality, family=binomial)
G25 <- resume.best25$deviance - resume.best$deviance
#G25
1 - pchisq(G25, 1)


# model with all explanatory variables
resume.all <- glm(resume$received_callback ~ resume$job_city + resume$job_industry + resume$job_type + resume$job_fed_contractor + resume$job_equal_opp_employer + resume$job_ownership + resume$job_req_any + resume$job_req_communication + resume$job_req_education + resume$job_req_min_experience + resume$job_req_computer + resume$job_req_organization + resume$job_req_school + resume$gender + resume$years_college + resume$college_degree + resume$honors + resume$worked_during_school + resume$years_experience + resume$computer_skills + resume$special_skills + resume$volunteer + resume$military + resume$employment_holes + resume$has_email_address + resume$resume_quality, family=binomial)

resume.best26 <- glm(resume$received_callback ~ resume$job_city + resume$job_industry + resume$job_type + resume$job_fed_contractor + resume$job_equal_opp_employer + resume$job_ownership + resume$job_req_any + resume$job_req_communication + resume$job_req_education + resume$job_req_min_experience + resume$job_req_computer + resume$job_req_organization + resume$job_req_school + resume$gender + resume$years_college + resume$college_degree + resume$honors + resume$worked_during_school + resume$years_experience + resume$computer_skills + resume$special_skills + resume$volunteer + resume$military + resume$employment_holes + resume$has_email_address, family=binomial)
G26 <- resume.best26$deviance - resume.best$deviance
#G26
1 - pchisq(G26, 1)

```


