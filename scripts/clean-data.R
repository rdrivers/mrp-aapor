library(tidyverse)
library(haven)

# add DC to list of states
# make state a factor with postal codes as levels
all.states <- data_frame(state = c(state.abb, "DC"),
  name = c(state.name, "District of Columbia"),
  division = c(as.character(state.division), "South Atlantic"),
  region = c(as.character(state.region), "South")) %>%
  arrange(state) %>%
  mutate(state = factor(state, levels = sort(state)),
    division = factor(division, levels = levels(state.division)),
    region = factor(region, levels = levels(state.region)))
name.to.abb <- function(name) 
  all.states$state[match(tolower(name), tolower(all.states$name))]


# data from final pew survey before 2016 presidential election
pew <- read_spss(file = "Data/Oct16 public.sav") %>%
  mutate_if(is.labelled, as_factor) %>%
  transmute(
    state = name.to.abb(as.character(sstate)),
    votereg = fct_recode(regfinal, 
      yes = "Registered/Plan to/N.Dakota", 
      no = "Not registered"),
    turnout = fct_collapse(plan1,
      yes = c("Plan to vote", "Already voted"),
      no = c("Already voted", "Don't plan to vote", "Don't know/Refused (VOL.)")),
    vote16full = fct_recode(q10horse, 
      clinton = "Clinton/lean Clinton",
      trump = "Trump/lean Trump",
      johnson = "Johnson/lean Johnson",
      stein = "Stein/lean Stein",
      other = "Other-refused to lean",
      dk = "DK-refused to lean"),
    vote16 = case_when(
      votereg == "no" | turnout == "no" ~ "nonvoter",
      vote16full == "clinton" ~ "clinton",
      vote16full == "trump" ~ "trump",
      vote16full %in% c("johnson", "stein", "other") ~ "other"),
    vote16 = factor(vote16, levels = c("clinton", "trump", "other", "nonvoter")),
    age = fct_recode(age,
      "97" = "97 or older",
      NULL = "Don't know/Refused (VOL.)"),
    age = as.integer(as.character(age)),
    age_top_codes = case_when(
      age < 80 ~ "<80",
      age >= 80 & age < 85 ~ "80-84",
      age >= 85 ~ "85+"),
    raceeth = fct_recode(racethn,
      white = "White, non-Hisp",
      black = "Black, non-Hisp",
      hispanic = "Hispanic",
      other = "Other",
      NULL = "Don't know/Refused (VOL.)"),
    gender = fct_relabel(sex, tolower),
    educ = fct_recode(educ2,
      "grades 1-8" = "Less than high school (Grades 1-8 or no formal schooling)",
      "hs dropout" = "High school incomplete (Grades 9-11 or Grade 12 with NO diploma)",
      "hs grad" = "High school graduate (Grade 12 with diploma or GED certificate)",
      "some col" = "Some college, no degree (includes some community college)",
      "assoc" = "Two year associate degree from a college or university",
      "col grad" = "Four year college or university degree/Bachelor's degree (e.g., BS, BA, AB)",
      "postgrad" = "Some postgraduate or professional schooling, no postgraduate degree",
      "postgrad" = "Postgraduate or professional degree, including master's, doctorate, medical or law degree",
      NULL = "Don't know/Refused (VOL.)"),
    weight = weight / mean(weight)) %>%
  left_join(all.states, by = "state")
for(v in pew) attr(v, "label") <- NULL

# current population survey registration and voting supplement november 2016
cps <- read_spss(file = "Data/cpsnov2016.sav") %>%
  mutate_if(is.labelled, as_factor) %>%
  mutate_if(is.factor, ~ fct_recode(., NULL = "-1")) %>%
  transmute(
    state = factor(as.character(gestfips)), # levels in alphabetical order
    age_top_codes = factor(case_when(prtage == "80-84 Years Old" ~ "80-84",
      prtage == "85+ Years Old" ~ "85+", !is.na(prtage) ~ "<80"),
      levels = c("<80", "80-84", "85+")),
    age = as.integer(ifelse(age_top_codes == "<80", as.character(prtage), NA)),
    gender = fct_relabel(pesex, tolower),
    hisp = fct_recode(pehspnon,
      yes = "HISPANIC",
      no = "NON-HISPANIC"),
    race = fct_collapse(ptdtrace,
      white = "White Only",
      black = "Black Only",
      other = c("American Indian, Alaskan Native Only", "Asian Only",
        "Hawaiian/Pacific Islander Only", "White-Black", "White-AI",                            
        "White-Asian", "White-HP", "Black-AI", "Black-Asian", "Black-HP",
        "AI-Asian", "AI-HP", "Asian-HP", "W-B-AI", "W-B-A", "W-B-HP", 
        "W-AI-A", "W-AI-HP", "W-A-HP", "B-AI-A", "W-B-AI-A", "W-AI-A-HP", 
        "Other 3 Race Combinations", "Other 4 and 5 Race Combinations")),
    raceeth = factor(ifelse(hisp == "yes", "hispanic", as.character(race)),
      levels = c("white", "black", "hispanic", "other")),
    educ = fct_collapse(peeduca,
      "grades 1-8" = c("LESS THAN 1ST GRADE", "1ST, 2ND, 3RD OR 4TH GRADE",                  
        "5TH OR 6TH GRADE", "7TH OR 8TH GRADE"),
      "hs dropout" = c("9TH GRADE", "10TH GRADE", "11TH GRADE", "12TH GRADE NO DIPLOMA"),
      "hs grad" = "HIGH SCHOOL GRAD-DIPLOMA OR EQUIV (GED)",
      "some col" = "SOME COLLEGE BUT NO DEGREE",
      "assoc" = c("ASSOCIATE DEGREE-OCCUPATIONAL/VOCATIONAL", "ASSOCIATE DEGREE-ACADEMIC PROGRAM"),
      "col grad" = "BACHELOR'S DEGREE (EX: BA, AB, BS)",
      "postgrad" = c("MASTER'S DEGREE (EX: MA, MS, MEng, MEd, MSW)",
        "PROFESSIONAL SCHOOL DEG (EX: MD, DDS, DVM)", "DOCTORATE DEGREE (EX: PhD, EdD)")),
    citizen = fct_collapse(prcitshp,
      yes = c("NATIVE, BORN IN THE UNITED STATES", 
        "NATIVE, BORN IN PUERTO RICO OR OTHER U.S. ISLAND AREAS",
        "NATIVE, BORN ABROAD OF AMERICAN PARENT OR PARENTS",
        "FOREIGN BORN, U.S. CITIZEN BY NATURALIZATION"),
      no = "FOREIGN BORN, NOT A CITIZEN OF THE UNITED STATES"),
    votereg = factor(case_when(
      pes1 == "Yes" | pes2 == "Yes" ~ "yes",
      citizen == "No" | pes2 %in% c("No", "Don't Know") ~ "no"),
      levels = c("yes", "no")),
    turnout = factor(case_when(
      pes1 == "Yes" ~ "yes",
      votereg == "no" | pes1 %in% c("No", "Don't Know") ~ "no"),
      levels = c("yes", "no")),
    weight = as.numeric(pwsswgt) / 10000) %>%
  left_join(all.states, by = "state") %>%
  filter(age_top_codes %in% c("80-84", "85+") | age >= 18, citizen == "yes")


# vote counts and citizen voting age population for 2012 and 2016
vap12 <- read_csv("Data/vap2012.csv") %>%
  mutate(state = name.to.abb(state),
    cvap = as.integer(1000 * cvap)) %>%
  select(state, cvap) 
votes12 <- read_csv("Data/2012_0_0_1.csv", skip = 1) %>%
  transmute(
    state = name.to.abb(name),
    obama = vote1,
    romney = vote2,
    johnson = vote4,
    stein = vote5,
    turnout = totalvote
  ) %>%
  inner_join(vap12, by = "state") %>%
  inner_join(all.states, by = "state") %>%
  arrange(state)
vap16 <- read_csv("Data/vap2016.csv") %>%
  mutate(state = name.to.abb(state),
    cvap = as.integer(1000 * cvap)) %>%
  select(state, cvap)
votes16 <- read_csv("Data/2016_0_0_1.csv", skip = 1) %>%
  transmute(
    state = name.to.abb(name),
    clinton = vote1,
    trump = vote2,
    johnson = vote4, 
    stein = vote5,
    mcmullin = vote3,
    turnout = totalvote) %>%
  inner_join(vap16, by = "state") %>%
  inner_join(all.states, by = "state") %>%
  arrange(state)

# save clean data
save(pew, cps, votes12, votes16, file = "Data/cleaned.RData")
