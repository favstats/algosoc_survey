
# remotes::install_github("pewresearch/pewmethods")

library(pewmethods)

# 
# varlabdat <- data %>% 
#   select(var1:var2) %>% 
#   names() %>% 
#   tibble(key = ., varlabs = thevarlabs)
# 
# education_levels <- c("Primary (basisonderwijs)", 
#                       "Pre-Vocational (vmbo)", 
#                       "Secondary (havo/vwo)", 
#                       "Vocational (mbo)", 
#                       "Applied Sciences (hbo)", 
#                       "University (wo)") %>% 
#   tibble(eng = ., oplcat = c("basisonderwijs",
#                              "vmbo",
#                              "havo/vwo",
#                              "mbo",
#                              "hbo",
#                              "wo"))


data <- read_sav("../L_AlgoSoc_wave1_1.0p.sav") %>% #table()
  mutate(age_groups = case_when(
    leeftijd %in% 16:17 ~ "16-17",
    leeftijd %in% 18:24 ~ "18-24",
    leeftijd %in% 25:34 ~ "25-34",
    leeftijd %in% 35:44 ~ "35-44",
    leeftijd %in% 45:54 ~ "45-54",
    leeftijd %in% 55:64 ~ "55-64",
    leeftijd %in% 65:150 ~ "65+",
  )) %>% 
  mutate(oplcat = sjmisc::to_label(oplcat)) %>% 
  mutate(edu = case_when(
    oplcat == "havo/vwo" ~ "mbo/havo/vwo",
    oplcat == "mbo" ~ "mbo/havo/vwo",
    T ~ oplcat
  ))  %>% 
  mutate(geslacht = sjmisc::to_label(geslacht)) %>% 
  mutate(gender = case_when(
    geslacht == "Vrouw" ~ "Woman",
    geslacht == "Anders" ~ "Other",
    T ~ geslacht
  )) %>% 
  select(nomem_encr , age_groups, edu, gender) %>% 
  filter(gender != "Other") %>% 
  drop_na(edu, age_groups)  %>% 
  filter(age_groups != "16-17")

edus <- openxlsx::read.xlsx("data/EduLevel_16plusAge_Gender_2022_ENG_edit.xlsx", sheet = "Table 1") %>% 
  select(-contains("x")) %>% 
  as_tibble() %>% 
  janitor::clean_names() %>% #table()
  mutate(age = parse_number(age)) %>% 
  mutate(age_groups = case_when(
    age %in% 16:17 ~ "16-17",
    age %in% 18:24 ~ "18-24",
    age %in% 25:34 ~ "25-34",
    age %in% 35:44 ~ "35-44",
    age %in% 45:54 ~ "45-54",
    age %in% 55:64 ~ "55-64",
    age %in% 65:150 ~ "65+",
  ))  %>% 
  filter(age_groups != "16-17")

wtar <- edus %>%  
  select(age_groups, gender, basisschool:hbo_wo_master) %>% 
  gather(edu, value, -age_groups, -gender) %>%
  group_by(age_groups, gender, edu) %>% 
  summarize(n = sum(value)) %>% 
  mutate(share = n/sum(n)) %>% 
  ungroup() %>% 
  mutate(edu = case_when(
    edu == "basisschool" ~ "basisonderwijs",
    edu == "vmbo_mbo_havo_vwo_onderbouw" ~ "vmbo",
    edu == "havo_bovenbouw_and_vwo_bovenbouw" ~ "mbo/havo/vwo",
    edu == "bo_wo_bachelor" ~ "hbo",
    edu == "hbo_wo_master" ~ "wo"
  )) %>% 
  mutate(gender = ifelse(gender == "men", "Man", "Woman")) %>% 
  filter(!(edu == "hbo" & age_groups == "16-17")) %>% 
  filter(!(edu == "wo" & age_groups == "16-17")) %>% 
  filter(age_groups != "16-17") %>% 
  mutate(edu = as.factor(edu),
         gender = as.factor(gender),
         age_groups = as.factor(age_groups))

# wtar %>% View()

targets <- create_raking_targets(
  wtar,
  vars = c("age_groups", 
           "gender",
           "edu",
           "age_groups:gender",
           "age_groups:edu",
           "gender:edu"
  ),
  wt = "share"
)

# targets[[5]] <- targets[[5]] %>%
#   # filter(rk_age_groups_edu != "16-17:hbo") %>%
#   # filter(rk_age_groups_edu != "16-17:wo") %>%
#   mutate(rk_age_groups_edu = factor(rk_age_groups_edu))
# 
# targets[[3]] <- targets[[3]] %>% 
#   filter(rk_age_groups_edu != "16-17:hbo") %>% 
#   filter(rk_age_groups_edu != "16-17:wo") %>% 
#   mutate(rk_age_groups_edu = factor(rk_age_groups_edu))

# debugonce(pewmethods:::unify_margins)
w2 <- data %>% 
  mutate_at(vars(age_groups:gender), as.factor) %>% 
  drop_na() %>%
  mutate(rk_age_groups = age_groups,
         rk_edu = edu,
         rk_gender = gender,
         rk_age_groups_gender = interaction(age_groups, gender, sep = ":"),
         rk_age_groups_edu = interaction(age_groups, edu, sep = ":"),
         rk_gender_edu = interaction(gender, edu, sep = ":")
         ) %>% 
  # filter()
  rake_survey(pop_margins = targets, maxit = 500)

wtm_weighted <- data  %>% 
  mutate(weight = w2) 
