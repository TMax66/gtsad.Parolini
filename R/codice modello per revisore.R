pos <- read_excel(here("data", "raw", "dati.xlsx"))

prevB <- pos %>%
  mutate_if(is.character,as.factor) %>% 
  # filter(pr %in% c("BS", "CR")) %>% 
  # filter(specie == "CINGHIALE") %>% 
  filter(!anno == 2022) %>% 
  filter(prova == "Brucella abortus/melitensis/suis: anticorpi") %>% 
  filter(!is.na(esito)) %>%  
  filter(!specie %in% c(
    "MINILEPRE (SYLVILAGUS FLORIDANUS)",
    "CAMOSCIO",
    "CONIGLIO",
    "CONIGLIO SELVATICO",
    "STAMBECCO", 
    "LEPRE", 
    "CERVO", 
    "CAPRIOLO", 
    "DAINO"
    )) %>%
  mutate(sex_en = case_when(
    sex == "M" ~ "Male",
    sex == "F" ~ "Female",
    is.na(sex) ~ "Unknown"),
    .after = sex) %>% 
  mutate(age_class = case_when(
    specie == "CINGHIALE" & age == "Cl 0: 0-12 mesi \\ 0-1 anni" ~ "Juvenile", 
    specie == "CINGHIALE" & age == "Cl 1: 13-24 mesi \\ 1-2 anni" ~ "Yearling",
    specie == "CINGHIALE" & age == "Cl 2: > 25 mesi \\ > 2 anni" ~ "Adult",
    is.na(age) ~ "Unknown",
    TRUE ~ paste0("?")),
    .after = age) %>%  
  mutate(specie_en = case_when(
    specie == "CINGHIALE" ~ "WILD BOAR",
    TRUE ~ paste0(specie)),
    .after = specie) %>%
  mutate(comune = str_to_upper(comune))


prevB %>% 
  filter(sex_en != "Unknown", 
         age_class != "Unknown")-> dfmod



library(rstanarm)

mod <- stan_glm(esito ~ sex_en+age_class,    data = prevB, family = binomial(link = "logit"))

library(bayestestR)

describe_posterior(
  mod,
  centrality = "median",
  test = c("rope", "p_direction"), 
  rope_range = c(-0.1, 0.1)
)


library(see)
library(sjPlot)
library(bayesplot)



dfmod %>% 
  group_by(sex_en, esito) %>% 
  count()
