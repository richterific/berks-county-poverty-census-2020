---
title: "Census poverty data for Berks County, 2020"
author: "Adam Richter"
date: "4/27/2022"
output: 
  html_document: 
    theme: cerulean
     
---

```{r setup, include=FALSE, dev=svg}
knitr::opts_chunk$set(warning = FALSE, message = FALSE)
options(kableExtra.auto_format = TRUE)
## Set Libraries ---- 
library(dplyr)
library(rio)
library(reactable)
library(rmarkdown)
library(kableExtra)
library(R.utils)
library(reactable)
library(glue)
library(htmltools)
library(tidyverse)
library(tidyr)
library(data.table)
library(janitor)
library(lubridate)
library(tidycensus)
library(googlesheets4)
library(RWordPress)
```
# Poverty in Berks County
Of the 72 municipalities in Berks County, only one had a definitive increase in the number of people in poverty in 2020, according to the U.S. Census Bureau's American Community Survey. 

And no, it wasn't Reading. 

The city — which, a decade after being named (inaccurately, IMHO) <a href = "https://www.readingeagle.com/2020/01/31/reading-pa-poorest-city-census/" target="_blank">the poorest in the country</a>, still has one of the highest poverty rates in the United States — saw its poverty rate fall considerably since earning that dubious title in 2011, and it fell more since 2015. 

The new ACS survey data now show that West Reading, a suburban community just across the Schuylkill River from Reading, is the only municipality to have seen its poverty rate rise between 2015 and 2020 (outside the survey's margin of error, that is). According to the latest ACS, more than 1 in 4 people in West Reading were below the federal poverty line in the last 12 months before 2020. That number fluctuates based on the margin of error, as you can see in the table below. But even accounting for that, West Reading's poverty rate rose when other places in Berks saw their rate stay flat or even fall from 2015 to 2020.

*NOTE: The source code for this document, as well as the raw data, is <a href="https://github.com/richterific/berks-county-poverty-census-2020" target="_blank">available at my Github page</a>.
```{r poverty in Berks, include=FALSE}
## Get data 
## Population growth, 2015-2020
berks_pop_2020 <- get_acs(geography = "county subdivision",  variables = c("S1701_C01_001E"), year = 2020, survey = "acs5", state = "42", county = "011") %>%
  mutate(Town= str_replace_all(NAME, ", Berks County, Pennsylvania", "")
  ) %>%
  filter(GEOID !="4201100364") %>%
  rename(pop_2020 = estimate) %>%
  rename(pop_2020_moe = moe) %>%
  select(GEOID, Town, pop_2020, pop_2020_moe) %>%
  glimpse()
berks_pop_2015 <- get_acs(geography = "county subdivision",  variables = c("S1701_C01_001E"), year = 2015, survey = "acs5", state = "42", county = "011") %>%
  mutate(Town= str_replace_all(NAME, ", Berks County, Pennsylvania", "")
  ) %>%
  filter(GEOID !="4201100364") %>%
  rename(pop_2015 = estimate) %>%
  rename(pop_2015_moe = moe) %>%
  select(GEOID, Town, pop_2015, pop_2015_moe) %>%
  full_join(berks_pop_2020, by = "GEOID") %>%
  mutate(
    chg_2015_2020 = pop_2020 - pop_2015,
    pct_chg = round((chg_2015_2020 / pop_2015)*100, digits = 2)
  ) %>%
  arrange(desc(pct_chg)) %>%
  glimpse()
## Change in poverty, 2015-2020
berks_pov_2020 <- get_acs(geography = "county subdivision",  variables = c("S1701_C02_001E"), year = 2020, survey = "acs5", state = "42", county = "011") %>%
  mutate(Town= str_replace_all(NAME, ", Berks County, Pennsylvania", "")
  ) %>%
  filter(GEOID !="4201100364") %>%
  rename(pov_2020 = estimate) %>%
  rename(pov_2020_moe = moe) %>%
  select(GEOID, Town, pov_2020, pov_2020_moe) %>%
  glimpse()
berks_pov_2015 <- get_acs(geography = "county subdivision",  variables = c("S1701_C02_001E"), year = 2015, survey = "acs5", state = "42", county = "011") %>%
  mutate(Town= str_replace_all(NAME, ", Berks County, Pennsylvania", "")
  ) %>%
  filter(GEOID !="4201100364") %>%
  rename(pov_2015 = estimate) %>%
  rename(pov_2015_moe = moe) %>%
  select(GEOID, Town, pov_2015, pov_2015_moe) %>%
  full_join(berks_pov_2020, by = "GEOID") %>%
  mutate(
    chg_2015_2020 = pov_2020 - pov_2015,
    pct_chg = round((chg_2015_2020 / pov_2015)*100, digits = 2)
  ) %>%
  arrange(desc(pct_chg)) %>%
  glimpse()
## Pct in poverty 
census_var <- c(Pct_below_Poverty = "S1701_C03_001")
pov_pct_2020 <- get_acs(geography = "county subdivision",  variables = census_var, year = 2020, survey = "acs5", state = "42", county = "011") %>%
  mutate(Town= str_replace_all(NAME, ", Berks County, Pennsylvania", "")
  ) %>%
  filter(GEOID !="4201100364") %>%
  rename(Poverty_Rate_2020 = estimate) %>%
  rename(MOE_2020 = moe) %>%
  select(GEOID, Town, Poverty_Rate_2020, MOE_2020) %>%
  glimpse()
berks_poverty <- get_acs(geography = "county subdivision",  variables = census_var, year = 2015, survey = "acs5", state = "42", county = "011") %>%
  mutate(Town= str_replace_all(NAME, ", Berks County, Pennsylvania", "")
  ) %>%
  filter(GEOID !="4201100364") %>%
  rename(Poverty_Rate_2015 = estimate) %>%
  rename(MOE_2015 = moe) %>%
  select(GEOID, Town, Poverty_Rate_2015, MOE_2015) %>%
  full_join(pov_pct_2020, by = "Town", "GEOID") %>%
  mutate(
    Change_2015_2020 = Poverty_Rate_2020 - Poverty_Rate_2015,
    Town= str_replace_all(Town, " borough",""),
    Town= str_replace_all(Town, "township", "Township")
  ) %>%
  arrange(desc(Change_2015_2020))
berks_poverty <- berks_poverty %>%
  select(Town, Poverty_Rate_2015, MOE_2015, Poverty_Rate_2020, MOE_2020, Change_2015_2020)
```

```{r table, include=FALSE, tableresults='asis'}
berks_table <- reactable::reactable(berks_poverty, sortable = TRUE, searchable = TRUE, filterable = TRUE, striped = TRUE)
# berks_table <- knitr::kable(berks_poverty, format = "html", align = "lcccccc", format.args = list(big.mark = ",",
  # scientific = FALSE), include = T, booktabs = T) %>%
    # kable_styling(bootstrap_options = c("striped", "hover"))
```
```{r table display, echo=FALSE, tableresults='asis'}
berks_table
```
### Poverty changes by age group
In Berks County, poverty fell for those under 65 but went up for the 65-and-older population:
```{r age, include=FALSE}
pov_age <- c(under_18 = "S1701_C01_002",
over_18 = "S1701_C01_006",
over_65 = "S1701_C01_010")
pov_age_2020 <- get_acs(geography = "county",  variables = pov_age, year = 2020, survey = "acs5", state = "42", county = "011") %>% #2020 poverty rates by age group
  rename(Poverty_2020 = estimate) %>%
  rename(Poverty_2020_moe = moe) 
pov_age_2015 <- get_acs(geography = "county",  variables = pov_age, year = 2015, survey = "acs5", state = "42", county = "011") %>% #2020 poverty rates by age group
  rename(Poverty_2015 = estimate) %>%
  rename(Poverty_2015_moe = moe) %>%
  full_join(pov_age_2020, by = "variable") %>% # Combine 2015 and 2020 files
  mutate(
    Change_2015_2020 = Poverty_2020 - Poverty_2015,
    Percent_Change = round((Change_2015_2020 / Poverty_2015)*100, digits = 2)
  ) %>%
  arrange(desc(Percent_Change)) %>%
  glimpse()
age_group <- pov_age_2015 %>%
  select(variable, Poverty_2015, Poverty_2015_moe, Poverty_2020, Poverty_2020_moe) %>%
  rename(Age_Group = variable)
```
```{r age table, echo=FALSE, tableresults='asis'}
age_chart <- reactable(age_group, striped = TRUE, columns = list(
  Poverty_2015 = colDef(format = colFormat(separators = TRUE, locales = "en-US")),
  Poverty_2020 = colDef(format = colFormat(separators = TRUE, locales = "en-US"))
))
age_chart
```
### Poverty changes by race
The table below shows the percentage of people living in poverty by race/Latino ethnicity in Berks. Click on a municipality to expand that row. 
```{r poverty race, include=FALSE}
census_race <- c(White_Alone = "S1701_C03_013",
                 Black_Alone = "S1701_C03_014",
                 Native_American_Alone = "S1701_C03_015",
                 Asian_American_Alone = "S1701_C03_016",
                 Pacific_Islander_Alone = "S1701_C03_017",
                 Other_Race_Alone = "S1701_C03_018",
                 Two_Or_More_Races = "S1701_C03_019",
                 Latino_Any_Race = "S1701_C03_020")
poverty_by_race_2015 <- get_acs(geography = "county subdivision",  variables = census_race, year = 2015, survey = "acs5", state = "42", county = "011") %>%
  rename(Poverty_Rate_2015 = estimate) %>%
  rename(MOE_2015 = moe) %>%
   mutate(Town= str_replace_all(NAME, ", Berks County, Pennsylvania", "")
  ) %>%
  filter(GEOID !="4201100364") %>%
  select(GEOID, Town, variable, Poverty_Rate_2015, MOE_2015) %>%
  glimpse()
poverty_by_race <- get_acs(geography = "county subdivision",  variables = census_race, year = 2020, survey = "acs5", state = "42", county = "011") %>%
  rename(Poverty_Rate_2020 = estimate) %>%
  rename(MOE_2020 = moe) %>%
   mutate(Town= str_replace_all(NAME, ", Berks County, Pennsylvania", "")
  ) %>%
  filter(GEOID !="4201100364") %>%
  select(GEOID, Town, variable, Poverty_Rate_2020, MOE_2020) %>%
  full_join(poverty_by_race_2015, by = c("Town", "GEOID","variable")) %>%
  mutate(
    Change_2015_2020 = Poverty_Rate_2020 - Poverty_Rate_2015,
    Town= str_replace_all(Town, " borough",""),
    Town= str_replace_all(Town, "township", "Township")
  ) %>%
    rename(Race_or_Ethnicity = variable) %>%
  arrange(desc(Change_2015_2020))
poverty_by_race <- poverty_by_race %>%
  select(Town, Race_or_Ethnicity, Poverty_Rate_2015, MOE_2015, Poverty_Rate_2020, MOE_2020, Change_2015_2020)
berks_race_table <- reactable(poverty_by_race, sortable = TRUE, searchable = TRUE, filterable = TRUE, striped = TRUE, columns = list(
  Change_2015_2020 = colDef(defaultSortOrder = "desc")
),
groupBy = "Town")
```
```{r race table, echo=FALSE, tableresults='asis'}
berks_race_table
```
### Reading's place among the top cities 
For most of the last decade, Reading has been among the poorest cities (with a 50,000-plus population) in the country. 
```{r top cities, include=FALSE}
pop_var <- c(Population = "S1701_C01_001")
poverty_var <- c(Poverty_rate = "S1701_C03_001")
pop_place_2020 <- get_acs(geography = "place",  variables = pop_var, year = 2020, survey = "acs5") %>%
  clean_names() %>%
  filter(estimate >= 50000) %>%
  rename(pppulation_2020 = "estimate") %>%
  rename(pop_moe_2020 = "moe") %>%
  glimpse()
poverty_place_2020 <- get_acs(geography = "place",  variables = poverty_var, year = 2020, survey = "acs5") %>%
  clean_names() %>% 
  rename(poverty_rate_2020 = "estimate") %>%
  rename(pov_moe_2020 = "moe")
poverty_top_cities_2020 <- left_join(pop_place_2020, poverty_place_2020, by = c("geoid","name")) %>%
  filter(!grepl(", Puerto Rico", name)) %>%
  arrange(desc(poverty_rate_2020)) %>%
    slice_max(poverty_rate_2020, n = 10) %>%
  mutate(City = str_replace_all(name, " city", "")) %>%
  select(City, poverty_rate_2020, pov_moe_2020) %>%
  glimpse()
pop_place_2015 <- get_acs(geography = "place",  variables = pop_var, year = 2015, survey = "acs5") %>%
  clean_names() %>%
  filter(estimate >= 50000) %>%
  rename(pppulation_2015 = "estimate") %>%
  rename(pop_moe_2015 = "moe") %>%
  glimpse()
poverty_place_2015 <- get_acs(geography = "place",  variables = poverty_var, year = 2015, survey = "acs5") %>%
  clean_names() %>% 
  rename(poverty_rate_2015 = "estimate") %>%
  rename(pov_moe_2015 = "moe")
poverty_top_cities_2015 <- left_join(pop_place_2015, poverty_place_2015, by = c("geoid","name")) %>%
  filter(!grepl(", Puerto Rico", name)) %>%
  arrange(desc(poverty_rate_2015)) %>%
    slice_max(poverty_rate_2015, n = 10) %>%
  mutate(City = str_replace_all(name, " city", "")) %>%
  select(City, poverty_rate_2015, pov_moe_2015) %>%
  glimpse()
top_2015_chart <- ggplot(poverty_top_cities_2015, aes(x=City, y = poverty_rate_2015, label = poverty_rate_2015)) +
  geom_col(position = "dodge", fill = "#0052a3", width = .65) + 
    geom_text(vjust = 3, hjust = 0.5, colour = "white", face = "bold", size = 6) +
  geom_errorbar(data = poverty_top_cities_2015, 
                aes(ymin = poverty_rate_2015 - pov_moe_2015, ymax = poverty_rate_2015 + pov_moe_2015), width = .6) +
  labs(title = "CITIES WITH THE HIGHEST POVERTY RATE, 2015",
       subtitle = "Populaion 50,000 and above",
       caption = "SOURCE: U.S. Census Bureau American Community Survey five-year estimates, 2011-2015",
       x = "CITY",
       y = "Poverty rate"
       ) +
  theme(axis.text.x = element_text(angle = 60, vjust = 1, hjust = 1.1)) +
  theme(plot.title = element_text(face = "bold", size = 18)) +
  theme(plot.subtitle = element_text(face = "italic", size = 12)) 
top_2020_chart <- ggplot(poverty_top_cities_2020, aes(x=City, y = poverty_rate_2020, label = poverty_rate_2020)) +
  geom_col(position = "dodge", fill = "#0052a3", width = .65) + 
  geom_text(vjust = 3, hjust = 0.5, colour = "white", face = "bold", size = 6) +
  geom_errorbar(data = poverty_top_cities_2020, 
                aes(ymin = poverty_rate_2020 - pov_moe_2020, ymax = poverty_rate_2020 + pov_moe_2020), width = .6) +
  labs(title = "CITIES WITH THE HIGHEST POVERTY RATE, 2020",
       subtitle = "Populaion 50,000 and above",
       caption = "SOURCE: U.S. Census Bureau American Community Survey five-year estimates, 2016-2020",
       x = "CITY",
       y = "Poverty rate"
       ) +
  theme(axis.text.x = element_text(angle = 60, vjust = 1.2, hjust = 1.1)) +
  theme(plot.title = element_text(face = "bold", size = 18)) +
  theme(plot.subtitle = element_text(face = "italic", size = 12)) 
```

#### Top 10 poorest cities in 2015
These were the 10 poorest cities in the U.S. in 2015. Note how close the error bars (the black I-beams) are to one another.
```{r top cities 2015 chart, echo=FALSE, fig.height=6, fig.width=8}
top_2015_chart
```

#### Top 10 poorest cities in 2020
These were the poorest cities in the U.S. in 2020. Note that the poverty rate for all of them is lower than it was in 2015. 
```{r 2020 chart, echo=FALSE, fig.height=6, fig.width=8}
top_2020_chart
```

I hope you find this useful. If you have questions about the data feel free to leave a comment or email me: adam (at) adam-richter dot com. 