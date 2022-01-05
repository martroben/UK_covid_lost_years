
#######################################################################################################
##                                                                                                   ##
##  Script name: UK_covid_lost_years.R                                                               ##
##  Purpose of script: Calculate average years unlived by a UK person who died of covid.             ##
##                     (Possible higher prevalence of co-morbidities in people who died of covid     ##
##                                                                                                   ##
##  Notes: Possible higher prevalence of co-morbidities in people who died of covid                  ##
##           is not controlled for                                                                   ##
##         Life expectancy used in the current period life expectancy                                ##
##           future rise in life expectancies is not modelled                                        ##
##                                                                                                   ##
##  Author: Mart Roben                                                                               ##
##  Date Created: 5. Jan 2022                                                                        ##
##                                                                                                   ##
##  Copyright: MIT License                                                                           ##
##  https://github.com/martroben/UK_covid_lost_years                                                 ##
##                                                                                                   ##
##  Contact: fb.com/martroben                                                                        ##
##                                                                                                   ##
#######################################################################################################


#################
# Load packages #
#################

if (!require("pacman", quietly = TRUE)) install.packages("pacman")

pacman::p_load(magrittr,
               dplyr,
               tidyr,
               purrr,
               readxl,
               rio)



################
# Data sources #
################

# UK 2018-2020 life tables
UK_life_tables_link <- "https://www.ons.gov.uk/file?uri=/peoplepopulationandcommunity/birthsdeathsandmarriages/lifeexpectancies/datasets/nationallifetablesunitedkingdomreferencetables/current/nationallifetables3yruk.xlsx"

# UK cumulative covid deaths by sex & age group @ 17. Dec 2021
UK_covid_deaths_link <- "https://www.ined.fr/fichier/rte/166/Page%20Data/England%20and%20Wales/EnglandWales_2022_01_03_Deaths_Covid-19.xlsx"



#############
# Functions #
#############

get_age_groups <- function(ages, age_groups_list) {

  get_one_age_group <- function(age, age_groups_list) {
    age_groups_list %>%
      purrr::map_lgl(~(age %in% .x)) %>%
      which() %>%
      purrr::pluck(age_groups_list, .)
  }
  
  ages %>% purrr::map(get_one_age_group, age_groups_list)
}

get_age_group_name <- function(age_range) {
  
  range_start <- age_range[1]
  range_end <- age_range[length(age_range)]
  
  if (length(age_range) == 1) return (as.character(range_start))
  return (paste(range_start, range_end, sep = "-"))
}

get_age_group_life_expectancy <- function(age_range, life_table) {
  
  life_table %>%
    dplyr::filter(age %in% age_range) %>%
    dplyr::summarise(M_life_expectancy = mean(M_life_expectancy_at_age), F_life_expectancy = mean(F_life_expectancy_at_age))
}



##################
# Importing data #
##################

life_table_var_names <- c("M_age", "M_age_group_mortality_rate", "M_mortality_within_1year", "M_survivors_from100k_births", "M_deaths_from_100k_births", "M_life_expectancy_at_age", "empty",
                          "F_age", "F_age_group_mortality_rate", "F_mortality_within_1year", "F_survivors_from100k_births", "F_deaths_from_100k_births", "F_life_expectancy_at_age")

UK_covid_deaths_var_names <- c("age_group", "M_population", "M_pop_percentage", "F_population", "F_pop_percentage", "both_popultaion", "both_pop_percentage",
                               "M_covid_cumulative_deaths", "M_deaths_percentage", "F_covid_cumulative_deaths", "F_deaths_percentage", "unknown", "both_covid_cumulative_deaths", "both_deaths_percentage")

UK_life_table_raw <- rio::import(UK_life_tables_link,
                                 format = "xlsx",
                                 which = "2018-2020",
                                 skip = 5,
                                 .name_repair = ~ life_table_var_names)

UK_covid_deaths_raw <- rio::import(UK_covid_deaths_link,
                                 format = "xlsx",
                                 which = "ONS_WeeklyOccurrenceDeaths",
                                 range = readxl::cell_limits(c(7, 1), c(27, 14)),
                                 .name_repair = ~ UK_covid_deaths_var_names)



##################
# Analyzing data #
##################

life_table <- UK_life_table_raw %>%
  dplyr::select(M_age, M_life_expectancy_at_age, F_life_expectancy_at_age) %>%
  dplyr::rename(age = M_age, male = M_life_expectancy_at_age, female = F_life_expectancy_at_age) %>%
  tidyr::pivot_longer(cols = c("male", "female"), names_to = "sex", values_to = "life_expectancy")

age_groups <- list(0,
                   1:4,
                   5:9,
                   10:14,
                   15:19,
                   20:24,
                   25:29,
                   30:34,
                   35:39,
                   40:44,
                   45:49,
                   50:54,
                   55:59,
                   60:64,
                   65:69,
                   70:74,
                   75:79,
                   80:84,
                   85:89,
                   90:100)

life_expectancy_by_age_group <- life_table %>%
  dplyr::mutate(age_group = get_age_groups(age, age_groups) %>% purrr::map_chr(get_age_group_name)) %>%
  dplyr::group_by(age_group, sex) %>%
  dplyr::summarise(age_group = unique(age_group), sex = unique(sex), life_expectancy = mean(life_expectancy), .groups = "drop") %>%
  dplyr::mutate(age_group = dplyr::case_when(age_group == "90-100" ~ "90+", TRUE ~ age_group))
  # assume that life expectancy for 90+ group is the same as life expectancy for 90-100 group

covid_death_totals <- UK_covid_deaths_raw %>%
  dplyr::summarise(male = sum(M_covid_cumulative_deaths), female = sum(F_covid_cumulative_deaths)) %>%
  dplyr::mutate(both = male + female)

covid_deaths <- UK_covid_deaths_raw %>%
  dplyr::select(age_group, M_covid_cumulative_deaths, F_covid_cumulative_deaths) %>%
  dplyr::rename(male = M_covid_cumulative_deaths, female = F_covid_cumulative_deaths) %>%
  tidyr::pivot_longer(cols = c("male", "female"), names_to = "sex", values_to = "cumulative_deaths") %>%
  dplyr::mutate(deaths_proportion = cumulative_deaths / covid_death_totals$both) %>%
  dplyr::inner_join(life_expectancy_by_age_group, by = c("age_group", "sex"))

# % of covid deaths occurred in a certain age group * life expectancy for this age group
avg_lost_years <- sum(covid_deaths$life_expectancy * covid_deaths$deaths_proportion)

# [5. Jan 2022] avg_lost_years result: 10.54055
