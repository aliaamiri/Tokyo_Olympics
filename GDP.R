library(tidyverse)
library(jsonlite)
library(rvest)

Tokyo_Medals_2021 <- read_csv("Tokyo Medals 2021.csv")
population_by_country_2020 <- read_csv("population_by_country_2020.csv")
gdp_per_capita_nominal <- read_csv("gdp_per_capita_nominal.csv",
skip = 2)
gdp_per_capita_ppp <- read_csv("API_NY.GDP.PCAP.PP.KD_DS2_en_csv_v2_2765238.csv",
skip = 4)
gdp_nom <- gdp_per_capita_nominal[-c(3:34)]
names(gdp_nom)[c(1, 2)] <- c("Country", "Country_Code")
gdp_nom$Country[c(113, 123, 203, 127, 24, 228, 
                  42, 97, 222, 68 )] <- c("Iran","Kyrgyzstan", "Russia", 
                                          "South Korea", "Bahamas", "Syria", 
                                          "Côte d'Ivoire", "Hong Kong", "Slovakia",
                                          "Egypt")
gdp_nom <- pivot_longer(gdp_nom, -(1:2), names_to = "year", values_to = "gdp_nom")
gdp_nom <- gdp_nom %>% group_by(Country) %>% filter (year %in% c(2017, 2018, 2019, 2020)) %>%
  mutate(four_year_gdp_nom = mean(gdp_nom, na.rm = TRUE)) %>% ungroup()

gdp_ppp <- gdp_per_capita_ppp[-c(3:34, 66)]
names(gdp_ppp)[c(1, 2)] <- c("Country", "Country_Code")
gdp_ppp$Country[c(113, 123, 203, 127, 24, 
                  228, 42, 97, 222, 68 )] <- c("Iran", "Kyrgyzstan", "Russia", 
                                               "South Korea", "Bahamas", "Syria", 
                                               "Côte d'Ivoire", "Hong Kong", "Slovakia",
                                               "Egypt")
gdp_ppp <- pivot_longer(gdp_ppp, -(1:2), names_to = "year", values_to = "gdp_ppp")
gdp_ppp <- gdp_ppp %>% group_by(Country) %>% filter (year %in% c(2017, 2018, 2019, 2020)) %>%
  mutate(four_year_gdp_ppp = mean(gdp_ppp, na.rm = TRUE)) %>% ungroup()

ol <- Tokyo_Medals_2021
ol$Country[c(1, 2, 4, 5,
16, 27, 34, 49, 93)] <- c("United States", "China", "United Kingdom",
"Russia", "South Korea", "Iran", "Taiwan", "Hong Kong", "Syria")
ol <- ol %>% mutate(year = as_factor(2020))
pop <- population_by_country_2020[-c(3:7)] %>% mutate(year = as_factor(2020))
names(pop)[c(1, 2)] <- c("Country", "population")
pop$Country[c(16, 21, 53, 57, 86, 121, 144, 156, 172, 187, 194, 196, 199, 211)] <- 
  c("Democratic Republic of the Congo", "Great Britain", "Ivory Coast", "Chinese Taipei",
    "Czech Republic", "Palestine", "The Gambia", "East Timor", "Cape Verde", 
    "São Tomé and Príncipe", "Federated States of Micronesia", "Saint Vincent and the Grenadines", "Virgin Islands",
    "Saint Kitts and Nevis")
dat <- left_join(ol, pop, by = c("Country", "year"))
dat[92, 8] <- 2597100
dat[43, 8] <- 1935259
#summary(dat)

dat <- left_join(dat, gdp_nom, by = c("Country", "year"))
dat <- left_join(dat, gdp_ppp, by = c("Country", "year"))
dat <- dat %>% mutate(medal_per_mil = Total / population * 10^6)

names(dat)[c(2, 3, 4, 11)] <- c("gold_medal", "silver_medal", "bronze_medal", "urban_pop_percent")
#dat[3, 14] <- 39375 #Japan's GDP per capita (nominal)
dat$urban_pop_percent <- parse_number(dat$urban_pop_percent)

#ggplot for Mr Kadivar

ggplot(dat, aes(gdp_nom, medal_per_mil, label = Country)) +
geom_point()+
scale_y_log10() +
theme_minimal()

#web scraping

dataset_full <- read.csv("dataset_full")
names(dataset_full) <- (c("index", "noc", "name", "dicipline", "gender", "age", 
                          "ranks", "x2", "x3", "x4", "x5", "x6", "x7"))
dataset_full_nas <- dataset_full %>% filter(age == 0)
names_nas <- dataset_full_nas$name
js_page <- fromJSON("https://olympics.com/tokyo-2020/olympic-games/en/results/all-sports/zzje001a.json")
links_and_names <- js_page %>% .[[1]] %>% .[4:5] 

links_and_names[[2]] <- str_replace(links_and_names[[2]], "\\.\\./\\.\\./\\.\\.", "https://olympics.com/tokyo-2020/olympic-games")
links_and_names <- links_and_names %>% filter(name %in% names_nas)
get_data <- function(link){
  page <- read_html(link) %>%
    html_elements("td.text-center:nth-child(3) , .col-md-6:nth-child(1) div+ div") %>%
    html_text
  return(page)
  
}
data_full_nas <- pbsapply(links_and_names[[2]], FUN = get_data, USE.NAMES = FALSE)

dataset_full <- dataset_full %>% group_by(dicipline, gender) %>% mutate(max_rank = max(ranks, na.rm = TRUE))

ages_nas <- data_full_nas %>% str_extract_all("Age:\\\\r\\d+") %>% str_extract_all("\\d+") %>%
  unlist()
gender_nas <- data_full_nas %>% str_extract_all("(Male)|(Female)") %>% unlist() 
ranks_nas <- data_full_nas %>% sapply(., FUN = function(link) str_extract_all(link, "\\\\rAge:\\\\r\\d+")) %>% str_c(sep = "")
str_replace_all(data_full_nas, "\\\\rAge:\\\\r\\d+", "") %>% str_extract_all("\\d+", simplify = TRUE) %>% cbind(links_and_names[[1]],.)

str_replace_all(data_full_nas, "\\\\rAge:\\\\r\\d+", "") %>% str_extract("$\\\\r\\d+")
str_extract_all(data_full_nas, "\\d+$")
str_view_all(data_full_nas[[121]], "\\d+$")
d <- data_full_nas %>% sapply(FUN = function(c) str_replace(c, "\\r|\\r ", "")) %>% sapply( FUN = function(c) str_extract(c, "\\d+$")) %>%
  sapply(FUN = function(c) parse_number(c, na = "NA")) %>% sapply(FUN = function(c) return(c[!is.na(c)]))
length(d)
d[[1]] 
sapply(d, FUN = function(c) return(c[!is.na(c)])) %>% rbind() %>% View 

n.obs <- sapply(d, length)
seq.max <- seq_len(max(n.obs))
mat <- t(sapply(d, "[", i = seq.max))
age_ranks_na <- data.frame(mat)
names(age_ranks_na) <- c("age", "ranks", "x2", "x3")
age_ranks_na
dataset_age_gender_rank_nas <- data.frame(name = links_and_names[[1]], gender = gender_nas, age_ranks_na)
left_join(dataset_full, dataset_age_gender_rank_nas, by = c("name" = "name", "age" = "age", "gender" = "gender", "ranks" = "ranks", "x2" = "x2", "x3" = "x3" )) %>% view
merge.data.frame(dataset_full, dataset_age_gender_rank_nas, by = "age") %>% view
names_na <- dataset_age_gender_rank_nas$name
dataset_full$age[dataset_full$name %in% names_na ] <- dataset_age_gender_rank_nas$age
dataset_full$gender[dataset_full$name %in% names_na ] <- dataset_age_gender_rank_nas$gender
dataset_full$ranks[dataset_full$name %in% names_na ] <- dataset_age_gender_rank_nas$ranks
dataset_full$x2[dataset_full$name %in% names_na ] <- dataset_age_gender_rank_nas$x2
dataset_full$x3[dataset_full$name %in% names_na ] <- dataset_age_gender_rank_nas$x3

saveRDS(dataset_full, "dataset_2020_summer")
write.csv(dataset_full, "dataset_2020_summer.csv", na = "NA")

#Implementing points for each athlete

data_wide <- read.csv("dataset_2020_summer.csv")
data_wide <- data_wide[-1]
names(data_wide)[7] <- "x1"
data <- data_wide %>% pivot_longer(7:13, names_to = "event", values_to = "rank") %>%
  group_by(gender, dicipline) %>% mutate(max_rank = max(rank, na.rm = TRUE),
                                         athletes_number = sum(event =="x1", na.rm = TRUE),
                                         equivalence_point = (max_rank +athletes_number) / 2) %>%
  ungroup()

data <- data %>% mutate(points = case_when(event == "x1" & is.na(rank) ~ 1000 / equivalence_point,
                                 !rank %in% c(1, 2, 3) ~ 1000 / rank,
                                 rank == 1 ~ 1000,
                                 rank == 2 ~ 611.7,
                                 rank == 3 ~ 397.8))
data <- data %>% filter(!is.na(points)) 
#write.csv(data,"2020_Olympics_points_included.csv")

#data %>% filter(event == "x1") %>% group_by(noc) %>% summarise(sum(points)) %>% view


#Incentives for 2020 olympic medalists

Incentives_for_Olympic_medalists_by_country <- read_html("https://en.wikipedia.org/wiki/Incentives_for_Olympic_medalists_by_country")
Incentives_for_Olympic <- Incentives_for_Olympic_medalists_by_country %>% html_elements("table") %>% .[[5]] %>% html_table()
Incentives_for_Olympic2020 <- Incentives_for_Olympic[-5]
Incentives_for_Olympic2020 <- Incentives_for_Olympic2020 %>% mutate(Gold = parse_number(Gold), Silver = parse_number(Silver),
                                      Bronze = parse_number(Bronze)) %>% filter(Gold != 0)
Incentives_for_Olympic2020 <- Incentives_for_Olympic2020 %>% mutate(Silver_prop = Silver / Gold,
                                                                    Bronze_prop = Bronze / Gold)
Silver_modifier <- mean(Incentives_for_Olympic2020$Silver_prop) 
Bronze_modifier <- mean(Incentives_for_Olympic2020$Bronze_prop)

#List of IOC country codes

country_code_page <- read_html("https://en.wikipedia.org/wiki/List_of_IOC_country_codes")
country_codes <- country_code_page %>% html_elements("table") %>% .[1] %>% 
  html_table() %>% .[[1]] 
country_codes <- country_codes[1:2]
country_codes$Code[1] <- "AFG"
names(country_codes)[2] <- "National_Olympic_Committee" 
country_codes <- rbind(country_codes, data.frame(Code = c("EOR", "ROC"), National_Olympic_Committee = c("Refugee Olympic Team", "Russia")))
names(data)[2] <- "Code"
country_codes[[2]][189] <- "Chinese Taipei"
data <- left_join(data, country_codes, by = "Code")

#sport codes

sport_codes_2020_summer <- read.csv("sport_codes.csv", strip.white = TRUE)
?read.csv
names(data)[4] <- "discipline_code"
names(sport_codes_2020_summer) <- c("discipline_code", "discipline")
data <- left_join(data, sport_codes_2020_summer, by = "discipline_code")

#-----------------------------Analysis--------------------------#

#US Athletics alternate athletes which are inconsistent in different olympics.com
# datasets

us_data_ahtletics_name <- data %>% filter(Code == "USA", discipline == "Athletics") %>% summarise(name = unique(name)) %>% pull(name)
usathletics <- read_csv("US_athletics_team_names.csv")
us_athletics_names <- usathletics[[1]][!is.na(usathletics[[1]])]
us_data_ahtletics_name[!us_data_ahtletics_name %in% us_athletics_names]

data %>% group_by(National_Olympic_Committee) %>% summarise(sum(points) / length(unique(index))) %>% view
world_population <- pop[c(1, 2)]
names(data)[13] <- "Country"
database <- left_join(data, world_population, by = "Country") 

database %>% group_by(Country) %>% summarise(unique((sum(points) ^ 2) / (length(unique(index)) * population))) %>% view

four_year_gdp <- left_join(unique(gdp_nom[c(2, 5)]), unique(gdp_ppp[c(2, 5)]))
four_year_gdp$Country[four_year_gdp$Country == c("United Kingdom", )] <- 
  c("Great Britain", "Hong Kong, China")
left_join(database, four_year_gdp) %>% view

write.csv(database, "database_summer_2020.csv")

pop_WB <- read_csv("API_SP.POP.TOTL_DS2_en_csv_v2_2763937.csv", skip = 4)
pop_WB <- pop_WB[c(1, 65)]
names(pop_WB) <- c("Country_Name", "Population_2020")
missing_pop_WB <- country_codes[[2]][!country_codes[[2]] %in% pop_WB[[1]]]

country_codes[[2]][!country_codes[[2]] %in% pop_WB_corrected[[1]]]

country_name_correction <- data.frame("Country" = missing_pop_WB, "Country_Name" = WB_enteties)
WB_enteties <- pop_WB[[1]][c(24, 32, 45, 42, 44, NA, 48, 68, 80, 87, 82, 97, 113, 257, 123, 127, 130, 134, NA,
  194, NA, 126, 220, 222, 228, 238, NA, 255, 254, 263, NA, 203)]
missing_rows_pop_WB <- data.frame(Country_Name = c("Cook Islands", "Palestine", 
                                                   "Chinese Taipei"),
                                  Population_2020 = c(17600, 4750000, 23561236))

country_name_correction <- country_name_correction %>% filter(!is.na(Country_Name))
pop_WB[[1]][c(24, 32, 45, 42, 44, 48, 68, 80, 87, 82, 97, 113, 257, 123, 127, 130, 134,
              194, 126, 220, 222, 228, 238, 255, 254, 263, 203)] <- country_name_correction[[1]]
pop_WB_corrected <- rbind(pop_WB, missing_rows_pop_WB)
names(pop_WB_corrected)[1] <- "Country" 
gdp_four_year <- left_join(gdp_nom, gdp_ppp) %>% filter(year == 2020) %>% select(-2)
gdp_four_year[[1]][c(24, 32, 45, 42, 44, 48, 68, 80, 87, 82, 97, 113, 257, 123, 127, 130, 134,
              194, 126, 220, 222, 228, 238, 255, 254, 263, 203)] <- country_name_correction[[1]]
missing_rows_gdp_WB <- data.frame(Country = c("Cook Islands", "Palestine", "Chinese Taipei"),
                                  year = rep("2020", 3), gdp_nom = c(NA, 3240, NA),
                                  four_year_gdp_nom = c(20683, 3520, 26291), 
                                  gdp_ppp = c(NA, 5690, 55078),
                                  four_year_gdp_ppp = c(NA, 6318, 52898))
gdp_four_year_complete <- rbind(gdp_four_year, missing_rows_gdp_WB)
left_join(data, pop_WB_corrected) %>% view
dataset_2020_summer <- left_join(data, gdp_four_year_complete, by = "Country")
dataset_2020_summer <- left_join(dataset_2020_summer, pop_WB_corrected)
names(dataset_2020_summer)[c(2, 13, 20)] <- c("country_code", "country", "population")
write.csv(dataset_2020_summer, "dataset_2020_summer.csv")

#gender equality

dataset_2020_summer %>% filter(event == "x1") %>% group_by(country) %>% 
  summarise(sum(gender == "Female") / sum(gender == "Male")) %>% view

dataset_2020_summer %>% group_by(country) %>% summarise (point = unique(sum(points) / population )) %>%
