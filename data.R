# Organize data

library(httr)
set_config(config(ssl_verifypeer = 0L))

library(tidyverse)
library(rvest)
library(stringr)
library(lubridate)


base_url <- 'https://www.rita.dot.gov/bts/sites/rita.dot.gov.bts/files/subject_areas/airline_information/passengers_denied_confirmed_space_report/'

all_data <- data.frame()

col_names <- c("CARRIER", "1(a)", "1(b)", "2(a)", "2(b)", "2(c)", "3", "4", "5", 
               "6(a)", "6(b)", "7", "8(a)", "8(b)", "8(c)")

for (year in 2008:2010){
  for (q in 1:4){
    data <- read_csv(paste0(base_url, year, '/csv/', year, '_', q, 'q.csv'), skip = 3, col_names = col_names)
    data$release <- paste0(year, "_q", q)
    all_data <- rbind(all_data, data)
  }
}

# Add Q1 2011
data_2011 <- read_csv("https://www.rita.dot.gov/bts/sites/rita.dot.gov.bts/files/subject_areas/airline_information/passengers_denied_confirmed_space_report/2011/csv/2011_1q.csv", skip = 3, col_names = col_names)
data_2011$release <- "2011_q1"
all_data <- rbind(all_data, data_2011)

#### New base url
base_url <- 'http://www.rita.dot.gov/bts/sites/rita.dot.gov.bts/files/'

for (year in 2012:2015){
  for (q in 1:4){
    data <- read_csv(paste0(base_url, year, '_', q, 'q.csv'), skip = 3, col_names = col_names)
    data$release <- paste0(year, "_q", q)
    all_data <- rbind(all_data, data)
  }
}

miscell_quarters <- c("2011_2q", "2011_3q", "2011_4q", 
                      "2016_1q")

for (m_q in miscell_quarters) {
  data <- read_csv(paste0(base_url, m_q, ".csv"), skip = 3, col_names =  col_names)
  data$release <- m_q
  all_data <- rbind(all_data, data)
}


## Filter out footnotes
clean_data <- all_data %>% 
  filter(is.na(as.numeric(substr(CARRIER, 1, 1)))) %>% 
  rename(carrier = CARRIER, 
         pass_trans = `1(a)`,
         pass_no_trans = `1(b)`,
         no_comp_new_flight = `2(a)`,
         no_comp_small_equip = `2(b)`,
         no_comp_fail_comply = `2(c)`,
         denied_boarding = `3`,
         receive_comp = `4`,
         volunteer_for_pay = `5`,
         upgrades = `6(a)`,
         downgrades = `6(b)`,
         total_boardings = `7`,
         comp_alt_trans = `8(a)`,
         comp_no_trans = `8(b)`,
         comp_volunteers = `8(c)`) 

# TODO
# Dedupe carrier names
# Remove , and $ charactersx
                      
clean_data2 <- clean_data %>% 
  mutate(carrier = str_trim(str_replace(str_replace(str_replace(carrier, "Air Lines", ""), "Airlines", ""), "Airways", ""))) %>% 
  mutate(carrier = ifelse(carrier == "Air Tran", "AirTran", carrier),
         carrier = ifelse(carrier == "SkyWest", "Skywest", carrier),
         carrier = ifelse(carrier == "Virgin America", "Virgin", carrier)) %>% 
  filter(!is.na(carrier)) %>% 
  gather(var, data, -c(carrier, release)) %>% 
  mutate(data = gsub(",", "", gsub("\\$", "", data)),
         data = ifelse(data == "n/a", NA, data),
         data = as.numeric(data)) %>% 
  spread(var, data) %>% 
  separate(release, into = c("year", "quarter")) %>% 
  mutate(quarter = ifelse(quarter %in% c("q1", "1q"), "1/1/", 
                          ifelse(quarter %in% c("q2", "2q"), "4/1/",
                                 ifelse(quarter %in% c("q3", "3q"), "7/1/", 
                                        ifelse(quarter %in% c("q4", "4q"), "10/1/", quarter))))) %>% 
  mutate(date = mdy(paste0(quarter, year))) %>% 
  select(carrier, date, 4:17) %>% 
  mutate(year = year(date),
         involuntary_db_per_10k = denied_boarding / (total_boardings/10000),
         voluntary_db_per_10k = volunteer_for_pay / (total_boardings/10000))

############### Summary statistics ###############

# currently operating airlines
current_carriers <- clean_data2 %>% 
  filter(date == "2016-01-01") %>% 
  select(carrier) %>% 
  unlist() %>% 
  as.character()

# involuntary DBs over time
clean_data2 %>%
  filter(carrier %in% current_carriers) %>% 
  ggplot(aes(x = date, group = carrier, color = carrier)) + 
  geom_line(aes(y = involuntary_db_per_10k)) 

# voluntary DBs over time
clean_data2 %>%
  filter(carrier %in% current_carriers) %>% 
  ggplot(aes(x = date, group = carrier, color = carrier)) + 
  geom_line(aes(y = voluntary_db_per_10k)) 

# avg boardings per quarter
clean_data2 %>% 
  group_by(carrier) %>% 
  summarize(avg_boarding = mean(total_boardings)) %>% 
  arrange(desc(avg_boarding))

# overall IDB rate


overall_db_rates <- clean_data2 %>% 
  filter(carrier %in% current_carriers) %>% 
  group_by(carrier) %>% 
  summarize(total_boardings = sum(total_boardings),
            involuntary_db = sum(denied_boarding),
            voluntary_db = sum(volunteer_for_pay),
            involuntary_db_per_10k = involuntary_db / (total_boardings/10000),
            voluntary_db_per_10k = voluntary_db / (total_boardings/10000)
            ) %>% 
  arrange(desc(involuntary_db_per_10k))

# compensation 
ggplot(clean_data2, aes(x = receive_comp, y = comp_no_trans)) + 
  geom_point() + ggtitle("Compensation for involuntary denied boarding")
  
ggplot(clean_data2, aes(x = volunteer_for_pay, y = comp_volunteers)) + 
  geom_point() + ggtitle("Compensation for voluntary denied boarding")

# yearly aggregates
clean_data2 %>% 
  mutate(year = year(date)) %>% 
  group_by(year) %>% 
  summarize(total_boardings = sum(total_boardings),
            involuntary_db = sum(denied_boarding),
            voluntary_db = sum(volunteer_for_pay),
            involuntary_db_per_10k = involuntary_db / (total_boardings/10000),
            voluntary_db_per_10k = voluntary_db / (total_boardings/10000)
            ) %>% View
  
# yearly aggregates by carrier
carrier_by_year <- clean_data2 %>% 
  # filter(carrier %in% current_carriers) %>% 
  mutate(year = year(date)) %>% 
  group_by(carrier, year) %>% 
  summarize(total_boardings = sum(total_boardings),
            involuntary_db = sum(denied_boarding),
            voluntary_db = sum(volunteer_for_pay),
            involuntary_db_per_10k = involuntary_db / (total_boardings/10000),
            voluntary_db_per_10k = voluntary_db / (total_boardings/10000)
  )

by_year <- carrier_by_year %>% 
  group_by(year) %>% 
  summarize(
            avg_involuntary_db_per_10k = mean(involuntary_db_per_10k),
            voluntary_db_per_10k = mean(voluntary_db_per_10k)
  )
  

invol_chart <- carrier_by_year %>% 
  ggplot(aes(x = year, group = carrier, color = carrier)) +
  geom_line(aes( y = involuntary_db_per_10k)) + 
  xlab("Year") + ylab("Per 10,000 passenergers") + 
  ggtitle("Involuntary denial of boarding by airline")

invol_chart
ggsave(filename = "invol_chart.png", plot = invol_chart)

vol_chart <- carrier_by_year %>% 
  ggplot(aes(x = year, group = carrier, color = carrier)) +
  geom_line(aes( y = voluntary_db_per_10k)) + 
  xlab("Year") + ylab("Per 10,000 passenergers") + 
  ggtitle("Voluntary denial of boarding by airline")

vol_chart
ggsave(filename = "vol_chart.png", plot = vol_chart)

options(signif=2)

graph_data <- overall_db_rates %>% 
  select(1, 5, 6) %>%
  mutate(involuntary_db_per_10k = round(involuntary_db_per_10k, digits = 2),
         voluntary_db_per_10k = round(voluntary_db_per_10k, digits = 2)) %>% 
  rename(Carrier = carrier,
         `Involuntary DB rate` = involuntary_db_per_10k,
         `Voluntary DB rate` = voluntary_db_per_10k) 

# IDB scatter
graph_data %>% 
  ggplot(aes(x = `Involuntary DB rate`, y = `Voluntary DB rate`, label = Carrier)) +
  geom_point() + geom_text(hjust = 0.4) + 
  ggtitle("U.S. Airline Voluntary and Involuntary Denial of Boarding \nPer 10,000 passengers, 2008-2016 average")

## United and average DB rates over time
united_comp <- carrier_by_year %>% 
  filter(carrier == "United") %>% 
  left_join(by_year, by = "year") %>% 
  select(1, 2, 6, 7, 11, 12)

united_comp %>% 
  ggplot(aes(x = year)) + 
  geom_line(aes(y = involuntary_db_per_10k.x)) +
  geom_line(aes(y = involuntary_db_per_10k.y)) +
  geom_line(aes(y = voluntary_db_per_10k.x)) +
  geom_line(aes(y = voluntary_db_per_10k.y))
  
         
         
