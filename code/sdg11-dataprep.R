library(dplyr)
library(readxl)
library(countrycode)
library(tidyr)

countries <- wbstats::wb_countries()

# GDP and urbanisation
waste.gdp.raw <- read_excel("../inputs/waste_charts_data.xlsx", sheet = "waste, gdp & urbanization")
waste.gdp <- select(waste.gdp.raw, iso3c, gdp, pop = population_population_number_of_people, waste = total_msw_total_msw_generated_tons_year, sharepopurban = `urban population (%)`, wastepercap = wastegen_pc) %>%
  mutate(gdp = as.numeric(gdp)) %>%
  mutate(pop = as.numeric(pop)) %>%
  mutate(waste = as.numeric(waste)) %>%
  mutate(sharepopurban = as.numeric(sharepopurban)) %>%
  mutate(wastepercap = as.numeric(wastepercap)*1000) %>%
  filter(!is.na(gdp) & !is.na(waste) & !is.na(sharepopurban) & !is.na(wastepercap))

write.csv(waste.gdp, file = "../outputs/waste_gdp_urban.csv", row.names = FALSE)

# Waste and population projections
waste.proj.raw <- read_excel("../inputs/waste_charts_data.xlsx", sheet = "waste & population - projection", range="A1:Z218")
waste.proj <- select(waste.proj.raw, 1, 7:10, 23:26)
colnames(waste.proj) <- c("country", "was2020", "was2030", "was2040", "was2050", "pop2020", "pop2030", "pop2040", "pop2050")

waste.world <- summarise(waste.proj, across(where(is.numeric), ~ sum(.x, na.rm = TRUE)))
waste.world$iso3c = "WLD"
waste.countries <- mutate(waste.proj, iso3c = countrycode(country, origin = "country.name", destination = "iso3c")) %>%
  filter(!is.na(iso3c)) %>%
  select(-country)

waste.all <- rbind(waste.world, waste.countries) %>%
  mutate(was2050 = round(was2050/was2020*100, 1)) %>%
  mutate(was2040 = round(was2040/was2020*100, 1)) %>%
  mutate(was2030 = round(was2030/was2020*100, 1)) %>%
  mutate(was2020 = 100) %>%
  mutate(pop2050 = round(pop2050/pop2020*100, 1)) %>%
  mutate(pop2040 = round(pop2040/pop2020*100, 1)) %>%
  mutate(pop2030 = round(pop2030/pop2020*100, 1)) %>%
  mutate(pop2020 = 100) %>%
  pivot_longer(cols = 1:8, names_to = 'catyear', values_to = 'growth') %>%
  mutate(year = substr(catyear, nchar(catyear)-3, nchar(catyear))) %>%
  mutate(cat = substr(catyear, 1, 3)) %>%
  select(-catyear) %>%
  pivot_wider(names_from = cat, values_from = growth)

write.csv(waste.all, file = "../outputs/waste_pop_projections.csv", row.names = FALSE)

# Global total waste
waste.proj.agg <- read_excel("../inputs/waste_charts_data.xlsx", sheet = "waste - aggregates")
colnames(waste.proj.agg) <- c("indicator", "year", "HIC", "UMC", "LMC", "LIC", "unclassified")
waste.proj.agg.long <- pivot_longer(waste.proj.agg, cols = 3:7, names_to = 'income', values_to = 'value') %>%
  mutate(indicator = case_when(
    indicator == "Total waste" ~ "total_waste",
    indicator == "Waste per capita" ~ "per_capita_waste"
  ))
total_waste_income <- filter(waste.proj.agg.long, indicator == "total_waste") %>%
  select(-indicator)
write.csv(total_waste_income, file = "../outputs/total_waste_income.csv", row.names = FALSE)

# Per capita waste, global and countries
percapita_waste_income <- filter(waste.proj.agg.long, indicator == "per_capita_waste") %>%
  select(income, year, value)
colnames(percapita_waste_income) <- c("iso3c", "year", "percap_waste")

waste.proj.raw <- read_excel("../inputs/waste_charts_data.xlsx", sheet = "waste & population - projection")
waste.proj <- select(waste.proj.raw, 1, 15:18, 23:26)
colnames(waste.proj) <- c("country", "waste_2020", "waste_2030", "waste_2040", "waste_2050", "pop_2020", "pop_2030", "pop_2040", "pop_2050")
percapita_waste_countries <- mutate(waste.proj, iso3c = countrycode(country, origin = "country.name", destination = "iso3c")) %>%
  filter(!is.na(iso3c)) %>%
  mutate(percap_waste_2020 = waste_2020/pop_2020) %>%
  mutate(percap_waste_2030 = waste_2030/pop_2030) %>%
  mutate(percap_waste_2040 = waste_2040/pop_2040) %>%
  mutate(percap_waste_2050 = waste_2050/pop_2050) %>%
  select(10:14) %>%
  pivot_longer(cols = 2:5, names_to = "year", values_to = "percap_waste") %>%
  mutate(year = as.numeric(gsub("percap_waste_", "", year)))

percapita_waste_all <- rbind(percapita_waste_income, percapita_waste_countries)
percapita_waste_all$percap_waste <- round(percapita_waste_all$percap_waste)

write.csv(percapita_waste_all, file="../outputs/percapita_waste_income_countries.csv", row.names = FALSE)

# Waste composition
waste.comp.agg <- read_excel("../inputs/waste_charts_data.xlsx", sheet = "waste composition - aggregates") %>%
  filter(Region %in% c('High-Income', 'Upper-Middle Income', 'Lower-Middle Income', 'Low-Income')) %>%
  filter(Indicator == 'Waste composition share') %>%
  pivot_longer(cols = 3:11, names_to = "cat", values_to = 'share') %>%
  mutate(share = share*100) %>%
  filter(!is.na(share)) %>%
  mutate(cat = case_when(
    cat == 'Food & Green' ~ 'foodgreen',
    cat == 'Glass' ~ 'glass',
    cat == 'Metal' ~ 'metal',
    cat == 'Other' ~ 'other',
    cat == 'Paper & Cardboard' ~ 'paper',
    cat == 'Plastic' ~ 'plastic',
    cat == 'Rubber & Leather' ~ 'rubber',
    cat == 'Wood' ~ 'wood'
  )) %>%
  pivot_wider(names_from = cat, values_from = share) %>%
  mutate(income = case_when(
    Region == 'High-Income' ~ 'HIC',
    Region == 'Upper-Middle Income' ~ 'UMC',
    Region == 'Lower-Middle Income' ~ 'LMC',
    Region == 'Low-Income' ~ 'LIC'
  ))

income.shares <- read_excel("../inputs/waste_charts_data.xlsx", sheet = "waste - aggregates", range="C1:F2")
colnames(income.shares) <- c("HIC", "UMC", "LMC", "LIC")
income.shares.long <- pivot_longer(income.shares, cols = 1:4, names_to = 'income', values_to = 'waste') %>%
  mutate(share = round(waste/sum(waste)*100, 1)) %>%
  mutate(cumshare = cumsum(share) - share)

waste.comp.agg.shares <- left_join(waste.comp.agg, income.shares.long, by = "income") %>%
  select(-Indicator, -Region)

write.csv(waste.comp.agg.shares, file = "../outputs/waste_composition.csv", row.names = FALSE)

# Waste collection
# Coverage
waste.collection.raw <- read_excel("../inputs/waste_charts_data.xlsx", sheet = "collection & treatment -country")
waste.collection.rate <- select(waste.collection.raw, iso3c, collection_rate = waste_collection_coverage_total_percent_of_population) %>%
  filter(collection_rate != 'NA') %>%
  filter(iso3c != "VEN")

waste.collection.aggregates <- read_excel("../inputs/waste_charts_data.xlsx", sheet = "collection& treatment-aggregate", range = "B1:D12") %>%
  select(-Rural) %>%
  rename(iso3c = `Collection Rates by Region`, collection_rate = Total) %>%
  filter(iso3c %in% c("HIC", "LIC", "UMC", "LMC")) %>%
  mutate(collection_rate = collection_rate*100)

waste.collection.rate <- rbind(waste.collection.rate, waste.collection.aggregates)
write.csv(waste.collection.rate, file = "../outputs/waste_collection_rate.csv", row.names = FALSE)

# Treatment
treatment <- read_excel("../inputs/waste_charts_data.xlsx", sheet = "collection& treatment-aggregate", range="A16:I28")
treatment.income <- tail(treatment, 4)
colnames(treatment.income) <- c("income", "digestion", "compost", "landfill_controlled", "incineration", "landfill_unspecified", "dump", "recycling", "landfill_sanitary")
treatment.income <- mutate(treatment.income, income = case_when(
  income == 'High-Income' ~ 'HIC',
  income == 'Upper-Middle Income' ~ 'UMC',
  income == 'Lower-Middle Income' ~ 'LMC',
  income == 'Low-Income' ~ 'LIC'
))
write.csv(treatment.income, file="../outputs/waste_treatment_income.csv")

# Plastic
plastic.total <- read_excel("../inputs/waste_charts_data.xlsx", sheet = "global plastic waste projection")
colnames(plastic.total) <- c("year", "generation")
plastic.fate <- read_excel("../inputs/waste_charts_data.xlsx", sheet = "plastic waste projection fate")
colnames(plastic.fate) <- c("year", "fate", "value")
plastic.fate <- pivot_wider(plastic.fate, names_from = fate, values_from = value)
colnames(plastic.fate) <- c("year", "incinerated", "landfilled", "mismanaged", "recycled", "littered")

plastic.all <- left_join(plastic.total, plastic.fate, by = "year") %>%
  arrange(year)
write.csv(plastic.all, file="../outputs/plastic_waste.csv")

# Plastic waste trade
income <- select(countries, iso3c, income_level_iso3c)

trade.raw <- read_excel("../inputs/waste_charts_data.xlsx", sheet = "plastic waste bilateral trade")
trade <- select(trade.raw, from = ReporterISO3, to = PartnerISO3, year = Year, weight = NetWeight.in.KGM) %>%
  filter(from != "EUN") %>%
  filter(weight != 'NA') %>%
  mutate(weight = as.numeric(weight)) %>%
  left_join(income, by=c("from"="iso3c")) %>%
  rename(from_income = income_level_iso3c) %>%
  left_join(income, by=c("to"="iso3c")) %>%
  rename(to_income = income_level_iso3c) %>%
  ungroup()

trade.agg <- group_by(trade, year, from_income, to_income) %>%
  summarise(weight = sum(weight), .groups = "drop")

trade.agg.17.22 <- filter(trade.agg, year %in% c(2017, 2022)) %>%
  filter(!is.na(from_income)) %>%
  filter(!is.na(to_income)) %>%
  filter(from_income != 'INX') %>%
  filter(to_income != 'INX') %>%
  mutate(from_income = paste(from_income, "_source", sep = "")) %>%
  mutate(to_income = paste(to_income, "_target", sep = ""))
colnames(trade.agg.17.22) <- c("year", "source", "target", "value")

write.csv(trade.agg.17.22, file="../outputs/plastic_waste_trade.csv")
