
# SCRIPT SETUP ------------------------------------------------------------

# load packages
library(tidyverse)
library(hrbrthemes)
library(mapview)
library(tigris)
library(sf)

# import data
county_pops_1900_2020 <- read_csv("Raw Data/county_pops_1900_2020.csv")



# DATA CLEANING -----------------------------------------------------------

# view structure and check for NAs
summary(county_pops_1900_2020)

# check if there are any duplicate GEOIDs
any(duplicated(county_pops_1900_2020$GEOID))

# show only rows with NAs in 'pop_2020' column
county_pops_1900_2020[is.na(county_pops_1900_2020$pop_2020), ]

# split the 'cty' field
county_pops_1900_2020[c('county', 'state')] <- str_split_fixed(county_pops_1900_2020$cty, ',', 2)

# view results
county_pops_1900_2020 %>% select(GEOID, cty, county, state) %>% head()

# select only needed columns
county_pops_1920_2020 <- county_pops_1900_2020 %>%
  select (
    -pop_1900,
    -pop_1910
  )


# ANALYSIS ----------------------------------------------------------------

# * Find max year for all counties ----------------------------------------

# Find max of each county by reshaping data

long_county_pops_1920_2020 <- county_pops_1920_2020 %>%
  pivot_longer(
    cols = !c(GEOID, cty, county, state),
    names_to = "year",
    values_to = "population"
  )

# Make a dataframe of the max population years

max_population_years <- long_county_pops_1920_2020 %>%
  group_by(cty) %>%
  slice_max(population, n = 1, with_ties = FALSE) %>%
  arrange(desc(population)) 

max_population_years

# Group by max population year and count
cty_by_max_yr <- max_population_years %>%
  group_by(year) %>%
  summarise(number_of_counties = n())

# replace "pop_" for cleaner formatting
cty_by_max_yr$year <- str_replace_all(cty_by_max_yr$year, "pop_", "")

# make a bar chart of the result
bar_cty_by_max_yr <- cty_by_max_yr %>%
  ggplot(
    aes(x = year, y = number_of_counties)
  ) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = number_of_counties), hjust = 0.5, vjust = -0.5, color= "black", size = 3) +
  labs(title = "How many counties peaked each year?", 
       subtitle = "All U.S. Counties", 
       y = "N Counties", 
       x = "Peak Population Year") +
  scale_y_continuous(limits = c(0, 1500)) +
  theme_ipsum()



# * Regional Variation ----------------------------------------------------

# Import census regions and divisions sheet
census_regions <- read_csv("Raw Data/census_regions.csv")

# Trim whitespace from max_population_years to facilitate merging
max_population_years$state <- trimws(max_population_years$state)

# Join regions and divisions to max_population_years
max_population_years <- merge(x = max_population_years, 
                              y = census_regions,
                              by.x = "state",
                              by.y = "State")

# Group by Region and max year
region_cty_by_max_yr <- max_population_years %>%
  group_by(Region, year) %>%
  summarise(number_of_counties = n())

# replace "pop_" for cleaner formatting
region_cty_by_max_yr$year <- str_replace_all(region_cty_by_max_yr$year, "pop_", "")

# Calculate percentage
region_cty_by_max_yr <- region_cty_by_max_yr %>%
  group_by(Region) %>%
  mutate(percentage = number_of_counties / sum(number_of_counties))

# Faceted bar chart by Region
bar_region_cty_by_max_yr <- region_cty_by_max_yr %>%
  ggplot(
    aes(x = year, y = percentage, fill = Region)
  ) +
  geom_bar(stat = "identity") +
  #geom_text(aes(label = number_of_counties), hjust = 0.5, vjust = -0.5, color= "black", size = 3) +
  labs(title = "What % of Counties Peaked Each Year?", 
       subtitle = "All U.S. Counties - by Region", 
       y = "Percentage", 
       x = "Peak Population Year") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) +
  #theme_ipsum() +
  facet_wrap(~ Region, nrow = 2)


# * Look and Midwest Divisions --------------------------------------------

# Group by Division (Midwest Only) and max year
mw_cty_by_max_yr <- max_population_years %>%
  filter(Region == "Midwest") %>%
  group_by(Division, year) %>%
  summarise(number_of_counties = n())

# replace "pop_" for cleaner formatting
mw_cty_by_max_yr$year <- str_replace_all(mw_cty_by_max_yr$year, "pop_", "")

# Calculate percentage
mw_cty_by_max_yr <- mw_cty_by_max_yr %>%
  group_by(Division) %>%
  mutate(percentage = number_of_counties / sum(number_of_counties))

# Faceted bar chart by Region
bar_mw_cty_by_max_yr <- mw_cty_by_max_yr %>%
  ggplot(
    aes(x = year, y = percentage, fill = Division)
  ) +
  geom_bar(stat = "identity") +
  #geom_text(aes(label = number_of_counties), hjust = 0.5, vjust = -0.5, color= "black", size = 3) +
  labs(title = "What % of Counties Peaked Each Year?", 
       subtitle = "Midwest Counties - by Division", 
       y = "Percentage", 
       x = "Peak Population Year") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) +
  #theme_ipsum() +
  facet_wrap(~ Division, nrow = 1)


# * Do any states stand out? -----------------------------------------------

# Group by state and max year
state_cty_by_max_yr <- max_population_years %>%
  group_by(state, year) %>%
  summarise(number_of_counties = n())

# replace "pop_" for cleaner formatting
state_cty_by_max_yr$year <- str_replace_all(state_cty_by_max_yr$year, "pop_", "")

# Calculate percentage
state_cty_by_max_yr <- state_cty_by_max_yr %>%
  group_by(state) %>%
  mutate(percentage = number_of_counties / sum(number_of_counties))

# Show states with high percent of counties peaking before 2020
state_cty_by_max_yr %>%
  filter(
    year != '2020'
  ) %>%
  arrange(desc(percentage))

# MAKE A MAP --------------------------------------------------------------
# get county geographies
county_geo <- tigris::counties(cb = TRUE, resolution = '20m')

# join county geographies to peak population year data
static_map_data <- merge(x = county_geo,
                         y = max_population_years,
                         by = "GEOID")

# remove Alaska/Hawaii for cleaner mapping
static_map_data <- static_map_data %>%
  filter(!(state %in% c("Alaska", "Hawaii")))

# replace "pop_" in peak year column and change to numeric
static_map_data$year <- str_replace_all(static_map_data$year, "pop_", "")

static_map_data$year <- as.numeric(static_map_data$year)

# Make a map
map <- ggplot(data = static_map_data, aes(fill = year)) + 
  geom_sf() + 
  labs(title = "  Peak Population Years of U.S. Counties",
       subtitle = "   between 1920 and 2020",
       caption = "Source: NHGIS",
       fill = "Peak Year  ") + 
  theme_void() + 
  scale_fill_continuous(type = "viridis",
                        breaks = c(1920, 1940, 1960, 1980, 2000, 2020))

