# Covid Report Improved
# Edgar Aguilar
# 03.22.2025

# 1. LOAD PACKAGES -------------------------------
library(tidyverse)
library(scales)
library(lfe)
library(modelsummary)
library(gt)
library(data.table)

# 2. CREATE THE DATASET ---------------------------------

## (a) COVID Deaths Data (2021-2022)
# Download NYT COVID-19 county data for 2022
covid_deaths <- data.table::fread('https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-counties-2022.csv') %>%
  filter(!is.na(fips), state != 'Puerto Rico') %>%   
  select(fips, county, state, date, deaths) %>%
  group_by(fips, county, state) %>%
  # Calculate deaths in 2022 as the difference between max and min death counts
  summarise(deaths = max(deaths, na.rm = TRUE) - min(deaths, na.rm = TRUE), .groups = "drop")

## (b) Mask Usage Data
# Download estimated mask usage data from July 2020 survey
mask <- read_csv('https://raw.githubusercontent.com/nytimes/covid-19-data/master/mask-use/mask-use-by-county.csv') %>%
  mutate(
    fips = as.integer(COUNTYFP),
    always.mask = ALWAYS   # rename for clarity
  ) %>%
  select(fips, always.mask)   # keep only relevant variables for merging

## (c) CDC Vaccination Data
# Load CDC vaccination data from local file (ensure the file is in your working directory)
vax <- read_csv('cdc vax mar1.csv') %>%
  filter( 
    FIPS != 'UNK', 
    Recip_State != 'VI', 
    Completeness_pct > 0, 
    !is.na(Administered_Dose1_Recip)
  ) %>% 
  mutate(
    fips = as.integer(FIPS),
    population = Census2019,                      # use 2019 census estimates
    vax.complete = Series_Complete_Pop_Pct,         # percent fully vaccinated
    svi.index = SVI_CTGY                           # social vulnerability index
  ) %>%
  select(fips, population, vax.complete, svi.index)

# 3. MERGE DATASETS -------------------------------------
# Merge COVID, mask, and vaccination data by FIPS code
covid <- covid_deaths %>%
  left_join(mask, by = "fips") %>%
  left_join(vax, by = "fips") %>%
  # Scale COVID deaths per 100,000 population
  mutate(deaths.scaled = (deaths / population) * 100000) %>%
  ungroup()

# Clean up workspace
rm(mask, vax)

# Quick summary to check merged data
summary(covid)

# Save the merged dataset as a CSV file in the working directory
write_csv(covid, "merged_covid_data.csv")

# 4. VISUALIZATIONS AND SUMMARY STATISTICS -------------

## 4a. COVID Deaths Nationally
# Calculate the mean of deaths
mean_deaths <- mean(covid$deaths, na.rm = TRUE)

covid %>%
  ggplot(aes(x = 1 + deaths)) +
  geom_histogram(fill = "skyblue", color = "white", bins = 30) +
  scale_x_log10(labels = scales::comma) +
  labs(
    title = "Distribution of COVID-19 Deaths by County (Log Scale)",
    x = "Total COVID-19 Deaths (by County)",
    y = "Count of Counties"
  ) +
# Minimal theme + no grid lines + centered, bold, dark title
theme_minimal() +
theme(
  panel.grid.major = element_blank(),
  panel.grid.minor = element_blank(),
  plot.title = element_text(hjust = 0.5, face = "bold", color = "black")
) +
# Dashed dark blue line from y=0 to y=Inf at the mean
geom_segment(
  aes(
    x = 1 + mean_deaths,
    xend = 1 + mean_deaths,
    y = 0,
    yend = Inf
  ),
  color = "darkblue",
  linetype = "dashed",
  size = 1
) +
# Annotate the mean line
annotate(
  "text",
  x = 1 + mean_deaths,
  y = Inf,
  label = paste("Mean =", round(mean_deaths, 1)),
  color = "darkblue",
  angle = 90,
  vjust = 2
)

# Print summary statistics for COVID deaths
summary(covid$deaths)


## 4b. Mask Usage
# Calculate mean of mask usage (as a fraction)
mean_mask <- mean(covid$always.mask, na.rm = TRUE)

covid %>%
  ggplot(aes(x = always.mask)) +
  geom_histogram(fill = "skyblue", color = "white", bins = 30) +
# Convert x-axis to percentage format
scale_x_continuous(labels = scales::percent_format(scale = 100)) +
labs(
  title = "Distribution of ‘Always Mask’ Use Across US Counties",
  x = "Percentage Always Masking",
  y = "Count of Counties"
) +
theme_minimal() +
theme(
  panel.grid.major = element_blank(),
  panel.grid.minor = element_blank(),
  plot.title = element_text(hjust = 0.5, face = "bold", color = "black")
) +
# Add a dashed darkblue mean line from y=0 to the top of the plot
geom_segment(
  data = data.frame(x = mean_mask),
  aes(x = x, xend = x, y = 0, yend = Inf),
  color = "darkblue",
  linetype = "dashed",
  size = 1,
  inherit.aes = FALSE
) +
# Annotate the mean line with a label
annotate(
  "text",
  x = mean_mask,
  y = Inf,
  label = paste("Mean =", round(mean_mask * 100, 1)),
  color = "darkblue",
  angle = 90,
  vjust = 2
)

# Summary for mask usage
summary(covid$always.mask)


## 4c. Vaccination Rates
# Calculate the mean of vaccination completion (data is already in percentage form)
mean_vax <- mean(covid$vax.complete, na.rm = TRUE)

covid %>%
  ggplot(aes(x = vax.complete)) +
  geom_histogram(fill = "skyblue", color = "white", bins = 30) +
# Convert x-axis to display a percent sign (e.g., 75 becomes 75%)
scale_x_continuous(labels = scales::percent_format(scale = 1)) +
labs(
  title = "Distribution of COVID-19 Vaccination Completion by County",
  x = "Vaccination Completion",
  y = "Count of Counties"
) +
theme_minimal() +
theme(
  panel.grid.major = element_blank(),
  panel.grid.minor = element_blank(),
  plot.title = element_text(hjust = 0.5, face = "bold", color = "black")
) +
# Dashed dark blue line from y=0 to y=Inf at the mean
geom_segment(
  aes(
    x = mean_vax,
    xend = mean_vax,
    y = 0,
    yend = Inf
  ),
  color = "darkblue",
  linetype = "dashed",
  size = 1
) +
# Annotate the mean line with a label
annotate(
  "text",
  x = mean_vax,
  y = Inf,
  label = paste("Mean =", round(mean_vax, 1)),
  color = "darkblue",
  angle = 90,
  vjust = 2
)

# Print summary statistics for vaccination completion
summary(covid$vax.complete)


# Vaccination rates by Social Vulnerability Index (SVI)
covid %>%
  filter(!is.na(svi.index)) %>%  # remove counties with missing SVI
  ggplot(aes(x = factor(svi.index), y = vax.complete)) +
  # Draw whiskers (error bars) first so they appear behind the boxes
  stat_boxplot(geom = "errorbar", width = 0.25) +
  # Draw boxplots on top
  geom_boxplot(fill = "skyblue", color = "black") +
  labs(
    title = "Vaccination Rates by Social Vulnerability Index",
    x = "Social Vulnerability Index Category",
    y = "Vaccination Completion Percentage"
  ) +
  theme_minimal() +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    plot.title = element_text(hjust = 0.5, face = "bold", color = "black")
  )


# Identify top 10 counties with the highest vaccination completion rates
top10_max <- covid %>%
  filter(!is.na(vax.complete)) %>%
  select(vax.complete, county, state) %>%
  arrange(desc(vax.complete)) %>%
  slice_head(n = 10) %>%
  rename("Complete Vaccination %" = vax.complete,
         "County" = county,
         "State" = state)

# Identify top 10 counties with the lowest vaccination completion rates
top10_min <- covid %>%
  filter(!is.na(vax.complete)) %>%
  select(vax.complete, county, state) %>%
  arrange(vax.complete) %>%
  slice_head(n = 10) %>%
  rename("Complete Vaccination %" = vax.complete,
         "County" = county,
         "State" = state)

# Print the results
print("Top 10 Counties with Highest Vaccination Completion Rates:")
print(top10_max)

print("Top 10 Counties with Lowest Vaccination Completion Rates:")
print(top10_min)



# 5. IMPACT ON 2022 COVID DEATHS: REGRESSION ANALYSIS ----

## Run regression models using felm() from lfe
mods <- list(
  # Model 1: Impact of mask usage controlling for population and SVI, with state fixed effects
  m1 = felm(deaths.scaled ~ always.mask + population + svi.index | state, data = covid),
  # Model 2: Impact of vaccination rates controlling for population and SVI, with state fixed effects
  m2 = felm(deaths.scaled ~ vax.complete + population + svi.index | state, data = covid),
  # Model 3: Both mask usage and vaccination rates included
  m3 = felm(deaths.scaled ~ always.mask + vax.complete + svi.index | state, data = covid)
)

## Display regression results in a formatted table
modelsummary(
  mods, 
  gof_map = c('nobs'), 
  stars = TRUE, 
  output = 'gt'
)
