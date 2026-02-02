
# Clean environment
remove(list = ls())

# Create list with needed libraries
pkgs <- c(
  "haven",
  "tidyverse",
  "janitor",
  "survey",
  "ggplot2",
  "markdown",
  "tinytex"
)

# Load each listed library and check if it is installed
for (pkg in pkgs) {
  if (!require(pkg, character.only = TRUE)) {
    install.packages(pkg)
    library(pkg, character.only = TRUE)
  }
}

library(dashboardr)
library(dplyr)

data <- read_dta("~/Library/CloudStorage/OneDrive-Persönlich/Dokumente/BRANCHES/6 TASK/PHD RESEARCHER/WEITERBILDUNG/R Software Workshop Project/EVS data and documentation/ZA7503_v3-0-0.dta")


# filter using dplyr

table(data$S003)

data$country <- factor(
  data$S003,
  levels = c(
    8, 31, 40, 51, 56, 70, 100, 112, 124, 191, 196, 197,
    203, 208, 233, 246, 250, 268, 276, 300, 348, 352,
    372, 380, 428, 440, 442, 470, 498, 499, 528, 578,
    616, 620, 642, 643, 688, 703, 705, 724, 752, 756,
    792, 804, 807, 826, 840, 909, 915
  ),
  labels = c(
    "Albania", "Azerbaijan", "Austria", "Armenia", "Belgium",
    "Bosnia and Herzegovina", "Bulgaria", "Belarus", "Canada",
    "Croatia", "Cyprus", "Northern Cyprus", "Czechia", "Denmark",
    "Estonia", "Finland", "France", "Georgia", "Germany", "Greece",
    "Hungary", "Iceland", "Ireland", "Italy", "Latvia", "Lithuania",
    "Luxembourg", "Malta", "Moldova", "Montenegro", "Netherlands",
    "Norway", "Poland", "Portugal", "Romania", "Russia", "Serbia",
    "Slovakia", "Slovenia", "Spain", "Sweden", "Switzerland",
    "Turkey", "Ukraine", "North Macedonia", "Great Britain",
    "United States", "Northern Ireland", "Kosovo"
  )
)

data <- data[data$S003 %in% c(276, 250, 528, 756), ] # germany, france, netherlands, sitzerland

data$country <- with(data, ifelse(
  S003 == 276, "Germany",
  ifelse(S003 == 250, "France",
         ifelse(S003 == 528, "Netherlands",
                ifelse(S003 == 756, "Switzerland", NA_character_)
         )
  )
))


data <- data[data$S002EVS >=5, ]


# specify nominal variable

data$G038
data$migration_job <- ifelse(
  as.numeric(data$G038) > 0,
  as.integer(as.numeric(data$G038)),
  NA_integer_
)

data$farmer_sec <- NA_integer_
data$farmer_sec[data$X036C >= 0] <- 0
data$farmer_sec[data$X036C %in% c(10, 11)] <- 1

data$weight <- as.numeric(data$pwght)


data <- data %>% select(weight, country, migration_job, farmer_sec)

data$farmer_sec <- factor(
  data$farmer_sec,
  levels = 0:1,
  labels = c("Other sector", "Agricult. sector")
)

table(data$farmer_sec)

table(data$migration_job)

data$migration_job <- ordered(
  data$migration_job,
  levels = 1:10,
  labels = c(
    rep("Migrants take away jobs", 4),
    rep("Undecisive", 2),
    rep("Migrants do not take away jobs", 4)
  )
)


table(data$country)

#### Dashboard 

library(dashboardr)
library(dplyr)

# LAYER 1: Build content
distribution <- create_content(data = data, type = "stackedbar") %>%
  add_text(
    "## Welcome to the Farmers’ Attitudes Explorer",
    "",
    "This dashboard presents what people in the agricultural sector think about migrants.",
    "",
    "Use the tabs above to explore different aspects of the data."
  ) %>% 
  add_viz(x_var = "country", stack_var = "farmer_sec", 
          title = "People employed in Agricult. sector", 
          tabgroup = "Sector",
          stacked_type = "percent",
          horizontal = TRUE,
          weight_var = "weight") %>%
  add_viz(x_var = "country", stack_var = "migration_job", 
          title = "Do migrants take away your jobs?", 
          tabgroup = "Attitudes",
          stacked_type = "percent",
          weight_var = "weight") 
  
print(distribution)
preview(distribution)

analysis <- create_content(data = data, type = "stackedbar") %>%
  add_text(
    "Now we check what the people in the farmers sector think about migrants",
    "",
    "Some extra text here",
    "",
    "Some extra text here as well"
  ) %>% 
  add_viz(x_var = "country", stack_var = "migration_job", 
          title = "Migration attitudes in Agricultural sector", 
          filter = ~ farmer_sec == "Agricult. sector", 
          tabgroup = "Agricult. sector",
          stacked_type = "percent",
          weight_var = "weight") %>% 
  add_viz(x_var = "country", stack_var = "migration_job", 
          title = "Migration attitudes in other sectors", 
          filter = ~ farmer_sec == "Other sector", 
          tabgroup = "Other sector",
          stacked_type = "percent",
          weight_var = "weight")


print(analysis)
preview(analysis)

# Step 2: Assemble into page
home <- create_page("Home", is_landing_page = TRUE) %>%
  add_text("# This is the super big Farmers Analysis!", "", "This is more information")

complex_page <- create_page("Full Analysis", data = data) %>%
  add_content(distribution) %>% 
  add_content(analysis)


# Step 3: Create the dashboard
my_dashboard <- create_dashboard(
  title = "What do the Farmers think?",
  output_dir = "farmers_dashboard",
  publish_dir = "../docs",
  theme = "journal"
) %>%
  add_pages(home, complex_page)

  
my_dashboard %>% 
  generate_dashboard(render = TRUE, open = "browser")

################
#### ISSUES ####
################

# The scale order is wrong (1, 10, 2, 3, 4, 5, 6, 7, 8, 9) without using bins = 25
Farmers_and_migration <- create_content(data = data, type = "stackedbar") %>%
  add_text(
    "## Welcome to the Farmers’ Attitudes Explorer",
    "",
    "This dashboard presents what people in the agricultural sector think about migrants.",
    "",
    "Use the tabs above to explore different aspects of the data."
  ) %>% 
  add_viz(x_var = "migration_job", 
          title = "Do migrants take away your jobs?", 
          tabgroup = "Overview",
          type = "histogram")

print(Farmers_and_migration)

preview(Farmers_and_migration)

# When using stackedbar on ONE variable, it will not show in the preview
Farmers_and_migration <- create_content(data = data, type = "stackedbar") %>%
  add_text(
    "## Welcome to the Farmers’ Attitudes Explorer",
    "",
    "This dashboard presents what people in the agricultural sector think about migrants.",
    "",
    "Use the tabs above to explore different aspects of the data."
  ) %>% 
  add_viz(stack_var = "farmer_sec", title = "People employed in Farmers sector", tabgroup = "Overview")

print(Farmers_and_migration)

preview(Farmers_and_migration)

