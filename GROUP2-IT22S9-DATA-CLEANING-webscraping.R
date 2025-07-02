library(rvest)
library(dplyr)
library(stringr)

# Step 1: Read the webpage
url <- "https://headphonesaddict.com/social-media-addiction-statistics/"
page <- read_html(url)

# Step 2: Extract table or paragraphs (depends on layout)
data_raw <- page %>% html_elements("ul li") %>% html_text()

# Step 3: Clean and convert numbers from text
data_clean <- data.frame(Stat = data_raw) %>%
  filter(str_detect(Stat, "%|students|average|hours")) %>%
  mutate(Stat = str_trim(Stat))

# Optional: Extract numeric values using regex (for analysis)
data_clean$Numeric <- str_extract(data_clean$Stat, "\\d+\\.*\\d*") %>% as.numeric()

View(data_clean)