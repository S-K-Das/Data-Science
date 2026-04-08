install.packages("tidyverse")
install.packages("ggplot2")
install.packages("lubridate")
install.packages("caret")
install.packages("stringr")
install.packages("caTools")
install.packages("reshape2")
install.packages("readr")
install.packages("naniar")
install.packages("VIM")


library(tidyverse)
library(dplyr)
library(ggplot2)
library(lubridate)
library(caret)
library(stringr)
library(caTools)
library(reshape2)
library(readr)
library(naniar)
library(VIM)


# 2. LOAD DATA

df <- read.csv("D:/Download/data science data3/Netflix Dataset.csv", stringsAsFactors = FALSE)
head(df)
str(df)



# 3. CHECK MISSING VALUES 

missing_counts <- sapply(df, function(x) {
  sum(is.na(x) | (is.character(x) & x == ""))
})
missing_counts



# 4. CONVERT EMPTY STRINGS TO NA

df <- df %>% mutate(across(where(is.character), ~na_if(., "")))



# 5. HANDLE MISSING CATEGORICAL VALUES

df$Director[is.na(df$Director)] <- "Unknown"
df$Cast[is.na(df$Cast)]       <- "Unknown"
df$Country[is.na(df$Country)] <- "Unknown"
df$Rating[is.na(df$Rating)]   <- "Unknown"


# 6. VISUALIZE MISSINGNESS

gg_miss_var(df)
vis_miss(df)


# 7. REMOVE DUPLICATE ROWS

sum(duplicated(df))
df <- df[!duplicated(df), ]



# 8. DETECT INVALID DURATION VALUES

invalid_duration <- df %>%
  filter(!str_detect(Duration, "min") & !str_detect(Duration, "Season"))
invalid_duration




# 9. PARSE RELEASE DATE SAFELY

df$Release_Date <- as.character(df$Release_Date)

df$Release_Date <- suppressWarnings(
  parse_date_time(
    df$Release_Date,
    orders = c("d-b-y", "d-B-y", "d-b-Y", "d-B-Y",
               "B d, Y", "b d, Y", "Y-m-d")
  )
)


# 10. EXTRACT RELEASE YEAR

df$Release_Year <- year(df$Release_Date)
summary(df$Release_Year)



# 11. DETECT OUTLIERS IN RELEASE YEAR

boxplot(df$Release_Year, main = "Outliers in Release Year")

Q1        <- quantile(df$Release_Year, 0.25, na.rm = TRUE)
Q3        <- quantile(df$Release_Year, 0.75, na.rm = TRUE)
IQR_value <- IQR(df$Release_Year, na.rm = TRUE)

lower_bound <- Q1 - 1.5 * IQR_value
upper_bound <- Q3 + 1.5 * IQR_value

outliers <- df %>%
  filter(Release_Year < lower_bound | Release_Year > upper_bound)
outliers

# REMOVE OUTLIERS

df <- df %>%
  filter(Release_Year >= lower_bound & Release_Year <= upper_bound)


# 12. CONVERT CATEGORY TO NUMERIC TARGET

df$Is_Movie <- ifelse(df$Category == "Movie", 1, 0)


# 13. CONVERT NUMERIC YEAR TO CATEGORICAL

df$Year_Group <- cut(
  df$Release_Year,
  breaks = c(1900, 2000, 2010, 2020, 2030),
  labels = c("Before 2000", "2000-2010", "2010-2020", "After 2020")
)

# 14. NORMALIZE RELEASE YEAR

df$Release_Year_Normalized <- scale(df$Release_Year)


# 15. FILTERING EXAMPLES

movies_only <- df %>% filter(Category == "Movie")
tv_shows_only <- df %>% filter(Category == "TV Show")
usa_content  <- df %>% filter(Country == "United States")


# 16. BALANCE DATASET MANUAL OVERSAMPLING

# USING TARGET VARIABLE Is_Movie (0 = TV SHOW, 1 = MOVIE)

table(df$Is_Movie)



# UPSAMPLING MINORITY CLASS TO MATCH MAJORITY SIZE

set.seed(123)
max_n <- max(table(df$Is_Movie))

df_balanced <- df %>%
  group_by(Is_Movie) %>%
  slice_sample(n = max_n, replace = TRUE) %>%
  ungroup()

table(df_balanced$Is_Movie)



# 17. TRAIN–TEST SPLIT

df_balanced$Is_Movie <- as.factor(df_balanced$Is_Movie)

set.seed(123)
split <- sample.split(df_balanced$Is_Movie, SplitRatio = 0.70)

train_data <- subset(df_balanced, split == TRUE)
test_data  <- subset(df_balanced, split == FALSE)

# 18. DESCRIPTIVE STATISTICS

# OVERALL SUMMARY OF RELEASE YEAR

summary(df$Release_Year)

# MEAN RELEASE YEAR BY CATEGORY

df %>%
  group_by(Category) %>%
  summarise(Mean_Year = mean(Release_Year, na.rm = TRUE))

# VARIANCE AND STANDARD DEVIATION BY CATEGORY

df %>%
  group_by(Category) %>%
  summarise(
    Variance = var(Release_Year, na.rm = TRUE),
    SD       = sd(Release_Year, na.rm = TRUE)
  )



# 19. EDA VISUALIZATIONS

# CATEGORY DISTRIBUTION

ggplot(df, aes(Category)) +
  geom_bar(fill = "skyblue") +
  ggtitle("Count of Movies and TV Shows")


# HISTOGRAM OF RELEASE YEAR

ggplot(df, aes(Release_Year)) +
  geom_histogram(binwidth = 3, fill = "purple") +
  ggtitle("Distribution of Release Year")

# BOXPLOT RELEASE YEAR BY CATEGORY

ggplot(df, aes(Category, Release_Year, fill = Category)) +
  geom_boxplot() +
  ggtitle("Release Year by Category")

