install.packages(c("rvest", "dplyr", "ggplot2", "tidyr",
                   "caret", "corrplot", "randomForest"))



library(rvest)
library(dplyr)
library(ggplot2)
library(tidyr)
library(caret)
library(corrplot)
library(randomForest)


url <- "https://scrapethissite.com/pages/forms/?page_num=1&per_page=100"

page <- read_html(url)

table <- page %>% html_node("table") %>% html_table()

head(table)

write.csv(table, "scrapethissite_hockey.csv", row.names = FALSE)

df <- read.csv("scrapethissite_hockey.csv")
str(df)

dim(df)
summary(df)
colSums(is.na(df))

 
df <- df %>%
  rename(
    team = Team.Name,
    year = Year,
    wins = Wins,
    losses = Losses,
    ot_loss = OT.Losses,
    pct = Win..,
    gf = Goals.For..GF.,
    ga = Goals.Against..GA.,
    diff = X.....
  )


str(df)
summary(df)
colSums(is.na(df))

df$ot_loss[is.na(df$ot_loss)] <- 0


ggplot(df, aes(x = gf)) +
  geom_histogram(bins = 30, fill = "steelblue") +
  labs(title = "Distribution of Goals Scored (GF)")

ggplot(df, aes(x = diff, y = pct)) +
  geom_point(color = "darkgreen") +
  geom_smooth(method = "lm") +
  labs(title = "Goal Difference vs Winning Percentage")


num_data <- df %>% select(wins, losses, gf, ga, diff, pct)
corrplot(cor(num_data), method = "color", type = "upper")


set.seed(123)
index <- createDataPartition(df$pct, p = 0.7, list = FALSE)

train <- df[index, ]
test  <- df[-index, ]

model <- lm(pct ~ wins + losses + gf + ga + diff, data = train)
summary(model)

pred <- predict(model, test)

rmse <- sqrt(mean((pred - test$pct)^2))
rmse

ggplot(data.frame(actual = test$pct, predicted = pred),
       aes(x = actual, y = predicted)) +
  geom_point(color = "red") +
  geom_abline(slope = 1, intercept = 0) +
  labs(title = "Actual vs Predicted Winning Percentage")


 
