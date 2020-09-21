# --> Set-up

install.packages("nflfastR")
library(nflfastR)
install.packages("tidyverse")
library(tidyverse)
install.packages("dplyr")
library(dplyr)
install.packages("ggplot2")
library(ggplot2)
install.packages("sjPlot")
library(sjPlot)
install.packages("sjmisc")
library(sjmisc)
install.packages("sjlabelled")
library(sjlabelled)

# --> getting game IDs

game_ids <- fast_scraper_schedules(2020)

game_ids_week1 <- game_ids %>%
  filter(week == 1) %>%
  select(game_id)

# --> scraping the data

game_ids_week1 <- as.data.frame(game_ids_week1)
class(game_ids_week1)

data1 <-
  fast_scraper(game_ids_week1[1, 1], source = "nfl", pp = FALSE)
data2 <-
  fast_scraper(game_ids_week1[2, 1], source = "nfl", pp = FALSE)
data3 <-
  fast_scraper(game_ids_week1[3, 1], source = "nfl", pp = FALSE)
data4 <-
  fast_scraper(game_ids_week1[4, 1], source = "nfl", pp = FALSE)
data5 <-
  fast_scraper(game_ids_week1[5, 1], source = "nfl", pp = FALSE)
data6 <-
  fast_scraper(game_ids_week1[6, 1], source = "nfl", pp = FALSE)
data7 <-
  fast_scraper(game_ids_week1[7, 1], source = "nfl", pp = FALSE)
data8 <-
  fast_scraper(game_ids_week1[8, 1], source = "nfl", pp = FALSE)
data9 <-
  fast_scraper(game_ids_week1[9, 1], source = "nfl", pp = FALSE)
data10 <-
  fast_scraper(game_ids_week1[10, 1], source = "nfl", pp = FALSE)
data11 <-
  fast_scraper(game_ids_week1[11, 1], source = "nfl", pp = FALSE)
data12 <-
  fast_scraper(game_ids_week1[12, 1], source = "nfl", pp = FALSE)
data13 <-
  fast_scraper(game_ids_week1[13, 1], source = "nfl", pp = FALSE)
data14 <-
  fast_scraper(game_ids_week1[14, 1], source = "nfl", pp = FALSE)
data15 <-
  fast_scraper(game_ids_week1[15, 1], source = "nfl", pp = FALSE)
data16 <-
  fast_scraper(game_ids_week1[16, 1], source = "nfl", pp = FALSE)

data <-
  rbind(
    data1,
    data2,
    data3,
    data4,
    data5,
    data6,
    data7,
    data8,
    data9,
    data9,
    data10,
    data11,
    data12,
    data13,
    data15
  )


# --> Expected Points graph for 1&10 plays of week 1

rel1 <- data %>%
  select(ep, down, goal_to_go, ydstogo, yardline_100, posteam_type, qtr) %>%
  filter(down == "1", ydstogo == "10")

plot <- rel1 %>%
  ggplot(aes(x = yardline_100, y = ep, color = factor(qtr))) +
  geom_point(size = 0.9) +
  geom_smooth(method = "lm", se = TRUE)

plot$labels$x <- "Yards to Go"
plot$labels$y <- "Expected Points"
plot$labels$colour <- "Quarter"

plot + scale_color_brewer(palette = "Spectral") + theme_light() + ggtitle("Expected Points per Quarter on 1&10") + labs(fill = "Dose (mg)")

# --> Expected Points graph for all plays of week 1

relall <- data %>%
  select(ep, down, goal_to_go, ydstogo, yardline_100, posteam_type, qtr) %>%
  filter(down != "NA", ep != "NA", yardline_100 != "NA")

plot_all <- relall %>%
  ggplot(aes(x = yardline_100, y = ep, color = factor(down))) +
  geom_point(size = 0.9) +
  geom_smooth(
    method = "lm",
    se = TRUE,
    formula = y ~ poly(x, 3, raw = TRUE)
  )

plot_all$labels$x <- "Yards to Go"
plot_all$labels$y <- "Expected Points"
plot_all$labels$colour <- "Down"

plot_all + scale_color_brewer(palette = "Spectral") + theme_light() + ggtitle("Expected Points per Down in Week 1")


# --> Simple OLS model

reg <- lm(data = relall, ep ~ qtr + yardline_100 + ydstogo * down)
summary(reg)

tab_model(reg)

