



# Mahomes

game_ids <- c("2020_02_KC_LAC")

week_2_KC <- fast_scraper(game_ids, source = "nfl", pp = FALSE)
week_2_KC <- add_qb_epa(week_2_KC)
week_2_only_KC <- subset(week_2_KC, week_2_KC$posteam == "KC")
week_2_only_KC_only_pass <-
  subset(week_2_only_KC, week_2_only_KC$play_type == "pass")


plot(
  week_2_only_KC$qb_epa,
  xlab = "# of KC play",
  ylab = "QB-EPA per play",
  main = "Mahomes EPA per play"
)
plot(
  week_2_only_KC_only_pass$qb_epa,
  xlab = "# of KC pass play",
  ylab = "QB-EPA per pass play",
  main = "Mahomes EPA per pass play"
)
abline(h = -2, col = 2)

plot(density(subset(
  week_2_only_KC$qb_epa, week_2_only_KC$qb_epa != "NA"
)))

week_2_only_KC %>%
  ggplot(aes(x = play_id, y = qb_epa)) +
  geom_point(alpha = 1) +
  geom_point(
    data = week_2_only_KC_only_pass,
    aes(x = play_id, y = qb_epa),
    color = 'red',
    size = 1.4
  )

# Herbert

game_ids <- c("2020_02_KC_LAC")

week_2_LAC <- fast_scraper(game_ids, source = "nfl", pp = FALSE)
week_2_LAC <- add_qb_epa(week_2_LAC)
week_2_only_LAC <- subset(week_2_LAC, week_2_LAC$posteam == "LAC")
week_2_only_LAC_only_pass <-
  subset(week_2_only_LAC, week_2_only_LAC$play_type == "pass")


plot(
  week_2_only_LAC$qb_epa,
  xlab = "# of LAC play",
  ylab = "QB-EPA per play",
  main = "Herbert EPA per play"
)
plot(
  week_2_only_LAC_only_pass$qb_epa,
  xlab = "# of LAC pass play",
  ylab = "QB-EPA per pass play",
  main = "Herbert EPA per pass play"
)
abline(h = -2, col = 2)

plot(density(
  subset(week_2_only_LAC$qb_epa, week_2_only_LAC$qb_epa != "NA")
))

week_2_only_LAC %>%
  ggplot(aes(x = play_id, y = qb_epa)) +
  geom_point(alpha = 1) +
  geom_point(
    data = week_2_only_LAC_only_pass,
    aes(x = play_id, y = qb_epa),
    color = 'red',
    size = 1.4
  )


# Newton

game_ids <- c("2020_02_NE_SEA")

week_2_NE <- fast_scraper(game_ids, source = "nfl", pp = FALSE)
week_2_NE <- add_qb_epa(week_2_NE)
week_2_only_NE <- subset(week_2_NE, week_2_NE$posteam == "NE")
week_2_only_NE_only_pass <-
  subset(week_2_only_NE, week_2_only_NE$play_type == "pass")


plot(
  week_2_only_NE$qb_epa,
  xlab = "# of NE play",
  ylab = "QB-EPA per play",
  main = "Newton EPA per play"
)
plot(
  week_2_only_NE_only_pass$qb_epa,
  xlab = "# of NE pass play",
  ylab = "QB-EPA per pass play",
  main = "Newton EPA per pass play"
)
abline(h = -2, col = 2)

plot(density(subset(
  week_2_only_NE$qb_epa, week_2_only_NE$qb_epa != "NA"
)))

week_2_only_NE %>%
  ggplot(aes(x = play_id, y = qb_epa)) +
  geom_point(alpha = 1) +
  geom_point(
    data = week_2_only_NE_only_pass,
    aes(x = play_id, y = qb_epa),
    color = 'red',
    size = 1.4
  )


# Wilson

game_ids <- c("2020_02_NE_SEA")

week_2_SEA <- fast_scraper(game_ids, source = "nfl", pp = FALSE)
week_2_SEA <- add_qb_epa(week_2_SEA)
week_2_only_SEA <- subset(week_2_SEA, week_2_SEA$posteam == "SEA")
week_2_only_SEA_only_pass <-
  subset(week_2_only_SEA, week_2_only_SEA$play_type == "pass")


plot(
  week_2_only_SEA$qb_epa,
  xlab = "# of SEA play",
  ylab = "QB-EPA per play",
  main = "Newton EPA per play"
)
plot(
  week_2_only_SEA_only_pass$qb_epa,
  xlab = "# of SEA pass play",
  ylab = "QB-EPA per pass play",
  main = "Newton EPA per pass play"
)
abliSEA(h = -2, col = 2)

plot(density(
  subset(week_2_only_SEA$qb_epa, week_2_only_SEA$qb_epa != "NA")
))

week_2_only_SEA %>%
  ggplot(aes(x = play_id, y = qb_epa)) +
  geom_point(alpha = 1) +
  geom_point(
    data = week_2_only_SEA_only_pass,
    aes(x = play_id, y = qb_epa),
    color = 'red',
    size = 1.4
  )


# Gegenüberstellung alle plays

plot(density(subset(
  week_2_only_KC$qb_epa, week_2_only_KC$qb_epa != "NA"
)),
xlim = c(-6, 6),
ylim = c(0, 0.55))
lines(density(
  subset(week_2_only_LAC$qb_epa, week_2_only_LAC$qb_epa != "NA")
), col = 4)
lines(density(subset(
  week_2_only_NE$qb_epa, week_2_only_NE$qb_epa != "NA"
)), col = 2)
lines(density(
  subset(week_2_only_SEA$qb_epa, week_2_only_SEA$qb_epa != "NA")
), col = 3)
abline(v = mean(subset(
  week_2_only_KC$qb_epa, week_2_only_KC$qb_epa != "NA"
)), lty = 3)
abline(v = mean(
  subset(week_2_only_LAC$qb_epa, week_2_only_LAC$qb_epa != "NA")
),
lty = 3,
col = 4)
abline(v = mean(subset(
  week_2_only_NE$qb_epa, week_2_only_NE$qb_epa != "NA"
)),
lty = 3,
col = 2)
abline(v = mean(
  subset(week_2_only_SEA$qb_epa, week_2_only_SEA$qb_epa != "NA")
),
lty = 3,
col = 3)

# Gegenüberstellung pass plays

plot(
  density(
    subset(
      week_2_only_KC_only_pass$qb_epa,
      week_2_only_KC_only_pass$qb_epa != "NA"
    )
  ),
  xlim = c(-8, 8),
  ylim = c(0, 0.4),
  main = "Densityplots of EPA per pass play of selected QBs",
  xlab = "EPA per pass play",
  ylab = "percentage of pass plays with a specific EPA"
)
lines(density(
  subset(
    week_2_only_LAC_only_pass$qb_epa,
    week_2_only_LAC_only_pass$qb_epa != "NA"
  )
), col = 4)
lines(density(
  subset(
    week_2_only_NE_only_pass$qb_epa,
    week_2_only_NE_only_pass$qb_epa != "NA"
  )
), col = 2)
lines(density(
  subset(
    week_2_only_SEA_only_pass$qb_epa,
    week_2_only_SEA_only_pass$qb_epa != "NA"
  )
), col = 3)
abline(v = mean(
  subset(
    week_2_only_KC_only_pass$qb_epa,
    week_2_only_KC_only_pass$qb_epa != "NA"
  )
), lty = 3)
abline(v = mean(
  subset(
    week_2_only_LAC_only_pass$qb_epa,
    week_2_only_LAC_only_pass$qb_epa != "NA"
  )
),
lty = 3,
col = 4)
abline(v = mean(
  subset(
    week_2_only_NE_only_pass$qb_epa,
    week_2_only_NE_only_pass$qb_epa != "NA"
  )
),
lty = 3,
col = 2)
abline(v = mean(
  subset(
    week_2_only_SEA_only_pass$qb_epa,
    week_2_only_SEA_only_pass$qb_epa != "NA"
  )
),
lty = 3,
col = 3)
legend(
  "topleft",
  legend = c("Mahomes", "Herbert", "Newton", "Wilson"),
  col = c(1, 4, 2, 3),
  pch = c(16, 16, 16, 16)
)

# Boxplots Pass Plays

Mahomes <- as.data.frame(as.table(week_2_only_KC_only_pass$qb_epa))
Herbert <- as.data.frame(as.table(week_2_only_LAC_only_pass$qb_epa))
Newton <- as.data.frame(as.table(week_2_only_NE_only_pass$qb_epa))
Wilson <- as.data.frame(as.table(week_2_only_SEA_only_pass$qb_epa))

epa_per_pass_play_sel_qb_1 <-
  merge(Mahomes, Herbert, by = "Var1" , all = TRUE)
epa_per_pass_play_sel_qb_2 <-
  merge(Newton, Wilson, by = "Var1" , all = TRUE)
epa_per_pass_play_sel_qb <-
  merge(
    epa_per_pass_play_sel_qb_1,
    epa_per_pass_play_sel_qb_2,
    by = "Var1",
    all = TRUE
  )


colnames(epa_per_pass_play_sel_qb) <-
  c("nb", "Mahomes", "Herbert", "Newton", "Wilson")
epa_per_pass_play_sel_qb <-
  subset(epa_per_pass_play_sel_qb,
         select = c("Mahomes", "Herbert", "Newton", "Wilson"))

boxplot(
  epa_per_pass_play_sel_qb,
  col = c("grey", 3, 2, 4),
  main = "Boxplots for selective QBs - pass plays",
  xlab = "QBs",
  ylab = "EPA per play"
)

# Boxplots Pass Plays

Mahomes <- as.data.frame(as.table(week_2_only_KC_only_pass$qb_epa))
Herbert <- as.data.frame(as.table(week_2_only_LAC_only_pass$qb_epa))
Newton <- as.data.frame(as.table(week_2_only_NE_only_pass$qb_epa))
Wilson <- as.data.frame(as.table(week_2_only_SEA_only_pass$qb_epa))

epa_per_pass_play_sel_qb_1 <-
  merge(Mahomes, Herbert, by = "Var1" , all = TRUE)
epa_per_pass_play_sel_qb_2 <-
  merge(Newton, Wilson, by = "Var1" , all = TRUE)
epa_per_pass_play_sel_qb <-
  merge(
    epa_per_pass_play_sel_qb_1,
    epa_per_pass_play_sel_qb_2,
    by = "Var1",
    all = TRUE
  )


colnames(epa_per_pass_play_sel_qb) <-
  c("nb", "Mahomes", "Herbert", "Newton", "Wilson")
epa_per_pass_play_sel_qb <-
  subset(epa_per_pass_play_sel_qb,
         select = c("Mahomes", "Herbert", "Newton", "Wilson"))

boxplot(
  epa_per_pass_play_sel_qb,
  col = c("grey", 3, 2, 4),
  main = "Boxplots for selective QBs - pass plays",
  xlab = "QBs",
  ylab = "EPA per play"
)

# Boxplots All Plays

Mahomes <- as.data.frame(as.table(week_2_only_KC$qb_epa))
Herbert <- as.data.frame(as.table(week_2_only_LAC$qb_epa))
Newton <- as.data.frame(as.table(week_2_only_NE$qb_epa))
Wilson <- as.data.frame(as.table(week_2_only_SEA$qb_epa))

epa_per_play_sel_qb_1 <-
  merge(Mahomes, Herbert, by = "Var1" , all = TRUE)
epa_per_play_sel_qb_2 <- merge(Newton, Wilson, by = "Var1" , all = TRUE)
epa_per_play_sel_qb <-
  merge(epa_per_play_sel_qb_1,
        epa_per_play_sel_qb_2,
        by = "Var1",
        all = TRUE)

colnames(epa_per_play_sel_qb) <-
  c("nb", "Mahomes", "Herbert", "Newton", "Wilson")
epa_per_play_sel_qb <-
  subset(epa_per_play_sel_qb,
         select = c("Mahomes", "Herbert", "Newton", "Wilson"))

boxplot(
  epa_per_play_sel_qb,
  col = c("grey", 3, 2, 4),
  main = "Boxplots for selective QBs - pass plays",
  xlab = "QBs",
  ylab = "EPA per play"
)
