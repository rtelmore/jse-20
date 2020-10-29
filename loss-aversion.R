## ----r-packages, include = F-----------------------------------------------------------
library(knitr)
library(tidyverse)
library(janitor)
library(readxl)
library(lme4)
library(broom)
library(kableExtra)
library(Rmisc)
library(multcomp)
options(scipen=999)
set.seed(92302)


## ----knitr-options, include = FALSE----------------------------------------------------
opts_chunk$set(fig.path = "fig/",
               fig.width = 6,
               fig.height = 5,
               fig.lp = "fig:",
               fig.keep="high",
               fig.show="hold",
               fig.align="center",
               echo = FALSE, 
               warning = FALSE, 
               message = FALSE,
               comment = NA)


## ----data, include = FALSE, cache = TRUE-----------------------------------------------
df <- read_csv("data/us-open.csv") %>%
  clean_names() %>%
  bind_rows(read_csv("data/us-open-2019-long.csv"))
df_par <- read_csv("data/par-by-hole.csv")

df_red <- df %>%
  dplyr::group_by(year, hole_number) %>%
  dplyr::summarize(avg = mean(sum_of_strokes)) %>%
  dplyr::left_join(df_par, by = c("year", "hole_number")) %>%
  dplyr::mutate(diff = avg - par)

df_oak <- read_excel("data/us-open-oakmont.xlsx") %>%
  clean_names()
names(df_oak)[1] <- "year"
df_par_oak <- read_csv("data/par-by-hole-oakmont.csv")

df_oak <- filter(df_oak, year >= 1970)
df_red_oak <- df_oak %>%
  dplyr::group_by(year, hole_number) %>%
  dplyr::summarize(avg = mean(sum_of_strokes)) %>%
  dplyr::left_join(df_par_oak, by = c("year", "hole_number")) %>%
  dplyr::mutate(diff = avg - par)

reg_df <- bind_rows(
  readRDS("data/regression-full-df-oakmont.rds") %>%
    mutate(full_rounds = if_else(total_rounds == 4, TRUE, FALSE),
           year_ind = if_else(year < 2007, TRUE, FALSE),
           year_player = paste(player, ":", year),
           location = "Oakmont") %>%
    left_join(df_par_oak),
  readRDS("data/regression-full-df.rds") %>%
    mutate(full_rounds = if_else(total_rounds == 4, TRUE, FALSE),
           year_ind = if_else(year < 2000, TRUE, FALSE),
           year_player = paste(player, ":", year),
           location = "Pebble") %>%
    left_join(df_par)) %>%
  filter(round_number <= 4)

reg_df <- filter(reg_df, round_number <= 4)

df_red_tot <- bind_rows(
  dplyr::filter(reg_df, location == "Pebble", hole_number == 2),
  dplyr::filter(reg_df, location == "Oakmont", hole_number == 9)) %>%
  dplyr::group_by(year, round_number, location) %>%
  dplyr::summarize(avg = mean(sum_of_strokes)) %>%
  dplyr::filter(round_number < 5) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(col = if_else(year < 2000, 1, 0))

df_hole_avg <- df %>%
  dplyr::group_by(year, hole_number) %>%
  dplyr::summarize(avg = mean(sum_of_strokes)) %>%
  dplyr::mutate(course = "Pebble") %>%
  dplyr::ungroup() %>%
  dplyr::bind_rows(filter(df_oak, year >= 1970) %>%
                     dplyr::group_by(year, hole_number) %>%
                     dplyr::summarize(avg = mean(sum_of_strokes)) %>%
                     dplyr::mutate(course = "Oakmont") %>%
                     dplyr::ungroup()) %>%
  dplyr::mutate(hole = factor(hole_number,
                              labels = c("hole 1", "hole 2", "hole 3", "hole 4",
                                         "hole 5", "hole 6", "hole 7", "hole 8",
                                         "hole 9", "hole 10", "hole 11", "hole 12",
                                         "hole 13", "hole 14", "hole 15", "hole 16",
                                         "hole 17", "hole 18")))


## ----reg-mods, cache = TRUE------------------------------------------------------------
peb_reg_df <- filter(reg_df, 
                     location == "Pebble",
                     year %in% c(1992, 2000)) %>% 
  mutate(hole_number = as.factor(hole_number))
lm_pebble <- lm(sum_of_strokes ~ hole_number*year_ind, 
                data = peb_reg_df)

oak_reg_df <- filter(reg_df, 
                     location == "Oakmont", 
                     year %in% c(1994, 2007)) %>%
  mutate(hole_number = as.factor(hole_number),
         hole_number = relevel(hole_number, ref = 9))
  
lm_oakmont <- lm(sum_of_strokes ~ hole_number*year_ind,
                 data = oak_reg_df)


## ---- include = FALSE, cache = TRUE----------------------------------------------------
res <- summarySE(peb_reg_df, 
                 measurevar = "sum_of_strokes", 
                 groupvars = c("hole_number", "year_ind"))
res_oak <- summarySE(oak_reg_df, 
                     measurevar = "sum_of_strokes", 
                     groupvars = c("hole_number", "year_ind"))

K <- matrix(c(rep(0, 18), 1, rep(0, 17),
              rep(0, 18), 1, 1, rep(0, 16),
              rep(0, 18), 1, 0, 1, rep(0, 15),
              rep(0, 18), 1, rep(0, 2), 1, rep(0, 14),
              rep(0, 18), 1, rep(0, 3), 1, rep(0, 13),
              rep(0, 18), 1, rep(0, 4), 1, rep(0, 12),
              rep(0, 18), 1, rep(0, 5), 1, rep(0, 11),
              rep(0, 18), 1, rep(0, 6), 1, rep(0, 10),
              rep(0, 18), 1, rep(0, 7), 1, rep(0, 9),
              rep(0, 18), 1, rep(0, 8), 1, rep(0, 8),
              rep(0, 18), 1, rep(0, 9), 1, rep(0, 7),
              rep(0, 18), 1, rep(0, 10), 1, rep(0, 6),
              rep(0, 18), 1, rep(0, 11), 1, rep(0, 5),
              rep(0, 18), 1, rep(0, 12), 1, rep(0, 4),
              rep(0, 18), 1, rep(0, 13), 1, rep(0, 3),
              rep(0, 18), 1, rep(0, 14), 1, rep(0, 2),
              rep(0, 18), 1, rep(0, 15), 1, rep(0, 1),
              rep(0, 18), 1, rep(0, 16), 1),
              nr = 18, byrow = T)

### set up general linear hypothesis
pebble_glht <- glht(lm_pebble, linfct = K)
pebble_glht_sum <- summary(pebble_glht)$test 
pebble_glht_df <- tibble(hole = 1:18,
                         coef = pebble_glht_sum$coefficients,
                         sigma = pebble_glht_sum$sigma) %>% 
  mutate(., lower = coef - 2*sigma, upper = coef + 2*sigma) %>% 
  inner_join(., filter(df_par, year == 2000),
             by = c("hole" = "hole_number"))
pebble_glht_df$par[2] <- "4/5"

oakmont_glht <- glht(lm_oakmont, linfct = K)
summary(oakmont_glht)

### set up general linear hypothesis
oakmont_glht <- glht(lm_oakmont, linfct = K)
oakmont_glht_sum <- summary(oakmont_glht)$test 
oakmont_glht_df <- tibble(hole = 1:18,
                         coef = oakmont_glht_sum$coefficients,
                         sigma = oakmont_glht_sum$sigma) %>% 
  mutate(., lower = coef - 2*sigma, upper = coef + 2*sigma) %>% 
  inner_join(., filter(df_par_oak, year == 2007),
             by = c("hole" = "hole_number"))
oakmont_glht_df$par[9] <- "4/5"


## ---- include = FALSE------------------------------------------------------------------
oak_reg_df %>% 
  dplyr::group_by(., year) %>% 
  dplyr::summarize(n = n_distinct(player))


## --------------------------------------------------------------------------------------
options(knitr.kable.NA = ' ')
tmp <- tidy(anova(lm_pebble))[, c(1, 2, 4, 5, 6)] %>% 
  bind_cols(tidy(anova(lm_oakmont))[, c(2, 4, 5, 6)])
tmp$term[1:3] <- c("Hole", "Year", "Hole*Year")
colnames(tmp)[1:9] <- c("", "DF", "SS", "F Stat.", "P-value", "DF", "SS", "F Stat.", "P-value")
kable(tmp, "latex", booktabs = T, digits = 3, linesep = "", escape = F, caption = "\\label{tab:anova} The estimated two-way layout ANOVA models for the data at each course under study.") %>%
  add_header_above(c("", "Pebble Beach" = 4, "Oakmont" = 4), italic = T) %>%
  kable_styling(latex_options = "striped") %>% 
  column_spec(1, border_right = T, bold = T) %>% 
  column_spec(5, border_right = T)
# tmp <- tidy(anova(lm_pebble)) %>% 
#   bind_cols(tidy(anova(lm_oakmont))[, -1])
# tmp$term[1:3] <- c("Hole", "Year", "Hole*Year")
# # colnames(tmp)[1:5] <- c("", "DF", "SS", "MS", "F", "P-value")
# kable(tmp, "latex", booktabs = T, digits = 3, linesep = "", escape = F, caption = "\\label{tab:anova-pebble} The two-way layout ANOVA.\\newline") %>%
#   kable_styling(latex_options = "striped")


## --------------------------------------------------------------------------------------
options(knitr.kable.NA = ' ')
tmp <- pebble_glht_df[, c(1, 7, 2, 3)] %>% 
  bind_cols(oakmont_glht_df[, c(7, 2, 3)])
# tmp$`par...2`[2] <- cell_spec(tmp$`par...2`[2], bold = T)
# tmp$`coef...3`[2] <- cell_spec(tmp$`coef...3`[2], bold = T)
# tmp$`sigma...4`[2] <- cell_spec(tmp$`sigma...4`[2], bold = T)
colnames(tmp)[1:7] <- c("Hole", "Par", "Coef.", "Std. Err.", "Par", "Coef.", "Std. Err.")
kable(tmp, "latex", booktabs = T, digits = 3, linesep = "", escape = F, caption = "\\label{tab:contrasts} The before changing par minus after changing par effect sizes for each hole on each golf course. Recall that only hole two and hole nine actually changed par at Pebble Beach and Oakmont, respectively. Note that positive values indicate that the hole played easier in the latter tournaments (2000 at Pebble Beach and 2007 at Oakmont). The estimated coefficients and standard errors are found using the constrast defined in Equation (2) and general linear hypothesis testing theory. Note that hole five at Pebble Beach was substantially redesigned prior to 2000 and, as a result, played significantly more difficult than its 1992 version.") %>%
  add_header_above(c("", "Pebble Beach" = 3, "Oakmont" = 3), italic = T) %>%
  kable_styling(latex_options = "striped") %>% 
  column_spec(1, border_right = T, bold = T) %>% 
  column_spec(4, border_right = T)


## ----bar-charts-pb, fig.width = 6.5, fig.cap = "The relative frequency of scores on Pebble Beach Golf Links' second hole by round. The black bars are relative frequencies for tournaments prior to 2000 and the grey bars are relative frequencies for the 2000 and 2010 tournaments."----
p <- ggplot(data = df %>%
              filter(hole_number == 2,
                     sum_of_strokes < 9,
                     round_number < 5, 
                     year %in% c(1992, 2000)) %>%
              mutate(year_ind = if_else(year < 2000, "< 2000", "≥ 2000"),
                     round = factor(round_number, labels = c("round 1",
                                                             "round 2",
                                                             "round 3",
                                                             "round 4"))),
            aes(sum_of_strokes))
p + geom_bar(aes(fill = as.factor(year),
                 y = ..prop..),
             position = position_dodge(preserve = "single"),
             stat = "count",
             col = "black") +
  facet_wrap(~ round) +
  scale_fill_grey("") +
  scale_x_continuous(breaks = 3:8, labels = 3:8) +
  scale_y_continuous(labels = scales::percent,
                     breaks = seq(0, 0.6, by = .1)) +
  xlab("strokes") +
  ylab("relative frequency") +
  theme_bw()


## ----bar-charts-oakmont, fig.width = 6.5, fig.cap = "The relative frequency of scores on Oakmont Country Club's ninth hole by round. The black bars are relative frequencies for tournaments prior to 2007 and the grey bars are relative frequencies for the 2007 and 2016 tournments."----
p <- ggplot(data = df_oak %>%
              filter(hole_number == 9,
                     sum_of_strokes < 9,
                     round_number < 5,
                     year %in% c(1994, 2007)) %>%
              mutate(year_ind = if_else(year < 2007, "< 2007", "≥ 2007"),
                     round = factor(round_number, labels = c("round 1",
                                                             "round 2",
                                                             "round 3",
                                                             "round 4"))),
            aes(sum_of_strokes))
p + geom_bar(aes(fill = as.factor(year),
                 y = ..prop..),
             position = position_dodge(preserve = "single"),
             stat = "count",
             col = "black") +
  facet_wrap(~ round) +
  scale_fill_grey("") +
  scale_x_continuous(breaks = 3:8, labels = 3:8) +
  scale_y_continuous(labels = scales::percent,
                     breaks = seq(0, 0.6, by = .1)) +
  xlab("strokes") +
  ylab("relative frequency") +
  theme_bw()


## ----pebble-y-hat, fig.width = 7, fig.height = 4, fig.cap = "The estimated fitted values of the model defined in Equation (1) using the data from Pebble Beach. Note that hole five at Pebble Beach was substantially redesigned prior to 2000 and, as a result, played significantly more difficult than its 1992 version."----
p <- ggplot(res, 
            aes(x = hole_number, 
                y = sum_of_strokes, 
                color = year_ind)) 

p + geom_errorbar(aes(ymin = sum_of_strokes - 2*se, 
                    ymax = sum_of_strokes + 2*se), 
                width = .2, size = 0.7, position = position_dodge(.2)) +
  geom_point(shape = 15, size = 4, position = position_dodge(.2)) +
  theme_bw() +
  labs(x = "hole number",
       y = "sum of strokes") +
  scale_color_manual("", labels = c("2000", "1992"),
                     values = c("#bdbdbd", "#636363")) 


## ----pebble-effect-size, fig.width = 7, fig.height = 4, fig.cap = "The effect sizes for each hole at Pebble Beach, as estimated using Equation (2). The raw values are given in Table (2). Note that hole five at Pebble Beach was substantially redesigned prior to 2000 and, as a result, played significantly more difficult than its 1992 version."----
p <- ggplot(data = pebble_glht_df,
            aes(x = reorder(as.factor(hole), coef), y = coef,
                col = as.factor(par),
                shape = as.factor(par)))
p + geom_point() +
  geom_errorbar(aes(ymin = lower,
                    ymax = upper)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  scale_color_brewer("par", palette = "Set2") +
  scale_shape("par") +
  labs(y = "average sum of strokes (before - after change)",
       x = "hole") +
  scale_y_continuous(limits = c(-.4, .4),
                     labels = seq(-.4, .4, by = .2)) +
  theme_bw()


## ----oakmont-y-hat, fig.width = 7, fig.height = 4, fig.cap = "The estimated fitted values of the model defined in Equation (1) using the data from Oakmont."----
p <- ggplot(res_oak, 
            aes(x = hole_number, 
                y = sum_of_strokes, 
                color = year_ind)) 

p + geom_errorbar(aes(ymin = sum_of_strokes - 2*se, 
                    ymax = sum_of_strokes + 2*se), 
                width = .2, size = 0.7, position = position_dodge(.2)) +
  geom_point(shape = 15, size = 4, position = position_dodge(.2)) +
  theme_bw() +
  labs(x = "hole number",
       y = "sum of strokes") +
  scale_color_manual("", labels = c("2007", "1994"),
                     values = c("#bdbdbd", "#636363"))


## ----oak-effect-size, fig.width = 7, fig.height = 4, fig.cap = "The effect sizes for each hole at Oakmont, as estimated using Equation (2). The raw values are given in Table (2)."----
p <- ggplot(data = oakmont_glht_df,
            aes(x = reorder(as.factor(hole), coef), y = coef,
                col = as.factor(par),
                shape = as.factor(par)))
p + geom_point() +
  geom_errorbar(aes(ymin = lower,
                    ymax = upper)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  scale_color_brewer("par", palette = "Set2") +
  scale_shape("par") +
  labs(y = "average sum of strokes (before - after change)",
       x = "hole") +
  scale_y_continuous(limits = c(-.4, .4),
                     labels = seq(-.4, .4, by = .2)) +
  theme_bw()


## ---- include = F----------------------------------------------------------------------
peb_1992 <- filter(peb_reg_df, year == 1992)
peb_2000 <- filter(peb_reg_df, year == 2000)

oak_1994 <- filter(oak_reg_df, year == 1994)
oak_2007 <- filter(oak_reg_df, year == 2007)
players <- unique(peb_1992$player)[unique(peb_1992$player) 
                                   %in% unique(peb_2000$player)]
peb_reg_df_2 <- peb_reg_df %>% 
  mutate(both = if_else(player %in% players, TRUE, FALSE))


## ----pb-two-cohorts, fig.width = 6.5, fig.cap = "The relative frequency of raw scores for players who played in both tournaments at Pebble Beach (gray) versus those who did not (black)."----
p <- ggplot(data = peb_reg_df_2 %>%
              filter(hole_number == 2),
            aes(x = sum_of_strokes))

p + geom_bar(aes(fill = both,
                 y = ..prop..),
             position = position_dodge(preserve = "single"),
             col = "black") +
  scale_fill_grey("", labels = c("1992", "1992/2000")) +
  scale_x_continuous(breaks = 3:8, labels = 3:8) +
  scale_y_continuous(labels = scales::percent,
                     breaks = seq(0, 0.5, by = .1)) +
  xlab("strokes") +
  ylab("relative frequency") +
  theme_bw()


## ---- include = F----------------------------------------------------------------------
players <- unique(oak_1994$player)[unique(oak_1994$player) 
                                   %in% unique(oak_2007$player)]
oak_reg_df_2 <- oak_reg_df %>% 
  mutate(both = if_else(player %in% players, TRUE, FALSE))


## ----oak-two-cohorts, fig.width = 6.5, fig.cap = "The relative frequency of raw scores for players who played in both tournaments at Oakmont (gray) versus those who did not (black)."----
p <- ggplot(data = oak_reg_df_2 %>%
              filter(hole_number == 9),
            aes(x = sum_of_strokes))

p + geom_bar(aes(fill = both,
                 y = ..prop..),
             position = position_dodge(preserve = "single"),
             col = "black") +
  scale_fill_grey("", labels = c("1994", "1994/2007")) +
  scale_x_continuous(breaks = 3:8, labels = 3:8) +
  scale_y_continuous(labels = scales::percent,
                     breaks = seq(0, 0.5, by = .1)) +
  xlab("strokes") +
  ylab("relative frequency") +
  theme_bw()

