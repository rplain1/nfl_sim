library(duckdb)
library(tidyverse)


con <- dbConnect(duckdb(), "~/Projects/nfl_etl/data/luna.duckdb")

dbListTables(con)

### Game lines

tbl(con, Id("BASE", "NFLFASTR_PBP")) |> 
  filter(season >= 2011) |> 
  group_by(season, week, game_id, season_type, home_team, away_team) |> 
  summarise(
    spread_line = max(spread_line),
    total_line = max(total_line),
    home_score = max(home_score),
    away_score = max(away_score),
    result = max(result),
    .groups = 'drop'
  ) |> 
  collect() -> df_base

df_stage <- df_base |> 
  pivot_longer(
    cols = c(home_team, away_team),
    names_to = 'home_away',
    values_to = 'team'
  ) |> 
  mutate(
    outcome = case_when(
      result == 0 ~ 'TIE',
      home_away == 'home_team' & result > 0 ~ 'WIN',
      home_away == 'away_team' & result < 0 ~ 'WIN',
      home_away == 'home_team' & result < 0 ~ 'LOSS',
      home_away == 'away_team' & result > 0 ~ 'LOSS',
      TRUE ~ NA_character_
    ),
    score = if_else(home_away == 'home_team', home_score, away_score),
    implied_score = case_when(
      home_away == 'home_team' & spread_line > 0 ~ (total_line + spread_line) / 2,
      home_away == 'away_team' & spread_line < 0 ~ (total_line + spread_line) / 2,
      home_away == 'home_team' & spread_line < 0 ~ (total_line - spread_line) / 2,
      home_away == 'away_team' & spread_line > 0 ~ (total_line - spread_line) / 2,
      spread_line == 0 ~ total_line / 2,
      TRUE ~ NA_real_
    )
  ) |> 
  mutate(total_score = home_score + away_score) |> 
  select(season, week, game_id, season_type, team, home_away, spread_line, total_line, outcome, score, implied_score, total_score) 

df <- df_stage |> 
  pivot_wider(
    id_cols = c(season, week, game_id, season_type),
    names_from = home_away,
    values_from = team
  ) |> 
  left_join(df_stage)

df

### EPA 

off_epa_rolling <- tbl(con, Id("BASE", "NFLFASTR_PBP")) |> 
  filter(season >= 2010) |> 
  filter(play == 1) |> 
  select(season, posteam, week, epa) |> 
  group_by(season, posteam, week) |> 
  summarise(offense_epa = mean(epa, na.rm = TRUE), .groups = 'drop') |>
  mutate(
    offense_epa_rolling = sql("AVG(offense_epa) OVER (PARTITION BY season, posteam ORDER BY week ROWS BETWEEN 4 PRECEDING AND 1 PRECEDING)"),
    offense_epa_season = sql("AVG(offense_epa) OVER (PARTITION BY season, posteam ORDER BY week ROWS BETWEEN UNBOUNDED PRECEDING AND 1 PRECEDING)")
  )

def_epa_rolling <- tbl(con, Id("BASE", "NFLFASTR_PBP")) |> 
  filter(season >= 2010) |> 
  filter(play == 1) |> 
  select(season, defteam, week, epa) |> 
  group_by(season, defteam, week) |> 
  summarise(defense_epa = mean(epa, na.rm = TRUE), .groups = 'drop') |>
  mutate(
    defense_epa_rolling = sql("AVG(defense_epa) OVER (PARTITION BY season, defteam ORDER BY week ROWS BETWEEN 4 PRECEDING AND 1 PRECEDING)"),
    defense_epa_season = sql("AVG(defense_epa) OVER (PARTITION BY season, defteam ORDER BY week ROWS BETWEEN UNBOUNDED PRECEDING AND 1 PRECEDING)")
  ) 

df_team_epa <- off_epa_rolling |> 
  left_join(def_epa_rolling, by = c('season', 'week', 'posteam' = 'defteam')) |> 
  collect() |> 
  rename(team = posteam) |> 
  group_by(team) |> 
  arrange(season, week, .by_group = TRUE) |> 
  mutate(
    offense_epa = if_else(is.na(offense_epa_season), lag(offense_epa_season), offense_epa_season),
    defense_epa = if_else(is.na(defense_epa_season), lag(defense_epa_season), defense_epa_season),
  ) |> 
  ungroup() |> 
  filter(season > 2010) |> 
  select(-contains('_rolling'), -contains('_season'))


df_merged <-  df |> 
  left_join(df_team_epa, by = c('season', 'week', 'home_team' = 'team')) |> 
  left_join(df_team_epa, by = c('season', 'week', 'away_team' = 'team'), suffix = c('_home', '_away')) 


df_merged |> 
  mutate(
    total_off_epa = offense_epa_home + offense_epa_away,
    total_def_epa = defense_epa_home + defense_epa_away
  ) |> 
  select(where(is.numeric)) |> cor() |> round(digits = 2)

df_model <- df_merged |> 
  mutate(across(contains('epa'), rethinking::standardize)) |> 
  mutate(offense_epa = offense_epa_home + offense_epa_away,
         defense_epa = defense_epa_away + defense_epa_home) |> 
  count(season, week, game_id, total_score, offense_epa, defense_epa)


library(brms)
library(tidybayes)

mod <- brm(
  total_score ~ offense_epa + defense_epa,
  data = df_model, family = negbinomial(),
  prior = c(
    prior(exponential(1), class = 'Intercept'),
    prior(normal(0, 3), class = 'b'), 
    prior(exponential(1), class = 'shape')
  ),
  chains = 4,
  cores = 4,
  warmup = 1000,
  iter = 3500,
  seed = 4,
  backend = 'cmdstan'
)

mod <- brm(
  total_score ~ offense_epa + defense_epa,
  data = df_model, family = gaussian(),
  prior = c(
    prior(normal(45, 2), class = 'Intercept'),
    prior(normal(0, 1), class = 'b'), 
    prior(exponential(2), class = 'sigma')
  ),
  chains = 4,
  cores = 4,
  warmup = 1000,
  iter = 3500,
  seed = 4,
  backend = 'cmdstan'
)

pp_check(mod)
summary(mod)
mcmc_plot(mod)
as_draws_df(mod) |> 
  ggplot(aes(b_offense_epa)) +
  tidybayes::stat_halfeye()

preds <-
  df_model |> 
  bind_cols(
    fitted(mod, newdata = df_model) |> 
      as_tibble() |> 
      rename(ll = Q2.5, ul = Q97.5) |> 
      bind_cols(
        predict(mod, newdata = df_model) |> 
          as_tibble() |> 
          select(p_ll = Q2.5, p_ul = Q97.5)
      )
  )


preds |> 
  ggplot(aes(Estimate, total_score)) +
  geom_ribbon(aes(ymin = ll, ymax = ul),
              fill = "firebrick", alpha = 1/5) +
  geom_ribbon(aes(ymin = p_ll, ymax = p_ul),
              stat = "identity",
              fill = "firebrick4", color = "firebrick4", alpha = 1/5, linewidth = 1/2) +
  geom_point(size = 2, color = "firebrick4", alpha = 1/3) 


df_preds <- df_model |> 
  filter(season >= 2024) |> 
  tidybayes::add_predicted_draws(mod) |> 
  ungroup()


df_preds |> 
  left_join(df_merged |> count(game_id, total_line) |> select(-n)) |>  
  group_by(season, week, game_id, total_line) |> 
  summarise(
    model_over = mean(.prediction >= total_line),
    total_score = max(total_score),
    .groups = 'drop'
  ) |> 
  mutate(pred = model_over > .50) |> 
  count(actual = total_score >= total_line, pred ) |> 
  mutate(
    condition = case_when(
      actual & pred ~ 'TP',
      actual & !pred ~ 'FN',
      !actual & pred ~ 'FP',
      !actual & !pred ~ 'TN'
    ),
    accurate = sum(ifelse(actual == pred, n, 0)) / sum(n)
  )

df_preds |> 
  #filter(game_id == '2024_20_BAL_BUF') |> 
  summarise(
    q0 = min(.prediction),
    q2.5 = quantile(.prediction, .025),
    q25 = quantile(.prediction, 0.25),
    q50 = quantile(.prediction, 0.5),
    q75 = quantile(.prediction, 0.75),
    q97.5 = quantile(.prediction, 0.975),
    q100 = max(.prediction)
  )

df_preds |> 
  filter(.prediction < 0)

df_test <- preds |>
  left_join(df_merged |> count(game_id, total_line)) |>
  select(Estimate, total_line)

lm(total_line ~ Estimate, data = df_test) |> 
  summary()

df_preds



### Plays


df_plays <- tbl(con, Id("BASE", "NFLFASTR_PBP")) |> 
  filter(season >= 2011) |> 
  filter(play == 1, penalty == 0) |> 
  mutate(head_coach = if_else(posteam == home_team, home_coach, away_coach)) |> 
  count(season, week, game_id, posteam, head_coach) 

df_passers <- tbl(con, Id("BASE", "NFLFASTR_PBP")) |> 
  filter(season >= 2011) |> 
  filter(!is.na(passer_id)) |> 
  filter(play == 1, penalty == 0) |> 
  mutate(head_coach = if_else(posteam == home_team, home_coach, away_coach)) |> 
  count(season, week, game_id, posteam, passer_id, passer) |> 
  collect() |> 
  group_by(posteam, game_id) |> 
  arrange(desc(n), .by_group = TRUE) |> 
  mutate(rn = row_number()) |> 
  filter(rn == 1) |> 
  ungroup() |> 
  select(-rn, -n)

assertthat::are_equal(df_passers |> count(game_id) |> pull(n) |> min(), 2)
assertthat::are_equal(df_passers |> count(game_id) |> pull(n) |> max(), 2)

df_plays_model <- df_plays |> 
  collect() |> 
  left_join(df_passers)

mod2 <- brm(
  n ~ 0 + (1|head_coach) + (1|passer_id),
  family = gaussian(),
  data = df_plays_model,
  chains = 4,
  cores = 4,
  warmup = 1000,
  iter = 3500,
  seed = 4,
  backend = 'cmdstan'
  
)

plot(mod2)
mcmc_plot(mod2)
pp_check(mod2)

summary(mod2)

plays_preds <-
  df_plays_model |> 
  bind_cols(
    fitted(mod2, newdata = df_plays_model) |> 
      as_tibble() |> 
      rename(ll = Q2.5, ul = Q97.5) |> 
      bind_cols(
        predict(mod2, newdata = df_plays_model) |> 
          as_tibble() |> 
          select(p_ll = Q2.5, p_ul = Q97.5)
      )
  )


plays_preds |> 
  ggplot(aes(Estimate, n)) +
  geom_ribbon(aes(ymin = ll, ymax = ul),
              fill = "firebrick", alpha = 1/5) +
  geom_ribbon(aes(ymin = p_ll, ymax = p_ul),
              stat = "identity",
              fill = "firebrick4", color = "firebrick4", alpha = 1/5, linewidth = 1/2) +
  geom_point(size = 2, color = "firebrick4", alpha = 1/3) 


plays_preds |> 
  mutate(se = (Estimate - n)) |> 
  summarise(rmse = sqrt(mean(se**2)))



df_plays_preds <- df_plays_model |> 
  filter(season >= 2024) |> 
  tidybayes::add_predicted_draws(mod2) |> 
  ungroup()


df_plays_preds |> 
  group_by(season, week, game_id, posteam, n) |> 
  median_qi(.prediction, .width = 0.75) |> 
  mutate(
    inner_third = between(n, .lower, .upper)
  ) |> 
  count(inner_third) |> 
  mutate(perc = n / sum(n))

nd = tibble(head_coach = rep('John Harbaugh', 2), passer_id = c('00-0034796', '00-0031409'))
nd = tibble(head_coach = c('Kliff Kingsbury', 'Adam Gase'), passer_id = c('00-0033873', '00-0033873'))
predict(mod2, newdata = nd)

nd |> 
  tidybayes::add_predicted_draws(mod2) |> 
  median_qi(.prediction, .width = c(.95, .8, .5)) |> 
  ggplot(aes(y = fct_rev(head_coach), x = .prediction)) +
  tidybayes::theme_tidybayes() +
  geom_interval(aes(xmin = .lower, xmax = .upper)) +
  scale_color_brewer()