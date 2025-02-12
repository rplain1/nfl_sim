library(duckdb)
library(tidyverse)
library(brms)
library(tidybayes)

con <- dbConnect(duckdb(), "~/Projects/nfl_etl/data/luna.duckdb")

dbListTables(con)


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