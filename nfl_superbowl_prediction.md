
All models are wrong, I hope this one is useful. We want to predict the
outcome of the 2020 Super Bowl between the Kansas City Chiefs and San
Francisco 49ers. The questions we seek to answer are below.

# 1\. Questions

  - Who is going to win the 2020 Super Bowl between the Kansas City
    Chiefs and San Francisco 49ers?
  - What is the predicted score?
  - What is the probability of the over/under?
  - What is the probability of covering the spread in one direction or
    another?
  - What is the probability of overtime?

# 2\. Data

``` r
# helpers
library(skimr)
library(tidyverse)
library(tictoc)
library(furrr)
library(future)

# bayesian modeling
library(brms)

# visualization
library(tidybayes)
library(ggridges)

# set theme for plots
theme_set(theme_bw(base_family = "Avenir"))

# scripts
source("R/fetch_nfl_plays.R")
```

## 2.1 Source Data

The data for this analysis comes from the
[nflscrapR](https://github.com/mcbarlowe/nflscraper) package that has
play-by-play game logs for the entire season.

``` r
data_dir <- fs::path("..", "data")

# wrapped for nflscrapR to save/load data
plays <- fetch_nfl_plays(season = 2019, 
                         type = c("reg", "post"),
                         data_dir = data_dir)
```

## 2.2 Prepare Data

We will use the outcomes of each game with the home and away team for
our model. To do this, we find the max score for each team in the game
(no negative points).

``` r
# aggregate plays to game level
games <- plays %>%
  group_by(game_id, home_team, away_team) %>%
  summarize(home_score = max(total_home_score),
            away_score = max(total_away_score)) %>%
  ungroup() %>%
  # game_id for first wildcard game (TEN @ NE)
  mutate(playoffs = ifelse(game_id >= 2020010400, TRUE, FALSE))
```

## 2.3 Skim Data

``` r
games %>% skimr::skim()
```

    ## Skim summary statistics
    ##  n obs: 266 
    ##  n variables: 6 
    ## 
    ## ── Variable type:character ────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────
    ##   variable missing complete   n min max empty n_unique
    ##  away_team       0      266 266   2   3     0       32
    ##  home_team       0      266 266   2   3     0       32
    ## 
    ## ── Variable type:logical ──────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────
    ##  variable missing complete   n  mean                    count
    ##  playoffs       0      266 266 0.038 FAL: 256, TRU: 10, NA: 0
    ## 
    ## ── Variable type:numeric ──────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────
    ##    variable missing complete   n     mean        sd    p0   p25   p50
    ##  away_score       0      266 266    22.83     10.26     0    16    23
    ##     game_id       0      266 266 2e+09    172572.37 2e+09 2e+09 2e+09
    ##  home_score       0      266 266    22.82     10.01     0    16    23
    ##    p75  p100     hist
    ##     30    59 ▂▅▇▇▅▂▁▁
    ##  2e+09 2e+09 ▇▁▁▁▁▁▁▁
    ##     30    53 ▁▅▅▇▆▃▁▁

``` r
# playoff games
games %>% tail(10)
```

    ## # A tibble: 10 x 6
    ##       game_id home_team away_team home_score away_score playoffs
    ##         <dbl> <chr>     <chr>          <dbl>      <dbl> <lgl>   
    ##  1 2020010400 HOU       BUF               22         19 TRUE    
    ##  2 2020010401 NE        TEN               13         20 TRUE    
    ##  3 2020010500 NO        MIN               20         26 TRUE    
    ##  4 2020010501 PHI       SEA                9         17 TRUE    
    ##  5 2020011100 SF        MIN               27         10 TRUE    
    ##  6 2020011101 BAL       TEN               12         28 TRUE    
    ##  7 2020011200 KC        HOU               51         31 TRUE    
    ##  8 2020011201 GB        SEA               28         23 TRUE    
    ##  9 2020011900 KC        TEN               35         24 TRUE    
    ## 10 2020011901 SF        GB                37         20 TRUE

## 2.4 Explore

We will explore a few different plots of our data:

1.  Each team’s deviation from the mean points for (home and away games)
2.  Each team’s deviation from the mean points against (home and away
    games)
3.  The shape of the distribution for home and away points for

### 2.4.1 Points For Deviation

``` r
home_teams_for <- games %>%
  mutate(score_centered = home_score - mean(home_score)) %>%
  select(home_team, score_centered) %>%
  mutate(game_type = "home") %>%
  rename(team = home_team)

away_teams_for <- games %>%
  mutate(score_centered = away_score - mean(away_score)) %>%
  select(away_team, score_centered) %>%
  mutate(game_type = "away") %>%
  rename(team = away_team)

teams_for <- bind_rows(home_teams_for, away_teams_for)

teams_for %>%
  ggplot(aes(game_type, score_centered, color = game_type)) +
  geom_boxplot() +
  facet_wrap(~team) +
  coord_flip() +
  labs(
    title = "Points For Deviation from Mean",
    subtitle = "2019 NFL Regular Season & Playoffs",
    x = "",
    y = "",
    caption = "Source: nflscrapR"
  ) + 
  guides(color = FALSE) +
  scale_color_viridis_d(direction = -1,
                        option = "inferno",
                        begin = 0, end = 0.5)
```

<img src="nfl_superbowl_prediction_files/figure-gfm/unnamed-chunk-6-1.png" width="70%" style="display: block; margin: auto;" />

### 2.4.2 Points Against Deviation

``` r
home_teams_against <- games %>%
  mutate(score_centered = away_score - mean(away_score)) %>%
  select(home_team, score_centered) %>%
  mutate(game_type = "home") %>%
  rename(team = home_team)

away_teams_against <- games %>%
  mutate(score_centered = home_score - mean(home_score)) %>%
  select(away_team, score_centered) %>%
  mutate(game_type = "away") %>%
  rename(team = away_team)

teams_against <- bind_rows(home_teams_against, away_teams_against)

teams_against %>%
  ggplot(aes(game_type, score_centered, color = game_type)) +
  geom_boxplot() +
  facet_wrap(~team) +
  coord_flip() +
  labs(
    title = "Points Against Deviation from Mean",
    subtitle = "2019 NFL Regular Season & Playoffs",
    x = "",
    y = "",
    caption = "Source: nflscrapR"
  ) + 
  guides(color = FALSE) +
  scale_color_viridis_d(direction = -1,
                        option = "inferno",
                        begin = 0, end = 0.5)
```

<img src="nfl_superbowl_prediction_files/figure-gfm/unnamed-chunk-7-1.png" width="70%" style="display: block; margin: auto;" />

### 2.4.3 Points Distribution

This is probably the most important plot as it will help inform which
distribution we should select for the model outcome.

``` r
games %>% 
  pivot_longer(cols = c(home_score, away_score),
               names_to = "type", values_to = "score") %>%
  ggplot(aes(score, fill = type)) +
  geom_histogram(binwidth = 5, color = "white") +
  facet_wrap(~type) +
  labs(
    title = "Points For Distribution",
    subtitle = "2019 NFL Regular Season & Playoffs",
    x = "",
    y = "",
    caption = "Source: nflscrapR"
  ) + 
  guides(fill = FALSE) +
  scale_fill_viridis_d(direction = -1,
                        option = "inferno",
                        begin = 0, end = 0.5)
```

<img src="nfl_superbowl_prediction_files/figure-gfm/unnamed-chunk-8-1.png" width="70%" style="display: block; margin: auto;" />

# 3\. Model

The goal of this analysis is to use a super simple model that carries
enough predictive power to be effective. We will use a [hierarchical
bayesian
regression](https://en.wikipedia.org/wiki/Bayesian_hierarchical_modeling)
that treats the home and away team as a random effect for predicting the
home and away score. This pools information together and reduces the
impact of outliers due to having a small sample size for each team (8
home and 8 away games).

In other words, we pull the more extreme values back towards the mean
when estimating the impact each team has on the home or away score
deviation away from the mean.

The outcome of the model is a posterior of the predicted score that we
will use to simulate game outcomes. We will use a poisson distribution
to model the outcome of the home or away score.

## 3.1 Home Team Points

### 3.1.1 Model Fit

``` r
# allow for parallel processing
plan(multiprocess)

# fit model with standard priors (weakly informative) 
home_pts_model <- brms::brm(
  family = poisson(),
  home_score ~ playoffs + (1 | home_team) + (1 | away_team),
  data = games,
  chains = 5, 
  iter = 3000,
  warmup = 1000,
  future = T,
  seed = 408,
  refresh = 0 # prevents print out
)
```

### 3.1.2 Model Summary

``` r
summary(home_pts_model)
```

    ##  Family: poisson 
    ##   Links: mu = log 
    ## Formula: home_score ~ playoffs + (1 | home_team) + (1 | away_team) 
    ##    Data: games (Number of observations: 266) 
    ## Samples: 5 chains, each with iter = 3000; warmup = 1000; thin = 1;
    ##          total post-warmup samples = 10000
    ## 
    ## Group-Level Effects: 
    ## ~away_team (Number of levels: 32) 
    ##               Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
    ## sd(Intercept)     0.19      0.03     0.14     0.26 1.00     2531     4437
    ## 
    ## ~home_team (Number of levels: 32) 
    ##               Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
    ## sd(Intercept)     0.17      0.03     0.13     0.24 1.00     2722     4635
    ## 
    ## Population-Level Effects: 
    ##              Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
    ## Intercept        3.09      0.05     3.00     3.18 1.00     1820     3233
    ## playoffsTRUE     0.10      0.08    -0.05     0.25 1.00    12296     8126
    ## 
    ## Samples were drawn using sampling(NUTS). For each parameter, Eff.Sample 
    ## is a crude measure of effective sample size, and Rhat is the potential 
    ## scale reduction factor on split chains (at convergence, Rhat = 1).

### 3.1.3 Model Evaluation

``` r
bayes_R2(home_pts_model)
```

    ##     Estimate  Est.Error      Q2.5     Q97.5
    ## R2 0.2949869 0.02454187 0.2456469 0.3419949

### 3.1.4 Team Coefficients

Below is the posterior distribution of each team along with the varying
intercept for points for when they are the home team. In other words,
how effective is the offense at home?

``` r
# sample draws of the home_team coefficient for home points
home_pts_for <- home_pts_model %>%
  spread_draws(b_Intercept, r_home_team[home_team,]) %>%
  # get deviation from mean and exponent to bring back to points
  median_qi(home_team_mean = exp(b_Intercept + r_home_team)) %>%
  arrange(desc(home_team_mean))

home_pts_for
```

    ## # A tibble: 32 x 7
    ##    home_team home_team_mean .lower .upper .width .point .interval
    ##    <chr>              <dbl>  <dbl>  <dbl>  <dbl> <chr>  <chr>    
    ##  1 SF                  31.0   27.0   35.5   0.95 median qi       
    ##  2 DAL                 29.2   25.3   33.5   0.95 median qi       
    ##  3 KC                  28.8   25.1   33.1   0.95 median qi       
    ##  4 BAL                 27.4   23.7   31.5   0.95 median qi       
    ##  5 SEA                 26.1   22.4   30.4   0.95 median qi       
    ##  6 NO                  25.5   22.1   29.4   0.95 median qi       
    ##  7 TEN                 24.8   21.3   28.7   0.95 median qi       
    ##  8 CLE                 23.6   20.1   27.5   0.95 median qi       
    ##  9 DET                 23.5   20.1   27.4   0.95 median qi       
    ## 10 GB                  23.4   20.3   27.0   0.95 median qi       
    ## # … with 22 more rows

``` r
home_pts_model %>%
  spread_draws(b_Intercept, r_home_team[home_team, ]) %>%
  mutate(score_mean = exp(b_Intercept + r_home_team)) %>%
  ungroup() %>%
  mutate(team = fct_reorder(home_team, score_mean)) %>%
  mutate(superbowl = ifelse(team %in% c("KC", "SF"), team, "other")) %>%
  ggplot(aes(y = team, x = score_mean,
             fill = superbowl)) +
  geom_density_ridges() +
  guides(fill = FALSE) +
  scale_fill_manual(values = c("#E31837", "#B3995D", "#999999")) +
  labs(
    title = "Points For as the Home Team",
    subtitle = "2019 NFL Regular Season & Playoffs",
    x = "Points For",
    y = "",
    caption = "Source: Bayesian Hierarchical Model"
  )
```

<img src="nfl_superbowl_prediction_files/figure-gfm/unnamed-chunk-13-1.png" width="50%" style="display: block; margin: auto;" />

Below is the posterior distribution of each team along with the varying
intercept for points against when they are the away team. In other
words, how effective is the defense on the road?

``` r
home_pts_model %>%
  spread_draws(b_Intercept, r_away_team[away_team,]) %>%
  # get deviation from mean and exponent to bring back to points
  median_qi(away_team_mean = exp(b_Intercept + r_away_team)) %>%
  arrange(away_team_mean)
```

    ## # A tibble: 32 x 7
    ##    away_team away_team_mean .lower .upper .width .point .interval
    ##    <chr>              <dbl>  <dbl>  <dbl>  <dbl> <chr>  <chr>    
    ##  1 NE                  15.5   12.8   18.5   0.95 median qi       
    ##  2 BUF                 16.6   14.0   19.4   0.95 median qi       
    ##  3 BAL                 17.0   14.3   20.1   0.95 median qi       
    ##  4 TEN                 17.7   15.2   20.7   0.95 median qi       
    ##  5 SF                  18.8   15.9   21.9   0.95 median qi       
    ##  6 KC                  19.3   16.4   22.6   0.95 median qi       
    ##  7 GB                  19.3   16.5   22.4   0.95 median qi       
    ##  8 LAC                 19.6   16.6   22.9   0.95 median qi       
    ##  9 MIN                 19.7   17.0   22.7   0.95 median qi       
    ## 10 CHI                 20.0   16.9   23.4   0.95 median qi       
    ## # … with 22 more rows

``` r
home_pts_model %>%
  spread_draws(b_Intercept, r_away_team[away_team, ]) %>%
  mutate(score_mean = exp(b_Intercept + r_away_team)) %>%
  ungroup() %>%
  mutate(team = fct_reorder(away_team, score_mean)) %>%
  mutate(superbowl = ifelse(team %in% c("KC", "SF"), team, "other")) %>%
  ggplot(aes(y = team, x = score_mean,
             fill = superbowl)) +
  geom_density_ridges() +
  guides(fill = FALSE) +
  scale_fill_manual(values = c("#B3995D", "#E31837", "#999999")) +
  labs(
    title = "Points Against as the Away Team",
    subtitle = "2019 NFL Regular Season & Playoffs",
    x = "Points Against",
    y = "",
    caption = "Source: Bayesian Hierarchical Model"
  )
```

<img src="nfl_superbowl_prediction_files/figure-gfm/unnamed-chunk-15-1.png" width="50%" style="display: block; margin: auto;" />

## 3.2 Away Team Points

### 3.2.1 Model Fit

``` r
plan(multiprocess)

away_pts_model <- brms::brm(
  family = poisson(),
  away_score ~ playoffs + (1 | home_team) + (1 | away_team),
  data = games,
  chains = 5, 
  iter = 3000,
  warmup = 1000,
  future = T,
  seed = 408,
  refresh = 0 # prevents print out
)
```

### 3.1.2 Model Summary

``` r
summary(away_pts_model)
```

    ##  Family: poisson 
    ##   Links: mu = log 
    ## Formula: away_score ~ playoffs + (1 | home_team) + (1 | away_team) 
    ##    Data: games (Number of observations: 266) 
    ## Samples: 5 chains, each with iter = 3000; warmup = 1000; thin = 1;
    ##          total post-warmup samples = 10000
    ## 
    ## Group-Level Effects: 
    ## ~away_team (Number of levels: 32) 
    ##               Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
    ## sd(Intercept)     0.21      0.03     0.15     0.28 1.00     2461     4854
    ## 
    ## ~home_team (Number of levels: 32) 
    ##               Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
    ## sd(Intercept)     0.19      0.03     0.14     0.25 1.00     2529     3926
    ## 
    ## Population-Level Effects: 
    ##              Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
    ## Intercept        3.09      0.05     2.99     3.20 1.00     1797     2890
    ## playoffsTRUE    -0.02      0.08    -0.17     0.13 1.00    10816     8063
    ## 
    ## Samples were drawn using sampling(NUTS). For each parameter, Eff.Sample 
    ## is a crude measure of effective sample size, and Rhat is the potential 
    ## scale reduction factor on split chains (at convergence, Rhat = 1).

### 3.1.3 Model Evaluation

``` r
bayes_R2(away_pts_model)
```

    ##     Estimate  Est.Error      Q2.5     Q97.5
    ## R2 0.3627646 0.02531987 0.3122051 0.4110088

### 3.1.4 Team Coefficients

Below is the posterior distribution of each team along with the varying
intercept for points for when they are the away team. In other words,
how effective is the offense on the road?

``` r
away_pts_model %>%
  spread_draws(b_Intercept, r_away_team[away_team,]) %>%
  # get deviation from mean and exponent to bring back to points
  median_qi(away_team_mean = exp(b_Intercept + r_away_team)) %>%
  arrange(desc(away_team_mean))
```

    ## # A tibble: 32 x 7
    ##    away_team away_team_mean .lower .upper .width .point .interval
    ##    <chr>              <dbl>  <dbl>  <dbl>  <dbl> <chr>  <chr>    
    ##  1 BAL                 33.8   29.3   38.8   0.95 median qi       
    ##  2 KC                  29.7   25.6   34.3   0.95 median qi       
    ##  3 TB                  29.6   25.5   34.1   0.95 median qi       
    ##  4 NO                  27.3   23.5   31.6   0.95 median qi       
    ##  5 LA                  25.7   22.1   29.9   0.95 median qi       
    ##  6 PHI                 25.4   21.8   29.4   0.95 median qi       
    ##  7 TEN                 25.2   21.8   28.9   0.95 median qi       
    ##  8 NE                  24.9   21.4   29.0   0.95 median qi       
    ##  9 ATL                 24.9   21.4   28.9   0.95 median qi       
    ## 10 MIN                 24.6   21.2   28.3   0.95 median qi       
    ## # … with 22 more rows

``` r
away_pts_model %>%
  spread_draws(b_Intercept, r_away_team[away_team, ]) %>%
  mutate(score_mean = exp(b_Intercept + r_away_team)) %>%
  ungroup() %>%
  mutate(team = fct_reorder(away_team, score_mean)) %>%
  mutate(superbowl = ifelse(team %in% c("KC", "SF"), team, "other")) %>%
  ggplot(aes(y = team, x = score_mean,
             fill = superbowl)) +
  geom_density_ridges() +
  guides(fill = FALSE) +
  scale_fill_manual(values = c("#B3995D", "#E31837", "#999999")) +
  labs(
    title = "Points For as the Away Team",
    subtitle = "2019 NFL Regular Season & Playoffs",
    x = "Points For",
    y = "",
    caption = "Source: Bayesian Hierarchical Model"
  )
```

<img src="nfl_superbowl_prediction_files/figure-gfm/unnamed-chunk-20-1.png" width="50%" style="display: block; margin: auto;" />

Below is the posterior distribution of each team along with the varying
intercept for points against when they are the away team. In other
words, how effective is the defense on the road?

``` r
away_pts_model %>%
  spread_draws(b_Intercept, r_home_team[home_team,]) %>%
  # get deviation from mean and exponent to bring back to points
  median_qi(home_team_mean = exp(b_Intercept + r_home_team)) %>%
  arrange(home_team_mean)
```

    ## # A tibble: 32 x 7
    ##    home_team home_team_mean .lower .upper .width .point .interval
    ##    <chr>              <dbl>  <dbl>  <dbl>  <dbl> <chr>  <chr>    
    ##  1 NE                  16.8   14.1   19.9   0.95 median qi       
    ##  2 PIT                 17.3   14.5   20.5   0.95 median qi       
    ##  3 CHI                 17.6   14.7   20.7   0.95 median qi       
    ##  4 BUF                 17.9   15.0   21.3   0.95 median qi       
    ##  5 MIN                 18.1   15.1   21.5   0.95 median qi       
    ##  6 PHI                 18.1   15.3   21.4   0.95 median qi       
    ##  7 DEN                 18.6   15.6   21.9   0.95 median qi       
    ##  8 SF                  19.0   16.1   22.2   0.95 median qi       
    ##  9 DAL                 19.8   16.8   23.3   0.95 median qi       
    ## 10 IND                 20.4   17.2   24.0   0.95 median qi       
    ## # … with 22 more rows

``` r
away_pts_model %>%
  spread_draws(b_Intercept, r_home_team[home_team, ]) %>%
  mutate(score_mean = exp(b_Intercept + r_home_team)) %>%
  ungroup() %>%
  mutate(team = fct_reorder(home_team, score_mean)) %>%
  mutate(superbowl = ifelse(team %in% c("KC", "SF"), team, "other")) %>%
  ggplot(aes(y = team, x = score_mean,
             fill = superbowl)) +
  geom_density_ridges() +
  guides(fill = FALSE) +
  scale_fill_manual(values = c("#E31837", "#B3995D", "#999999")) +
  labs(
    title = "Points Against as the Home Team",
    subtitle = "2019 NFL Regular Season & Playoffs",
    x = "Ponits Against",
    y = "",
    caption = "Source: Bayesian Hierarchical Model"
  )
```

<img src="nfl_superbowl_prediction_files/figure-gfm/unnamed-chunk-22-1.png" width="50%" style="display: block; margin: auto;" />

# 4\. Evaluation

## 4.1 Score Predictions

We will go back through each game to see how well the model predicts the
winner. To predict each winner, we will generate predicted draws from
the predicted posterior distribution. We will also record whether the
home or away team actual won the game to compare to our predicted
outcomes.

``` r
plan(multiprocess)

add_score_predictions <- function(data, ...) {
  
  data %>% 
    add_predicted_draws(..., seed = 408) %>%
    ungroup() %>%
    select(.prediction)
  
}

tic()

# nest makes use of furrr::future_map for functional programming
game_preds <- games %>%
  mutate(
    actual_winner = case_when(
      home_score > away_score ~ "home",
      home_score < away_score ~ "away",
      T ~ "tie"
    ),
    home_score_diff = home_score - away_score
  ) %>%
  group_by(game_id, actual_winner, home_score_diff) %>% 
  nest() %>%
  mutate(
    home_prediction = future_map(data, add_score_predictions, home_pts_model),
    away_prediction = future_map(data, add_score_predictions, away_pts_model)
  ) 

toc()
```

    ## 293.573 sec elapsed

## 4.2 Game Predictions

We then use the predicted posterior draws to calculate the home team’s
win probability. This is based on the share of predicted posterior draws
of the home team that are greater than the away team’s predicted
posterior. Additionally, we also generate a 95% credible interval for
the predicted score differential to use to compare to the actual score
differential.

``` r
game_prediction <- function(home_pred, away_pred) {
  
  # convert to vectors
  home_pred <- home_pred %>% pull()
  away_pred <- away_pred %>% pull()
  
  # what share of home scores are above away scores?
  home_win_prob   <- mean(home_pred > away_pred)
  
  # 95% credible interval for the score differential
  home_score_diff <- quantile(home_pred - away_pred,
                              probs = c(.025, .50, .975)
                              ) %>% t() %>% as_tibble()
  
  names(home_score_diff) <- c("diff_low_95", "diff_mid_50", "diff_high_95")
  
  game_pred <- tibble(home_win_prob) %>% 
    bind_cols(home_score_diff)
  
  return(game_pred)
  
}

# use purrr to apply to each nested dataset
game_preds$outcome_prediction <- map2(game_preds$home_prediction,
                                      game_preds$away_prediction,
                                      game_prediction)
```

## 4.3 Prediction Accuracy

Lastly for model accuracy, we check the accuracy by classifying the home
team as the winner if their predicted probability is greater than 50%.
When then compare that to the actual winner to determine if the
prediction was accurate.

NOTE: Generally, we would want to create a train/test split to evaluate
the model on data it has never seen before. However, due to smaller
sample sizes and only wanting to estimate a single game for the same
season, I am okay with training on all the data.

There are some arguments
[for](https://www.econjobrumors.com/topic/can-someone-explain-to-me-the-bayesian-methods-dont-overfit-argument)
and
[against](https://stats.stackexchange.com/questions/265094/is-it-true-that-bayesian-methods-dont-overfit)
this approach being prone to overfitting the data.

``` r
game_preds <- game_preds %>%
  unnest(outcome_prediction) %>%
  mutate(
    pred_score = case_when(
      home_win_prob > 0.5 & actual_winner == "home" ~ 1,
      home_win_prob < 0.5 & actual_winner == "away" ~ 1,
      home_win_prob > 0.5 & actual_winner == "away" ~ 0,
      home_win_prob < 0.5 & actual_winner == "home" ~ 0,
      T ~ 0
    )
  )
```

What percentage of winner predictions were correct?

``` r
mean(game_preds$pred_score)
```

    ## [1] 0.6992481

What percentage of score differential predictions fell between the
predicted 95% credible
interval?

``` r
mean(between(game_preds$home_score_diff, game_preds$diff_low_95, game_preds$diff_high_95))
```

    ## [1] 0.7969925

# 5\. Super Bowl Prediction

For reference,
[CBS](https://www.cbssports.com/nfl/news/super-bowl-2020-spread-odds-line-points-total-chiefs-remain-favorites-in-close-call-matchup-vs-49ers/)
shows the opening betting lines as -1 for KC Chiefs with an over/under
of 52.5. The current lines, as of January 29th, 2020, are -1.5 for KC
and an over/under of 54.5.

## 5.1 Data Setup

Finally, we will generate predictions for the Super Bowl. Since the game
is being played at a neutral site, we will generate predictions for two
games, one where each team is the home and away team.

``` r
superbowl_tbl <- tibble(
    game_id = c("SB1 - KC Home", "SB2 - SF Home"), 
    home_team = c("KC", "SF"),
    away_team = c("SF", "KC"),
    playoffs  = c(TRUE, TRUE)
  ) %>%
  group_by(game_id) %>%
  nest() %>%
  mutate(
      home_prediction = future_map(data, add_score_predictions, home_pts_model),
      away_prediction = future_map(data, add_score_predictions, away_pts_model)
    ) 

superbowl_tbl
```

    ## # A tibble: 2 x 4
    ## # Groups:   game_id [2]
    ##   game_id       data             home_prediction       away_prediction     
    ##   <chr>         <list>           <list>                <list>              
    ## 1 SB1 - KC Home <tibble [1 × 3]> <tibble [10,000 × 1]> <tibble [10,000 × 1…
    ## 2 SB2 - SF Home <tibble [1 × 3]> <tibble [10,000 × 1]> <tibble [10,000 × 1…

``` r
superbowl_tbl$outcome_prediction <- map2(superbowl_tbl$home_prediction,
                                         superbowl_tbl$away_prediction,
                                         game_prediction)
```

## 5.2 Predicted Games

To generate outcomes, we will sample scores for each team from their
combined home/away posteriors. Arguments could be made to have each team
be the home team, each team be the away team, or to have KC be the home
team since they are listed as such on the game card.

We will take a hybrid approach and weight the the home and away scores
equally. A function is used so this number can easily be changed and
compare outcomes.

``` r
superbowl_outcome <- function(home_team_advantage = .50) {
  
  set.seed(408)
  sample_size <- 10000
  
  kc_home_scores <- superbowl_tbl %>%
    filter(game_id =="SB1 - KC Home") %>%
    pull(home_prediction) %>%
    unlist()
  
  kc_away_scores <- superbowl_tbl %>%
    filter(game_id =="SB2 - SF Home") %>%
    pull(away_prediction) %>%
    unlist()
  
  sf_away_scores <- superbowl_tbl %>%
    filter(game_id =="SB1 - KC Home") %>%
    pull(away_prediction) %>%
    unlist()
  
  sf_home_scores <- superbowl_tbl %>%
    filter(game_id =="SB2 - SF Home") %>%
    pull(home_prediction) %>%
    unlist()
  
  # sample from the home and away score posteriors with more weight towards home scores
  kc_scores <- c(sample(kc_home_scores,
                        size = (sample_size * home_team_advantage), replace = T),
                 sample(kc_away_scores,
                        size = (sample_size * (1 - home_team_advantage)), replace = T))
  # sample from the home and away score posteriors with more weight towards away scores
  sf_scores <- c(sample(sf_away_scores, 
                        size = (sample_size * home_team_advantage), replace = T),
                 sample(sf_home_scores, 
                        size = (sample_size * (1 - home_team_advantage)), replace = T))
  
  # round the sample averages for the final score
  print(glue("KC Score: {median(kc_scores)}"))
  print(glue("SF Score: {median(sf_scores)}"))
  print(glue("KC Win Probability: {mean(kc_scores > sf_scores)}"))
  print(glue("SF Win Probability: {mean(kc_scores < sf_scores)}"))
  print(glue("Tie Probability: {mean(kc_scores == sf_scores)}"))
  
  # combine collected of games
  scores_tbl <- tibble(game = 1:sample_size, kc_scores, sf_scores) %>%
    rename(kc = kc_scores, sf = sf_scores)
  
  return(scores_tbl)
} 
```

We see below that based on the share of simulations, there is a slightly
higher chance for KF (48.6%) to beat SF (47.2%), with 4% of games
resulting in a tie (which cannot happen in the Super Bowl).

The median score for each team is 26. Since this can’t happen, I would
alter this prediction to a 27-26 finish for KC. This aligns with both
the line (-1 for KC) and initial over/under of 52.5.

``` r
scores_tbl <- superbowl_outcome()
```

    ## KC Score: 26
    ## SF Score: 26
    ## KC Win Probability: 0.4864
    ## SF Win Probability: 0.472
    ## Tie Probability: 0.0416

## 5.3 Predicted Score Distribution

Below shows the distribution of scores for each team. SF has a slightly
wider variability, which would seem to indicate a larger boom or bust
potential (high or low scores) compared to KC.

``` r
scores_tbl %>%
  pivot_longer(cols = kc:sf, names_to = "team", values_to = "score") %>%
  ggplot(aes(score, fill = team)) +
  geom_histogram(color = "black",
                 position = "identity",
                 binwidth = 1, alpha = 3/4)  +
  labs(
    title = "Predicted Score Distribution",
    subtitle = "Super Bowl LIV (NFL - 2020)",
    x = "Points Scored",
    y = "",
    caption = "Source: b-clark predictions"
  ) + 
  scale_fill_manual(values = c("#E31837", "#B3995D"))
```

<img src="nfl_superbowl_prediction_files/figure-gfm/unnamed-chunk-32-1.png" width="70%" style="display: block; margin: auto;" />

We can also take the approach of looking at the most common game
outcomes. Using this approach, one could predict a 27-26 outcome for SF
over KC. Each of these scores indicate a close game with less than one
score seperating both teams.

``` r
scores_tbl %>%
  count(kc, sf) %>%
  arrange(desc(n))
```

    ## # A tibble: 1,104 x 3
    ##       kc    sf     n
    ##    <int> <int> <int>
    ##  1    26    27    56
    ##  2    25    25    50
    ##  3    24    25    47
    ##  4    26    26    47
    ##  5    25    24    46
    ##  6    23    27    44
    ##  7    24    23    42
    ##  8    25    26    42
    ##  9    26    23    42
    ## 10    27    21    42
    ## # … with 1,094 more rows

## 5.4 Over/Under

``` r
scores_tbl %>%
  mutate(total_score = sf + kc) %>%
  ggplot(aes(total_score)) +
  geom_histogram(fill = "#2c3e50",
                 color = "black",
                 position = "identity",
                 binwidth = 1, alpha = 3/4)  +
  labs(
    title = "Predicted Total Score Distribution",
    subtitle = "Super Bowl LIV (NFL - 2020)",
    x = "Total Points Scored",
    y = "",
    caption = "Source: b-clark predictions"
  ) 
```

<img src="nfl_superbowl_prediction_files/figure-gfm/unnamed-chunk-34-1.png" width="70%" style="display: block; margin: auto;" />

Given the current line, there is a 61% chance the total score will be
under 54.5 points and a 39% chance it will be over. Given this
information, we would take the under.

``` r
scores_tbl %>%
  mutate(total_score = sf + kc) %>%
  summarize(over_pct = mean(ifelse(total_score > 54.5, 1, 0)),
            under_pct = mean(ifelse(total_score < 54.5, 1, 0)))
```

    ## # A tibble: 1 x 2
    ##   over_pct under_pct
    ##      <dbl>     <dbl>
    ## 1    0.388     0.612

# 6\. Conclusion

## 6.1 Prediction Summary

In conclusion, we used a relatively simple statistical model to produce
simulations of 10,000 games. The final predictions are:

  - KC Chiefs 27 - SF 49ers 26
  - Take the under at 54.5
  - 4% chance of there being overtime

## 6.2 Improvements

There is ample room for improving this model. It doesn’t take into
account several factors that affect performance during the year such as
injuries, weather, or field conditions. Adding variables for stadium
type, field type, and weather could greater improve the the performance
of the model.
