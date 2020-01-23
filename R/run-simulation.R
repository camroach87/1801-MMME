#' Create simulated time series for buildings
#'
#' @param A 
#' @param p 
#' @param n_bldng 
#' @param ts_horizon 
#' @param n_period 
#'
#' @return
#' @export
#'
#' @examples
sim_profiles <- function(A, p, n_bldng, ts_horizon, n_period) {
  n_attr <- nrow(A)
  
  X <- p %>% 
    map(~ matrix(rbinom(n_sim_bldng, 1, .), nrow = n_sim_bldng)) %>% 
    bind_cols() %>% 
    as.matrix()
  colnames(X) <- rownames(A)
  
  W <- 1 + matrix(rnorm(n_sim_bldng*n_attr, 0, sd_a), nrow = n_sim_bldng)
  
  # Create expected profile for each building (and replicate to add to arma proc)
  E_P <- (X*W) %*% A
  E_P <- do.call(cbind, replicate(ts_horizon/n_period, E_P, simplify=FALSE))
  dimnames(E_P) <- NULL
  
  # Add model error
  # NOTE: sd_e is the s.d. of the white noise in the ARMA model. NOT the s.d. of 
  # the predicted values. Var(y_t) = \sigma^2/(1-phi^2) for an AR(1) model
  Y <- replicate(n_sim_bldng, arima.sim(list(ar = ar_1, ma = ma_1), sd = sd_e, ts_horizon))
  Y <- t(Y) + E_P
  Y <- exp(Y)
  E_P <- exp(E_P)
  
  list(E_P = E_P, Y = Y, X = X)
}


A <- sim_params %>% 
  select(Variable, Period, A) %>% 
  spread(Period, A) %>% 
  column_to_rownames("Variable") %>% 
  as.matrix()

p <- sim_params %>%
  select(Variable, p) %>% 
  distinct() %>% 
  column_to_rownames("Variable") %>% 
  as.matrix()
p <- p[rownames(A),]

simulations <- sim_profiles(A, p, n_bldng, sim_horizon, n_sim_period)

sim_df <- simulations$Y %>% 
  as_tibble() %>% 
  rownames_to_column("BID") %>% 
  gather(ts, Wh, -BID) %>% 
  mutate(BID = sprintf("SIM%04d", as.numeric(BID)),
         ts = parse_number(ts) - 1,
         Hour = floor((ts %% n_sim_period) / (n_sim_period/24)))

attr_df <- simulations$X %>% 
  as_tibble() %>% 
  rownames_to_column("BID") %>% 
  mutate(BID = sprintf("SIM%04d", as.numeric(BID)))


# Run analysis
n_sim_df <- sim_df %>% 
  inner_join(attr_df, by = "BID") %>% 
  select(-ts) %>% 
  mutate(Season = "Simulation") %>% 
  group_by(Season, Hour, BID) %>% 
  nest(.key = "Data") %>% 
  mutate(Data = map(Data, filter_outliers, probs = outlier_filter_range)) %>% 
  unnest() %>% 
  group_by(Season, Hour) %>% 
  nest(.key = "Data")

options(na.action = "na.fail")

n_sim_df <- n_sim_df %>%
  mutate(Dredge = future_map(Data, possibly(n_dredge, NULL), termlabels = c(attribute_names, "(1|BID)")),
         MMAvg = map(Dredge, possibly(n_mmavg, NULL), level = 0.96),
         Summary = map(MMAvg, possibly(summary, NULL)),
         Coefficients = map(MMAvg, possibly(n_coef, NULL), full = TRUE, level = c(0.8, 0.9)))

options(na.action = "na.omit")
