#' Dredge for lme4 lmer
#'
#' Fits a mixed effects model and then dredges.
#'
#' @param data data frame
#' @param termlabels character vector giving the right-hand side of a model
#'   formula. Cannot be zero-length.
#'
#' @return
#' @export
#'
#' @examples
n_dredge <- function(data, termlabels) {
  # FIXME: Need to create formula here otherwise get.models fails. Uses the
  # formula environment in some of its operations. Really annoying but can't
  # figure out a way around this.
  formula <- reformulate(termlabels, response=quote(log(Wh)))
  
  global_model <- lmer(formula,
                       data = data,
                       REML = FALSE)
  
  output <- dredge(global_model)
  
  # FIXME: Add this attribute to model.selection object because attempting to
  # calculate R2 values in separate function seems to introduce issues that I
  # suspect are related to environment stuff. Either get.models or the lmer
  # function seem to be a bit buggy when dealing with multiple environments.
  attr(output, "best_model_r2") <- get.models(output, subset = 1, data = data, REML = FALSE)[[1]] %>% 
    r.squaredGLMM() %>% 
    as_tibble()
  
  output
}


#' Dredge for random intercept lme model
#'
#' Fits a random intercept model and runs dredge.
#'
#' This is an old version that uses nlme's lme function. Need to have formulas
#' specified inside the function as dredge uses the environment of the formula
#' for some of its machinations.
#'
#' @param data data frame
#'
#' @return model.selection object
#' @export
n_ri_dredge <- function(data) {
  global_model <- lme(data = data,
                      fixed = log(Wh) ~ TenantFeed + WaterCooledCondenser + DXSystem + GasFiredBoiler + ElectricElementHeating + CentralDist,
                      random = ~ 1 | BID,
                      # correlation = corAR1(),
                      control=lmeControl(maxIter = 100,
                                         returnObject=TRUE),
                      method = "ML")

  dredge(global_model)
}


#' Dredge for random intercept and slope lme model
#'
#' Fits a random intercept and slope mixed effects model and runs dredge.
#'
#' This is an old version that uses nlme's lme function. Need to have formulas
#' specified inside the function as dredge uses the environment of the formula
#' for some of its machinations.
#'
#' @param data data frame
#'
#' @return model.selection object
#' @export
n_ris_dredge <- function(data) {
  global_model <- lme(data = data,
                      fixed = log(Wh) ~ TenantFeed + WaterCooledCondenser + DXSystem + GasFiredBoiler + ElectricElementHeating + CentralDist,
                      random = ~ Year | BID,
                      # correlation = corAR1(),
                      control=lmeControl(maxIter = 100,
                                         returnObject=TRUE),
                      method = "ML")

  dredge(global_model)
}


#' Model averaging
#'
#' @param dredge model.selection object
#' @param level cumulative sum of weights cutoff level
#'
#' @return averaging object
#' @export
n_mmavg <- function(dredge, level = 0.95) {
  model.avg(dredge, subset = cumsum(weight) < level)
}


#' Coefficients and confidence intervals
#'
#' @param mmavg averaging object
#' @param full full model averaging. Defaults to FALSE.
#' @param level vector. Confidence interval levels.
#'
#' @return Dataframe containing coefficient estimates and confidence intervals.
#' @export
n_coef <- function(mmavg, full = FALSE, level = c(0.8, 0.9)) {
  coef_df <- coefTable(mmavg, full = full, adjust.se = TRUE) %>% 
    as.data.frame() %>% 
    select(Estimate, ASE = `Std. Error`) %>% 
    rownames_to_column("Variable")
  
  for (iL in level) {
    coef_df <- confint(mmavg, full = full, level = iL) %>% 
      as.data.frame() %>% 
      rownames_to_column("Variable") %>% 
      inner_join(coef_df, ., by = "Variable")
  }
  
  coef_df
}


#' Filter outliers
#' 
#' Trims data to 1st and 99th quantiles.
#' 
#' @param df dataframe
#'
#' @return dataframe
#' @export
filter_outliers <- function(df, probs) {
  quantile_range <- quantile(df$Wh, probs = probs)
  df %>% 
    filter(Wh >= quantile_range[1],
           Wh <= quantile_range[2],
           Wh > 0)
}
