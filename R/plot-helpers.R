library(ggplot2)

ba_palette <- c("#3d6b78", "#60bb6b", "#d52f59", "#f5b835", "#2dbbd6",
                "#816b93", "#b84f80", "#f08c3e", "#c1b97d", "#7e450a",
                "#d4d700", "#00978f")

ba_palette_ramp <- colorRampPalette(ba_palette)

theme_set(theme_bw() +
            theme(strip.background = element_blank()))

ggplot <- function(...) {
  ggplot2::ggplot(...) + 
    scale_colour_manual(values = ba_palette) +
    scale_fill_manual(values = ba_palette)
}


get_plot_df <- function(df) {
  base_profile_by_var <- function(df) {
    df %>%
      select(Wh, !!attribute_names) %>%
      gather(Variable, Value, -Wh) %>%
      filter(Value == FALSE) %>% 
      group_by(Variable) %>%
      summarise(Absent = mean(Wh))
  }
  
  base_wh_df <- df %>% 
    mutate(base_wh = map(Data, base_profile_by_var)) %>% 
    select(Season, Hour, base_wh) %>% 
    unnest(base_wh)
  
  
  plot_df <- df %>% 
    select(Season, Hour, Coefficients) %>% 
    filter(!map_lgl(Coefficients, is_null)) %>% 
    unnest()
  
  # Set excluded variables to zero and CI's to NA
  plot_df <- plot_df %>%
    tidyr::expand(Season, Hour, Variable) %>% 
    left_join(plot_df) %>% 
    mutate(Variable = str_replace(Variable, "TRUE", "")) #%>% 
  #mutate(Estimate = if_else(is.na(Estimate), 0, Estimate))
  
  plot_impact_df <- plot_df %>% 
    inner_join(base_wh_df, by = c("Season", "Hour", "Variable")) %>% 
    mutate(Present = Absent*exp(Estimate - 0.5*ASE^2),  # Kennedy's estimator
           Impact = Present - Absent) %>% 
    select(Variable, Season, Hour, Present, Absent, Impact) %>% 
    gather(Profiles, Value, -c(Variable, Season, Hour)) %>% 
    mutate(FacetRow = if_else(Profiles == "Impact", "Impact", "Profiles"),
           FacetRow = fct_relevel(FacetRow, "Impact", after = 1))
  
  list(plot_df = plot_df,
       plot_impact_df = plot_impact_df)
}


plot_profile_impact <- function(df, variable) {
  ggplot() +
    geom_line(data = df %>% 
                filter(Variable == !!variable,
                       Profiles %in% c("Present", "Absent")) %>% 
                mutate(Value = Value*4),  # FIXME: Hack to convert Wh to W
              aes(x = Hour, y = Value, linetype = Profiles)) +
    geom_area(data = df %>% 
                filter(Variable == !!variable,
                       Profiles %in% c("Impact")) %>% 
                mutate(Value = Value*4),  # FIXME: Hack to convert Wh to W,
              aes(x = Hour, y = Value), alpha = 0.3, fill = ba_palette[1]) +
    facet_grid(FacetRow ~ Season, space = "free_y", scales = "free_y") +
    labs(y = expression(Normalised~electricity~(W/m^2)),
         # linetype = "Attribute") +
         linetype = camel_case_to_normal(variable)) +
    theme(legend.position = "bottom") +
    scale_linetype_manual(values=c("dashed", "solid"),
                          limits=c("Present", "Absent"))
}


camel_case_to_normal <- function(x) {
  # gsub("([a-z])([A-Z])", "\\1 \\L\\2", x, perl = TRUE)
  gsub("([a-z])([A-Z])", "\\1 \\2", x)
}
