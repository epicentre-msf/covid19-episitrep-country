
# set ggplot theme
ggplot2::theme_set(ggplot2::theme_light(base_size = 14) + theme(panel.grid.minor = element_blank()))
nord_pal <- c("#486090FF", "#6078A8FF", "#7890A8FF", "#90A8C0FF", "#F0D8C0FF", "#D6BBCFFF", "#A8C0C0FF", "#C0D8D8FF", "#A8A890FF")

cm_to_in <- 0.39370079

rounder <- function(x,y) {
  if(y >= 0) { x + (y - x %% y)}
  else { x - (x %% abs(y))}
}


pyramid_brks <- function(x, n = 5) {
  brks <- pretty(0:max(abs(x)), n = n)
  c(-brks, brks)
}

pyramid_limits <- function(x) {
  c(-max(abs(x)), max(abs(x)))
}


add_brks <- function(x, n = 5, style = "jenks") {
  breaks <- classInt::classIntervals(x, n = n, style = style)
  br <- breaks$brks
  cut(x, br, include.lowest = TRUE, labels = label_breaks(br))
}


label_breaks <- function(breaks, big_numbers = FALSE, replace_Inf = TRUE) {
  
  if (big_numbers){
    labs <- sprintf("%s-%s", frmt_num(breaks[1:length(breaks) - 1]), frmt_num(breaks[2:length(breaks)]))
  } else {
    labs <- sprintf("%s-%s", breaks[-length(breaks)], breaks[-1] - 1)
  }
  
  if(replace_Inf){
    labs <- gsub("-Inf", "+", labs)
  }
  
  return(labs)
}



frmt_num <- function(x) {
  scales::label_number_si()(x)
}


guide_axis_label_trans <- function(label_trans = identity, ...) {
  axis_guide <- guide_axis(...)
  axis_guide$label_trans <- rlang::as_function(label_trans)
  class(axis_guide) <- c("guide_axis_trans", class(axis_guide))
  axis_guide
}

guide_train.guide_axis_trans <- function(x, ...) {
  trained <- NextMethod()
  trained$key$.label <- x$label_trans(trained$key$.label)
  trained
}



freq_prct <- function(x, value){
  paste0(sum(x == value, na.rm = TRUE), 
         ' (', 
         format(round(sum(x == value, na.rm = TRUE) / sum(!is.na(x)) * 100, digits = 1), nsmall = 1), 
         ')')
}



# Formatting Confidence Intervals
combine_ci <- function(lwr, upr, digits = 1) {
  
  if (is.na(lwr) & is.na(upr)) {
    x <- NA
  } else {
    x <- sprintf(glue("[%.{digits}f - %.{digits}f]"), 
                 round(lwr, digits = digits),
                 round(upr, digits = digits))
  }
  return(x)
}






cbind_diff <- function(x = list()){
  # Find max length
  max_length <- max(unlist(lapply(x, length)))
  
  # Set length of each vector as
  res <- lapply(x, function(x){
    length(x) <- max_length
    return(x)
  })
  
  return(as.data.frame(res))
}




plot_map_world_count <- function(tbl_dta, series){
  
  legend_title <- switch(series, 
                         cases  = 'Covid-19 cases', 
                         deaths = 'Covid-19 associated deaths')
  
  plot_title <- switch(series, 
                       cases  = 'Cases count', 
                       deaths = 'Deaths count')
  
  plot_palette <- switch(series, 
                         cases  = 'Blues', 
                         deaths = 'Reds')
  
  labels <- tbl_dta %>% pull(brks) %>% levels()
  
  sf_dta <- tbl_dta %>% 
    full_join(
      select(sf_world, iso_a3),
      by = "iso_a3"
    ) %>% 
    st_as_sf()
  
  plot_map <- ggplot(sf_dta) + 
    geom_sf(aes(fill = brks), size = .1) + 
    scale_fill_brewer(
      name = legend_title, palette = plot_palette, 
      drop = FALSE, 
      guide = guide_legend(
        keyheight = unit(3, units = "mm"),
        keywidth = unit(70 / length(labels), units = "mm"),
        title.hjust = 0.5,
        nrow = 1,
        label.position = "bottom",
        title.position = 'top')) +
    labs(title = plot_title, 
         caption = caption_world_map) +
    theme_minimal() +
    theme(plot.title = element_text(hjust = 0.5), 
          legend.position = "bottom")
  
  return(plot_map)
  
}



# To plot both cases and deaths into a single graphic plot
country_six_plots <- function(dta, mdl1_cases, mdl2_cases, mdl1_deaths, mdl2_deaths) {
  
  # Parameters
  country_id  <- dta %>% distinct(iso_a3, country)
  main_colour <- c(cases = '#1A62A3', deaths = '#e10000')
  date_min    <- dta %>% filter(cases != 0) %>% pull(date) %>% min()
  model_name <- mdl1_cases$par$model_name
  
  # Table observations
  dta_obs <- dta %>% 
    select(date, cases, deaths) %>% 
    pivot_longer(-date, names_to = 'obs', values_to = 'count')
  
  
  # Models plot extents
  mdl1_dates_extent <- mdl1_cases$par$time_unit_sourced
  mdl2_dates_extent <- mdl2_cases$par$time_unit_sourced
  
  
  # Models tables
  mdl1_cases_dta <-  mdl1_cases$preds %>% mutate(obs = 'cases') %>% rename(n = cases)
  mdl2_cases_dta <-  mdl2_cases$preds %>% mutate(obs = 'cases') %>% rename(n = cases)
  mdl1_deaths_dta <- mdl1_deaths$preds %>% mutate(obs = 'deaths') %>% rename(n = deaths)
  mdl2_deaths_dta <- mdl2_deaths$preds %>% mutate(obs = 'deaths') %>% rename(n = deaths)
  
  mdl1_dta <- rbind(mdl1_cases_dta, mdl1_deaths_dta)
  mdl2_dta <- rbind(mdl2_cases_dta, mdl2_deaths_dta)

  
  # Plots
  plot_obs <- ggplot(dta_obs, aes(x = date, y = count)) + 
    facet_wrap(~obs, scales = "free_y", ncol = 1) + 
    geom_col(aes(colour = obs, fill = obs)) + 
    scale_colour_manual(values = main_colour) + 
    scale_fill_manual(values = main_colour) + 
    scale_x_date(limits = c(date_min, NA), breaks = '2 months', date_labels = "%b-%Y") +
    xlab('') + 
    ylab('frequency') + 
    labs(subtitle = 'Since the first cases reported') + 
    theme_light() + 
    theme(legend.position = "none", 
          strip.text = element_text(size = 11))
  
  plot_mdl1 <- ggplot(mdl1_dta, aes(x = date, y = n)) + 
    facet_wrap(~ obs, scales = "free_y", ncol = 1) + 
    geom_point(aes(colour = obs), size = 2) + 
    scale_colour_manual(values = main_colour) + 
    geom_ribbon(aes(ymin = lwr, ymax = upr, fill = obs), alpha = 0.4) + 
    geom_line(aes(y = fit, colour = obs), size = 1) + 
    scale_fill_manual(values = main_colour) + 
    scale_x_date(limits = mdl1_dates_extent, date_labels = "%d-%b") +
    xlab('') + 
    ylab(paste0('frequency and fitted values')) + 
    labs(subtitle = paste('Last', (mdl1_dates_extent[[2]] - mdl1_dates_extent[[1]] + 1), 'days')) + 
    theme_light() + 
    theme_light() + 
    theme(legend.position = "none", 
          strip.text = element_text(size = 11))
  
  plot_mdl2 <- ggplot(mdl2_dta, aes(x = date, y = n)) + 
    facet_wrap(~ obs, scales = "free_y", ncol = 1) + 
    geom_point(aes(colour = obs), size = 2) + 
    scale_colour_manual(values = main_colour) + 
    geom_ribbon(aes(ymin = lwr, ymax = upr, fill = obs), alpha = 0.4) + 
    geom_line(aes(y = fit, colour = obs), size = 1) + 
    scale_fill_manual(values = main_colour) + 
    scale_x_date(limits = mdl2_dates_extent, breaks = '4 days', date_labels = "%d-%b") +
    xlab('') + 
    ylab(paste0('frequency and fitted values')) + 
    labs(subtitle = paste('Last', (mdl2_dates_extent[[2]] - mdl2_dates_extent[[1]] + 1), 'days')) + 
    theme_light() + 
    theme_light() + 
    theme(legend.position = "none", 
          strip.text = element_text(size = 11))
  
  # Arrange plots
  plot_obs + 
    plot_mdl1 + 
    plot_mdl2 + 
    plot_layout(ncol = 3, widths = c(2, 1.4, 1.1)) +
    plot_annotation(title = paste('Covid-19 cases and deaths and trend estimations in', country_id$country),
                    caption = paste('Fitting using a', model_name))

}





country_plot_coeff <- function(series, country_iso) {
  
  name_country <- df_countries %>% filter(iso_a3 == country_iso) %>% pull(country)
  
  df_country <- switch(series, 
                       cases  = lst_coeffs_cases[[country_iso]], 
                       deaths = lst_coeffs_deaths[[country_iso]])
  
  quo_series <- sym(series)
  
  main_colour <- switch(series, 
                        cases  = '#1A62A3',
                        deaths = '#e10000')
  
  date_min <- min(df_country$date, na.rm = TRUE)
  date_max <- max(df_country$date, na.rm = TRUE)
  
  plot_crv <- ggplot(df_country, aes(x = date, y = !!quo_series)) + 
    geom_col(colour = main_colour,  fill = main_colour) + 
    xlim(c(date_min, date_max)) + 
    xlab('') + 
    ylab(series) + 
    labs(subtitle = glue('Number of {series} reported')) + 
    theme_light()
  
  plot_cff <- ggplot(df_country, aes(x = date)) +
    geom_line(aes(y = coeff), colour = '#1B9E77', size = 1) + 
    geom_ribbon(aes(ymin = lwr, ymax = upr), fill = '#1B9E77', alpha = 0.4) + 
    xlim(c(date_min, date_max)) + 
    #scale_x_date(date_breaks = "4 weeks", date_labels = "%d %b") + 
    xlab(NULL) + 
    ylab('Slope coefficient') + 
    labs(subtitle = 'Slope coefficient curve') + 
    theme_light()
  
  grid_plot <- grid.arrange(rbind(ggplotGrob(plot_crv), ggplotGrob(plot_cff)), 
                               top = textGrob(glue('Evolution of the slope cofficient in {name_country}'), 
                                              gp = gpar(fontface = 'bold')))
  
  return(grid_plot)
  
}



make_tbl_prop <- function(dta, var1, var2 = NULL) {
  
  var1 <- sym(var1)
  
  tbl1 <- dta %>%
    group_by(!!var1, .drop = FALSE) %>%
    summarise(
      n = n()) %>% 
    mutate(
      pct = n/sum(n)) %>% 
    ungroup()
  
  if (!is.null(var2)){
    
    var2 <- sym(var2)
    
    tbl2 <- dta %>%
      group_by(!!var2, !!var1, .drop = FALSE) %>%
      summarise(
        n = n()) %>% 
      mutate(
        pct = n/sum(n)) %>% 
      pivot_wider(names_from = !!var2, values_from = c('n', 'pct')) %>% 
      full_join(tbl1) %>% 
      rename(n_Total = n, pct_Total = pct) %>% 
      ungroup()
  }
  
  if (is.null(var2)) {
    return(tbl1)
  } else {
    return(tbl2)
  }
  
}



make_tbl_cfr <- function(dta, x_var, label_var){
  
  x_var <- sym(x_var)
  
  dta <- dta %>% 
    filter(covid_status == 'Confirmed', outcome_status %in% c('Cured', 'Died'), !is.na(!!x_var))
  
  tbl_cfr_total <- dta %>% 
    select(!!x_var, outcome_status) %>% 
    group_by(!!x_var) %>% 
    summarise(
      totals_Total  = paste0(sum(outcome_status == 'Died'), '/', n()), 
      died_p_Total = mean(outcome_status == 'Died')) %>% 
    mutate(
      label_var = label_var)
  
  tbl_cfr_continent <- dta %>%
    select(continent, !!x_var, outcome_status) %>%
    group_by(continent, !!x_var) %>%
    summarise(
      totals = paste0(sum(outcome_status == 'Died'), '/', n()),
      died_p = mean(outcome_status == 'Died')) %>%
    pivot_wider(names_from = continent, values_from = c(totals, died_p))
  
  tbl_cfr <- right_join(tbl_cfr_continent, tbl_cfr_total)
  
  return(tbl_cfr)
}

