##### custom functions
summary_all_func <- function(data = df) {
  summary_attacks <- data %>%
    group_by(
      year,
      month,
      country = `Country / Territory`
    ) %>%
    tally(name = "total_attacks")
  
  summary_all <- data %>%
    group_by(
      year,
      month,
      country = `Country / Territory`
    ) %>%
    summarise(
      total_injured = sum(`Total Injured`),
      total_death = sum(`Total Death`),
      'total_casualties' = (total_injured + total_death),
      hw_abduction = sum(`HW Abduction`),
      hw_arrest = sum(`HW Arrest`),
      hw_detention = sum(`HW Detention`),
      'hw_abduction/arrest/detention' = (hw_abduction + hw_arrest + hw_detention),
      patient_abduction = sum(`Patient Abduction`),
      patient_arrest = sum(`Patient Arrest`),
      patient_detention = sum(`Patient Detention`),
      'patient_abduction/arrest/detention' = (patient_abduction + patient_arrest + patient_detention),
    )
  
  summary_all <- left_join(
    summary_attacks,
    summary_all,
    by = c("year", "month", "country")
  )
  
  return(summary_all)
}

# summary_all_func(data = df)

common_attack_types_func <- function(data = df, selected_country = NULL) {
  
  if (is.null(selected_country)) {
    table <- data
  } else {
    table <- data %>% 
      filter(`Country / Territory` == selected_country)
  }
  
  table %>%
    select(all_of(attack_types)) %>% 
    summarise(across(everything(), ~ sum(.x))) %>%
    pivot_longer(cols = everything(), names_to = "attack_type", values_to = "Freq") %>% 
    filter(Freq >0) %>% 
    arrange(-Freq)
}

# common_attack_types_func(data = df, selected_country = "Afghanistan")
# common_attack_types_func(data = df, selected_country = NULL)

main_report_func <- function(data = df) {
  
  tbl <- summary_all_func(data)
  common_attacks <- common_attack_types_func(data = data, selected_country = NULL)
  
  n_attacks <- sum(tbl$total_attacks)
  n_casualties <- sum(tbl$total_casualties)
  n_injured <- sum(tbl$total_injured)
  n_death <- sum(tbl$total_death)
  n_hw <- sum(tbl$`hw_abduction/arrest/detention`)
  n_patient <- sum(tbl$`patient_abduction/arrest/detention`)
  
  year1 = min(data$year)
  year2 = max(data$year)
  
  if (year1 != year2) {
    year_report <- glue::glue("between {year1} and {year2}")
  } else {
    year_report <- glue::glue("in {year1}")
  }
  
  result <- HTML(glue::glue(
                            "<br>
                            <p><span style='font-size:22px; color:orange'>{n_attacks}</span> attacks have been reported on healthcare facilities {year_report}.</p>
                            <p>The number of reported casualties is <span style='font-size:22px; color:orange'>{n_casualties}</span> (injuries = {n_injured}, deaths = {n_death}).</p>
                            <p>Besides, <span style='font-size:22px; color:orange'>{n_hw}</span> health workers and <span style='font-size:22px; color:orange'>{n_patient}</span> patients have been abducted, arrested, or detained.</p>
                            <br>
                            <p>The most common reported attack types are:</p>
                            <ul>
                            "
                            ))
  
  for (i in 1:nrow(common_attacks)) {
    result <- HTML(glue::glue("{result}
                         <li>{common_attacks[i,]$attack_type}: {common_attacks[i,]$Freq}</li>
                         "))
  }
  
  result <- HTML(glue::glue("{result}</ul>"))
  
  return(result)
  
}

# main_report_func(data = df)

country_report_func <- function(data = df, selected_country) {
  
  tbl <- summary_all_func(data %>% filter(`Country / Territory` == selected_country))
  common_attacks <- common_attack_types_func(data = data, selected_country = selected_country) %>% head(5)
  
  n_attacks <- sum(tbl$total_attacks)
  n_casualties <- sum(tbl$total_casualties)
  n_injured <- sum(tbl$total_injured)
  n_death <- sum(tbl$total_death)
  n_hw <- sum(tbl$`hw_abduction/arrest/detention`)
  n_patient <- sum(tbl$`patient_abduction/arrest/detention`)
  n_casualty_death_ratio <- n_casualties / n_attacks
  
  result <- HTML(
    glue::glue("
        <h3>Casualties to attacks ratio: {round(n_casualty_death_ratio, 2)}:1</h3>
        <br>
        <p>Overall, <b>{n_attacks}</b> attacks have been reported on healthcare facilities in {selected_country}.
        The number of reported casualties is <b>{n_casualties}</b> (injuries = {n_injured}, death = {n_death}).</p>
        <br>
        <p>Moreover, <b>{n_hw}</b> health workers and <b>{n_patient}</b> patients have been abducted, arrested, or detained.</p>
        <br>
        <p>Common attack types that occurred in {selected_country} are:</p>
        <ul>
        "
    )
  )
  
  for (i in 1:nrow(common_attacks)) {
    result <- HTML(glue::glue("{result}
                         <li>{common_attacks[i,]$attack_type}: {common_attacks[i,]$Freq}</li>
                         "))
  }
  
  result <- HTML(glue::glue("{result}</ul>"))
  
  return(result)
}

# country_report_func(data = df, selected_country = "Afghanistan")

summary_indicator_func <- function(data = df, indicator = "total_injured", wider = TRUE) {
  table <-  summary_all_func(data) %>%
    group_by(
      year,
      month,
    ) %>%
    summarise(
      n = sum(!!sym(indicator))
    )
  
  if(wider) {
    table <- table %>%
      pivot_wider(names_from = year, values_from = n) %>%
      ungroup() %>%
      arrange(month) %>%
      mutate(across(-c(month), function(x) ifelse(is.na(x), 0, x)))
    
  }
  
  return(table)
}

# summary_indicator_func(data = df, indicator = "total_injured")
# summary_indicator_func(data = df, indicator = "total_injured", wider = FALSE)

plot_line_func <- function(data = df, indicator = "total_injured") {
  
  table <- summary_indicator_func(data, indicator, wider = FALSE) %>% 
    mutate(year_month = paste0(year, "-", month))
  
  type <- ifelse(length(unique(table$year)) > 1, "line", "column")
  table %>% 
    hchart(type, hcaes(x = year_month, y = n)) %>%
    hc_exporting(enabled = TRUE, filename = "linechart") %>% 
    hc_xAxis(title = list(text = '')) %>% 
    hc_yAxis(title = list(text = '')) %>% 
    hc_title(
      text = glue::glue("<p>{toupper(str_replace(indicator, '_', ' '))}</p>"),
      margin = 20,
      align = "center",
      style = list(color = "gray", useHTML = TRUE)
    )
  
}

# plot_line_func(data = df, indicator = "total_injured")
# plot_line_func(data = df %>% filter(year == 2017), indicator = "total_injured")

plot_countries_bar_func <- function(data = df, indicator = "total_injured") {
  
  table <- summary_all_func(data) %>%
    group_by(country) %>%
    summarise(
      n = sum(!!sym(indicator))
    ) %>% 
    ungroup() %>% 
    arrange(-n) %>% 
    filter(n > 0)
  
  red_to_yellow_palette <- colorRampPalette(c("red", "yellow"))
  
  table %>%
    mutate(color = red_to_yellow_palette(nrow(.))) %>%
    hchart('column', hcaes(x = country, y = n, color = color)) %>%
    hc_exporting(enabled = TRUE, filename = "barchart") %>% 
    hc_xAxis(title = list(text = '')) %>% 
    hc_yAxis(title = list(text = '')) %>% 
    hc_title(
      text = glue::glue("<p>{toupper(str_replace(indicator, '_', ' '))}</p>"),
      margin = 20,
      align = "center",
      style = list(color = "gray", useHTML = TRUE)
    )
  
}

# plot_countries_bar_func(data = df, indicator = "total_attacks")
# plot_countries_bar_func(data = df, indicator = "total_injured")
# plot_countries_bar_func(data = df, indicator = "patient_arrest")

# library(ggplot2)
# library(plotly)
# plot_map_func <- function(data = df, indicator = "total_injured") {
# 
#   table <- summary_all_func(data) %>%
#     group_by(country) %>%
#     summarise(
#       n = sum(!!sym(indicator))
#     )
# 
#   map_table <- map_data("world") %>%
#     filter(region != "Antarctica") %>%
#     mutate(region = case_when(
#       region == "Syria" ~ "Syrian Arab Republic",
#       region == "Palestine" ~ "occupied Palestinian territory",
#       TRUE ~ region
#     )) %>%
#     left_join(
#       table,
#       by = c("region" = "country")
#     )
# 
#   ggplotly(
#     ggplot(map_table, aes(long, lat, group = group,
#                          text = paste("Country: ", region, "<br>N: ", n)
#     )) +
#       geom_polygon(aes(fill = n), color = "white", linewidth = 0.2, show.legend = T) +
#       scale_fill_gradient2(low = "yellow", mid = "orange", high = "red", na.value="lightgray") +
#       theme_void() +
#       theme(
#         panel.border = element_blank(),
#         axis.line = element_blank(),
#         legend.position = "bottom"
#       ) +
#       labs(fill = NULL),
#     tooltip = "text"
#   ) %>%
#     layout(legend = list(orientation = "h", x = 0.5, y = -0.1))
# 
# }
# 
# plot_map_func(data = df, indicator = "total_attacks")

plot_wrapper_func <- function(data, indicator, type) {
  if(type == "map") {
    plot_map_func(data, indicator)
  } else if (type == "by_year") {
    plot_line_func(data, indicator)
  } else if (type == "by_country") {
    plot_countries_bar_func(data, indicator)
  }
}

# plot_wrapper_func(data = df, indicator = "total_attacks", type = "by_year")
# plot_wrapper_func(data = df %>% filter(year == 2018), indicator = "total_attacks", type = "by_year")
# plot_wrapper_func(data = df, indicator = "total_attacks", type = "by_country")
# # plot_wrapper_func(data = df, indicator = "total_attacks", type = "map")

##### extra ---------------------------------------------------------
# Nagorno-Karabakh


