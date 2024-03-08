##### read data and data manipulation ---------------------------------------------------------
# df <- read.csv("input/data.csv", check.names = FALSE)
df <- readxl::read_excel("input/data_20240308.xlsx")

df <- df %>%
  mutate(
  year_month_day = as.Date(`Attack Date`, format = "%m/%d/%y %I:%M %p"),
  year = format(year_month_day, "%Y"),
  month = format(year_month_day, "%m"),
  `Certainty Level` = ifelse(`Certainty Level` == "Confirmed", "Confirmed", "Not Confirmed")
)

split_attack_types_func <- function(attack_type) {
  unlist(strsplit(attack_type, ",(?![^()]*\\))(?=\\s*[A-Z])", perl=TRUE))
}

attack_types <- lapply(df$`Attack Type`, split_attack_types_func)
attack_types <- unique(trimws(unlist(attack_types)))

for (i in attack_types) {
  attack <- str_trim(gsub("\\(.*?\\)", "", i))
  result <- str_detect(df$`Attack Type`, attack)
  df[[i]] <- result
}

certainty_level <- unique(df$`Certainty Level`)

rm(attack, i, result, split_attack_types_func)
##### custom functions ---------------------------------------------------------

summary_all_func <- function(tbl) {
  summary_attacks <- tbl %>%
    group_by(
      year,
      month,
      country = `Country / Territory`
    ) %>%
    tally(name = "total_attacks")
  
  summary_all <- tbl %>%
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

# summary_all <- summary_all_func(df)

common_attack_types_func <- function(tbl, selected_country = NULL) {
  
  if (is.null(selected_country)) {
    table <- tbl
  } else {
    table <- tbl %>% 
      filter(`Country / Territory` == selected_country)
  }
  
  table %>%
    select(attack_types) %>% 
    # mutate(across(everything(), function(x) ifelse(x == "True", TRUE, FALSE))) %>%
    summarise(across(everything(), ~ sum(.x))) %>%
    pivot_longer(cols = everything(), names_to = "attack_type", values_to = "Freq") %>% 
    filter(Freq >0) %>% 
    arrange(-Freq)
}

# common_attack_types_func(tbl = df, selected_country = "Afghanistan")
# common_attack_types_func(tbl = df, selected_country = NULL)

main_report_func <- function(tbl = summary_all, data = df, years = c(year1, year2)) {
  n_attacks <- sum(tbl$total_attacks)
  n_casualties <- sum(tbl$total_casualties)
  n_injured <- sum(tbl$total_injured)
  n_death <- sum(tbl$total_death)
  n_hw <- sum(tbl$`hw_abduction/arrest/detention`)
  n_patient <- sum(tbl$`patient_abduction/arrest/detention`)
  
  year1 = years[1]
  year2 = years[2]
  
  if (year1 != year2) {
    year_report <- glue::glue("between {year1} and {year2}")
  } else {
    year_report <- glue::glue("in {year1}")
  }
  
  common_attacks <- common_attack_types_func(tbl = data, selected_country = NULL)
  
  result <- HTML(
    glue::glue("
                              <br>
                              <p><span style='font-size:22px; color:orange'>{n_attacks}</span>attacks been reported on healthcare facilities {year_report}.</p>
                              <p>The number of reported casualties is <span style='font-size:22px; color:orange'>{n_casualties}</span> (injured = {n_injured}, death = {n_death}).</p>
                              <p>Besides, <span style='font-size:22px; color:orange'>{n_hw}</span> health workers and <span style='font-size:22px; color:orange'>{n_patient}</span> patients have been abducted, arrested, or detained.</p>
                              <br>
                              <p>The most common reported attack types are:</p>
                              <ul>
                              ")
  )
  
  for (i in 1:nrow(common_attacks)) {
    result <- HTML(glue::glue("{result}
                         <li>{common_attacks[i,]$attack_type}: {common_attacks[i,]$Freq}</li>
                         "))
  }
  
  result <- HTML(glue::glue("{result}</ul>"))
  
  return(result)
  
}

# main_report_func(tbl = summary_all, data = df, years = c(2017, 2024))
# main_report_func(tbl = summary_all, data = df, years = c(2020, 2020))

summary_country_func <- function(tbl = summary_all, selected_country = NULL) {
  
  if (is.null(selected_country)) {
    table <- tbl
  } else {
    table <- tbl %>% 
      filter(country == selected_country)
  }
  
  table %>% 
    ungroup() %>% 
    select(-c(year, month, country)) %>% 
    summarise_all(~sum(.)) %>% 
    mutate(casualty_death_ratio = (total_injured + total_death) / total_attacks)
  
}

# summary_country_func(tbl = summary_all, selected_country = "Afghanistan")
# summary_country_func(tbl = summary_all, selected_country = NULL)

country_report_func <- function(tbl = summary_all, data = df, selected_country) {
  
  selected_country <- selected_country
  table <- summary_country_func(tbl = tbl, selected_country = selected_country)
  common_attacks <- common_attack_types_func(tbl = data, selected_country = selected_country) %>% 
    head(5)
  
  result <- HTML(
    glue::glue("
        <h3>Casualties to attacks ratio: 1:{round(table$casualty_death_ratio, 2)}</h3>
        <br>
        <p>Overall, <b>{table$total_attacks}</b> attacks have happened on healthcare facilities in {selected_country}.
        The number of reported casualties is <b>{table$total_injured+table$total_death}</b> (injured = {table$total_injured}, death = {table$total_death}).</p>
        <br>
        <p>Moreover, <b>{table$hw_abduction+table$hw_arrest+table$hw_detention}</b> health workers and <b>{table$patient_abduction+table$patient_arrest+table$patient_detention}</b> patients have been abducted, arrested, or detained.</p>
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

# country_report_func(tbl = summary_all, data = df, selected_country = "Afghanistan")

summary_table_func <- function(tbl = summary_all, indicator = "total_injured", wider = TRUE) {
  table <- tbl %>%
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

# summary_table_func(tbl = summary_all, indicator = "total_injured")
# summary_table_func(tbl = summary_all, indicator = "total_injured", wider = FALSE)

# custom_table <- function(table, show_btns = FALSE) {
#   options <- if (show_btns) {
#     list(
#       dom = 'tB',
#       buttons = c('copy', 'csv', 'excel'),
#       searching = FALSE,
#       lengthChange = FALSE,
#       paging = FALSE,
#       columnDefs = list(
#         list(targets = "_all", className = "dt-left")
#       ),
#       initComplete = JS('function(settings, json) { $(this.api().table().node()).css("width", "100%"); }')
#     )
#   } else {
#     list(
#       dom = 't',
#       buttons = c('copy', 'csv', 'excel'),
#       searching = FALSE,
#       lengthChange = FALSE,
#       paging = FALSE,
#       columnDefs = list(
#         list(targets = "_all", className = "dt-left")
#       ),
#       initComplete = JS('function(settings, json) { $(this.api().table().node()).css("width", "100%"); }')
#     )
#   }
#   
#   datatable(table, rownames = FALSE, extensions = c('Buttons'), options = options)
# }
# summary_table_func(tbl = summary_all, indicator = "total_injured") %>%
#   custom_table(show_btns = TRUE)

plot_line_func <- function(tbl = summary_all, indicator = "total_injured") {
  
  table <- summary_table_func(tbl, indicator, wider = FALSE) %>% 
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

# plot_line_func(tbl = summary_all, indicator = "total_injured")
# plot_line_func(tbl = summary_all%>% filter(year == 2017), indicator = "total_injured")

plot_countries_bar_func <- function(tbl = summary_all, indicator = "total_injured") {
  
  table <- tbl %>%
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

# plot_countries_bar_func(tbl = summary_all, indicator = "total_attacks")
# plot_countries_bar_func(tbl = summary_all, indicator = "total_injured")
# plot_countries_bar_func(tbl = summary_all, indicator = "patient_arrest")

plot_map_func <- function(tbl = summary_all, indicator = "total_injured") {
  
  table <- tbl %>%
    group_by(country) %>%
    summarise(
      n = sum(!!sym(indicator))
    )
  
  map_table <- map_data("world") %>% 
    filter(region != "Antarctica") %>% 
    mutate(region = case_when(
      region == "Syria" ~ "Syrian Arab Republic",
      region == "Palestine" ~ "occupied Palestinian territory",
      TRUE ~ region
    )) %>% 
    left_join(
      table,
      by = c("region" = "country")
    )
  
  ggplotly(
    ggplot(map_table, aes(long, lat, group = group,
                         text = paste("Country: ", region, "<br>N: ", n)
    )) +
      geom_polygon(aes(fill = n), color = "white", linewidth = 0.2, show.legend = T) +
      scale_fill_gradient2(low = "yellow", mid = "orange", high = "red", na.value="lightgray") +
      theme_void() +
      theme(
        panel.border = element_blank(),
        axis.line = element_blank(),
        legend.position = "bottom"
      ) +
      labs(fill = NULL),
    tooltip = "text"
  ) %>%
    layout(legend = list(orientation = "h", x = 0.5, y = -0.1))
  
}

# plot_map_func(tbl = summary_all, indicator = "total_attacks")

plot_wrapper_func <- function(tbl, indicator, type) {
  if(type == "map") {
    plot_map_func(tbl, indicator)
  } else if (type == "by_year") {
    plot_line_func(tbl, indicator)
  } else if (type == "by_country") {
    plot_countries_bar_func(tbl, indicator)
  }
}

# plot_wrapper_func(tbl = summary_all, indicator = "total_attacks", type = "line")
# plot_wrapper_func(tbl = summary_all %>% filter(year == 2018), indicator = "total_attacks", type = "line")
# plot_wrapper_func(tbl = summary_all, indicator = "total_attacks", type = "top10")
# plot_wrapper_func(tbl = summary_all, indicator = "total_attacks", type = "map")

# summary_country_plot_func <- function(tbl, selected_country, title = "Casualties to Attacks ratio") {
#   table <- summary_country_func(tbl = tbl, selected_country = selected_country)
#   red_to_yellow_palette <- colorRampPalette(c("red", "yellow"))
#   
#   table %>% 
#     select(-casualty_death_ratio) %>% 
#     mutate(grp = "grp") %>% 
#     pivot_longer(-grp, names_to = "ind", values_to = "n") %>% 
#     mutate(
#       ind = case_match(ind,
#                        c("hw_abduction", "hw_arrest", "hw_detention") ~ "4) HW Abduction / Arrest / Detention",
#                        c("patient_abduction", "patient_arrest", "patient_detention") ~ "5) Patient Abduction / Arrest / Detention",
#                        "total_attacks" ~ "1) Total Attacks",
#                        "total_injured" ~ "2) Total Injured",
#                        "total_death" ~ "3) Total Death",
#                        .default = ind
#       )
#     ) %>% 
#     group_by(ind) %>% 
#     summarise(n = sum(n)) %>% 
#     mutate(color = red_to_yellow_palette(nrow(.))) %>% 
#     hchart('column', hcaes(x = ind, y = n, color = color)) %>%
#     hc_exporting(enabled = TRUE, filename = "barchart") %>% 
#     hc_xAxis(title = NULL) %>% 
#     hc_yAxis(title = list(text = "Count")) %>% 
#     hc_title(
#       text = glue::glue("<p>{title} 1:{round(table$casualty_death_ratio, 2)}</p>"),
#       margin = 20,
#       align = "left",
#       style = list(color = "gray", useHTML = TRUE)
#     )
# }

# summary_country_plot_func(tbl = summary_all, selected_country = "Afghanistan")

##### extra ---------------------------------------------------------
# Nagorno-Karabakh
# attack_types <- c('Violence with heavy weapons',
#                   'Violence with individual weapons',
#                   'Criminalization of health care',
#                   'Armed or violent search of health care personnel', # 'facility or transport',
#                   'Militarization of a health care asset',
#                   'Abduction/Arrest/Detention of health personnel or patients',
#                   'Psychological violence/threat of violence/intimidation',
#                   'Chemical agent',
#                   'Obstruction to health care delivery',
#                   'Removal of health care assets',
#                   'Assault',
#                   'Setting fire',
#                   'Sexual assault',
#                   'Unknown',
#                   'Other')
# certainty_level <- c("Confirmed", "Not Confirmed")

# ggplot() +
#   geom_polygon(data = world_map, aes(x = long, y = lat, group = group, fill = n), color = "gray", show.legend = F) +
#   scale_fill_gradient(low = "lightblue", high = "darkblue", name = "Count") +
#   coord_equal() +
#   theme_void()

