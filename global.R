##### read data and data manipulation ---------------------------------------------------------
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
start_date <- glue::glue("{min(df$month)}.{min(df$year)}")
end_date <- glue::glue("{max(df$month)}.{max(df$year)}")

rm(attack, i, result, split_attack_types_func)

##### custom functions ---------------------------------------------------------
source("functions.R")

##### markdown files ---------------------------------------------------------

# rmarkdown::render(
#   input = "markdown/data_source.Rmd",
#   output_format = "html_document",
#   params = list(start = start_date,
#                 end = end_date),
#   output_file = "data_source"
# )

# rmarkdown::render(
#   input = "markdown/main_report.Rmd",
#   output_format = "html_document",
#   params = list(df = df),
#   output_file = "main_report"
# )
 
# rmarkdown::render(
#   input = "markdown/country_report.Rmd",
#   output_format = "html_document",
#   params = list(df = df, selected_country = "Afghanistan"),
#   output_file = "country_report"
# )

