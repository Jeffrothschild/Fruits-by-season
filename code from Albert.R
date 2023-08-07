library(tidyverse)
#   https://www.produce.co.nz/seasonality-chart/
fruits <- tibble::tribble(
  ~ fruit,       ~start,      ~ end,
  "Blueberry",   "October",    "April",
  "Blackberry",   "October",   "March",
  "Raspberry",    "November",  "April",
  "Strawberry",  "September",  "April",
  "Feijoa",       "March",     "May",
  "Grapefruit",  "June",       "December",
  "Lemon",       "May",       "October", 
  "Persimmon",    "April",    "August",
  "Cherry",      "December",  "January",
  "Peach",        "January",    "March",
  "Plum",       "December",     "April"
) 

fruit_colors <- c(
  "Blueberry" = "#4F86F7",  # Blue
  "Blackberry" = "#0B3D91", # Dark Blue
  "Raspberry" = "#E30B5D",  # Raspberry Red
  "Strawberry" = "#FC5A8D", # Strawberry Pink
  "Feijoa" = "#98FB98",     # Pale Green
  "Grapefruit" = "#FD7C6E", # Grapefruit Color
  "Lemon" = "#e4d600",      # Bright Yellow
  "Persimmon" = "#EC5800",  # Persimmon Orange
  "Cherry" = "#B80058",     # Cherry Red
  "Peach" = "#ffca68",      # Peach Color
  "Plum" = "#8E4585"        # Plum Purple
)
month_order <- c("January", "February", "March", "April", "May", "June",
                 "July", "August", "September", "October", "November", "December")
current_month <- month.abb[month(now())]
# Convert month names to numbers
month_to_num <- function(month) {
  match(month, month.name)
}
# Convert start and end months to numeric
fruit_data <- fruits %>%
  mutate(start_month = month_to_num(start),
         end_month = month_to_num(end))
# Function to handle start and end months
expand_months <- function(fruit, start, end, start_month, end_month) {
  if (end_month < start_month) end_month <- end_month + 12
  months <- (start_month:end_month) %% 12 + 1
  tibble(fruit = fruit, month = factor(months, levels = 1:12, labels = month.abb))
}
# Expand the data
expanded_data <- fruit_data %>%
  pmap_dfr(expand_months) %>% 
  mutate(color = fruit_colors[fruit])
# Reorder the factor levels in expanded_data
expanded_data$fruit <- factor(expanded_data$fruit, #levels = rev(fruit_order))
                              levels = c("Strawberry", "Raspberry", "Cherry", "Persimmon",
                                         "Grapefruit", "Peach", "Lemon", "Feijoa", "Plum", "Blueberry",
                                         "Blackberry"))


# camcorder::gg_record(
#   dir = 'img',
#   dpi = 300,
#   width = 16,
#   height = 9,
#   unit = 'cm',
#   bg = 'white'
# )

min_months_from_aug <- expanded_data %>%
  mutate(
    month_number = match(month, month.abb),
    months_from_aug = pmin(month_number - 8) %% 12
  ) |> 
  slice_min(months_from_aug, by = fruit) |> 
  mutate(
    text_color = if_else(
      fruit %in% c('Blackberry', 'Cherry', 'Plum', 'Raspberry', 'Persimmon'),
      'white',
      'grey10'
    )
  )
joined_data <- expanded_data %>%
  left_join(min_months_from_aug) |> 
  mutate(
    month_number =  match(month, month.abb),
    # month = factor(
    #   month,
    #   levels = month.abb[c(8:12, 1:7)]
    # ),
    fruit = fct_reorder(fruit, months_from_aug, .fun = min, .na_rm = TRUE)
  ) 

joined_data |>
  ggplot(aes(x = month, y = fct_rev(fruit), fill = color)) +
  geom_tile(aes(color = color)) +
  geom_text(
    data = min_months_from_aug,
    aes(label = fruit),
    color = min_months_from_aug$text_color,
    hjust = 0,
    nudge_x = -0.45,
    size = 3.5,
    # family = 'Source Sans Pro',
    fontface = 'bold'
  ) +
  annotate(
    'curve',
    x = 1.5,
    xend = 1,
    yend = 8.25,
    y = 6,
    curvature = -0.3,
    arrow = arrow(length = unit(0.1, "cm"))
  ) +
  annotate(
    'curve',
    x = 4.5,
    xend = 5.45,
    yend = 3.25,
    y = 3,
    curvature = -0.3,
    arrow = arrow(length = unit(0.1, "cm"))
  ) +
  annotate(
    'text',
    x = 1.5,
    y = 5.2,
    label = 'What\'s\navailable today.',
    hjust = 0,
    size = 3,
    lineheight = 1,
    # family = 'Source Sans Pro',
    color = 'grey10'
  ) +
  annotate(
    'text',
    x = 4.5,
    y = 2.25,
    label = '4.5 months till long\nawaited plum time.',
    hjust = 1,
    size = 3,
    lineheight = 1,
    # family = 'Source Sans Pro',
    color = 'grey10'
  ) +
  scale_color_identity() +
  scale_fill_identity() +
  scale_y_discrete(breaks = NULL) +
  labs(
    title = "When are the plums ready?",
    subtitle = 'Seasonality of fruits in New Zealand',
    x = element_blank(),
    y = element_blank(), 
    caption = "Source: https://www.produce.co.nz/seasonality-chart/"
  ) +
  coord_cartesian(expand = FALSE) +
  scale_x_discrete(position = 'top') +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "none",
    text = element_text(color = 'grey20'),
    axis.text = element_text(color = 'grey20', size = 8),
    plot.caption = element_text(size = 6, color = 'grey40'),
    panel.grid = element_blank(),
    plot.title.position = 'plot',
    plot.title = element_text(
      # family = 'Pacifico',
      face = 'bold',
      size = 22,
      margin = margin(b = 0.3, unit = 'cm')
    ),
    plot.subtitle = element_text(size = 10)
  )
