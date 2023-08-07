library(tidyverse)

theme_JR1 <- function(axis_lines = TRUE, 
                      grid_lines = FALSE,     
                      text_size = 12,       
                      line_size = 0.2,
                      base_family= 'sans'){ 
  
  th <- ggplot2::theme_minimal(base_family = base_family, 
                               base_size = text_size)
  th <- th + theme(panel.grid=element_blank())
  if (axis_lines) {
    th <- th + 
      theme(axis.line = element_line(linewidth = line_size, color = "black"),
            axis.ticks = element_line(linewidth = line_size, color = "black"),
            axis.text=element_text(color = "black"),
            axis.title = element_text(size=13, colour="black", face="bold"))
  } 
  if (grid_lines) {
    th <- th + 
      theme(panel.grid.major = element_line(linewidth = line_size))
  }
  th <- th + theme(
    axis.text.x=element_text(margin=margin(t=2)),
    axis.text.y=element_text(margin=margin(r=2)),
    axis.title.x=element_text(margin=margin(t=5)),
    axis.title.y=element_text(margin=margin(r=5)),
    plot.title=element_text(margin=margin(b=5)))
  
  return (th)
}

ggplot2::theme_set(theme_JR1()) 

month_order <- c("January", "February", "March", "April", "May", "June",
                 "July", "August", "September", "October", "November", "December")


current_month <- month.abb[month(now())]

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
# Expand the data
expanded_data <- fruit_data %>%
  pmap_dfr(expand_months) %>% 
  mutate(color = fruit_colors[fruit])


# Reorder the factor levels in expanded_data
expanded_data$fruit <- factor(expanded_data$fruit, #levels = rev(fruit_order))
                              levels = c("Strawberry", "Raspberry", "Cherry", "Persimmon",
                                         "Grapefruit", "Peach", "Lemon", "Feijoa", "Blueberry",
                                         "Blackberry", "Plum"))


expanded_data %>%
  ggplot(aes(x = month, y = fct_rev(fruit), fill = color)) +
  geom_tile(color = "white") +
  geom_vline(xintercept = current_month) +
  # scale_fill_brewer(palette = "Set3") +
  scale_fill_identity() +
  labs(title = "Fruits in Season - New Zealand", x = NULL,y = NULL, 
       caption = "Source: https://www.produce.co.nz/seasonality-chart/") +
  # theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "none")




#
# extras to order by brightness and family --------------------------------
# 
# 
# # Function to calculate intensity
# calculate_intensity <- function(color) {
#   channels <- col2rgb(color)
#   R <- channels[1]
#   G <- channels[2]
#   B <- channels[3]
#   0.299 * R + 0.587 * G + 0.114 * B
# }
# 
# # Separate the colors into families
# reds <- c("Raspberry", "Strawberry", "Cherry", "Persimmon")
# oranges <- c("Peach", "Grapefruit")
# yellows <- c("Lemon")
# blues <- c("Blueberry", "Blackberry", "Plum")
# greens <- c("Feijoa")
# 
# # Order each family by intensity
# order_family <- function(family) family[rev(order(sapply(fruit_colors[family], calculate_intensity)))]
# 
# # Combine the families in the desired order
# fruit_order <- c(order_family(reds), order_family(oranges), order_family(yellows), order_family(greens), order_family(blues))
# 
