library(gridExtra)

# Select year
data <- reise_20
# data <- reise_21

## Plots
data %>% str()

variablen <- c(
  "gereist", 
  "alter", "haushaltseinkommen", "ortsgroesse", "haushaltsgroesse",
  "geschlecht", "herkunft", "kind_alter_0_5", "kind_alter_6_13",
  "kind_alter_14_17", "vollberufstaetig", "bildung", "migration"
)

variablen_verteilung <- variablen %>% lapply(function(x) {
  if (x %in% c("alter", "haushaltseinkommen", "ortsgroesse")) {
    data %>% ggplot() +
      geom_boxplot(aes_string(x)) +
      xlab("") +
      ggtitle(x) 
  } else {
    data %>% ggplot(aes_string(x)) +
      geom_bar() +
      stat_count(geom = "text", colour = "white", size = 4,
                 mapping = aes(label = ..count..), position = position_stack(vjust = 0.5)) +
      xlab("") +
      ggtitle(x) 
  }
}) 
lay_mat <- matrix(1:15, 3, 5, TRUE)
variablen_verteilung_grid <- marrangeGrob(
  variable_distributions,
  layout_matrix = lay_mat
)
variablen_verteilung_grid
ggsave("central_tendency_and_measure_of_dispersion.pdf", variablen_verteilung_grid,
     width = 60, height = 25, units = "cm"
)

## Summaries
get_summaries <- function(x) {
  
  mode <- function(x) {                     
    unique_x <- unique(x)
    tabulate_x <- tabulate(match(x, unique_x))
    unique_x[tabulate_x == max(tabulate_x)]
  }
  as.table(c(min = min(x), max = max(x), mean = mean(x),
                   median = median(x), sd = sd(x), mode = mode(x)))
}
# Without variable city size
variables <- c(
  "alter", "haushaltseinkommen", "ortsgroesse"
)
sapply(variables, function(x) {
  get_summaries(data[, x])
})

