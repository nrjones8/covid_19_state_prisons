

make_mirror_graph <- function(data) {

  data$total <- rowSums(data[, c("inmates_positive", "staff_positive")],
                                 na.rm = TRUE)
  data$inmates_positive <- data$inmates_positive * -1
  current_date <- unique(data$scrape_date)
  
  temp <- data[, c("state", "total")]
  
  data <-
    data %>%
    dplyr::select(state,
           inmates_positive,
           staff_positive)
  data <- tidyr::gather(data,type,  count, c("inmates_positive", "staff_positive"))
  
  

  data <-
    data %>%
    left_join(temp) %>%
    dplyr::arrange(dplyr::desc(total))
  data$state <- factor(data$state, levels = rev(unique(data$state)))
  
  

  
  data %>%
    ggplot2::ggplot(ggplot2::aes(x = state,
                                 y = count,
                                 fill = type)) +
    ggplot2::geom_bar(stat = "identity") +
    ggplot2::labs(title = "Reported Positive Covid-19 Tests",
                  subtitle = glue::glue("Among State Prison Populations: 
    Total States: {length(unique(data$state))}
    Total Positives: {  format(abs(sum(data$count[data$type == 'inmates_positive'], na.rm = TRUE)), big.mark = ",")} Incarcerated People, {  format(abs(sum(data$count[data$type == 'staff_positive'], na.rm = TRUE)), big.mark = ",")} Corrections Staff"),
                  x = "# of Reported Positive Covid-19 Tests",
                  y = "" )+
    ggplot2::coord_flip() +
    ggplot2::scale_y_continuous(breaks = pretty(data$count),
                                labels = abs(pretty(data$count))) +
    patchwork::plot_annotation(caption = glue::glue("State Departments of Corrections differ in their testing and reporting practices. 
                  Data were collected from various state DOCs on {make_pretty_date(current_date)}.
                  See covidprisondata.com for more")) +
    ggplot2::scale_fill_manual(values = c("#d95f02", "#1b9e77")) + 
    ggplot2::guides(fill = FALSE) +
    ggplot2::geom_text(x     = 8, 
                       y     = min(data$count, na.rm = TRUE) * 0.60,
                       label = "Incarcerated People", 
                       color = "#d95f02") +
    ggplot2::annotate("segment",
                      x      = 7, 
                      xend   = 7, 
                      y      = min(data$count, na.rm = TRUE) * .3,
                      yend   = min(data$count, na.rm = TRUE), 
                      colour = "#d95f02",
                      arrow  = ggplot2::arrow()) +
    ggplot2::geom_text(x     = 8,
                       y     = max(data$count, na.rm = TRUE) * .60, 
                       label = "Corrections Staff", 
                       color = "#1b9e77") +
    ggplot2::annotate("segment",
                      x      = 7,
                      xend   = 7,
                      y      = max(data$count, na.rm = TRUE) * .3,
                      yend   = max(data$count, na.rm = TRUE), 
                      colour = "#1b9e77",
                      arrow  = ggplot2::arrow()) +
    ggplot2::theme_minimal() 
 # plotly::ggplotly()
}
