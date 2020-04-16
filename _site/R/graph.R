make_mirror_graph <- function(data) {
  
  data$scrape_date <- NULL
  library(tidyr)
  data$total_staff_positive <- data$total_staff_positive * -1
  data <- tidyr::gather(data,type,  count, c("total_inm_positive", "total_staff_positive"))
  
  data <-
    data %>%
    dplyr::arrange(dplyr::desc(count))
  data$state <- factor(data$state, levels = rev(unique(data$state)))
  
  
  data %>%
    ggplot2::ggplot(ggplot2::aes(x = state,
                                 y = count, fill = type)) +
    ggplot2::geom_bar(stat="identity",position="identity") +
    ggplot2::labs(title = "Reported Positive COVID-19 Tests",
                  subtitle = glue::glue("Among State Prison Populations: 
    Total States: {length(unique(data$state))}
    Total Positives: {sum(data$count[data$type == 'total_inm_positive'])} Incarcerated People, {abs(sum(data$count[data$type == 'total_staff_positive']))} Corrections Staff"),
                  x = "",
                  y = "")+
    ggplot2::coord_flip()+
    ggplot2::scale_y_continuous(breaks = data$count,
                                labels = abs(data$count)) +
    patchwork::plot_annotation(caption = "State Departments of Corrections differ in their testing and reporting practices. 
                  Data were collected from various State DOCs on 4/13.
                  See covidprisonsdata.com for more") +
    ggplot2::scale_fill_manual(values = c("#d95f02", "#1b9e77")) + 
    ggplot2::guides(fill = FALSE) +
    ggplot2::geom_text(x     = 8, 
                       y     = -200, 
                       label = "Incarcerated People", 
                       color = "#1b9e77") +
    ggplot2::annotate("segment",
                      x      = 7, 
                      xend   = 7, 
                      y      = -130,
                      yend   = -270, 
                      colour = "#1b9e77",
                      arrow  = ggplot2::arrow()) +
    ggplot2::geom_text(x     = 8,
                       y     = 200, 
                       label = "Corrections Staff", 
                       color = "#d95f02") +
    ggplot2::annotate("segment",
                      x      = 7,
                      xend   = 7,
                      y      = 130,
                      yend   = 270, 
                      colour = "#d95f02",
                      arrow  = ggplot2::arrow()) +
    ggplot2::theme_minimal()
  
}
