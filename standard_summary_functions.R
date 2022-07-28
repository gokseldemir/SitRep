########################################
###Summary functions###########
###Created by: Goksel#########
###Created on: June 1, 2021
###Modified by: 
###Modified reasons:
#####################################


####Create an object with formatting standards for charts
standards <- theme_classic() + 
  theme(axis.text.x = element_text(size = 12), 
        axis.text.y = element_text(size = 12), 
        legend.text = element_text(size = 15), 
        panel.grid.major = element_blank(), 
        panel.grid.minor=element_blank(),
        axis.title = element_text(size=12),
        legend.title = element_blank(), 
        plot.title = element_text(size = 13))

###Create a function that formats dates to 2020-Jan, 2020-Feb etc

format_dates <- function(x) {
  months <-  month(x, label = T, abbr=T)            
  years <- lubridate::year(x) 
  
  if_else(is.na(lag(years)) | lag(years) != years,  
          true = paste(years, months, sep = " - "), 
          false = paste(months))
}

###Create a function that counts number of ROWS by certain grouping variables
counts_by_time <- function(.data, ...) {
  .data %>% 
    group_by(...) %>% 
    summarise(total=n()) %>%
    mutate(prop = total/sum(total), 
           suppressed_total = ifelse(total<11, NA, total))
  
}


###Create a BAR chart that plots over time (note this will only work with date variables on x axis)

scatter_plot <- function(data_frame, x_axis, y_axis) {
  ggplot(data_frame, aes(y = {{ y_axis }}, x = {{ x_axis }})) +
    geom_point(alpha = 0.5)
}


bar_plot_time <- function(data, x_axis, y_axis, group_var, fill_var, colour_vec, colour_lab=NULL, date_breaker, plot_titles) {

data %>% 
  ggplot(aes(x= {{x_axis}}, y={{y_axis}},group={{group_var}}, fill={{fill_var}})) + 
  geom_bar(stat="identity", position= "dodge")+
  geom_vline(xintercept = ymd("2020-03-01"),linetype="dotted", color = "grey", size=1)+ 
  geom_text(aes(label={{y_axis}}), size=4, vjust=-0.75, color="black")+
  standards + 
  scale_fill_manual(name=NULL, values = colour_vec, label=colour_lab) +
  scale_x_date(labels = format_dates, date_breaks = date_breaker, expand = c(0.015, 0.015))+
  theme(axis.text.x = element_text(angle=90,hjust=1, vjust=0.5), 
        legend.position = "bottom") +
  plot_titles
}

stack_bar_time <- function(data, x_axis, y_axis, group_var, fill_var, colour_vec, colour_lab=NULL, date_breaker, plot_titles) {
  
  data %>% 
    ggplot(aes(x= {{x_axis}}, y={{y_axis}},group={{group_var}}, fill={{fill_var}})) + 
    geom_bar(stat = "identity", position = "stack")+ 
    geom_text(aes(label={{y_axis}}), position = position_stack(vjust = 0.5), size = 5) +
    geom_vline(xintercept = ymd("2020-03-01"),linetype="dotted", color = "grey", size=1)+ 
    standards + 
    scale_fill_manual(name=NULL, values = colour_vec, label=colour_lab) +
    scale_x_date(labels = format_dates, date_breaks = date_breaker, expand = c(0.015, 0.015))+
    theme(axis.text.x = element_text(angle=90,hjust=1, vjust=0.5), 
          legend.position = "bottom") +
    plot_titles
}

bar_plot_gen <- function(data, x_axis, y_axis, group_var, fill_var, colour_vec, colour_lab=NULL, plot_titles) {
  
  data %>% 
    ggplot(aes(x= {{x_axis}}, y={{y_axis}},group={{group_var}}, fill={{fill_var}})) + 
    geom_bar(stat="identity", position= "dodge")+
   # geom_text(aes(label={{y_axis}}), size=4, vjust=-0.75, color="black")+
    standards + 
    scale_fill_manual(name=NULL, values = colour_vec, label=colour_lab) +
    theme(axis.text.x = element_text(angle=90,hjust=1, vjust=0.5), 
          legend.position = "bottom") +
    plot_titles
}
