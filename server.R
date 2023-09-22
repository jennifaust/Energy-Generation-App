#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(markdown)

# Define server logic required to draw a histogram
shinyServer(function(input, output, session) {
  
####################
# SUMMARY VIEWS    #
####################
  
# the basic data dictionary
output$energy_dictionary <- renderTable({
  energy_dictionary
},
striped = TRUE, 
bordered = TRUE,
hover = TRUE
)

# used to update the second time selection options based on the user's first selection
observe({
  first_summary_time <- input$user_summary_time_1
  
  time_options <- c('daily', 'monthly', 'quarterly', 'yearly')
  
  second_summary_time_options <- time_options[time_options != first_summary_time]
  
  updateSelectInput(session,
                    'user_summary_time_2',
                    choices = second_summary_time_options,
                    selected = second_summary_time_options[1])
})
  
# box plots based on the user selected region, energy type, time interval 1, and time interval 2  
output$summary_plot_1 <- renderPlot({
  
  # using a series of if else statements to select the time intervals
  if(input$user_summary_time_1 == 'hourly' & input$user_summary_time_2 == 'daily'){
    if(input$show_violin_plot){
      g <- energy_long %>% 
        mutate(hour = lubridate::hour(datetime),
               wday = lubridate::wday(datetime)) %>% 
        filter(generation_type == input$user_summary_gen_type,
               region == input$user_summary_region) %>% 
        ggplot(mapping = aes(x = as.factor(hour), y = value)) +
        geom_violin(alpha = 0.33,
                    mapping = aes(fill = as.factor(hour))) +
        geom_boxplot(mapping = aes(fill = as.factor(hour),
                                   color = as.factor(hour)),
                     alpha = 0.2, size = 0.85) +
        stat_summary(fun.data = 'mean_se',
                     fun.args = list(mult = 2),
                     mapping = aes(color = as.factor(hour)),
                     size =  0.8) +
        facet_grid(wday ~ region + generation_type, scales = 'free') +
        labs(x = 'hour', y = 'energy generated (megawatthours)')
      
    } else {
      g <- energy_long %>% 
        mutate(hour = lubridate::hour(datetime),
               wday = lubridate::wday(datetime)) %>% 
        filter(generation_type == input$user_summary_gen_type,
               region == input$user_summary_region) %>% 
        ggplot(mapping = aes(x = as.factor(hour), y = value)) +
        geom_boxplot(mapping = aes(fill = as.factor(hour),
                                   color = as.factor(hour)),
                     alpha = 0.2, size = 0.85) +
        stat_summary(fun.data = 'mean_se',
                     fun.args = list(mult = 2),
                     mapping = aes(color = as.factor(hour)),
                     size =  0.8) +
        facet_grid(wday ~ region + generation_type, scales = 'free') +
        labs(x = 'hour', y = 'energy generated (megawatthours)')
    }

    
  } else if(input$user_summary_time_1 == 'hourly' & input$user_summary_time_2 == 'monthly') {
    if(input$show_violin_plot){
      g <- energy_long %>% 
        mutate(hour = lubridate::hour(datetime),
               month = lubridate::month(datetime)) %>% 
        filter(generation_type == input$user_summary_gen_type,
               region == input$user_summary_region) %>% 
        ggplot(mapping = aes(x = as.factor(hour), y = value)) +
        geom_violin(alpha = 0.33,
                    mapping = aes(fill = as.factor(hour))) +
        geom_boxplot(mapping = aes(fill = as.factor(hour),
                                   color = as.factor(hour)),
                     alpha = 0.2, size = 0.85) +
        stat_summary(fun.data = 'mean_se',
                     fun.args = list(mult = 2),
                     mapping = aes(color = as.factor(hour)),
                     size =  0.8) +
        facet_grid(month ~ region + generation_type) +
        labs(x = 'hour', y = 'energy generated (megawatthours)')
      
    } else {
      g <- energy_long %>% 
        mutate(hour = lubridate::hour(datetime),
               month = lubridate::month(datetime)) %>% 
        filter(generation_type == input$user_summary_gen_type,
               region == input$user_summary_region) %>% 
        ggplot(mapping = aes(x = as.factor(hour), y = value)) +
        geom_boxplot(mapping = aes(fill = as.factor(hour),
                                   color = as.factor(hour)),
                     alpha = 0.2, size = 0.85) +
        stat_summary(fun.data = 'mean_se',
                     fun.args = list(mult = 2),
                     mapping = aes(color = as.factor(hour)),
                     size =  0.8) +
        facet_grid(month ~ region + generation_type) +
        labs(x = 'hour', y = 'energy generated (megawatthours)')
    }

    
  } else if(input$user_summary_time_1 == 'hourly' & input$user_summary_time_2 == 'quarterly') {
    if(input$show_violin_plot) {
      g <- energy_long %>% 
        mutate(hour = lubridate::hour(datetime),
               quarter = lubridate::quarter(datetime)) %>% 
        filter(generation_type == input$user_summary_gen_type,
               region == input$user_summary_region) %>% 
        ggplot(mapping = aes(x = as.factor(hour), y = value)) +
        geom_violin(alpha = 0.33,
                    mapping = aes(fill = as.factor(hour))) +
        geom_boxplot(fill = NA, outlier.shape = NA,
                     size = 1.,
                     mapping = aes(color = as.factor(hour))) +
        stat_summary(fun.data = 'mean_se',
                     fun.args = list(mult = 2),
                     mapping = aes(color = as.factor(hour)),
                     size =  0.8) +
        facet_grid(quarter ~ generation_type + region) +
        labs(x = 'hour', y = 'energy generated (megawatthours)')
    } else {
      g <- energy_long %>% 
        mutate(hour = lubridate::hour(datetime),
               quarter = lubridate::quarter(datetime)) %>% 
        filter(generation_type == input$user_summary_gen_type,
               region == input$user_summary_region) %>% 
        ggplot(mapping = aes(x = as.factor(hour), y = value)) +
        geom_boxplot(mapping = aes(fill = as.factor(hour),
                                   color = as.factor(hour)),
                     alpha = 0.2, size = 0.85) +
        stat_summary(fun.data = 'mean_se',
                     fun.args = list(mult = 2),
                     mapping = aes(color = as.factor(hour)),
                     size =  0.8) +
        facet_grid(quarter ~ generation_type + region) +
        labs(x = 'hour', y = 'energy generated (megawatthours)')
    }

    
  } else if(input$user_summary_time_1 == 'hourly' & input$user_summary_time_2 == 'yearly') {
    if(input$show_violin_plot){
      g <- energy_long %>% 
        mutate(hour = lubridate::hour(datetime),
               year = lubridate::year(datetime)) %>% 
        filter(generation_type == input$user_summary_gen_type,
               region == input$user_summary_region) %>% 
        ggplot(mapping = aes(x = as.factor(hour), y = value)) +
        geom_violin(alpha = 0.33,
                    mapping = aes(fill = as.factor(hour))) +
        geom_boxplot(mapping = aes(fill = as.factor(hour),
                                   color = as.factor(hour)),
                     alpha = 0.2, size = 0.85) +
        stat_summary(fun.data = 'mean_se',
                     fun.args = list(mult = 2),
                     mapping = aes(color = as.factor(hour)),
                     size =  0.8) +
        facet_grid(year ~ region + generation_type, scales = 'free') +
        labs(x = 'hour', y = 'energy generated (megawatthours)')
      
    } else {
      g <- energy_long %>% 
        mutate(hour = lubridate::hour(datetime),
               year = lubridate::year(datetime)) %>% 
        filter(generation_type == input$user_summary_gen_type,
               region == input$user_summary_region) %>% 
        ggplot(mapping = aes(x = as.factor(hour), y = value)) +
        geom_boxplot(mapping = aes(fill = as.factor(hour),
                                   color = as.factor(hour)),
                     alpha = 0.2, size = 0.85) +
        stat_summary(fun.data = 'mean_se',
                     fun.args = list(mult = 2),
                     mapping = aes(color = as.factor(hour)),
                     size =  0.8) +
        facet_grid(year ~ region + generation_type, scales = 'free') +
        labs(x = 'hour', y = 'energy generated (megawatthours)')

    }

    
  } else if(input$user_summary_time_1 == 'daily' & input$user_summary_time_2 == 'hourly') { ## this will never be triggered
    if(input$show_violin_plot){
      g <- energy_long %>% 
        mutate(wday = lubridate::wday(datetime),
               hour = lubridate::hour(datetime)) %>% 
        filter(generation_type == input$user_summary_gen_type,
               region == input$user_summary_region) %>% 
        ggplot(mapping = aes(x = as.factor(wday), y = value)) +
        geom_violin(alpha = 0.33,
                    mapping = aes(fill = as.factor(wday))) +
        geom_boxplot(mapping = aes(fill = as.factor(wday),
                                   color = as.factor(wday)),
                     alpha = 0.2, size = 0.85) +
        stat_summary(fun.data = 'mean_se',
                     fun.args = list(mult = 2),
                     mapping = aes(color = as.factor(wday)),
                     size =  0.8) +
        facet_grid(hour ~ region + generation_type) +
        labs(x = 'daily', y = 'energy generated (megawatthours)')
      
    } else {
      g <- energy_long %>% 
        mutate(wday = lubridate::wday(datetime),
               hour = lubridate::hour(datetime)) %>% 
        filter(generation_type == input$user_summary_gen_type,
               region == input$user_summary_region) %>% 
        ggplot(mapping = aes(x = as.factor(wday), y = value)) +
        geom_boxplot(mapping = aes(fill = as.factor(wday),
                                   color = as.factor(wday)),
                     alpha = 0.2, size = 0.85) +
        stat_summary(fun.data = 'mean_se',
                     fun.args = list(mult = 2),
                     mapping = aes(color = as.factor(wday)),
                     size =  0.8) +
        facet_grid(hour ~ region + generation_type) +
        labs(x = 'daily', y = 'energy generated (megawatthours)')
    }
    
  } else if(input$user_summary_time_1 == 'daily' & input$user_summary_time_2 == 'monthly') {
    if(input$show_violin_plot){
      g <- energy_long %>% 
        mutate(wday = lubridate::wday(datetime),
               month = lubridate::month(datetime)) %>% 
        filter(generation_type == input$user_summary_gen_type,
               region == input$user_summary_region) %>% 
        ggplot(mapping = aes(x = as.factor(wday), y = value)) +
        geom_violin(alpha = 0.33,
                    mapping = aes(fill = as.factor(wday))) +
        geom_boxplot(mapping = aes(fill = as.factor(wday),
                                   color = as.factor(wday)),
                     alpha = 0.2, size = 0.85) +
        stat_summary(fun.data = 'mean_se',
                     fun.args = list(mult = 2),
                     mapping = aes(color = as.factor(wday)),
                     size =  0.8) +
        facet_grid(month ~ region + generation_type, scales = 'free') +
        labs(x = 'daily', y = 'energy generated (megawatthours)')
      
    } else {
      g <- energy_long %>% 
        mutate(wday = lubridate::wday(datetime),
               month = lubridate::month(datetime)) %>% 
        filter(generation_type == input$user_summary_gen_type,
               region == input$user_summary_region) %>% 
        ggplot(mapping = aes(x = as.factor(wday), y = value)) +
        geom_boxplot(mapping = aes(fill = as.factor(wday),
                                   color = as.factor(wday)),
                     alpha = 0.2, size = 0.85) +
        stat_summary(fun.data = 'mean_se',
                     fun.args = list(mult = 2),
                     mapping = aes(color = as.factor(wday)),
                     size =  0.8) +
        facet_grid(month ~ region + generation_type, scales = 'free') +
        labs(x = 'daily', y = 'energy generated (megawatthours)')
    }

    
  } else if(input$user_summary_time_1 == 'daily' & input$user_summary_time_2 == 'quarterly') {
    if(input$show_violin_plot){
      g <- energy_long %>% 
        mutate(wday = lubridate::wday(datetime),
               quarter = lubridate::quarter(datetime)) %>% 
        filter(generation_type == input$user_summary_gen_type,
               region == input$user_summary_region) %>% 
        ggplot(mapping = aes(x = as.factor(wday), y = value)) +
        geom_violin(alpha = 0.33,
                    mapping = aes(fill = as.factor(wday))) +
        geom_boxplot(mapping = aes(fill = as.factor(wday),
                                   color = as.factor(wday)),
                     alpha = 0.2, size = 0.85) +
        stat_summary(fun.data = 'mean_se',
                     fun.args = list(mult = 2),
                     mapping = aes(color = as.factor(wday)),
                     size =  0.8) +
        facet_grid(quarter ~ region + generation_type, scales = 'free') +
        labs(x = 'daily', y = 'energy generated (megawatthours)')
      
    } else {
      g <- energy_long %>% 
        mutate(wday = lubridate::wday(datetime),
               quarter = lubridate::quarter(datetime)) %>% 
        filter(generation_type == input$user_summary_gen_type,
               region == input$user_summary_region) %>% 
        ggplot(mapping = aes(x = as.factor(wday), y = value)) +
        geom_boxplot(mapping = aes(fill = as.factor(wday),
                                   color = as.factor(wday)),
                     alpha = 0.2, size = 0.85) +
        stat_summary(fun.data = 'mean_se',
                     fun.args = list(mult = 2),
                     mapping = aes(color = as.factor(wday)),
                     size =  0.8) +
        facet_grid(quarter ~ region + generation_type, scales = 'free') +
        labs(x = 'daily', y = 'energy generated (megawatthours)')
    }

    
  } else if(input$user_summary_time_1 == 'daily' & input$user_summary_time_2 == 'yearly') {
    if(input$show_violin_plot){
      g <- energy_long %>% 
        mutate(wday = lubridate::wday(datetime),
               year = lubridate::year(datetime)) %>% 
        filter(generation_type == input$user_summary_gen_type,
               region == input$user_summary_region) %>% 
        ggplot(mapping = aes(x = as.factor(wday), y = value)) +
        geom_violin(alpha = 0.33,
                    mapping = aes(fill = as.factor(wday))) +
        geom_boxplot(mapping = aes(fill = as.factor(wday),
                                   color = as.factor(wday)),
                     alpha = 0.2, size = 0.85) +
        stat_summary(fun.data = 'mean_se',
                     fun.args = list(mult = 2),
                     mapping = aes(color = as.factor(wday)),
                     size =  0.8) +
        facet_grid(year ~ region + generation_type, scales = 'free') +
        labs(x = 'daily', y = 'energy generated (megawatthours)')
      
    } else {
      g <- energy_long %>% 
        mutate(wday = lubridate::wday(datetime),
               year = lubridate::year(datetime)) %>% 
        filter(generation_type == input$user_summary_gen_type,
               region == input$user_summary_region) %>% 
        ggplot(mapping = aes(x = as.factor(wday), y = value)) +
        geom_boxplot(mapping = aes(fill = as.factor(wday),
                                   color = as.factor(wday)),
                     alpha = 0.2, size = 0.85) +
        stat_summary(fun.data = 'mean_se',
                     fun.args = list(mult = 2),
                     mapping = aes(color = as.factor(wday)),
                     size =  0.8) +
        facet_grid(year ~ region + generation_type, scales = 'free') +
        labs(x = 'daily', y = 'energy generated (megawatthours)')
      
    }

    
  } else if(input$user_summary_time_1 == 'monthly' & input$user_summary_time_2 == 'hourly') {
    g <- energy_long %>% 
      mutate(month = lubridate::month(datetime),
             hour = lubridate::hour(datetime)) %>% 
      filter(generation_type == input$user_summary_gen_type,
             region == input$user_summary_region) %>% 
      ggplot(mapping = aes(x = as.factor(month), y = value)) +
      geom_boxplot() +
      facet_grid(hour ~ region + generation_type) +
      labs(x = 'monthly', y = 'energy generated (megawatthours)')
    
  } else if(input$user_summary_time_1 == 'monthly' & input$user_summary_time_2 == 'daily') {
    if(input$show_violin_plot){
      g <- energy_long %>% 
        mutate(month = lubridate::month(datetime),
               wday = lubridate::wday(datetime)) %>% 
        filter(generation_type == input$user_summary_gen_type,
               region == input$user_summary_region) %>% 
        ggplot(mapping = aes(x = as.factor(month), y = value)) +
        geom_violin(alpha = 0.33,
                    mapping = aes(fill = as.factor(month))) +
        geom_boxplot(mapping = aes(fill = as.factor(month),
                                   color = as.factor(month)),
                     alpha = 0.2, size = 0.85) +
        stat_summary(fun.data = 'mean_se',
                     fun.args = list(mult = 2),
                     mapping = aes(color = as.factor(month)),
                     size =  0.8) +
        facet_grid(wday ~ region + generation_type, scales = 'free') +
        labs(x = 'monthly', y = 'energy generated (megawatthours)')
      
    } else {
      g <- energy_long %>% 
        mutate(month = lubridate::month(datetime),
               wday = lubridate::wday(datetime)) %>% 
        filter(generation_type == input$user_summary_gen_type,
               region == input$user_summary_region) %>% 
        ggplot(mapping = aes(x = as.factor(month), y = value)) +
        geom_boxplot(mapping = aes(fill = as.factor(month),
                                   color = as.factor(month)),
                     alpha = 0.2, size = 0.85) +
        stat_summary(fun.data = 'mean_se',
                     fun.args = list(mult = 2),
                     mapping = aes(color = as.factor(month)),
                     size =  0.8) +
        facet_grid(wday ~ region + generation_type, scales = 'free') +
        labs(x = 'monthly', y = 'energy generated (megawatthours)')
      
    }

    
  } else if(input$user_summary_time_1 == 'monthly' & input$user_summary_time_2 == 'quarterly') {
    if(input$show_violin_plot){
      g <- energy_long %>% 
        mutate(month = lubridate::month(datetime),
               quarter = lubridate::quarter(datetime)) %>% 
        filter(generation_type == input$user_summary_gen_type,
               region == input$user_summary_region) %>% 
        ggplot(mapping = aes(x = as.factor(month), y = value)) +
        geom_violin(alpha = 0.33,
                    mapping = aes(fill = as.factor(month))) +
        geom_boxplot(mapping = aes(fill = as.factor(month),
                                   color = as.factor(month)),
                     alpha = 0.2, size = 0.85) +
        stat_summary(fun.data = 'mean_se',
                     fun.args = list(mult = 2),
                     mapping = aes(color = as.factor(month)),
                     size =  0.8) +
        facet_grid(quarter ~ region + generation_type, scales = 'free') +
        labs(x = 'monthly', y = 'energy generated (megawatthours)')
    } else {
      g <- energy_long %>% 
        mutate(month = lubridate::month(datetime),
               quarter = lubridate::quarter(datetime)) %>% 
        filter(generation_type == input$user_summary_gen_type,
               region == input$user_summary_region) %>% 
        ggplot(mapping = aes(x = as.factor(month), y = value)) +
        geom_boxplot(mapping = aes(fill = as.factor(month),
                                   color = as.factor(month)),
                     alpha = 0.2, size = 0.85) +
        stat_summary(fun.data = 'mean_se',
                     fun.args = list(mult = 2),
                     mapping = aes(color = as.factor(month)),
                     size =  0.8) +
        facet_grid(quarter ~ region + generation_type, scales = 'free') +
        labs(x = 'monthly', y = 'energy generated (megawatthours)')
    }

    
  } else if(input$user_summary_time_1 == 'monthly' & input$user_summary_time_2 == 'yearly') {
    if(input$show_violin_plot){
      g <- energy_long %>% 
        mutate(month = lubridate::month(datetime),
               year = lubridate::year(datetime)) %>% 
        filter(generation_type == input$user_summary_gen_type,
               region == input$user_summary_region) %>% 
        ggplot(mapping = aes(x = as.factor(month), y = value)) +
        geom_violin(alpha = 0.33,
                    mapping = aes(fill = as.factor(month))) +
        geom_boxplot(mapping = aes(fill = as.factor(month),
                                   color = as.factor(month)),
                     alpha = 0.2, size = 0.85) +
        stat_summary(fun.data = 'mean_se',
                     fun.args = list(mult = 2),
                     mapping = aes(color = as.factor(month)),
                     size =  0.8) +
        facet_grid(year ~ region + generation_type, scales = 'free') +
        labs(x = 'monthly', y = 'energy generated (megawatthours)')
      
    } else {
      g <- energy_long %>% 
        mutate(month = lubridate::month(datetime),
               year = lubridate::year(datetime)) %>% 
        filter(generation_type == input$user_summary_gen_type,
               region == input$user_summary_region) %>% 
        ggplot(mapping = aes(x = as.factor(month), y = value)) +
        geom_boxplot(mapping = aes(fill = as.factor(month),
                                   color = as.factor(month)),
                     alpha = 0.2, size = 0.85) +
        stat_summary(fun.data = 'mean_se',
                     fun.args = list(mult = 2),
                     mapping = aes(color = as.factor(month)),
                     size =  0.8) +
        facet_grid(year ~ region + generation_type, scales = 'free') +
        labs(x = 'monthly', y = 'energy generated (megawatthours)')
    }

    
  } else if(input$user_summary_time_1 == 'quarterly' & input$user_summary_time_2 == 'hourly') {
    g <- energy_long %>% 
      mutate(quarter = lubridate::quarter(datetime),
             hour = lubridate::hour(datetime)) %>% 
      filter(generation_type == input$user_summary_gen_type,
             region == input$user_summary_region) %>% 
      ggplot(mapping = aes(x = as.factor(quarter), y = value)) +
      geom_boxplot() +
      facet_grid(hour ~ region + generation_type) +
      labs(x = 'quarterly', y = 'energy generated (megawatthours)')
    
  } else if(input$user_summary_time_1 == 'quarterly' & input$user_summary_time_2 == 'daily') {
    if(input$show_violin_plot){
      g <- energy_long %>% 
        mutate(quarter = lubridate::quarter(datetime),
               week = lubridate::wday(datetime)) %>% 
        filter(generation_type == input$user_summary_gen_type,
               region == input$user_summary_region) %>% 
        ggplot(mapping = aes(x = as.factor(quarter), y = value)) +
        geom_violin(alpha = 0.33,
                    mapping = aes(fill = as.factor(quarter))) +
        geom_boxplot(mapping = aes(fill = as.factor(quarter),
                                   color = as.factor(quarter)),
                     alpha = 0.2, size = 0.85) +
        stat_summary(fun.data = 'mean_se',
                     fun.args = list(mult = 2),
                     mapping = aes(color = as.factor(quarter)),
                     size =  0.8) +
        facet_grid(week ~ region + generation_type, scales = 'free') +
        labs(x = 'quarterly', y = 'energy generated (megawatthours)')
    } else {
      g <- energy_long %>% 
        mutate(quarter = lubridate::quarter(datetime),
               week = lubridate::wday(datetime)) %>% 
        filter(generation_type == input$user_summary_gen_type,
               region == input$user_summary_region) %>% 
        ggplot(mapping = aes(x = as.factor(quarter), y = value)) +
        geom_boxplot(mapping = aes(fill = as.factor(quarter),
                                   color = as.factor(quarter)),
                     alpha = 0.2, size = 0.85) +
        stat_summary(fun.data = 'mean_se',
                     fun.args = list(mult = 2),
                     mapping = aes(color = as.factor(quarter)),
                     size =  0.8) +
        facet_grid(week ~ region + generation_type, scales = 'free') +
        labs(x = 'quarterly', y = 'energy generated (megawatthours)')
    }

    
  } else if(input$user_summary_time_1 == 'quarterly' & input$user_summary_time_2 == 'monthly') {
    if(input$show_violin_plot){
      g <- energy_long %>% 
        mutate(quarter = lubridate::quarter(datetime),
               month = lubridate::month(datetime)) %>% 
        filter(generation_type == input$user_summary_gen_type,
               region == input$user_summary_region) %>% 
        ggplot(mapping = aes(x = as.factor(quarter), y = value)) +
        geom_violin(alpha = 0.33,
                    mapping = aes(fill = as.factor(quarter))) +
        geom_boxplot(mapping = aes(fill = as.factor(quarter),
                                   color = as.factor(quarter)),
                     alpha = 0.2, size = 0.85) +
        stat_summary(fun.data = 'mean_se',
                     fun.args = list(mult = 2),
                     mapping = aes(color = as.factor(quarter)),
                     size =  0.8) +
        facet_grid(month ~ region + generation_type, scales = 'free') +
        labs(x = 'quarterly', y = 'energy generated (megawatthours)')
    } else {
      g <- energy_long %>% 
        mutate(quarter = lubridate::quarter(datetime),
               month = lubridate::month(datetime)) %>% 
        filter(generation_type == input$user_summary_gen_type,
               region == input$user_summary_region) %>% 
        ggplot(mapping = aes(x = as.factor(quarter), y = value)) +
        geom_boxplot(mapping = aes(fill = as.factor(quarter),
                                   color = as.factor(quarter)),
                     alpha = 0.2, size = 0.85) +
        stat_summary(fun.data = 'mean_se',
                     fun.args = list(mult = 2),
                     mapping = aes(color = as.factor(quarter)),
                     size =  0.8) +
        facet_grid(month ~ region + generation_type, scales = 'free') +
        labs(x = 'quarterly', y = 'energy generated (megawatthours)')
      
    }

    
  } else if(input$user_summary_time_1 == 'quarterly' & input$user_summary_time_2 == 'yearly') {
    if(input$show_violin_plot){
      g <- energy_long %>% 
        mutate(quarter = lubridate::quarter(datetime),
               year = lubridate::year(datetime)) %>% 
        filter(generation_type == input$user_summary_gen_type,
               region == input$user_summary_region) %>% 
        ggplot(mapping = aes(x = as.factor(quarter), y = value)) +
        geom_violin(alpha = 0.33,
                    mapping = aes(fill = as.factor(quarter))) +
        geom_boxplot(mapping = aes(fill = as.factor(quarter),
                                   color = as.factor(quarter)),
                     alpha = 0.2, size = 0.85) +
        stat_summary(fun.data = 'mean_se',
                     fun.args = list(mult = 2),
                     mapping = aes(color = as.factor(quarter)),
                     size =  0.8) +
        facet_grid(year ~ region + generation_type) +
        labs(x = 'quarterly', y = 'energy generated (megawatthours)')
    } else {
      g <- energy_long %>% 
        mutate(quarter = lubridate::quarter(datetime),
               year = lubridate::year(datetime)) %>% 
        filter(generation_type == input$user_summary_gen_type,
               region == input$user_summary_region) %>% 
        ggplot(mapping = aes(x = as.factor(quarter), y = value)) +
        geom_boxplot(mapping = aes(fill = as.factor(quarter),
                                   color = as.factor(quarter)),
                     alpha = 0.2, size = 0.85) +
        stat_summary(fun.data = 'mean_se',
                     fun.args = list(mult = 2),
                     mapping = aes(color = as.factor(quarter)),
                     size =  0.8) +
        facet_grid(year ~ region + generation_type) +
        labs(x = 'quarterly', y = 'energy generated (megawatthours)')
    }
    

    
  } else if(input$user_summary_time_1 == 'yearly' & input$user_summary_time_2 == 'hourly') {
    g <- energy_long %>% 
      mutate(year = lubridate::year(datetime),
             hour = lubridate::hour(datetime)) %>% 
      filter(generation_type == input$user_summary_gen_type,
             region == input$user_summary_region) %>% 
      ggplot(mapping = aes(x = as.factor(year), y = value)) +
      geom_boxplot() +
      facet_grid(hour ~ region + generation_type) +
      labs(x = 'yearly', y = 'energy generated (megawatthours)')
    
  } else if(input$user_summary_time_1 == 'yearly' & input$user_summary_time_2 == 'daily') {
    if(input$show_violin_plot){
      g <- energy_long %>% 
        mutate(year = lubridate::year(datetime),
               week = lubridate::wday(datetime)) %>% 
        filter(generation_type == input$user_summary_gen_type,
               region == input$user_summary_region) %>% 
        ggplot(mapping = aes(x = as.factor(year), y = value)) +
        geom_violin(alpha = 0.33,
                    mapping = aes(fill = as.factor(year))) +
        geom_boxplot(mapping = aes(fill = as.factor(year),
                                   color = as.factor(year)),
                     alpha = 0.2, size = 0.85) +
        stat_summary(fun.data = 'mean_se',
                     fun.args = list(mult = 2),
                     mapping = aes(color = as.factor(year)),
                     size =  0.8) +
        facet_grid(week ~ region + generation_type) +
        labs(x = 'yearly', y = 'energy generated (megawatthours)')
    } else {
      g <- energy_long %>% 
        mutate(year = lubridate::year(datetime),
               week = lubridate::wday(datetime)) %>% 
        filter(generation_type == input$user_summary_gen_type,
               region == input$user_summary_region) %>% 
        ggplot(mapping = aes(x = as.factor(year), y = value)) +
        geom_boxplot(mapping = aes(fill = as.factor(year),
                                   color = as.factor(year)),
                     alpha = 0.2, size = 0.85) +
        stat_summary(fun.data = 'mean_se',
                     fun.args = list(mult = 2),
                     mapping = aes(color = as.factor(year)),
                     size =  0.8) +
        facet_grid(week ~ region + generation_type) +
        labs(x = 'yearly', y = 'energy generated (megawatthours)')
    }
    
  } else if(input$user_summary_time_1 == 'yearly' & input$user_summary_time_2 == 'monthly') {
    if(input$show_violin_plot){
      g <- energy_long %>% 
        mutate(year = lubridate::year(datetime),
               month = lubridate::month(datetime)) %>% 
        filter(generation_type == input$user_summary_gen_type,
               region == input$user_summary_region) %>% 
        ggplot(mapping = aes(x = as.factor(year), y = value)) +
        geom_violin(alpha = 0.33,
                    mapping = aes(fill = as.factor(year))) +
        geom_boxplot(mapping = aes(fill = as.factor(year),
                                   color = as.factor(year)),
                     alpha = 0.2, size = 0.85) +
        stat_summary(fun.data = 'mean_se',
                     fun.args = list(mult = 2),
                     mapping = aes(color = as.factor(year)),
                     size =  0.8) +
        facet_grid(month ~ region + generation_type, scales = 'free') +
        labs(x = 'yearly', y = 'energy generated (megawatthours)')
    } else {
      g <- energy_long %>% 
        mutate(year = lubridate::year(datetime),
               month = lubridate::month(datetime)) %>% 
        filter(generation_type == input$user_summary_gen_type,
               region == input$user_summary_region) %>% 
        ggplot(mapping = aes(x = as.factor(year), y = value)) +
        geom_boxplot(mapping = aes(fill = as.factor(year),
                                   color = as.factor(year)),
                     alpha = 0.2, size = 0.85) +
        stat_summary(fun.data = 'mean_se',
                     fun.args = list(mult = 2),
                     mapping = aes(color = as.factor(year)),
                     size =  0.8) +
        facet_grid(month ~ region + generation_type, scales = 'free') +
        labs(x = 'yearly', y = 'energy generated (megawatthours)')
    }

    
  } else if(input$user_summary_time_1 == 'yearly' & input$user_summary_time_2 == 'quarterly') {
    if(input$show_violin_plot){
      g <- energy_long %>% 
        mutate(year = lubridate::year(datetime),
               quarter = lubridate::quarter(datetime)) %>% 
        filter(generation_type == input$user_summary_gen_type,
               region == input$user_summary_region) %>% 
        ggplot(mapping = aes(x = as.factor(year), y = value)) +
        geom_violin(alpha = 0.33,
                    mapping = aes(fill = as.factor(year))) +
        geom_boxplot(mapping = aes(fill = as.factor(year),
                                   color = as.factor(year)),
                     alpha = 0.2, size = 0.85) +
        stat_summary(fun.data = 'mean_se',
                     fun.args = list(mult = 2),
                     mapping = aes(color = as.factor(year)),
                     size =  0.8) +
        facet_grid(quarter ~ region + generation_type, scales = 'free') +
        labs(x = 'yearly', y = 'energy generated (megawatthours)')
    } else {
      g <- energy_long %>% 
        mutate(year = lubridate::year(datetime),
               quarter = lubridate::quarter(datetime)) %>% 
        filter(generation_type == input$user_summary_gen_type,
               region == input$user_summary_region) %>% 
        ggplot(mapping = aes(x = as.factor(year), y = value)) +
        geom_boxplot(mapping = aes(fill = as.factor(year),
                                   color = as.factor(year)),
                     alpha = 0.2, size = 0.85) +
        stat_summary(fun.data = 'mean_se',
                     fun.args = list(mult = 2),
                     mapping = aes(color = as.factor(year)),
                     size =  0.8) +
        facet_grid(quarter ~ region + generation_type, scales = 'free') +
        labs(x = 'yearly', y = 'energy generated (megawatthours)')
    }

    
  } else if(input$user_summary_time_2 == 'hourly' & input$user_summary_time_1 == 'daily'){ # will never trigger
    g <- energy_long %>% 
      mutate(hour = lubridate::hour(datetime),
             wday = lubridate::wday(datetime)) %>% 
      filter(generation_type == input$user_summary_gen_type,
             region == input$user_summary_region) %>% 
      ggplot(mapping = aes(x = as.factor(wday), y = value)) +
      geom_boxplot() +
      facet_grid(hour ~ region + generation_type)
    
  } else if(input$user_summary_time_2 == 'hourly' & input$user_summary_time_1 == 'monthly') { # will never trigger
    g <- energy_long %>% 
      mutate(hour = lubridate::hour(datetime),
             month = lubridate::month(datetime)) %>% 
      filter(generation_type == input$user_summary_gen_type,
             region == input$user_summary_region) %>% 
      ggplot(mapping = aes(x = as.factor(month), y = value)) +
      geom_boxplot() +
      facet_grid(hour ~ region + generation_type)
    
  } else if(input$user_summary_time_2 == 'hourly' & input$user_summary_time_1 == 'quarterly') { # will never trigger
    g <- energy_long %>% 
      mutate(hour = lubridate::hour(datetime),
             quarter = lubridate::quarter(datetime)) %>% 
      filter(generation_type == input$user_summary_gen_type,
             region == input$user_summary_region) %>% 
      ggplot(mapping = aes(x = as.factor(quarter), y = value)) +
      geom_boxplot() +
      facet_grid(hour ~ region + generation_type)
    
  } else if(input$user_summary_time_2 == 'hourly' & input$user_summary_time_1 == 'yearly') { # will never trigger
    g <- energy_long %>% 
      mutate(hour = lubridate::hour(datetime),
             year = lubridate::year(datetime)) %>% 
      filter(generation_type == input$user_summary_gen_type,
             region == input$user_summary_region) %>% 
      ggplot(mapping = aes(x = as.factor(year), y = value)) +
      geom_boxplot() +
      facet_grid(hour ~ region + generation_type)
    
  } else if(input$user_summary_time_2 == 'daily' & input$user_summary_time_1 == 'hourly') {  # will never trigger
    g <- energy_long %>% 
      mutate(wday = lubridate::wday(datetime),
             hour = lubridate::hour(datetime)) %>% 
      filter(generation_type == input$user_summary_gen_type,
             region == input$user_summary_region) %>% 
      ggplot(mapping = aes(x = as.factor(hour), y = value)) +
      geom_boxplot() +
      facet_grid(wday ~ region + generation_type)
  } else if(input$user_summary_time_2 == 'daily' & input$user_summary_time_1 == 'monthly') { # will never trigger
    g <- energy_long %>% 
      mutate(wday = lubridate::wday(datetime),
             month = lubridate::month(datetime)) %>% 
      filter(generation_type == input$user_summary_gen_type,
             region == input$user_summary_region) %>% 
      ggplot(mapping = aes(x = as.factor(month), y = value)) +
      geom_boxplot() +
      facet_grid(wday ~ region + generation_type)
    
  } else if(input$user_summary_time_2 == 'daily' & input$user_summary_time_1 == 'quarterly') { # will never trigger
    g <- energy_long %>% 
      mutate(wday = lubridate::wday(datetime),
             quarter = lubridate::quarter(datetime)) %>% 
      filter(generation_type == input$user_summary_gen_type,
             region == input$user_summary_region) %>% 
      ggplot(mapping = aes(x = as.factor(quarter), y = value)) +
      geom_boxplot() +
      facet_grid(wday ~ region + generation_type)
    
  } else if(input$user_summary_time_2 == 'daily' & input$user_summary_time_1 == 'yearly') { # will never trigger
    g <- energy_long %>% 
      mutate(wday = lubridate::wday(datetime),
             year = lubridate::year(datetime)) %>% 
      filter(generation_type == input$user_summary_gen_type,
             region == input$user_summary_region) %>% 
      ggplot(mapping = aes(x = as.factor(year), y = value)) +
      geom_boxplot() +
      facet_grid(wday ~ region + generation_type)
    
  } else if(input$user_summary_time_2 == 'monthly' & input$user_summary_time_1 == 'hourly') { # will never trigger
    g <- energy_long %>% 
      mutate(month = lubridate::month(datetime),
             hour = lubridate::hour(datetime)) %>% 
      filter(generation_type == input$user_summary_gen_type,
             region == input$user_summary_region) %>% 
      ggplot(mapping = aes(x = as.factor(hour), y = value)) +
      geom_boxplot() +
      facet_grid(month ~ region + generation_type)
    
  } else if(input$user_summary_time_2 == 'monthly' & input$user_summary_time_1 == 'daily') { # will never trigger
    g <- energy_long %>% 
      mutate(month = lubridate::month(datetime),
             wday = lubridate::wday(datetime)) %>% 
      filter(generation_type == input$user_summary_gen_type,
             region == input$user_summary_region) %>% 
      ggplot(mapping = aes(x = as.factor(wday), y = value)) +
      geom_boxplot() +
      facet_grid(month ~ region + generation_type)
    
  } else if(input$user_summary_time_2 == 'monthly' & input$user_summary_time_1 == 'quarterly') { # will never trigger
    g <- energy_long %>% 
      mutate(month = lubridate::month(datetime),
             quarter = lubridate::quarter(datetime)) %>% 
      filter(generation_type == input$user_summary_gen_type,
             region == input$user_summary_region) %>% 
      ggplot(mapping = aes(x = as.factor(quarter), y = value)) +
      geom_boxplot() +
      facet_grid(month ~ region + generation_type)
    
  } else if(input$user_summary_time_2 == 'monthly' & input$user_summary_time_1 == 'yearly') { # will never trigger
    g <- energy_long %>% 
      mutate(month = lubridate::month(datetime),
             year = lubridate::year(datetime)) %>% 
      filter(generation_type == input$user_summary_gen_type,
             region == input$user_summary_region) %>% 
      ggplot(mapping = aes(x = as.factor(year), y = value)) +
      geom_boxplot() +
      facet_grid(month ~ region + generation_type)
    
  } else if(input$user_summary_time_2 == 'quarterly' & input$user_summary_time_1 == 'hourly') { # will never trigger
    g <- energy_long %>% 
      mutate(quarter = lubridate::quarter(datetime),
             hour = lubridate::hour(datetime)) %>% 
      filter(generation_type == input$user_summary_gen_type,
             region == input$user_summary_region) %>% 
      ggplot(mapping = aes(x = as.factor(hour), y = value)) +
      geom_boxplot() +
      facet_grid(quarter ~ region + generation_type)
    
  } else if(input$user_summary_time_2 == 'quarterly' & input$user_summary_time_1 == 'daily') { # will never trigger
    g <- energy_long %>% 
      mutate(quarter = lubridate::quarter(datetime),
             wday = lubridate::wday(datetime)) %>% 
      filter(generation_type == input$user_summary_gen_type,
             region == input$user_summary_region) %>% 
      ggplot(mapping = aes(x = as.factor(wday), y = value)) +
      geom_boxplot() +
      facet_grid(quarter ~ region + generation_type)
    
  } else if(input$user_summary_time_2 == 'quarterly' & input$user_summary_time_1 == 'monthly') { # will never trigger
    g <- energy_long %>% 
      mutate(quarter = lubridate::quarter(datetime),
             month = lubridate::month(datetime)) %>% 
      filter(generation_type == input$user_summary_gen_type,
             region == input$user_summary_region) %>% 
      ggplot(mapping = aes(x = as.factor(month), y = value)) +
      geom_boxplot() +
      facet_grid(quarter ~ region + generation_type)
    
  } else if(input$user_summary_time_2 == 'quarterly' & input$user_summary_time_1 == 'yearly') { # will never trigger
    g <- energy_long %>% 
      mutate(quarter = lubridate::quarter(datetime),
             year = lubridate::year(datetime)) %>% 
      filter(generation_type == input$user_summary_gen_type,
             region == input$user_summary_region) %>% 
      ggplot(mapping = aes(x = as.factor(year), y = value)) +
      geom_boxplot() +
      facet_grid(quarter ~ region + generation_type)
    
  } else if(input$user_summary_time_2 == 'yearly' & input$user_summary_time_1 == 'hourly') { # will never trigger
    g <- energy_long %>% 
      mutate(year = lubridate::year(datetime),
             hour = lubridate::hour(datetime)) %>% 
      filter(generation_type == input$user_summary_gen_type,
             region == input$user_summary_region) %>% 
      ggplot(mapping = aes(x = as.factor(hour), y = value)) +
      geom_boxplot() +
      facet_grid(year ~ region + generation_type)
    
  } else if(input$user_summary_time_2 == 'yearly' & input$user_summary_time_1 == 'daily') { # will never trigger
    g <- energy_long %>% 
      mutate(year = lubridate::year(datetime),
             wday = lubridate::wday(datetime)) %>% 
      filter(generation_type == input$user_summary_gen_type,
             region == input$user_summary_region) %>% 
      ggplot(mapping = aes(x = as.factor(wday), y = value)) +
      geom_boxplot() +
      facet_grid(year ~ region + generation_type)
    
  } else if(input$user_summary_time_2 == 'yearly' & input$user_summary_time_1 == 'monthly') { # will never trigger
    g <- energy_long %>% 
      mutate(year = lubridate::year(datetime),
             month = lubridate::month(datetime)) %>% 
      filter(generation_type == input$user_summary_gen_type,
             region == input$user_summary_region) %>% 
      ggplot(mapping = aes(x = as.factor(month), y = value)) +
      geom_boxplot() +
      facet_grid(year ~ region + generation_type)
    
  } else if(input$user_summary_time_2 == 'yearly' & input$user_summary_time_1 == 'quarterly') { # will never trigger
    g <- energy_long %>% 
      mutate(year = lubridate::year(datetime),
             quarter = lubridate::quarter(datetime)) %>% 
      filter(generation_type == input$user_summary_gen_type,
             region == input$user_summary_region) %>% 
      ggplot(mapping = aes(x = as.factor(quarter), y = value)) +
      geom_boxplot() +
      facet_grid(year ~ region + generation_type)
  } 
  
  g + 
    scale_color_viridis_d(guide = 'none', option = 'viridis') + 
    scale_fill_viridis_d(guide = 'none', option = 'viridis') +
    theme_bw()
})

#################
# DRILL DOWNS   #
#################

output$drill_down_plot_1 <- renderPlot({
  if(input$user_drill_down_x_axis == 'hourly' & input$user_drill_down_grouping == 'quarter'){
    g <- energy_long %>% 
      mutate(hour = lubridate::hour(datetime),
             quarter = lubridate::quarter(datetime)) %>% 
      filter(region == input$user_drill_down_region,
             generation_type == input$user_drill_down_gen_type) %>% 
      ggplot(mapping = aes(x = as.factor(hour), y = value)) +
      geom_boxplot(mapping = aes(fill = as.factor(quarter),
                                 color = as.factor(quarter)),
                   alpha = 0.33) +
      stat_summary(fun.data = "mean_se",
                   mapping = aes(group = interaction(hour, quarter)),
                   color = 'red',
                   fun.args = list(mult=2),
                   position = position_dodge(0.73)) +
      facet_wrap( ~ region + generation_type) +
      scale_fill_viridis_d("quarter") +
      scale_color_viridis_d("quarter") +
      labs(x = 'hours of the day', y = 'energy generated (megawatthours)')

    
  } else if (input$user_drill_down_x_axis == 'hourly' & input$user_drill_down_grouping == 'year'){
    g <- energy_long %>% 
      mutate(hour = lubridate::hour(datetime),
             year = lubridate::year(datetime)) %>% 
      filter(region == input$user_drill_down_region,
             generation_type == input$user_drill_down_gen_type) %>% 
      ggplot(mapping = aes(x = as.factor(hour), y = value)) +
      geom_boxplot(mapping = aes(fill = as.factor(year),
                                 color = as.factor(year)),
                   alpha = 0.33) +
      stat_summary(fun.data = "mean_se",
                   mapping = aes(group = interaction(hour, year)),
                   color = 'red',
                   fun.args = list(mult=2),
                   position = position_dodge(0.73)) +
      facet_wrap( ~ region + generation_type) +
      scale_fill_viridis_d("year") +
      scale_color_viridis_d("year") +
      labs(x = 'hours of the day', y = 'energy generated (megawatthours)')
    
  } else if (input$user_drill_down_x_axis == 'daily' & input$user_drill_down_grouping == 'quarter'){
    g <- energy_long %>% 
      mutate(wday = lubridate::wday(datetime),
             quarter = lubridate::quarter(datetime)) %>% 
      filter(region == input$user_drill_down_region,
             generation_type == input$user_drill_down_gen_type) %>% 
      ggplot(mapping = aes(x = as.factor(wday), y = value)) +
      geom_boxplot(mapping = aes(fill = as.factor(quarter),
                                 color = as.factor(quarter)),
                   alpha = 0.33) +
      stat_summary(fun.data = "mean_se",
                   mapping = aes(group = interaction(wday, quarter)),
                   color = 'red',
                   fun.args = list(mult=2),
                   position = position_dodge(0.73)) +
      facet_wrap( ~ region + generation_type) +
      scale_fill_viridis_d("quarter") +
      scale_color_viridis_d("quarter")  +
      labs(x = 'days of the week', y = 'energy generated (megawatthours)')
    
  } else if (input$user_drill_down_x_axis == 'daily' & input$user_drill_down_grouping == 'year'){
    g <- energy_long %>% 
      mutate(wday = lubridate::wday(datetime),
             year = lubridate::year(datetime)) %>% 
      filter(region == input$user_drill_down_region,
             generation_type == input$user_drill_down_gen_type) %>% 
      ggplot(mapping = aes(x = as.factor(wday), y = value)) +
      geom_boxplot(mapping = aes(fill = as.factor(year),
                                 color = as.factor(year)),
                   alpha = 0.33) +
      stat_summary(fun.data = "mean_se",
                   mapping = aes(group = interaction(wday, year)),
                   color = 'red',
                   fun.args = list(mult=2),
                   position = position_dodge(0.73)) +
      facet_wrap( ~ region + generation_type) +
      scale_fill_viridis_d("year") +
      scale_color_viridis_d("year") +
      labs(x = 'days of the week', y = 'energy generated (megawatthours)')
    
  }
  g +
    theme_bw()
})


#################
# PROPORTIONS   #
#################

output$prop_plot_1 <- renderPlot({
  if(input$user_prop_time_interval == 'quarter') {
    
    g <- energy_long_v %>%
      mutate(the_quarter = lubridate::quarter(datetime)) %>%
      filter(generation_type == input$user_prop_gen_type) %>%
      ggplot(mapping = aes(y = region, x = prop_value)) +
      geom_boxplot(mapping = aes(fill = as.factor(the_quarter),
                                 color = as.factor(the_quarter)),
                   alpha = 0.2) +
      facet_wrap(~generation_type) +
      scale_fill_viridis_d("quarter") +
      scale_color_viridis_d("quarter")
    

  } else {
    
    g <- energy_long_v %>%
    mutate(year = lubridate::year(datetime)) %>%
    filter(generation_type == input$user_prop_gen_type) %>%
    ggplot(mapping = aes(y = region, x = prop_value)) +
    geom_boxplot(mapping = aes(fill = as.factor(year),
                               color = as.factor(year)),
                 alpha = 0.2) +
    facet_wrap(~generation_type) +
    scale_fill_viridis_d("year") +
    scale_color_viridis_d("year")
  }
  g +
    theme_bw()
})

#################
# PCP           #
#################

# looking at weekly values instead of hourly values
output$pcp_plot_1 <- renderPlot({
  if(input$user_pcp_drill_down == 'quarter'){
    if(input$user_pcp_scaled_prop == 'scaled proportion'){
      g <- energy_wide_gen_type %>% 
        tibble::rowid_to_column() %>% 
        select(rowid, input$user_pcp_gen_type) %>% 
        right_join(energy_pcp_week_lf_ready) %>% 
        filter(region == input$user_pcp_region) %>% 
        ggplot(mapping = aes(x = name, y = scaled_value)) +
        geom_line(mapping = aes(group = rowid,
                                color = prop_value),
                  alpha = input$user_pcp_transparency) +
        facet_wrap(~ region + quarter) +
        coord_flip() +
        scale_color_viridis_c(option = 'viridis') +
        labs(y = "relative scaled proportion")
    } else {
      g <- energy_wide_gen_type %>% 
        tibble::rowid_to_column() %>% 
        select(rowid, input$user_pcp_gen_type) %>% 
        right_join(energy_pcp_week_lf_ready) %>% 
        filter(region == input$user_pcp_region) %>% 
        ggplot(mapping = aes(x = name, y = prop_value)) +
        geom_line(mapping = aes(group = rowid,
                                color = prop_value),
                  alpha = input$user_pcp_transparency) +
        facet_wrap(~ region + quarter) +
        coord_flip() +
        scale_color_viridis_c(option = 'viridis') +
        labs(y = "energy proportion for one week in a region")
    }
    
   
    # previous bad plot
     # g <- energy_wide_gen_type %>% 
    #   tibble::rowid_to_column() %>% 
    #   select(rowid, input$user_pcp_gen_type) %>% 
    #   right_join(energy_pcp_lf_ready) %>% 
    #   filter(region == input$user_pcp_region
    #          # rowid < input$user_pcp_num_rows
    #   ) %>% 
    #   mutate(quarter = lubridate::quarter(datetime)) %>% 
    #   ggplot(mapping = aes(x = name, y = scaled_value)) +
    #   geom_line(mapping = aes(group = rowid,
    #                           color = .data[[input$user_pcp_gen_type]])) +
    #   facet_wrap(~ quarter + region) +
    #   coord_flip() +
    #   scale_color_viridis_c() +
    #   theme_bw()
    
    # this is the plot that is incorrect
    # g <-  energy_pcp_lf_ready %>%
    #   filter(region == input$user_pcp_region) %>%
    #   group_by(region, name, datetime) %>%
    #   mutate(region_row = 1:n(),
    #          quarter = lubridate::quarter(datetime)) %>%
    #   ungroup() %>%
    #   filter(region_row < input$user_pcp_num_rows + 1) %>%
    #   ggplot(mapping = aes(x = name, y = scaled_value)) +
    #   geom_line(mapping = aes(group = rowid),
    #             size = 1.2,
    #             alpha = 0.22) +
    #   coord_flip() +
    #   facet_wrap(~region + quarter, labeller = 'label_both')

  # allow the user to drill down by year
  } else if (input$user_pcp_drill_down == 'year') {
    if(input$user_pcp_scaled_prop == 'scaled proportion'){
      g <- energy_wide_gen_type %>% 
        tibble::rowid_to_column() %>% 
        select(rowid, input$user_pcp_gen_type) %>% 
        right_join(energy_pcp_week_lf_ready) %>% 
        filter(region == input$user_pcp_region) %>% 
        ggplot(mapping = aes(x = name, y = scaled_value)) +
        geom_line(mapping = aes(group = rowid,
                                color = prop_value),
                  alpha = input$user_pcp_transparency) +
        facet_wrap(~ region + year) +
        coord_flip() +
        scale_color_viridis_c(option = 'viridis') +
        labs(y = "relative scaled proportion")
    } else {
      g <- energy_wide_gen_type %>% 
        tibble::rowid_to_column() %>% 
        select(rowid, input$user_pcp_gen_type) %>% 
        right_join(energy_pcp_week_lf_ready) %>% 
        filter(region == input$user_pcp_region) %>% 
        ggplot(mapping = aes(x = name, y = prop_value)) +
        geom_line(mapping = aes(group = rowid,
                                color = prop_value),
                  alpha = input$user_pcp_transparency) +
        facet_wrap(~ region + year) +
        coord_flip() +
        scale_color_viridis_c(option = 'viridis') +
        labs(y = "energy proportion for one week in a region")
    } 
    
    
    
    
    # previous bad plot
    # g <- energy_wide_gen_type %>% 
    #   tibble::rowid_to_column() %>% 
    #   select(rowid, input$user_pcp_gen_type) %>% 
    #   right_join(energy_pcp_lf_ready) %>% 
    #   filter(region == input$user_pcp_region
    #          # rowid < input$user_pcp_num_rows
    #   ) %>% 
    #   mutate(year = lubridate::year(datetime)) %>% 
    #   ggplot(mapping = aes(x = name, y = scaled_value)) +
    #   geom_line(mapping = aes_string(group = .data[[rowid]],
    #                           color = input$user_pcp_gen_type)) +
    #   facet_wrap(~ year + region) +
    #   coord_flip() +
    #   scale_color_viridis_c() +
    #   theme_bw()
    
    # this is the plot that is incorrect
    # g <-  energy_pcp_lf_ready %>%
    #   filter(region == input$user_pcp_region) %>%
    #   group_by(region, name, datetime) %>%
    #   mutate(region_row = 1:n(),
    #          year = lubridate::year(datetime)) %>%
    #   ungroup() %>%
    #   filter(region_row < input$user_pcp_num_rows + 1) %>%
    #   ggplot(mapping = aes(x = name, y = scaled_value)) +
    #   geom_line(mapping = aes_string(group = rowid,
    #                           color = input$user_pcp_gen_type),
    #             size = 1.2,
    #             alpha = 0.22) +
    #   coord_flip() +
    #   facet_wrap(~region + year, labeller = 'label_both')
    
    # no drill down wanted
  } else {
    if(input$user_pcp_scaled_prop == 'scaled proportion'){
      g <- energy_wide_gen_type %>% 
        tibble::rowid_to_column() %>% 
        select(rowid, input$user_pcp_gen_type) %>% 
        right_join(energy_pcp_week_lf_ready) %>% 
        filter(region == input$user_pcp_region) %>% 
        ggplot(mapping = aes(x = name, y = scaled_value)) +
        geom_line(mapping = aes(group = rowid,
                                color = prop_value),
                  alpha = input$user_pcp_transparency) +
        facet_wrap(~ region) +
        coord_flip() +
        scale_color_viridis_c(option = 'viridis') +
        labs(y = "relative scaled proportion")
        
    } else {
      g <- energy_wide_gen_type %>% 
        tibble::rowid_to_column() %>% 
        select(rowid, input$user_pcp_gen_type) %>% 
        right_join(energy_pcp_week_lf_ready) %>% 
        filter(region == input$user_pcp_region) %>% 
        ggplot(mapping = aes(x = name, y = prop_value)) +
        geom_line(mapping = aes(group = rowid,
                                color = prop_value),
                  alpha = input$user_pcp_transparency) +
        facet_wrap(~ region) +
        coord_flip() +
        scale_color_viridis_c(option = 'viridis') +
        labs(y = "energy proportion for one week in a region")
    }
    
  }
  g + 
    labs(x = "energy generation type") +
    theme_bw() 

  
})

# iris_scaled_lf %>% 
#   group_by(Species, name) %>% 
#   mutate(species_row = 1:n()) %>% 
#   ungroup() %>% 
#   filter(species_row < 3) %>% 
#   ggplot(mapping = aes(x = name, y = scaled_value)) +
#   geom_linerange(mapping = aes(ymin = 0, 
#                                ymax = scaled_value,
#                                color = Species),
#                  size = 1.2) +
#   geom_point(size = 9,
#              mapping = aes(color = Species)) +
#   geom_text(mapping = aes(label = rowid),
#             color = 'white') +
#   facet_wrap(~rowid, labeller = 'label_both') +
#   ggthemes::scale_color_colorblind() +
#   theme_bw()

###################
# PCA and Cluster #
###################

observe({
  first_pc_selected <- input$user_pca_x_axis
  
  pc_options <- c("PC1", "PC2", "PC3", "PC4", "PC5", "PC6", "PC7", "PC8", "PC9", "PC10")
  
  second_pc_options <- pc_options[pc_options != first_pc_selected]
  
  updateSelectInput(session,
                    'user_pca_y_axis',
                    choices = second_pc_options,
                    selected = second_pc_options[1])
})

# create the scatter between different PC scores
output$pca_plot_1 <- renderPlot({
  if(input$show_pca_text){
    g <- energy_pca$x %>% as.data.frame() %>% 
      cbind(group_name) %>% 
      tidyr::separate(group_name, c("region", "generation_type"), sep = "/", remove = FALSE) %>% 
      ggplot(mapping = aes(x = .data[[input$user_pca_x_axis]], y = .data[[input$user_pca_y_axis]])) +
      geom_point(mapping = aes_string(color = input$user_pca_viz_op_1), 
                 size = as.numeric(input$user_pca_point_size),
                 alpha = as.numeric(input$user_pca_transparency)) +
      geom_text(mapping = aes_string(label = input$user_pca_viz_op_1),
                check_overlap = TRUE)

  } else {
    g <- energy_pca$x %>% as.data.frame() %>% 
      cbind(group_name) %>% 
      tidyr::separate(group_name, c("region", "generation_type"), sep = "/", remove = FALSE) %>% 
      ggplot(mapping = aes(x = .data[[input$user_pca_x_axis]], y = .data[[input$user_pca_y_axis]])) +
      geom_point(mapping = aes_string(color = input$user_pca_viz_op_1), 
                 size = as.numeric(input$user_pca_point_size),
                 alpha = as.numeric(input$user_pca_transparency)) 
      
  }
  
  g + 
    scale_color_viridis_d(option = "viridis") +
    theme_bw()

})

# create the variable contributions plot
output$pca_var_contrib_plot_1 <- renderPlot({
  g <- (factoextra::get_pca(var_contrib_pca))$contrib %>% as.data.frame() %>%
    tibble::rownames_to_column() %>%
    tibble::as_tibble() %>%
    pivot_longer(!c('rowname')) %>%
    mutate(pc_id = as.numeric(stringr::str_extract(name, "\\d+"))) %>%
    filter(pc_id ==  input$user_pca_var_contr) %>%
    tidyr::separate(rowname, c("year", "week"), sep = "-", remove = FALSE) %>%
    ggplot(mapping = aes(x = as.factor(week), y = as.factor(year))) +
    geom_tile(mapping = aes(fill = value > 100 * (1 / length(var_contrib_pca$center)),
                            group = interaction(pc_id, rowname)),
              color = 'black') +
    scale_fill_manual("Variable actively contributes to PC?",
                      values = c("TRUE" = 'darkred',
                                 "FALSE" = 'grey70')) +
    facet_wrap(~ as.factor(pc_id)) +
    labs(x = "week", y = "year") +
    theme_bw() +
    theme(legend.position = 'top')
  g
})

# create raw data plot
output$pca_raw_data_1 <- renderPlot({
  g <- wide_format_sep %>% 
    tidyr::separate(group_name, c("region", "type"), sep = '/', remove = FALSE) %>% 
    pivot_longer(cols = !c(group_name, region, type), names_to = c("year_week")) %>% 
    ggplot(mapping = aes(x = year_week, y = value)) +
    geom_line() +
    facet_wrap(~ .data[[input$user_pca_raw]], scales = 'free')
  
  g
})

# create the cluster plot
output$pca_cluster_1 <- renderPlot({
  g <- factoextra::fviz_cluster(list(data = wide_format %>% select(-group_name),
                                     cluster = cutree(energy_hclust, k = 3))) +
    theme_bw()
  g
})
  
#################
# HEATMAPS      #
#################
  # create marginal heatmap
  output$marginal_heatmap_1 <- renderPlot({
    # using an if else statement to toggle the marginal correlation plot on and off
    energy_wide_gen_type %>% 
      purrr::keep(is.numeric) %>% 
      correlate(diagonal = 1, quiet = TRUE) %>% 
      stretch() %>% 
      ggplot(mapping = aes(x = x, y = y)) +
      geom_tile(mapping = aes(fill = r),
                color = 'black') +
      coord_equal() +
      scale_fill_gradient2(low = 'red', mid = 'white', high = 'blue',
                           midpoint = 0,
                           limits = c(-1, 1)) +
      scale_color_manual(guide = 'none',
                         values = c("TRUE" = 'white',
                                    "FALSE" = 'black')) +
      labs(x = '', y = '') +
      theme_bw() +
      theme(axis.text.x = element_text(size = 8))
  })
  
  # create conditional heatmap for first categorical variable selection
  output$conditional_heatmap_1 <- renderPlot({
    heatmap1 <- energy_wide_gen_type %>% 
      filter(region == input$user_heatmap_region) %>% 
      select(-datetime) %>% 
      group_by(region) %>% 
      group_modify(~stretch(correlate(., diagonal = 1, quiet = TRUE))) %>% 
      ungroup() %>% 
      ggplot(mapping = aes(x = x, y = y)) +
      geom_tile(mapping = aes(fill = r),
                color =  'black') +
      coord_equal() +
      scale_fill_gradient2(low = 'red', mid = 'white', high = 'blue',
                           midpoint = 0,
                           limits = c(-1, 1)) +
      scale_color_manual(guide = 'none',
                         values = c("TRUE" = 'white',
                                    "FALSE" = 'black')) +
      facet_wrap( ~ region) 
    
    heatmap1
      
  })
  
  # create conditional heatmap for second categorical variable selection
  output$conditional_heatmap_2 <- renderPlot({
    # using an if else statement to select yearly or quarterly view
    if(input$user_heatmap_time == 'yearly'){
      # plot the heatmap and facet on year
      heatmap2 <- energy_wide_gen_type %>% 
        mutate(the_year = lubridate::year(datetime)) %>%
        filter(region == input$user_heatmap_region) %>% 
        select(-datetime) %>% 
        group_by(region, the_year) %>% 
        group_modify(~stretch(correlate(., diagonal = 1, quiet = TRUE))) %>% 
        ungroup() %>% 
        ggplot(mapping = aes(x = x, y = y)) +
        geom_tile(mapping = aes(fill = r),
                  color =  'black') +
        coord_equal() +
        scale_fill_gradient2(low = 'red', mid = 'white', high = 'blue',
                             midpoint = 0,
                             limits = c(-1, 1)) +
        scale_color_manual(guide = 'none',
                           values = c("TRUE" = 'white',
                                      "FALSE" = 'black')) +
        facet_grid(region ~ the_year) 
    } else if(input$user_heatmap_time == 'quarterly'){
      # plot the heatmap and facet on quarter
      heatmap2 <- energy_wide_gen_type %>% 
        mutate(the_quarter = lubridate::quarter(datetime)) %>% 
        filter(region == input$user_heatmap_region) %>% 
        select(-datetime) %>% 
        group_by(region, the_quarter) %>% 
        group_modify(~stretch(correlate(., diagonal = 1, quiet = TRUE))) %>% 
        ungroup() %>% 
        ggplot(mapping = aes(x = x, y = y)) +
        geom_tile(mapping = aes(fill = r),
                  color =  'black') +
        coord_equal() +
        scale_fill_gradient2(low = 'red', mid = 'white', high = 'blue',
                             midpoint = 0,
                             limits = c(-1, 1)) +
        scale_color_manual(guide = 'none',
                           values = c("TRUE" = 'white',
                                      "FALSE" = 'black')) +
        facet_grid(region ~ the_quarter) 
    } else {
      # plot the heatmap and facet on month
      heatmap2 <- energy_wide_gen_type %>% 
        mutate(the_month = lubridate::month(datetime)) %>% 
        filter(region == input$user_heatmap_region) %>% 
        select(-datetime) %>% 
        group_by(region, the_month) %>% 
        group_modify(~stretch(correlate(., diagonal = 1, quiet = TRUE))) %>% 
        ungroup() %>% 
        ggplot(mapping = aes(x = x, y = y)) +
        geom_tile(mapping = aes(fill = r),
                  color =  'black') +
        coord_equal() +
        scale_fill_gradient2(low = 'red', mid = 'white', high = 'blue',
                             midpoint = 0,
                             limits = c(-1, 1)) +
        scale_color_manual(guide = 'none',
                           values = c("TRUE" = 'white',
                                      "FALSE" = 'black')) +
        facet_grid(region ~ the_month) 
      
    }
    heatmap2 +
      labs(x = '', y = '') +
      theme_bw() +
      theme(axis.text.x = element_text(size = 5.5))
  })
  
####################
# PAIRWISE PLOTS   #
####################
  
# marginal pairwise plot
# output$pairwise_plot_1 <- renderPlot({
#   energy_wide_gen_type %>% 
#     purrr::keep(is.numeric) %>% 
#     GGally::ggpairs()
#   
# })
  
  
##################
# 2D Histograms  #
##################
  
# used to observe the selected x or y axis
  observe({
    first_x_axis <- input$user_histo_x

    second_y_axis <- energy_gen_names[energy_gen_names != first_x_axis]

    updateSelectInput(session,
                      'user_histo_y',
                      choices = second_y_axis,
                      selected = second_y_axis[1])
  })
  
# create the marginal 2D histogram

output$two_d_histogram_1 <- renderPlot({
    energy_wide_gen_type %>%
      ggplot(mapping = aes_string(x = as.name(input$user_histo_x), y = as.name(input$user_histo_y))) +
      geom_hex() +
      scale_fill_viridis_c(option = 'viridis') +
      theme_bw()
})  

# create the regional 2D histogram
output$two_d_histogram_2 <- renderPlot({
  energy_wide_gen_type %>%
    filter(region == input$user_histo_region) %>%
    ggplot(mapping = aes_string(x = as.name(input$user_histo_x), y = as.name(input$user_histo_y))) +
    geom_hex() +
    scale_fill_viridis_c(option = 'viridis') +
    theme_bw()
})

# create the regional and time 2D histogram
output$two_d_histogram_3 <- renderPlot({
  if(input$user_histo_time == 'monthly'){
    g <- energy_wide_gen_type %>%
      filter(region == input$user_histo_region) %>%
      mutate(month = lubridate::month(datetime)) %>%
      ggplot(mapping = aes_string(x = as.name(input$user_histo_x), y = as.name(input$user_histo_y))) +
      geom_hex() +
      facet_wrap(~ region + month)

  } else if(input$user_histo_time == 'quarterly') {
    g <- energy_wide_gen_type %>%
      filter(region == input$user_histo_region) %>%
      mutate(quarter = lubridate::quarter(datetime)) %>%
      ggplot(mapping = aes_string(x = as.name(input$user_histo_x), y = as.name(input$user_histo_y))) +
      geom_hex() +
      facet_wrap(~ region + quarter)

  } else if(input$user_histo_time == 'yearly'){
    g <- energy_wide_gen_type %>%
      filter(region == input$user_histo_region) %>%
      mutate(year = lubridate::year(datetime)) %>%
      ggplot(mapping = aes_string(x = as.name(input$user_histo_x), y = as.name(input$user_histo_y))) +
      geom_hex() +
      facet_wrap(~ region + year)

  }
  g +
    scale_fill_viridis_c(option = 'viridis') +
    theme_bw()

})
  
#####################
# TIMES SERIES      #
#####################

  # create hourly time series plot based on user input
  output$time_series_plot_1 <- renderPlot({
    # logic for viewing raw data or proportions
    if(input$show_data_as_proportions){
      energy_pcp_lf_ready %>% 
        filter(name == input$user_gen_type,
               region == input$user_region) %>% 
        ggplot(mapping = aes(x = datetime, y = prop_value)) +
        geom_line() +
        theme_bw() +
        labs(y = 'energy generated (megawatthours)')
    } else {
      energy_long %>% 
        filter(generation_type == input$user_gen_type,
               region == input$user_region) %>% 
        ggplot(mapping = aes(x = datetime, y = value)) +
        geom_line() +
        theme_bw() +
        labs(y = 'energy generated (megawatthours)')
    }

  })

  # create the weekly time series plot based on user input
  output$time_series_plot_2 <- renderPlot({
    # logic for viewing raw data or proportions
    if(input$show_data_as_proportions){
      energy_pcp_week_lf_ready %>% 
        filter(name == input$user_gen_type,
               region == input$user_region) %>% 
        ggplot(mapping = aes(x = datetime, y = prop_value)) +
        geom_line() +
        theme_bw() +
        labs(y = 'energy generated (megawatthours)')
    } else {
      energy_pcp_week_lf_ready %>% 
        filter(name == input$user_gen_type,
               region == input$user_region) %>% 
        ggplot(mapping = aes(x = datetime, y = value)) +
        geom_line() +
        theme_bw() +
        labs(y = 'energy generated (megawatthours)')
    }
    
  })
  
})
