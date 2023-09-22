#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(markdown)

# Define UI for application that draws a histogram
shinyUI(
  navbarPage('US Regional Energy Generation',
             tabPanel('Welcome!',
                      includeMarkdown('welcome_page.md'),
                      tableOutput('energy_dictionary')
                      ),
             # tabPanel('Time Series',
             #          sidebarLayout(
             #            sidebarPanel(
             #              h4("Change Time Series Settings"),
             #              selectInput('user_region',
             #                          'Select a region',
             #                          choices = region_names,
             #                          selected = region_names[1]),
             #              selectInput('user_gen_type',
             #                          'Select an energy generation type',
             #                          choices = energy_gen_names,
             #                          selected = energy_gen_names[1]),
             #              checkboxInput('show_data_as_proportions',
             #                            'View data as proportions?',
             #                            value = FALSE
             #                            )
             #            ),
             #            mainPanel(
             #              tabsetPanel(type = 'pills',
             #                          tabPanel("Hourly",
             #                                   plotOutput("time_series_plot_1"))
             #                          # tabPanel("Weekly",
             #                          #          h3("In Progress"))
             #                          )
             #              
             #              # h3("Energy Generation Over Time for a Specific Region and Energy Type"),
             #              # plotOutput('time_series_plot_1')
             #            )
             #          )
             #          ),
             navbarMenu("Summary Stats",
                        tabPanel('Energy Patterns Over Time',
                                 sidebarLayout(
                                   sidebarPanel(
                                     h4("Change Plot Settings"),
                                     selectInput('user_summary_region',
                                                 'Select a region',
                                                 choices = region_names,
                                                 selected = region_names[1]),
                                     selectInput('user_summary_gen_type',
                                                 'Select an energy generation type',
                                                 choices = energy_gen_names,
                                                 selected = energy_gen_names[1]),
                                     selectInput('user_summary_time_1',
                                                 'Select the first time interval',
                                                 choices = c('hourly', 'daily', 'monthly', 'quarterly', 'yearly'),
                                                 selected = 'weekly'),
                                     selectInput('user_summary_time_2',
                                                 'Select the second time interval',
                                                 choices = c('daily', 'monthly', 'quarterly', 'yearly'), 
                                                 selected = 'quarterly'),
                                     checkboxInput('show_violin_plot',
                                                   'Show violin plot?',
                                                   value = FALSE)
                                   ),
                                   mainPanel(
                                     h3("Energy Patterns Over Time"),
                                     plotOutput('summary_plot_1')
                                   )
                                 )
                        ),
                        tabPanel('Comparing Energy Patterns',
                                 sidebarLayout(
                                   sidebarPanel(
                                     h4("Change Plot Settings"),
                                     selectInput('user_drill_down_region',
                                                 'Select a region',
                                                 choices = region_names,
                                                 selected = region_names[1]),
                                     selectInput('user_drill_down_gen_type',
                                                 'Select an energy generation type',
                                                 choices = energy_gen_names,
                                                 selected = energy_gen_names[1]),
                                     radioButtons('user_drill_down_x_axis',
                                                  'X axis',
                                                  choices = c('hourly', 'daily'),
                                                  selected = 'daily',
                                                  inline = TRUE),
                                     radioButtons('user_drill_down_grouping',
                                                  'Drill down by',
                                                  choices = c('quarter', 'year'),
                                                  selected = 'quarter',
                                                  inline = TRUE)
                                   ),
                                   mainPanel(
                                     h3('Comparing Energy Patterns'),
                                     plotOutput('drill_down_plot_1')
                                   )
                                 ))
                        ),
             navbarMenu("Relationships",
                        tabPanel('Heatmaps',
                                 sidebarLayout(
                                   sidebarPanel(
                                     h4("Change Heatmap Settings"),
                                     selectInput('user_heatmap_region',
                                                 'Select a region',
                                                 choices = region_names,
                                                 selected = region_names[1])
                                   ),
                                   mainPanel(
                                     tabsetPanel(type = 'pills',
                                                 tabPanel("Marginal",
                                                          plotOutput("marginal_heatmap_1")),
                                                 tabPanel("Region",
                                                          plotOutput("conditional_heatmap_1")),
                                                 tabPanel("Region and Time",
                                                          plotOutput("conditional_heatmap_2"),
                                                          selectInput('user_heatmap_time',
                                                                      'Select a time interval',
                                                                      choices = c('monthly', 'quarterly', 'yearly'),
                                                                      selected = 'yearly'))
                                     )
                                   )
                                 )
                        ),
                        tabPanel("2D Histograms",
                                 sidebarLayout(
                                   sidebarPanel(
                                     h4("Change 2D Histogram Settings"),
                                     selectInput('user_histo_x',
                                                 'Select x axis:',
                                                 choices = energy_gen_names,
                                                 selected = energy_gen_names[1]),
                                     selectInput('user_histo_y',
                                                 'Select y axis:',
                                                 choices = energy_gen_names,
                                                 selected = energy_gen_names[1]),
                                     
                                     h4("Drill Down Options"),
                                     selectInput('user_histo_region',
                                                 'Select a region',
                                                 choices = region_names,
                                                 selected = region_names[1]
                                     ),
                                     selectInput('user_histo_time',
                                                 'Select a time inteval',
                                                 choices = c('monthly', 'quarterly', 'yearly'),
                                                 selected = 'yearly'
                                                 )
                                     
                                   ),
                                   mainPanel(
                                     tabsetPanel(type = 'pills',
                                                 tabPanel("Marginal",
                                                          plotOutput('two_d_histogram_1')
                                                          ),
                                                 tabPanel("Region",
                                                          plotOutput("two_d_histogram_2")
                                                          ),
                                                 tabPanel("Region and Time",
                                                          plotOutput('two_d_histogram_3')
                                                          ))
                                   )
                                 ))
                        ),
             navbarMenu("Proportions",
                        tabPanel('Proportions of Energy Generation Types by Region',
                                 sidebarLayout(
                                   sidebarPanel(
                                     h4("Change Boxplot Settings"),
                                     selectInput('user_prop_gen_type',
                                                 'Select an energy generation type',
                                                 choices = energy_gen_names,
                                                 selected = energy_gen_names[1]),
                                     radioButtons('user_prop_time_interval',
                                                  'Drill down by',
                                                  choices = c('quarter', 'year'),
                                                  selected = 'quarter',
                                                  inline = TRUE)
                                   ),
                                   mainPanel(
                                     h3("Proportions of Energy Generation"),
                                     plotOutput('prop_plot_1')
                                   )
                                 ))
                        # tabPanel('Parallel Coordinates Plot',
                        #          sidebarLayout(
                        #            sidebarPanel(
                        #              h4("Change PCP Settings"),
                        #              selectInput('user_pcp_region',
                        #                          'Select a region',
                        #                          choices = region_names,
                        #                          selected = region_names[1]),
                        #              radioButtons('user_pcp_drill_down',
                        #                           'Drill down by',
                        #                           choices = c('none', 'quarter', 'year'),
                        #                           selected = 'none',
                        #                           inline = TRUE),
                        #              # selectInput('user_pcp_gen_type',
                        #              #             'Color by specific energy generation type',
                        #              #             choices = energy_gen_names,
                        #              #             selected = energy_gen_names[1]),
                        #              radioButtons('user_pcp_scaled_prop',
                        #                           'View by the relative scaled proportion or by the proportion?',
                        #                           choices = c('scaled proportion', 'proportion'),
                        #                           selected = 'scaled proportion',
                        #                           inline = TRUE),
                        #              sliderInput("user_pcp_transparency",
                        #                          "Change line transparency?",
                        #                          min = 0,
                        #                          max = 1,
                        #                          value = 1,
                        #                          step = 0.01)
                        #            ),
                        #            mainPanel(
                        #              h3("Parallel Coordinates Plot"),
                        #              plotOutput('pcp_plot_1')
                        #            )
                        #          ))
                        ),

             tabPanel('PCA',
                      sidebarLayout(
                        sidebarPanel(
                          h4("Change Scatter Plot Settings"),
                          selectInput('user_pca_x_axis',
                                      'Select x axis value',
                                      choices = c("PC1", "PC2", "PC3", "PC4", "PC5", "PC6", "PC7", "PC8", "PC9", "PC10"),
                                      selected = "PC1"),
                          selectInput('user_pca_y_axis',
                                      'Select a y axis value',
                                      choices = c("PC1", "PC2", "PC3", "PC4", "PC5", "PC6", "PC7", "PC8", "PC9", "PC10"),
                                      selected = "PC2"),
                          selectInput('user_pca_viz_op_1',
                                       'Color by: ',
                                       choices = c('region', 'generation_type'),
                                       selected = 'region'),
                          checkboxInput('show_pca_text',
                                        'Show text on top of data points?',
                                        value = FALSE),
                          sliderInput("user_pca_point_size",
                                      'Change marker size:',
                                      min = 1,
                                      max = 10,
                                      value = 6,
                                      step = 1),
                          sliderInput("user_pca_transparency",
                                      "Change marker transparency",
                                      min = 0,
                                      max = 1,
                                      value = 1,
                                      step = 0.01)
                          
                        ),
                        mainPanel(
                          tabsetPanel(type = 'pills',
                                      tabPanel("PC Scores Scatter",
                                               plotOutput("pca_plot_1")
                                               ),
                                      tabPanel("Variable Contribution",
                                               sliderInput("user_pca_var_contr",
                                                           "Choose a PC Id",
                                                           min = 1,
                                                           max = 10,
                                                           value = 1,
                                                           step = 1),
                                               plotOutput("pca_var_contrib_plot_1")
                                               ),
                                      tabPanel("Raw Data",
                                               radioButtons("user_pca_raw",
                                                            "Facet by region or type",
                                                            choices = c("region", "type"),
                                                            selected = "region",
                                                            inline = TRUE),
                                               plotOutput("pca_raw_data_1")),
                                      tabPanel("Cluster Plot",
                                               plotOutput("pca_cluster_1"))
                          
                          )
                          


                                      
                                      

                        )
                      )),
             tabPanel("Appendix",
                      includeMarkdown('appendix_page.md'))
             )
)


