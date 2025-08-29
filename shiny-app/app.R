library(shiny)
library(bslib)
library(shinycssloaders) # for loading screens

# Globally setting the spinner colour and type (will change)
options(spinner.type = 8, spinner.color = "#6990EE")

ui <- page_sidebar(
  theme = bs_theme(version = 5,
                   bootswatch = "flatly",
                   "navbar-bg" = "#2C3E50"), # may want to change theme

  title = "Simulating the Bivariate Gaussian Distribution Using MCMC Methods",
  # adding information needed to use latex; see ./R/ui-latex.R
  latex_tags,

  sidebar = sidebar(

    selectInput(inputId = "sampler_method_type",
                label = 'What method would you like to see?',
                choices = list("Independence MH" = "indmh",
                               "Random Walk MH" = "rwmh",
                               "Gibbs Sampler" = "gibbs"),
                selected = "indmh"),

    selectInput(inputId = "modify_type",
                label = 'What would you like to modify?',
                choices = list("Simulation Inputs" = "simulation",
                               "Graph Output" = "graph"),
                selected = "simulation"),

    # inputs based on the simulation; see .R/ui-sim_input.R
    sim_inputs,

    conditionalPanel(
      condition = "input.modify_type == 'graph'",

      selectInput(inputId = "graph_version",
                  label = 'Which graph would you like to view and modify?',
                  choices = list("Scatterplot" = "scat",
                                 "Histogram" = "hist",
                                 "Q-Q Plot" = "qqplot"),
                  selected = "scat"),

      # inputs based on the graph types; see ./R/ui-sim_input.R
      sim_graph_inputs,

    ), # End conditionalPanel

  width = 400,
  open = "always"), # End sidebar

  # mainPanel below...

  conditionalPanel(
    condition = "input.sampler_method_type == 'indmh'",

    conditionalPanel(
      condition = "input.graph_version == 'scat'",
      withSpinner(plotOutput("indmh_plot_scat"))
    ), # end of Conditional Panel

    conditionalPanel(
      condition = "input.graph_version == 'hist'",
      withSpinner(plotOutput("indmh_plot_hist"))
    ), # end of Conditional Panel

    conditionalPanel(
      condition = "input.graph_version == 'qqplot'",
      withSpinner(plotOutput("indmh_plot_qqplot"))
    ), # end of Conditional Panel
  ), # end of Conditional Panel (for the independence MH)


  conditionalPanel(
    condition = "input.sampler_method_type == 'rwmh'",

    conditionalPanel(
      condition = "input.graph_version == 'scat'",
      withSpinner(plotOutput("rwmh_plot_scat"))
    ), # end of Conditional Panel

    conditionalPanel(
      condition = "input.graph_version == 'hist'",
      withSpinner(plotOutput("rwmh_plot_hist"))
    ), # end of Conditional Panel

    conditionalPanel(
      condition = "input.graph_version == 'qqplot'",
      withSpinner(plotOutput("rwmh_plot_qqplot"))
    ), # end of Conditional Panel
  ), # end of Conditional Panel (for the random walk MH)

  conditionalPanel(
    condition = "input.sampler_method_type == 'gibbs'",

    conditionalPanel(
      condition = "input.graph_version == 'scat'",
      withSpinner(plotOutput("gibbs_plot_scat"))
    ), # end of Conditional Panel

    conditionalPanel(
      condition = "input.graph_version == 'hist'",
      withSpinner(plotOutput("gibbs_plot_hist"))
    ), # end of Conditional Panel

    conditionalPanel(
      condition = "input.graph_version == 'qqplot'",
      withSpinner(plotOutput("gibbs_plot_qqplot"))
    ), # end of Conditional Panel
  ), # end of Conditional Panel (for the random walk MH)

  # put outputs later...
)

server <- function(input, output, session) {
  source(file.path("server-plots.R"),  local = TRUE)$value
}

shinyApp(ui = ui, server = server)
