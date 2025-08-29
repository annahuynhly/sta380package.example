sim_inputs <- div(
  conditionalPanel(
    condition = "input.modify_type == 'simulation'",

    numericInput(inputId = "example_seed",
                 label = "Insert the seed for simulation.",
                 value = 1),

    numericInput(inputId = "sample_size",
                 label = 'Insert the Monte Carlo sample size',
                 value = 10000),

    textInput(inputId = "mean_vector",
              label = "Insert $\\mu_{1}, \\mu_{2}$ of the target bivariate
                normal distribution.",
              value = "0,0"),

    textInput(inputId = "sd_vector",
              label = "Insert $\\sigma_{1}, \\sigma_{2}$ of the target bivariate
                normal distribution.",
              value = "1,1"),

    sliderInput(inputId = "rho",
                label = "Insert $\\rho$, the correlation coefficient",
                min = 0, max = 1, value = 0.7),
  ), # End conditionalPanel
)

sim_graph_inputs <- div(
  # Conditional panel for modifying the scatterplots
  conditionalPanel(
    condition = "input.graph_version == 'scat'",

    # Explanation for the pch types; see .R/ui-extra.R
    pch_types,

    selectInput(inputId = "scat_pch",
                label = 'Select the point symbol for the scatterplot',
                choices = 0:25,
                selected = 1),

    colourInput(inputId = "scat_col",
                label = 'Input colour for the point symbol',
                value =  "#5b10a7"),

    conditionalPanel(
      condition = "input.scat_pch >= 21 && input.scat_pch <= 25",

      colourInput(inputId = "scat_line_col",
                  label = 'Input colour for the background of the point symbol.',
                  value = "#6699FF"),
    )

  ), # End conditionalPanel

  # Conditional panel for modifying the histograms
  conditionalPanel(
    condition = "input.graph_version == 'hist'",

    colourInput(inputId = "hist_col",
                label = 'Input colour for the histogram.',
                value = "#6699FF"),

    # Explanation for the line types; see .R/ui-extra.R
    lty_types,

    selectInput(inputId = "hist_line_lty",
                label = 'Select the density line type.',
                choices = 0:6,
                selected = 1),

    colourInput(inputId = "hist_line_col",
                label = 'Input the density line colour.',
                value = "#FF6666"),

    sliderInput(inputId = "hist_line_lwd",
                label = "Insert the density line width.",
                min = 0, max = 10, value = 2),

  ), # end of conditionalPanel

  # Conditional panel for modifying the qqplots
  conditionalPanel(
    condition = "input.graph_version == 'qqplot'",

    # Explanation for the pch types; see .R/ui-extra.R
    pch_types,

    selectInput(inputId = "qqplot_pch",
                label = 'Select the point symbol for the scatterplot',
                choices = 0:25,
                selected = 1),

    colourInput(inputId = "qqplot_col",
                label = 'Input colour for the point symbol',
                value = "#3333FF"),

    conditionalPanel(
      condition = "input.qqplot_pch >= 21 && input.qqplot_pch <= 25",

      colourInput(inputId = "qqplot_border_col",
                  label = 'Input colour for the background of the point symbol.',
                  value = "#6699FF"),
    ),

    # Explanation for the line types; see .R/ui-extra.R
    lty_types,

    selectInput(inputId = "qqplot_line_lty",
                label = 'Select the Q-Q line type.',
                choices = 0:6,
                selected = 1),

    colourInput(inputId = "qqplot_line_col",
                label = 'Input the Q-Q line colour.',
                value = "#EE4266"),

    sliderInput(inputId = "qqplot_line_lwd",
                label = "Insert the Q-Q line width.",
                min = 0, max = 10, value = 2),

  ), # end of conditionalPanel
)
