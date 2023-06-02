# header =======================================================================
header <- shinydashboard::dashboardHeader(title = "IP, IR correction")

# side bar =====================================================================
sidebarMenu <- shinydashboard::sidebarMenu(

  id = "tabs",
  shinydashboard::menuItem(text = "Validation & IRs",
                           tabName = "validationIr"),

  shinydashboard::menuItem(text = " IR forest plots",
                           tabName = "irForestPlots"),

  shinydashboard::menuItem(text = "Correction thresholds",
                           tabName = "correctionThresholds"),

  shiny::conditionalPanel(condition = "input.tabs == 'validationIr'",
                          shinyWidgets::pickerInput(inputId = "method1",
                                                    label = "Method",
                                                    choices = methods,
                                                    selected = methods[1],
                                                    multiple = TRUE),

                          shinyWidgets::pickerInput(inputId = "database1",
                                                    label = "Database",
                                                    choices = databases,
                                                    selected = databases,
                                                    multiple = TRUE),

                          shinyWidgets::pickerInput(inputId = "target1",
                                                    label = "Target",
                                                    choices = targets,
                                                    selected = targets[1],
                                                    multiple = TRUE),

                          shinyWidgets::pickerInput(inputId = "outcome1",
                                                    label = "Outcome",
                                                    choices = outcomes,
                                                    selected = outcomes[10],
                                                    multiple = TRUE)),

  shiny::conditionalPanel(condition = "input.tabs == 'irForestPlots'",

                          shinyWidgets::pickerInput(inputId = "target2",
                                                    label = "Target",
                                                    choices = targets,
                                                    selected = targets[1],
                                                    multiple = TRUE),

                          shinyWidgets::pickerInput(inputId = "outcome2",
                                                    label = "Outcome",
                                                    choices = outcomes,
                                                    selected = outcomes[10],
                                                    multiple = TRUE),
                          shinyWidgets::pickerInput(inputId = "method2",
                                                    label = "Method",
                                                    choices = methods,
                                                    selected = methods,
                                                    multiple = TRUE)
  ),

  shiny::conditionalPanel(condition = "input.tabs == 'correctionThresholds'",
                          shinyWidgets::pickerInput(inputId = "method3",
                                                    label = "Method",
                                                    choices = methods,
                                                    selected = methods[1],
                                                    multiple = FALSE),

                          shinyWidgets::pickerInput(inputId = "database3",
                                                    label = "Database",
                                                    choices = databases,
                                                    selected = databases[1],
                                                    multiple = FALSE),

                          shinyWidgets::pickerInput(inputId = "target3",
                                                    label = "Target",
                                                    choices = targets,
                                                    selected = targets[1],
                                                    multiple = FALSE),

                          shinyWidgets::pickerInput(inputId = "outcome3",
                                                    label = "Outcome",
                                                    choices = outcomes,
                                                    selected = outcomes[10],
                                                    multiple = FALSE),

                          shiny::sliderInput(inputId = "sens",
                                             label = "Sensitivity",
                                             value = 1,
                                             min = 0,
                                             max = 1,
                                             step = 0.0001),
                          shiny::sliderInput(inputId = "spec",
                                             label = "Specificity",
                                             value = 1,
                                             min = 0,
                                             max = 1,
                                             step = 0.0001),
                          shiny::sliderInput(inputId = "ppv",
                                             label = "PPV",
                                             value = 1,
                                             min = 0,
                                             max = 1,
                                             step = 0.0001),
                          shiny::sliderInput(inputId = "npv",
                                             label = "NPV",
                                             value = 1,
                                             min = 0,
                                             max = 1,
                                             step = 0.0001)))
sidebar <- shinydashboard::dashboardSidebar(sidebarMenu)


# body =========================================================================
bodyTabItems <- shinydashboard::tabItems(

  shinydashboard::tabItem(tabName = "validationIr",
                          shinydashboard::box(title = "Validation",
                                              width = NULL,
                                              DT::dataTableOutput(outputId = "validationResults")),
                          shinydashboard::box(title = "IP, IR results",
                                              width = NULL,
                                              DT::dataTableOutput(outputId = "irResults"))
  ),

  shinydashboard::tabItem(tabName = "irForestPlots",
                          shinydashboard::box(title = "IR forest plots",
                                              width = NULL,
                                              shiny::plotOutput(outputId = "irForestPlot")),
                          shinydashboard::box(title = "MA metrics",
                                              width = NULL,
                                              DT::dataTableOutput(outputId = "maMetrics"))
  ),

  shinydashboard::tabItem(tabName = "correctionThresholds",
                          shinydashboard::box(title = "Correction thresholds",
                                              width = NULL,
                                              DT::dataTableOutput(outputId = "ipIrCorrections"))
  )
)
body <- shinydashboard::dashboardBody(bodyTabItems)

# dashboard ====================================================================
shinydashboard::dashboardPage(
  header = header,
  sidebar = sidebar,
  body = body
)




