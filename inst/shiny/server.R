
shiny::shinyServer(function(input, output) {

  getValidationResultsReactive <- shiny::reactive(x = {
    data <- validationTable %>%
      dplyr::filter(.data$source %in% input$database1 &
                      .data$oName %in% input$outcome1)
    return(data)
  })

  output$validationResults <- DT::renderDataTable(expr = {
    data <-  getValidationResultsReactive()
    table <- DT::datatable(data,
                           rownames = FALSE,
                           class = "stripe compact",
                           options = list(autoWidth = FALSE,
                                          scrollX = TRUE))
    return(table)
  })


  getIrResultsReactive <- shiny::reactive(x = {
    data <- irTable %>%
      dplyr::filter(.data$method %in% input$method1 &
                      .data$source %in% input$database1 &
                      .data$tName %in% input$target1 &
                      .data$oName %in% input$outcome1)
    return(data)
  })

  output$irResults <- DT::renderDataTable(expr = {
    data <- getIrResultsReactive()
    table <- DT::datatable(data,
                           rownames = FALSE,
                           class = "stripe compact",
                           options = list(autoWidth = FALSE,
                                          scrollX = TRUE))
    return(table)
  })


  irForestPlotsReactive <- shiny::reactive({
    plot <- plotIrForest2(dbMaIrSummary,
                          targetName = input$target2,
                          outcomeName = input$outcome2,
                          method = input$method2)
    return(plot)
  })

  output$irForestPlot <- shiny::renderPlot({
    return(irForestPlotsReactive())
  }, res = 100)


  maMetricsReactive <- shiny::reactive({
    data <- maMetrics %>%
      dplyr::filter(targetName %in% input$target2 &
                      outcomeName %in% input$outcome2 &
                      method %in% input$method2) %>%
      dplyr::mutate_if(is.numeric, round, digits = 3)
    return(data)
  })


  output$maMetrics <- DT::renderDataTable(expr = {
    data <- maMetricsReactive()
    table <- DT::datatable(data,
                           rownames = FALSE,
                           class = "stripe compact",
                           options = list(autoWidth = FALSE,
                                          scrollX = TRUE))
    return(table)
  })


  getIpIrCorrectionsReactive <- shiny::reactive(x = {
    row <- irTable %>%
      dplyr::filter(.data$method == input$method3 &
                      .data$source == input$database3 &
                      .data$tName == input$target3 &
                      .data$oName == input$outcome3)
    row <- getCorrectedIrIp(row = row,
                            method = input$method3,
                            sens = input$sens,
                            spec = input$spec,
                            ppv = input$ppv,
                            npv = input$npv) %>%
      dplyr::select(-c(method, source, tName, oName))
    return(row)
  })

  output$ipIrCorrections <- DT::renderDataTable(expr = {
    data <- getIpIrCorrectionsReactive()
    table <- DT::datatable(data,
                           rownames = FALSE,
                           class = "stripe compact",
                           options = list(autoWidth = FALSE,
                                          scrollX = TRUE))
    return(table)
  })

})
