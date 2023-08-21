plotIrForest2 <- function(dbMaIrSummary,
                          targetName,
                          outcomeName,
                          method,
                          interval) {

  plotData <- dbMaIrSummary[dbMaIrSummary$outcomeName %in% outcomeName &
                             dbMaIrSummary$targetName %in% targetName &
                             dbMaIrSummary$method %in% method, ]

  plotData <- plotData %>%
    dplyr::mutate(
      sourceName = sub("cdm_", "", sourceName),
      sourceName = sub("_v\\d{4}", "", sourceName),
    )

  if (interval == "ci") {
    limits <- c(min(plotData$irLowerCi), max(plotData$irUpperCi))
    plotData <- plotData %>%
      dplyr::mutate(
        irLower = irLowerCi,
        irUpper = irUpperCi,
      )
  }

  if (interval == "prediction") {
    limits <- c(min(plotData$irLowerPredict), max(plotData$irUpperPredict))
    plotData <- plotData %>%
      dplyr::mutate(
        irLower = irLowerPredict,
        irUpper = irUpperPredict,
      )
  }

  plot <- ggplot2::ggplot(
    data = plotData,
    mapping = ggplot2::aes(
      x = ir,
      xmin = irLower,
      xmax = irUpper,
      y = as.factor(sourceName),
      colour = method
    )
  ) +
    ggplot2::geom_point(
      shape = 18,
      size = 2,
      alpha = 0.85,
      position = ggplot2::position_dodge(width=0.5)
    ) +
    ggplot2::geom_errorbarh(
      height = 0,
      size = 0.75,
      alpha = 0.85,
      position = ggplot2::position_dodge(width=0.5)
    ) +
    ggplot2::scale_x_continuous("IR", limits = limits) +
    ggplot2::facet_grid(outcomeName ~ targetName) +
    ggthemes::scale_colour_colorblind()

  return(plot)
}
