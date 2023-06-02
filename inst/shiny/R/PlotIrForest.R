plotIrForest2 <- function(dbMaIrSmmary,
                          targetName,
                          outcomeName,
                          method) {

  plotData <- dbMaIrSmmary[dbMaIrSmmary$outcomeName %in% outcomeName &
                             dbMaIrSmmary$targetName %in% targetName &
                             dbMaIrSmmary$method %in% method, ]

  plotData <- plotData %>%
    dplyr::mutate(
      sourceName = sub("cdm_", "", sourceName),
      sourceName = sub("_v\\d{4}", "", sourceName),
    )

  limits <- c(min(plotData$irLower), max(plotData$irUpper))

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
