getCorrectedIrIp <- function(row,
                             method,
                             sens,
                             spec,
                             ppv,
                             npv) {

  atRisk <- row$atRisk
  personDays <- row$personDays
  outcomes <- row$outcomes
  IP <- row$IP
  IR <- row$IR
  daysPerPerson <- personDays / atRisk

  if (method == "sensSpec") {
    cOutcomes <- (outcomes - (1 - spec) * atRisk) / (sens - (1 - spec))
  }
  if (method == "ppvNpv") {
    cOutcomes <- (outcomes * ppv + (atRisk - outcomes) * (1 - npv))
  }
  if (method == "sensPpv") {
    cOutcomes <- outcomes * ppv / sens
  }
  if (method == "fpRate") {
    fpRate <- outcomes * (1 - spec) / personDays
    cOutcomes <- (outcomes - (fpRate * personDays)) / sens
  }

  if (cOutcomes > 0) {

    outcomesDiff <- cOutcomes - outcomes
    personDaysDiff <- outcomesDiff * daysPerPerson / 2
    cPersonDays <- personDays + personDaysDiff

    # IP
    cIP <- cOutcomes / atRisk * 100000
    IPdiff <- cIP - IP
    IPrel <- cIP / IP
    IPeame <- abs(log(IPrel))

    # IR
    if (method == "fpRate") {
      cIR <- cOutcomes / personDays * 100000 * 365
      cPersonDays <- personDays
    } else {
      cIR <- cOutcomes / cPersonDays * 100000 * 365
    }
    IRdiff <- cIR - IR
    IRrel <- cIR / IR
    IReame <- abs(log(IRrel))

  } else {
    cIP <- NA
    IPdiff <- NA
    IPrel <- NA
    IPeame <- NA
    cPersonDays <- NA
    cIR <- NA
    IRdiff <- NA
    IRrel <- NA
    IReame <- NA
  }
  row$cOutcomes <- round(cOutcomes, 0)
  row$cIP <- round(cIP, 3)
  row$IPdiff <- round(IPdiff, 3)
  row$IPrel <- round(IPrel, 3)
  row$IPeame <- round(IPeame, 3)
  row$cPersonDays <- round(cPersonDays, 0)
  row$cIR <- round(cIR, 3)
  row$IRdiff <- round(IRdiff, 3)
  row$IRrel <- round(IRrel, 3)
  row$IReame <- round(IReame, 3)

  return(row)
}
