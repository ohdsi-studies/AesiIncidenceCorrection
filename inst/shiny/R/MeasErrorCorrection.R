getCorrectedIrIp <- function(row,
                             method,
                             sens,
                             spec,
                             ppv,
                             npv) {

  atRisk <- row$atRisk
  personDays <- row$personDays
  outcomes <- row$outcomes
  ip100p <- row$ip100p
  ir100p <- row$ir100py
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

  if (cOutcomes >= 0) {

    outcomesDiff <- cOutcomes - outcomes
    personDaysDiff <- outcomesDiff * daysPerPerson / 2
    cPersonDays <- personDays + personDaysDiff

    # IP
    cIp100p <- cOutcomes / atRisk * 100
    ipDiff <- cIp100p - ip100p
    ipRel <- cIp100p / ip100p
    eameIp <- abs(log(ipRel))

    # IR
    if (method == "fpRate") {
      cIr100py <- cOutcomes / personDays * 100 * 365
      cPersonDays <- personDays
    } else {
      cIr100py <- cOutcomes / cPersonDays * 100 * 365
    }
    irDiff <- cIr100py - ir100p
    irRel <- cIr100py / ir100p
    eameIr <- abs(log(irRel))

  } else {
    cIp100p <- NA
    ipDiff <- NA
    ipRel <- NA
    eameIp <- NA
    cPersonDays <- NA
    cIr100py <- NA
    irDiff <- NA
    irRel <- NA
    eameIr <- NA
  }
  row$cOutcomes <- round(cOutcomes, 0)
  row$cIp100p <- round(cIp100p, 3)
  row$ipDiff <- round(ipDiff, 3)
  row$ipRel <- round(ipRel, 3)
  row$eameIp <- round(eameIp, 3)
  row$cPersonDays <- round(cPersonDays, 0)
  row$cIr100py <- round(cIr100py, 3)
  row$irDiff <- round(irDiff, 3)
  row$irRel <- round(irRel, 3)
  row$eameIr <- round(eameIr, 3)

  return(row)
}
