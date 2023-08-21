getCorrectedIrIp <- function(row,
                             method,
                             sens,
                             spec,
                             ppv,
                             npv) {

  atRisk <- row$atRisk
  personDays <- row$personDays
  outcomes <- row$outcomes
  ip100k_p <- row$ip100k_p
  ir100k_py <- row$ir100k_py
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
    cIp100k_p <- cOutcomes / atRisk * 100000
    ipDiff <- cIp100k_p - ip100k_p
    ipRel <- cIp100k_p / ip100k_p
    eameIp <- abs(log(ipRel))

    # IR
    if (method == "fpRate") {
      cIr100k_py <- cOutcomes / personDays * 100000 * 365
      cPersonDays <- personDays
    } else {
      cIr100k_py <- cOutcomes / cPersonDays * 100000 * 365
    }
    irDiff <- cIr100k_py - ir100k_py
    irRel <- cIr100k_py / ir100k_py
    eameIr <- abs(log(irRel))

  } else {
    cIp100k_p <- NA
    ipDiff <- NA
    ipRel <- NA
    eameIp <- NA
    cPersonDays <- NA
    cIr100k_py <- NA
    irDiff <- NA
    irRel <- NA
    eameIr <- NA
  }
  row$cOutcomes <- round(cOutcomes, 0)
  row$cIp100k_p <- round(cIp100k_p, 3)
  row$ipDiff <- round(ipDiff, 3)
  row$ipRel <- round(ipRel, 3)
  row$eameIp <- round(eameIp, 3)
  row$cPersonDays <- round(cPersonDays, 0)
  #row$cIr100py <- round(cIr100k_py, 3)
  row$irDiff <- round(irDiff, 3)
  row$irRel <- round(irRel, 3)
  row$eameIr <- round(eameIr, 3)

  return(row)
}
