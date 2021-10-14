model_diagnostics <- function(map_mod, nChains, max_depth = 10){
  mon <- as.data.frame(monitor(map_mod@stanfit))
  out <- data.frame(diagnostic = "max_Rhat",
                    value = signif(max(mon$Rhat, na.rm = TRUE),2),
                    pass = ifelse(max(mon$Rhat, na.rm = TRUE) < 1.1,
                                  "Y","Should be <1.1"))
  out <- rbind(out, data.frame(diagnostic = "min_bulk_ESS",
                               value = signif(min(mon$Bulk_ESS/nChains),2),
                               pass = ifelse(min(mon$Bulk_ESS/nChains) >= 100,
                                             "Y",
                                             paste0(paste(row.names(mon)[which(mon$Bulk_ESS/nChains < 100)], collapse = ', '),
                                                    " have Bulk ESS < 100/chain"))))
  out <- rbind(out, data.frame(diagnostic = "min_tail_ESS",
                               value = signif(min(mon$Tail_ESS/nChains),2),
                               pass = ifelse(min(mon$Tail_ESS/nChains) >= 100,
                                             "Y",
                                             paste0(paste(row.names(mon)[which(mon$Tail_ESS/nChains < 100)], collapse = ', '),
                                                    " have Tail ESS < 100/chain"))))
  sampler_params <- get_sampler_params(map_mod@stanfit, inc_warmup=FALSE)
  treedepths <- do.call(rbind, sampler_params)[,'treedepth__']
  n_over <- length(treedepths[sapply(treedepths, function(x) x == max_depth)])
  n <- length(treedepths)
  out <- rbind(out, data.frame(diagnostic = "max_treedepth",
                               value = signif(n_over,2),
                               pass = ifelse(n_over == 0,
                                             "Y",
                                             paste(n_over, "of", n, "iterations exceeded max tree depth of", max_depth))))
  out <- rbind(out, data.frame(diagnostic = "min_BFMI",
                               value = signif(min(get_bfmi(map_mod@stanfit)),2),
                               pass = ifelse(min(get_bfmi(map_mod@stanfit)) >= 0.2,
                                             "Y",
                                             "At least one chain had Energy Bayesian Fraction of Missing Information < 0.2")))
  divergent <- do.call(rbind, sampler_params)[,'divergent__']
  n_div = sum(divergent)
  N = length(divergent)
  out <- rbind(out, data.frame(diagnostic = "divergent_transitions",
                               value = signif(n_div,2),
                               pass = ifelse(n_div == 0,
                                             "Y",
                                             paste0(n_div, " divergent transitions (", signif(100*n_div/N,2), "%)"))))
  out
}
