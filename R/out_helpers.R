#' @noRd
#'
# Defining Additional Rt Extraction Function Here
extract_Rt <- function(out, iso = NULL) {

  if (is.null(iso)) {
  iso <- squire::population$iso3c[match(out$parameters$country, squire::population$country)]
  }

  # create the Rt data frame
  rts <- lapply(seq_len(length(out$replicate_parameters$R0)), function(y) {

    tt <- squire:::intervention_dates_for_odin(dates = out$interventions$date_R0_change,
                                               change = out$interventions$R0_change,
                                               start_date = out$replicate_parameters$start_date[y],
                                               steps_per_day = 1/out$parameters$dt)

    Rt <- squire:::evaluate_Rt_pmcmc(
      R0_change = tt$change,
      date_R0_change = tt$dates,
      R0 = out$replicate_parameters$R0[y],
      pars = list(
        Meff = out$replicate_parameters$Meff[y],
        Meff_pl = out$replicate_parameters$Meff_pl[y],
        Rt_shift = out$replicate_parameters$Rt_shift[y],
        Rt_shift_scale = out$replicate_parameters$Rt_shift_scale[y]
      ),
      Rt_args = out$pmcmc_results$inputs$Rt_args)

    df <- data.frame(
      "Rt" = Rt,
      "date" = tt$dates,
      "iso" = iso,
      rep = y,
      stringsAsFactors = FALSE)

    df$pos <- seq_len(nrow(df))
    return(df)
  } )

  rt <- do.call(rbind, rts)
  rt$date <- as.Date(rt$date)

  return(rt)
}
