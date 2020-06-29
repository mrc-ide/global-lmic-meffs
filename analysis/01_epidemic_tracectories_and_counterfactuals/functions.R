unmit_sim_i <- function(x, i, date_end) {
  run <- squire::run_explicit_SEEIR_model(
    country = x$parameters$country,
    R0 = x$replicate_parameters$R0[i],
    day_return = TRUE,
    replicates = 1,
    dt = 0.1,
    time_period = as.integer(as.Date(date_end) - as.Date(x$replicate_parameters$start_date[i]))
  )
  return(run)
}

# function to run multiple unmitigated simulations based on replicate_parameters
unmit_sim <- function(x, date_end) {
  
  # run the first one
  r <- unmit_sim_i(x, 1, date_end)
  
  # assign to our results
  out <- list()
  out[[1]] <- r
  
  # what is the mix mat
  mat <- squire:::process_contact_matrix_scaled_age(r$parameters$contact_matrix_set[[1]],
                                                    r$parameters$population)
  
  # running and storing the model output for each of the different initial seeding cases
  for(i in 2:nrow(x$replicate_parameters)) {
    
    beta <- squire:::beta_est_explicit(dur_IMild = r$parameters$dur_IMild,
                                       dur_ICase = r$parameters$dur_ICase,
                                       prob_hosp = r$parameters$prob_hosp,
                                       mixing_matrix = mat,
                                       R0 = x$replicate_parameters$R0[i])
    r$model$set_user(beta_set = beta)
    
    time_period <- as.integer(as.Date(date_end) - as.Date(x$replicate_parameters$start_date[i]))
    t <- seq(from = 1, to = time_period/x$parameters$dt)
    t <- round(seq(1/x$parameters$dt, length(t) + (1/x$parameters$dt), by = 1/x$parameters$dt))
    r$output <- r$model$run(t, replicate = 1)
    out[[i]] <- r
  }
  
  # different lengths of sims
  num_rows <- unlist(lapply(out, function(x){nrow(x$output)}))
  max_rows <- max(num_rows)
  seq_max <- seq_len(max_rows)
  
  # build results again
  outarray <- array(NA, dim = c(max_rows, ncol(out[[1]]$output), length(out)))
  
  # assign the names
  colnames(outarray) <- colnames(x$output)
  rownames(outarray) <- rownames(x$output)
  
  # fill it in
  for(i in seq_len(length(out))){
    outarray[tail(seq_max, num_rows[i]), ,i] <- out[[i]]$output[, , 1]
    outarray[, "time", i] <- outarray[, "time", i] - max(outarray[, "time", i], na.rm = TRUE)
  }
  
  r$output <- outarray
  r$parameters$replicates <- length(out)
  return(r)
}
