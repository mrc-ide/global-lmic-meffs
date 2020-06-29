# Set global variable date_0
#' @param date ISO date to be used for date_0
set_date <- function(date) {
  date_0 <<- date
}

#' Get lmic reports when using 3 parameters
#'
#' @param date ISO Date of reports
#'
#' @return data.frame of report ids and iso3c countries
#' @import orderly
#' @export
reports_3parameter_day <- function(date = NULL) {

  wd <- file.path(here::here(),"analysis/data/raw_data/server_results/")
  wdold <- getwd()
  setwd(wd)

  db <- orderly::orderly_db("destination")
  if (is.null(date)) {
    date <- as.character(Sys.Date())
  }

  ## First find the id corresponding to the ecdc report with data.  If
  ## there are more than one, it's not totally clear what you want to
  ## do as you might want to take the earliest or the latest.
  ## Probably we want to take *all* and do the join over that, which
  ## is easy enough to do if you replace the '= $1' and replace with
  ## 'IN (%s)' and interpolate 'paste(sprintf('"%s"', id), collapse = ", ")'
  sql <- 'SELECT report_version.id
            FROM report_version
            JOIN parameters
              ON parameters.report_version = report_version.id
           WHERE report_version.report = "ecdc"
             AND parameters.value = $1'
  id <- DBI::dbGetQuery(db, sql, date)$id
  if (length(id) == 0L) {
    stop(sprintf("No 'ecdc' report for '%s'", as.character(date)))
  }

  ## Then find all lmic_reports reports that use files from this ecdc
  ## report.  This is a bit awful and I might add direct link or a
  ## view to make this easier at some point.
  sql <- 'SELECT report_version.id, parameters.value as country
            FROM report_version_artefact
            JOIN file_artefact
              ON file_artefact.artefact = report_version_artefact.id
            JOIN depends
              ON depends.use = file_artefact.id
            JOIN report_version
              ON report_version.id = depends.report_version
            JOIN parameters
              ON parameters.report_version = report_version.id
           WHERE report_version_artefact.report_version IN (%s)
             AND report = "lmic_reports_google_pmcmc_no_decouple"
             AND parameters.name = "iso3c"
           ORDER BY country, report_version.id'
  sql <- sprintf(sql, paste(sprintf('"%s"', id), collapse = ", "))
  reports <- DBI::dbGetQuery(db, sql)

  if (any(duplicated(reports$country))) {
    keep <- tapply(seq_len(nrow(reports)), reports$country, max)
    reports <- reports[keep, ]
    rownames(reports) <- NULL
  }

  reports$date <- as.character(date)
  setwd(wdold)
  return(reports)
}

#' Get lmic reports using 4 parameters
#'
#' @param date ISO Date of reports
#'
#' @return data.frame of report ids and iso3c countries
#' @import orderly
#' @export
reports_4parameter_day <- function(date = NULL) {

  wd <- file.path(here::here(),"analysis/data/raw_data/server_results/")
  wdold <- getwd()
  setwd(wd)

  db <- orderly::orderly_db("destination")
  if (is.null(date)) {
    date <- as.character(Sys.Date())
  }

  ## First find the id corresponding to the ecdc report with data.  If
  ## there are more than one, it's not totally clear what you want to
  ## do as you might want to take the earliest or the latest.
  ## Probably we want to take *all* and do the join over that, which
  ## is easy enough to do if you replace the '= $1' and replace with
  ## 'IN (%s)' and interpolate 'paste(sprintf('"%s"', id), collapse = ", ")'
  sql <- 'SELECT report_version.id
            FROM report_version
            JOIN parameters
              ON parameters.report_version = report_version.id
           WHERE report_version.report = "ecdc"
             AND parameters.value = $1'
  id <- DBI::dbGetQuery(db, sql, date)$id
  if (length(id) == 0L) {
    stop(sprintf("No 'ecdc' report for '%s'", as.character(date)))
  }

  ## Then find all lmic_reports reports that use files from this ecdc
  ## report.  This is a bit awful and I might add direct link or a
  ## view to make this easier at some point.
  sql <- 'SELECT report_version.id, parameters.value as country
            FROM report_version_artefact
            JOIN file_artefact
              ON file_artefact.artefact = report_version_artefact.id
            JOIN depends
              ON depends.use = file_artefact.id
            JOIN report_version
              ON report_version.id = depends.report_version
            JOIN parameters
              ON parameters.report_version = report_version.id
           WHERE report_version_artefact.report_version IN (%s)
             AND report = "lmic_reports_google_pmcmc"
             AND parameters.name = "iso3c"
           ORDER BY country, report_version.id'
  sql <- sprintf(sql, paste(sprintf('"%s"', id), collapse = ", "))
  reports <- DBI::dbGetQuery(db, sql)

  if (any(duplicated(reports$country))) {
    keep <- tapply(seq_len(nrow(reports)), reports$country, max)
    reports <- reports[keep, ]
    rownames(reports) <- NULL
  }

  reports$date <- as.character(date)
  setwd(wdold)
  return(reports)
}

#' @noRd
out_3parameter_list <- function(date) {

  reports <- reports_3parameter_day(date)

  grids <- pbapply::pblapply(seq_along(reports$id), function(x) {

    fs <- file.path(here::here(),
                    "analysis/data/raw_data/server_results/archive/lmic_reports_google",
                    reports$id[x],
                    "grid_out.rds")

    return(readRDS(fs))

  })

  names(grids) <- reports$country
  return(grids)
}

#' @noRd
out_4parameter_list <- function(date) {

  reports <- reports_4parameter_day(date)

  grids <- pbapply::pblapply(seq_along(reports$id), function(x) {

    fs <- file.path(here::here(),
                    "analysis/data/raw_data/server_results/archive/lmic_reports_google_pmcmc",
                    reports$id[x],
                    "grid_out.rds")

    return(readRDS(fs))

  })

  names(grids) <- reports$country
  return(grids)
}

#' @noRd
get_brt_model <- function(date) {

  wd <- file.path(here::here(),"analysis/data/raw_data/server_results/")
  wdold <- getwd()
  setwd(wd)

  db <- orderly::orderly_db("destination")
  if (is.null(date)) {
    date <- as.character(Sys.Date())
  }

  ## First find the id corresponding to the ecdc report with data.  If
  ## there are more than one, it's not totally clear what you want to
  ## do as you might want to take the earliest or the latest.
  ## Probably we want to take *all* and do the join over that, which
  ## is easy enough to do if you replace the '= $1' and replace with
  ## 'IN (%s)' and interpolate 'paste(sprintf('"%s"', id), collapse = ", ")'
  sql <- 'SELECT report_version.id
            FROM report_version
            JOIN parameters
              ON parameters.report_version = report_version.id
           WHERE report_version.report = "ecdc"
             AND parameters.value = $1'
  id <- DBI::dbGetQuery(db, sql, date)$id
  if (length(id) == 0L) {
    stop(sprintf("No 'ecdc' report for '%s'", as.character(date)))
  }


  sql <- 'SELECT report_version.id
            FROM report_version
            JOIN parameters
              ON parameters.report_version = report_version.id
           WHERE report_version.report = "brt_google_mobility"
             AND parameters.value = $1'
  sql <- sprintf(sql, paste(sprintf('"%s"', id), collapse = ", "))
  reports <- DBI::dbGetQuery(db, sql, date)
  brt_id_max <- max(reports$id)

  # copy brt
  src <- file.path(wd, "archive", "brt_google_mobility", brt_id_max, "google_brt_model.rds")
  brt <- readRDS(src)
  return(brt)

  }


#' @noRd
get_brt_predictions <- function(date) {

  wd <- file.path(here::here(),"analysis/data/raw_data/server_results/")
  wdold <- getwd()
  setwd(wd)

  db <- orderly::orderly_db("destination")
  if (is.null(date)) {
    date <- as.character(Sys.Date())
  }

  ## First find the id corresponding to the ecdc report with data.  If
  ## there are more than one, it's not totally clear what you want to
  ## do as you might want to take the earliest or the latest.
  ## Probably we want to take *all* and do the join over that, which
  ## is easy enough to do if you replace the '= $1' and replace with
  ## 'IN (%s)' and interpolate 'paste(sprintf('"%s"', id), collapse = ", ")'
  sql <- 'SELECT report_version.id
            FROM report_version
            JOIN parameters
              ON parameters.report_version = report_version.id
           WHERE report_version.report = "ecdc"
             AND parameters.value = $1'
  id <- DBI::dbGetQuery(db, sql, date)$id
  if (length(id) == 0L) {
    stop(sprintf("No 'ecdc' report for '%s'", as.character(date)))
  }


  sql <- 'SELECT report_version.id
            FROM report_version
            JOIN parameters
              ON parameters.report_version = report_version.id
           WHERE report_version.report = "brt_google_mobility"
             AND parameters.value = $1'
  sql <- sprintf(sql, paste(sprintf('"%s"', id), collapse = ", "))
  reports <- DBI::dbGetQuery(db, sql, date)
  brt_id_max <- max(reports$id)

  # copy brt
  src <- file.path(wd, "archive", "brt_google_mobility", brt_id_max, "google_brt.rds")
  brt <- readRDS(src)
  return(brt)

}


#' @noRd
get_brt_world_bank_classification <- function(date) {

  wd <- file.path(here::here(),"analysis/data/raw_data/server_results/")
  wdold <- getwd()
  setwd(wd)

  db <- orderly::orderly_db("destination")
  if (is.null(date)) {
    date <- as.character(Sys.Date())
  }

  ## First find the id corresponding to the ecdc report with data.  If
  ## there are more than one, it's not totally clear what you want to
  ## do as you might want to take the earliest or the latest.
  ## Probably we want to take *all* and do the join over that, which
  ## is easy enough to do if you replace the '= $1' and replace with
  ## 'IN (%s)' and interpolate 'paste(sprintf('"%s"', id), collapse = ", ")'
  sql <- 'SELECT report_version.id
            FROM report_version
            JOIN parameters
              ON parameters.report_version = report_version.id
           WHERE report_version.report = "ecdc"
             AND parameters.value = $1'
  id <- DBI::dbGetQuery(db, sql, date)$id
  if (length(id) == 0L) {
    stop(sprintf("No 'ecdc' report for '%s'", as.character(date)))
  }


  sql <- 'SELECT report_version.id
            FROM report_version
            JOIN parameters
              ON parameters.report_version = report_version.id
           WHERE report_version.report = "brt_google_mobility"
             AND parameters.value = $1'
  sql <- sprintf(sql, paste(sprintf('"%s"', id), collapse = ", "))
  reports <- DBI::dbGetQuery(db, sql, date)
  brt_id_max <- max(reports$id)

  # copy brt
  src <- file.path(wd, "archive", "brt_google_mobility", brt_id_max, "World_Bank_Country_Metadata.csv")
  brt <- read.csv(src)
  return(brt)

}
