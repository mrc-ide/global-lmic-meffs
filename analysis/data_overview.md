## Raw Data Help

The raw data from the server is available from analysis/data/raw_data/server_results. This data is available as an orderly repository, with the task outputs in the archive directory. The outputs of the calibration runs are saved as `grid_out.rds` in the `lmic_reports_google` as well as the projections as a csv. 

To accesss these there are some helper functions, e.g. `reports_date` to get the report IDs, which can be used to construct file paths to the rds and csv files as needed. 

## Date

The raw data is from a specific date that orderly was run (see the commit message for what date is currently there). Please then don't hardcode date into any scripts but use a global variable `date_0`. 
