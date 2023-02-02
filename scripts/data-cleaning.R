library("tidyverse")
theme_set(theme_bw())
library("here")
library("lubridate")


# Open Loop Experiments -------------------------------------------------------

# Names of raw data files
file_names_open <- 
  here("data") |> 
  list.files() |> 
  str_subset("Both.csv")

# Reorder 10th experiment
file_names_open[11] <- file_names_open[2]
file_names_open <- file_names_open[-2]

# Import raw data
dat_raw_open <-
  file_names_open |> 
  map(\(x) {
    read_csv(here("data", x), skip = 1)
  })

# Step Change Experiments -------------------------------------------------

# Get file names
file_names_step <-
  here("data") |> 
  list.files() |> 
  str_subset("Dataset")

# Order file names
file_names_step[17:23] <- file_names_step[2:8]
file_names_step <- file_names_step[-c(2:8)]

# Import raw data
dat_raw_step <-
  file_names_step |> 
  map(\(x) {
    read_csv(here("data", x))
  })


# Clean -----------------------------------------------------------------------

# Format open loop data
clean_data_open <- function(x) {
  x |> 
    transmute(
      date_time = mdy_hms(paste(Date, Time)),
      p_f = `Approx. Feed Pump Power Consumption (kW)`,
      p_r = `Approx. Recirc. Pump Power Consumption (kW)`,
      f_p = `Permeate Flow (L/min)`,
      f_r = `Reject Flow (L/min)`,
      c_p = `Permeate Conductivity (uS/cm)`,
      c_r = `Reject Conductivity (mS.cm)`,
      ph_f = `Feed pH`,
      t_f = `Feed Temperature (C)`)
}

# Format step change data
clean_data_step <- function(x) {
  x |> 
    transmute(
      date_time = mdy_hms(paste(Date, Time)),
      p_f = `Feed Pump Power Draw (kW)`,
      p_r = `Recirculation Pump Power Draw (kW)`,
      f_p = `Permeate Flow (L/min)`,
      f_r = `Reject Flow (L/min)`,
      c_p = `Permeate Conductivity (uS/cm)`,
      c_r = `Reject Conductivity (mS/cm)`,
      t_f = `Feed Temperature (C)`)
}

# Clean data frames
dat_clean_open <- map(dat_raw_open, clean_data_open)
dat_clean_step <- map(dat_raw_step, clean_data_step)


# Save --------------------------------------------------------------------

# Save all clean data as list of data frames
saveRDS(dat_clean_open, file = here("data", "open-loop.rds"))
saveRDS(dat_clean_step, file = here("data", "step-change.rds"))

# Save each open loop experiment as a csv
1:10 |> 
  walk(\(i) {
    write_csv(dat_clean_open[[i]],
              file = here("data", paste0("open-loop-", i, ".csv")))
  })

# Save each step change experiment as a csv
1:16 |> 
  walk(\(i) {
    write_csv(dat_clean_step[[i]],
              file = here("data", paste0("step-change-", i, ".csv")))
  })
 





