library("tidyverse")
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
  here("data", "Set Point Step Change Experiment", "Step Change 1") |> 
  list.files() |> 
  str_subset("Dataset")

# Order file names
file_names_step[16:21] <- file_names_step[2:7]
file_names_step <- file_names_step[-c(2:7)]

# Import raw data
dat_raw_step <-
  file_names_step |> 
  map(\(x) {
    read_csv(here("data", "Set Point Step Change Experiment", "Step Change 1", x))
  })


# Clean -----------------------------------------------------------------------

# Format open loop data
clean_data_open <- function(x, i) {
  x |> 
    transmute(
      sequence = as_factor(i),
      date_time = mdy_hms(paste(Date, Time)),
      hr = `Runtime (hr)`,
      p_f = `Approx. Feed Pump Power Consumption (kW)`,
      p_r = `Approx. Recirc. Pump Power Consumption (kW)`,
      f_p = `Permeate Flow (L/min)`,
      f_r = `Reject Flow (L/min)`,
      c_p = `Permeate Conductivity (uS/cm)`/1000,
      c_r = `Reject Conductivity (mS.cm)`,
      ph_f = `Feed pH`,
      t_f = `Feed Temperature (C)`)
}

# Format step change data
clean_data_step <- function(x, i) {
  x |> 
    transmute(
      sequence = as_factor(i),
      drain_valve = as_factor(`Drain Valve Position`),
      date_time = mdy_hms(paste(Date, Time)),
      hr = `Runtime (hr)`,
      p_f = `Feed Pump Power Draw (kW)`,
      p_r = `Recirculation Pump Power Draw (kW)`,
      f_p = `Permeate Flow (L/min)`,
      f_r = `Reject Flow (L/min)`,
      c_p = `Permeate Conductivity (uS/cm)`/1000,
      c_r = `Reject Conductivity (mS/cm)`,
      t_f = `Feed Temperature (C)`)
}

# Clean data frames
dat_clean_open <- imap_dfr(dat_raw_open, clean_data_open)
dat_clean_step <- imap_dfr(dat_raw_step, clean_data_step)

# Save --------------------------------------------------------------------

# Save all clean data as list of data frames
saveRDS(dat_clean_open, file = here("data", "open-loop.rds"))
saveRDS(dat_clean_step, file = here("data", "step-change-1.rds"))
