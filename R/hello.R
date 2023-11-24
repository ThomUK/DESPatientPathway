library(tidyverse)
library(simmer)
library(simmer.plot)

# Time unit  = minutes
if(exists("env")) env |> reset()
env <- simmer("pathway")

# create the distribution functions

# patient arrivals
patients_per_month <- 400
rate <- patients_per_month * 12 / 365 / 24 / 60
dist_patient_arrival <- function() rexp(1, rate)
#dist_patient_arrival()

# initial backlog of patients
dist_starting_backlog <- at(rep(0,1000)) # 1000 patients backlog
dist_starting_backlog()

dist_pre_op_ward_los <- function() rnorm(1, 60*12, sd = 6)
dist_pre_op_ward_los()

dist_operating_time <- function() rnorm(1, 90, sd = 6)
dist_operating_time()

dist_post_op_ward_los <- function() rnorm(1, 60*36, sd = 6)
dist_post_op_ward_los()

# OP conversion rate. 1 = admitted, 2 = discharged
dist_op_conversion_rate <- function() sample(1:2, 1, FALSE, c(0.10, 0.9))
# dist_op_conversion_rate()

# create some schedules to close resources overnight
op_clinic_schedule <- schedule(
  c(60*8, 60*16),
  c(1, 0),
  period = 60*24
)
theatre_schedule <- schedule(
  c(60*8, 60*16),
  c(1, 0),
  period = 60*24
)

# create the patient pathway branches
branch_admit <- trajectory("admit for treatment") |>
  set_attribute("admitted_for_treatment", 1) |>

  # take a pre-op bed
  set_attribute("moved_to_pre_op_bed", 1) |>
  seize("bed") |>
  timeout(dist_pre_op_ward_los) |>
  release("bed") |>

  # operate
  set_attribute("moved_to_theatre", 1) |>
  seize("theatre") |>
  timeout(dist_operating_time) |>
  release("theatre") |>
  log_("I'm recovering") |>

  # take a recovery ward bed
  set_attribute("moved_to_post_op_bed", 1) |>
  seize("bed") |>
  timeout(dist_post_op_ward_los) |>
  release("bed") |>
  set_attribute("discharged home", 1) |>
  log_("I'm Home!")

branch_no_admit <- trajectory("not admitted for treatment") |>
  set_attribute("admitted_for_treatment", 0)


# create the overall patient pathway
patient <- trajectory("patient pathway") |>
  log_("I've just been referred in") |>
  ## add an intake activity
  seize("OP clinic", 1) |>
  timeout(function() rnorm(1, mean = 30, sd = 6)) |>
  release("OP clinic", 1) |>

  # branch into admission and discharge
  branch(dist_op_conversion_rate, TRUE,
         branch_admit,
         branch_no_admit)

sim <- env |>
  add_resource("OP clinic", op_clinic_schedule, mon = 2) |>
  add_resource("bed", 2, mon = 2) |> # 2 beds
  add_resource("theatre", capacity = theatre_schedule, queue_size = 0, mon = 2) |>
  add_generator("backlog patient", patient, dist_starting_backlog, mon = 2) |>
  add_generator("new patient", patient, dist_patient_arrival, mon = 2)

env |> run(60*24*365*5) # 60 * 24 * 365 = 1 year

sim_arrivals <- sim |>
  get_mon_arrivals(per_resource = TRUE) |>
#  get_mon_arrivals() |>
  arrange(start_time)

sim_attributes <- sim |>
  get_mon_attributes()

sim_resources <- sim |>
  get_mon_resources()


plot(sim_resources, metric = "usage", items = c("server", "queue"), steps = FALSE)
# plot(sim_resources, metric = "usage", items = "server", steps = TRUE)
# plot(sim_resources, metric = "usage", items = "queue", steps = TRUE)
plot(sim_resources, metric = "utilization")

# time spent in system
# ggplot(sim_arrivals, aes((end_time - start_time)/60/24)) +
# #  scale_x_binned(breaks = scales::breaks_pretty(n=10)) +
#   geom_histogram() +
#   xlab("Time spent in the system (days)") +
#   ylab("Number of patients")
