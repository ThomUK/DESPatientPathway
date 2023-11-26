#library(tidyverse)
#library(simmer)
#library(simmer.plot)

source("R/run_sim.R")

#set.seed(NULL)
#set.seed(1)

# prepare the config object
model_config <- function(){
  list(
    forecast_length = 30,
    pat_referral_rate = 100,
    pat_backlog_size = 500,
    op_conversion_rate = 0.1,
    op_fup_rate = 0.25,
    op_clinic_length = 30,
    total_beds = 4,
    pre_op_los = 6,
    post_op_los = 36,
    theatre_proc_length = 90
  )
}

# run the simulation
sim <- run_sim(model_config = model_config)


sim_arrivals <- sim |>
  get_mon_arrivals(per_resource = TRUE, ongoing = TRUE) |>
  dplyr::arrange(start_time)

sim_attributes <- sim |>
  get_mon_attributes()

sim_resources <- sim |>
  get_mon_resources()

plot(get_mon_arrivals(sim), metric = "waiting_time")

#timeseries utilisation plot
plot_data <- sim_resources |>
  dplyr::mutate(
    day = floor(time /60/24) + 1
  ) |>
  dplyr::group_by(day, resource) |>
  dplyr::mutate(
    mean_server = mean(server),
    mean_capacity = mean(capacity),
    utilisation = mean_server / mean_capacity
  ) |>
  dplyr::ungroup()
ggplot(plot_data, aes(day, utilisation, colour = resource)) +
  geom_line()

plot(patient, verbose = TRUE) |>
  htmlwidgets::saveWidget("output/patient_pathway.html")

plot(sim_resources, metric = "usage", items = c("server", "queue"), steps = FALSE) +
  scale_x_continuous(name = "days", labels = scales::number_format(scale = 1/60/24))  # format labels to represent days

#plot(sim_resources, metric = "usage", items = c("server", "queue"), steps = TRUE)
plot(sim_resources, metric = "usage", items = "server", steps = TRUE)
plot(sim_resources, metric = "usage", items = "queue", steps = TRUE)
plot(sim_resources, metric = "utilization")

#plot(sim_attributes)

# plot(sim_arrivals, metric = "activity_time")
# plot(sim_arrivals, metric = "waiting_time")
# plot(sim_arrivals, metric = "flow_time")

# time spent in system
# ggplot(sim_arrivals, aes((end_time - start_time)/60/24)) +
#   geom_histogram() +
#   xlab("Time spent in the system (days)") +
#   ylab("Number of patients")
