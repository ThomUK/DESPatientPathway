library(tidyverse)
library(simmer)
library(simmer.plot)

source("R/run_sim.R")

#set.seed(NULL)
#set.seed(1)

# run the simulation
sim <- run_sim()


sim_arrivals <- sim |>
  get_mon_arrivals(per_resource = TRUE) |>
#  get_mon_arrivals() |>
  arrange(start_time)

sim_attributes <- sim |>
  get_mon_attributes()

sim_resources <- sim |>
  get_mon_resources()

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
