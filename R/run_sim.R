#' Run the model simulation
#'
#' @param model_config list. A config object containing variables for model adjustment
#'
#' @return simulation
#' @export
#' @noRd
#'
run_sim <- function(model_config) {
  # read the model config to a short variable name
  mc <- model_config()

  #### create the distribution functions ####
  ## continuous distributions ##
  # patient arrivals
  rate <- mc$pat_referral_rate * 12 / 52 # monthly -> annual, then calculate patients per week
  dist_patient_arrival <- function() rexp(1, rate)
  # dist_patient_arrival()

  # initial backlog of patients
  dist_starting_backlog <- at(rep(0, mc$pat_backlog_size))
  # dist_starting_backlog()

  dist_pre_op_ward_los <- function() rexp(1, 1 / (mc$pre_op_los / 7)) # days to weeks
  # dist_pre_op_ward_los()

  dist_post_op_ward_los <- function() rexp(1, 1 / (mc$post_op_los / 7)) # days to weeks
  # dist_post_op_ward_los()

  ## discrete distributions ##
  # OP did not attend (DNA) rate. 0 = attended, 1 = did not attend
  dist_op_dna <- function() sample(0:1, 1, FALSE, c(100 - mc$op_dna_rate, mc$op_dna_rate))
  dist_op_dna()

  # OP outcoming result. 1 = admit to wl, 2 = OP followup, 3 = discharged
  op_disch_rate <- (100 - mc$op_admit_rate - mc$op_fup_rate)

  dist_op_outcome <- function() sample(1:3, 1, FALSE, c(mc$op_admit_rate, mc$op_fup_rate, op_disch_rate))
  # dist_op_outcome()

  # create the patient pathway branches
  branch_op_dna <- trajectory("op did not attend") |>
    # the dna consumes the same clinic resource as an attendance
    log_("OP: DNA") |>
    set_attribute("OP appt DNA, patient rebooked", 1) |>
    timeout(1 / mc$op_clinic_slots) |>
    release("OP Clinic", 1) |>
    rollback("op_clinic") # rollback to tagged resource

  branch_discharge_from_op <- trajectory("discharged from OP appt") |>
    log_("OP outcome: Discharge") |>
    set_attribute("OP outcome: discharged home", 1)

  branch_followup_later <- trajectory("book OP followup") |>
    log_("OP outcome: Follow-up later") |>
    set_attribute("OP outcome: followup booked", 1) |>
    timeout(4) |> # followup in 4 weeks
    rollback("op_clinic") # rollback to tagged resource

  branch_admit <- trajectory("admit for treatment") |>
    log_("OP outcome: Admit") |>
    set_attribute("OP outcome: admit for treatment", 1) |>
    # take a pre-op bed
    seize("Bed") |>
    log_("Pre-op bed") |>
    set_attribute("IP moved to pre-op bed", 1) |>
    timeout(dist_pre_op_ward_los) |>
    release("Bed") |>
    # operate
    seize("Theatre") |>
    log_("Theatre") |>
    set_attribute("IP moved to theatre", 1) |>
    timeout(1 / mc$theatre_slots) |>
    release("Theatre") |>
    # take a recovery ward bed
    seize("Bed") |>
    log_("Post-op bed") |>
    set_attribute("IP moved to post-op bed", 1) |>
    timeout(dist_post_op_ward_los) |>
    release("Bed") |>
    log_("IP discharged") |>
    set_attribute("IP discharged home", 1)


  # create the overall patient pathway
  patient <- trajectory("patient pathway") |>
    #  log_("Referred in") |>
    ## add an intake activity
    seize("OP Clinic", 1, tag = "op_clinic") |>

    # create a branch to model OP DNAs
    branch(
      dist_op_dna,
      continue = FALSE,
      branch_op_dna
    ) |>

    # if no DNA, continue with the OP appointment
    log_("OP: Attended") |>
    set_attribute("OP appt attended", 1) |>
    timeout(1 / mc$op_clinic_slots) |>
    release("OP Clinic", 1) |>

    # branch into admission and discharge
    branch(
      dist_op_outcome,
      continue = FALSE,
      branch_admit,
      branch_followup_later,
      branch_discharge_from_op
    )

  #env |> run(mc$forecast_length)

  # windows machines can only use a single core
  if(Sys.info()[['sysname']] == "Windows"){
    num_cores <- 1
  } else{
    # use one core, or one less than max cores if more than one available
    num_cores <- max(1, parallel::detectCores() - 1)
  }
  print(glue::glue("Using {num_cores} cores"))
  # Need option to make this dynamic
  # Might want to use set.seed for testing purposes
  envs <- parallel::mclapply(1:mc$num_simulations, function(i) {
    print(i)
    sim <- simmer("pathway") |>
      add_resource("OP Clinic", capacity = 1) |>
      add_resource("Bed", mc$total_beds) |>
      add_resource("Theatre", capacity = 1) |>
      add_generator("backlog patient ", patient, dist_starting_backlog, mon = 2) |>
      add_generator("new patient ", patient, dist_patient_arrival, mon = 2) |>
      run(mc$forecast_length) |>
      wrap()
  }, mc.cores = num_cores)

  return(list(
    sim = envs,
    patient = patient
  ))
}
