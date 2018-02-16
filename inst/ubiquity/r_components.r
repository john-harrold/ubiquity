<COMMENTS>

system_fetch_cfg = function(){
#
# This function stores all of the information about the system including
# parameter values, system indices used, iniital condition assignments, etc.
#

# System parameter information
<FETCH_SYS_PARAMS>

# Static secondary parameters
<FETCH_SYS_SSP>

# Dynamic secondary parameters
<FETCH_SYS_DSP>

# Indices mapping state, parameter, etc. names
# to their index in the different vectors
<FETCH_SYS_INDICES>

<FETCH_SYS_INDICES_ODE_OUTPUT>

# Parameter Sets
<FETCH_SYS_PSETS>

# Interindiviudal Varability Information
<FETCH_SYS_IIV>

# Variance Equations
<FETCH_SYS_VE>  

<FETCH_SYS_INFUSIONS>

<FETCH_SYS_COVARIATES>


# identifying that the current set is the default
cfg$parameters$current_set = 'default';

# Nonzero initial conditions
<FETCH_SYS_IC>   

# timescale information
<FETCH_SYS_TS>       

# data set 
<FETCH_SYS_DATA>

# bolus inputs
<FETCH_SYS_BOLUS>

# misc options
<FETCH_SYS_MISC>


# titration options
cfg$titration$titrate = FALSE
cfg$titration$times   = c()     # vector of times where titration can occur
cfg$titration$rules   = list()  # titration rules


# By default we will indicate that we're running at the 
# scripting level. This will be altered at the GUI level
cfg$options$misc$operating_environment = 'script'

# defaulting to integrating with r file
cfg$options$simulation_options$integrate_with  = "r-file"

# defaulting to no specified initial condition
cfg$options$simulation_options$initial_conditions = NA   

# If the library has been loaded we switch to C
if(is.null(getLoadedDLLs()$r_ode_model) == FALSE){
  if(getLoadedDLLs()$r_ode_model[["dynamicLookup"]] == TRUE){
    cfg$options$simulation_options$integrate_with  = "c-file"
  }
}


# defaulting the solver_opts                  
cfg$options$simulation_options$parallel       = "No"
cfg$options$simulation_options$compute_cores  = 1
cfg$options$simulation_options$solver_opts    = list()

# Defaulting output times to gui output times
cfg$options$simulation_options$output_times =  eval(parse(text=cfg$options$misc$output_times))

# default stochastic options
cfg$options$stochastic$nsub             = 100
cfg$options$stochastic$seed             = 8675309
cfg$options$stochastic$ci               = 95
cfg$options$stochastic$ponly            = FALSE
cfg$options$stochastic$sub_file         = NULL
cfg$options$stochastic$sub_file_sample  = 'with replacement'

# default logging options
cfg$options$logging$enabled   = 'yes'
cfg$options$logging$file      = sprintf('transient%subiquity_log.txt', .Platform$file.sep)
cfg$options$logging$timestamp = 'yes'
cfg$options$logging$ts_str    = "%Y-%m-%d %H:%M:%S"

# defaulting to verbose output
cfg$options$verbose           = 'yes'

# default estimation options
cfg$estimation$options$observation_function   = 'system_od_general'
cfg$estimation$options$optimizer              = "optim"       
cfg$estimation$options$method                 = "Nelder-Mead"
cfg$estimation$options$control                = list(trace=TRUE, 
                                                     REPORT=10)

#--------------------------------------------------------------------
# default reporting options
# 
# Set sub_title fields to NULL if they do not exist in the template
#
# this is the information the title slide
cfg$reporting$meta$title$layout$general                   = "Title Slide"
cfg$reporting$meta$title$master$general                   = "Office Theme"             
cfg$reporting$meta$title$type$title                       = 'ctrTitle'
cfg$reporting$meta$title$type$sub_title                   = 'subTitle'
cfg$reporting$meta$title$indices$title                    = NULL
cfg$reporting$meta$title$indices$sub_title                = NULL

# this is the information the title slide
cfg$reporting$meta$section$layout$general                 = "Section Header"
cfg$reporting$meta$section$master$general                 = "Office Theme"             
cfg$reporting$meta$section$type$title                     = 'ctrTitle'
cfg$reporting$meta$section$type$sub_title                 = 'subTitle'
cfg$reporting$meta$section$indices$title                  = NULL
cfg$reporting$meta$section$indices$sub_title              = NULL

# these contain the mapping information for content in the template
# units = inches, height = 5.09, width = 9.45
cfg$reporting$meta$content$indices$list_body              = 2
cfg$reporting$meta$content$indices$content_body           = 1 
cfg$reporting$meta$content$indices$list_sub_title         = 4 
cfg$reporting$meta$content$indices$content_sub_title      = 4 
cfg$reporting$meta$content$layout$list                    = "Title and Content List"
cfg$reporting$meta$content$master$list                    = "Office Theme"             
cfg$reporting$meta$content$layout$general                 = "Title and Content Text"
cfg$reporting$meta$content$master$general                 = "Office Theme"             

# Two column slide options
# Each place holder has dimensions of:
# units = inches, height = 5.08, width = 4.65
# No headers with text
cfg$reporting$meta$two_col$indices$text_sub_title         = 4 
cfg$reporting$meta$two_col$indices$text_left              = 5 
cfg$reporting$meta$two_col$indices$text_right             = 3 
cfg$reporting$meta$two_col$layout$text                    = "Two Content Text"
cfg$reporting$meta$two_col$master$text                    = "Office Theme"             

# No headers with lists
cfg$reporting$meta$two_col$indices$list_sub_title         = 1 
cfg$reporting$meta$two_col$indices$list_left              = 5 
cfg$reporting$meta$two_col$indices$list_right             = 4 
cfg$reporting$meta$two_col$layout$list                    = "Two Content List"
cfg$reporting$meta$two_col$master$list                    = "Office Theme"             

# Headers with text
# Each place holder has dimensions of:
# units = inches, height = 4.41, width = 4.65
cfg$reporting$meta$two_col$indices$text_head_sub_title    = 3
cfg$reporting$meta$two_col$indices$text_head_left_title   = 4 
cfg$reporting$meta$two_col$indices$text_head_left         = 6 
cfg$reporting$meta$two_col$indices$text_head_right_title  = 7 
cfg$reporting$meta$two_col$indices$text_head_right        = 2 
cfg$reporting$meta$two_col$layout$text_head               = "Two Content Header Text"
cfg$reporting$meta$two_col$master$text_head               = "Office Theme"             

# Headers with text
cfg$reporting$meta$two_col$indices$list_head_sub_title    = 6
cfg$reporting$meta$two_col$indices$list_head_left_title   = 2 
cfg$reporting$meta$two_col$indices$list_head_left         = 3 
cfg$reporting$meta$two_col$indices$list_head_right_title  = 5 
cfg$reporting$meta$two_col$indices$list_head_right        = 1 
cfg$reporting$meta$two_col$layout$list_head               = "Two Content Header List"
cfg$reporting$meta$two_col$master$list_head               = "Office Theme"             

#--------------------------------------------------------------------

#cfg$estimation$effort                         = 1
#cfg$estimation$optimizer                      = 'fminsearch'
#cfg$estimation$monitor$status_function        = 'estimation_status'
#cfg$estimation$monitor$exit_when_stable       = 'no'
#cfg$estimation$monitor$iteration_history      = 100
#cfg$estimation$monitor$slope_tolerance        = 0.001


cfg = system_select_set(cfg, "default")


return(cfg);
}

system_prepare_inputs = function(SIMINT_cfg, SIMINT_p, SIMINT_force_times){
# System parameters
<SYSTEM_PARAM>

for(SIMINT_cov_name in names(SIMINT_cfg$options$inputs$covariates)){
# Looping through each covariate and creating a variable in the current
# function with the covariate name 

  # plucking out the covariate
  SIMINT_my_cov = SIMINT_cfg$options$inputs$covariates[[SIMINT_cov_name]]

  # This is an initialization function, and these should only use covariates
  # that are constant (like gender or race), so we just use the first value
  SIMINT_cov_value = SIMINT_cfg$options$inputs$covariates[[SIMINT_cov_name]]$values$values[1]
  
  # creating the named value for the covariate
  # at the current time
  eval(parse(text=paste(sprintf("%s = SIMINT_cov_value",SIMINT_cov_name))))
}

# Static secondary parameters
<SS_PARAM>


# Making sure the SIMINT_force_times has some value
if(is.null(SIMINT_force_times)){
  SIMINT_force_times = SIMINT_cfg$options$simulation_options$output_times[1]
}

# If the first compartment has dosing defined then we add the zeros to the
# vector where force_times are not already present in the bolus dosing times.
# If the first compartment does not have bolus defined then we created
# the data structure and add doses of 0 to at the force_times 

if(is.null(SIMINT_cfg$options$inputs$bolus)){ 
  # if there is no bolus information specified we add a dummy bolus of zero
  # into the first compartment at the first sample time
  SIMINT_var    = rep(x=names(cfg$options$mi$states)[1], times=length(SIMINT_force_times))
  SIMINT_time   = SIMINT_force_times
  SIMINT_value  = rep(x=0,     times=length(SIMINT_force_times))
  SIMINT_method = rep(x='add', times=length(SIMINT_force_times))
  }
else{
  # If there are bolus values specified then we add all of 
  # those to the events list
  SIMINT_var    = c()
  SIMINT_time   = c()
  SIMINT_value  = c()
  SIMINT_method = c()

  # turning the time scale from a string
  # into a numeric value:
  SIMINT_time_scale = eval(parse(text=SIMINT_cfg$options$inputs$bolus$times$scale))
  for(SIMINT_name in names(SIMINT_cfg$options$inputs$bolus$species)){
    SIMINT_dose_scale = eval(parse(text=SIMINT_cfg$options$inputs$bolus$species[[SIMINT_name]]$scale))
    SIMINT_var    = c(SIMINT_var,     rep(SIMINT_name,length(SIMINT_cfg$options$inputs$bolus$times$values)))
    SIMINT_method = c(SIMINT_method,  rep('add',length(SIMINT_cfg$options$inputs$bolus$times$values)))
    SIMINT_time   = c(SIMINT_time,             SIMINT_cfg$options$inputs$bolus$times$values*SIMINT_time_scale)
    SIMINT_value  = c(SIMINT_value,            SIMINT_cfg$options$inputs$bolus$species[[SIMINT_name]]$values*SIMINT_dose_scale)
  }
  # pulling out the times in force_times that were not in the bolus_times
  # Then we add in empty boluses there to force them to update
  SIMINT_force_times_add = setdiff(SIMINT_force_times,  unique(SIMINT_time))
  if(length(SIMINT_force_times_add) > 0){
    SIMINT_var    = c(SIMINT_var,     rep(SIMINT_name,length(SIMINT_force_times_add)))
    SIMINT_method = c(SIMINT_method,  rep('add',length(SIMINT_force_times_add)))
    SIMINT_time   = c(SIMINT_time,             SIMINT_force_times_add)
    SIMINT_value  = c(SIMINT_value,   rep(0, length(SIMINT_force_times_add)))
     
  
    }

  }


 SIMINT_events = data.frame(
    var    = SIMINT_var, 
    time   = SIMINT_time,
    value  = SIMINT_value,
    method = SIMINT_method)

 #SIMINT_events = SIMINT_events[order(SIMINT_events$time),]


return(SIMINT_events)
}

system_IC = function(SIMINT_cfg, SIMINT_p){
#
# Returns initial condition information based on information stored in the cfg
# variable and an parameter vector. 
#
# Example usage:
#  cfg = system_fetch_cfg()
#  cfg = system_select_set(cfg, 'default')
#  parameters = cfg$parameters$values
#  IC = system_IC(cfg, parameters)
#

# System parameters
<SYSTEM_PARAM>


for(SIMINT_cov_name in names(SIMINT_cfg$options$inputs$covariates)){
# Looping through each covariate and creating a variable in the current
# function with the covariate name 

  # plucking out the covariate
  SIMINT_my_cov = SIMINT_cfg$options$inputs$covariates[[SIMINT_cov_name]]

  # This is an initialization function, and these should only use covariates
  # that are constant (like gender or race), so we just use the first value
  SIMINT_cov_value = SIMINT_cfg$options$inputs$covariates[[SIMINT_cov_name]]$values$values[1]
  
  # creating the named value for the covariate
  # at the current time
  eval(parse(text=paste(sprintf("%s = SIMINT_cov_value",SIMINT_cov_name))))
}

# Static secondary parameters
<SS_PARAM>



#
# Assigning initial conditions
#
# Looping through each state to see if there 
# is an entry in cfg for the initial condition.
# If If there isnt well default to zero, if there
# is an entry we will evaluate that assignment:
for (SIMINT_sname in names(SIMINT_cfg$options$mi$states)){
  if(is.null(SIMINT_cfg$options$initial_conditions[[SIMINT_sname]])){
    # Here there is no initial condition specified for this state
    SIMINT_tmp_assignment = sprintf('SIMINT_%s_IC = 0.0', SIMINT_sname) }
  else{
    # Here the initial condition has been specified
    SIMINT_tmp_assignment = sprintf('SIMINT_%s_IC = %s', SIMINT_sname, SIMINT_cfg$options$initial_conditions[[SIMINT_sname]]) }
  eval(parse(text=SIMINT_tmp_assignment))
}

# Remapping state ICs into vector form
SIMINT_all_ICs = c(
<STATE_ICS_REMAP>)
return(SIMINT_all_ICs);

}



run_simulation_titrate  <- function(SIMINT_p, SIMINT_cfg){
#
# This runs titration or rule based simulations 
#
# The following are defined locally within the environment
#
# Parameters and states
# SIMINT_p       - vector of system parameters
# SIMINT_cfg     - system configuration sent into the titration routine
# SIMINT_cfgtt   - system configuration at the current titration event time
# SIMINT_ttimes  - vector of titration times (in simulation units)
# SIMINT_tt_ts   - list of time scales for the current titration 
# SIMINT_history - data frame tracking the history of conditions that
#                  evaluated true with the following structure:
#                tname     - name of titration rule
#                value     - value of that rule 
#                simtime   - simulation time when that rule/value were triggered
#                timescale - time at the rule timescale when that rule/value were triggered


  # Zeroing all of the inputs because these are expeted
  # to be handled through titration below
  SIMINT_cfg = system_zero_inputs(SIMINT_cfg)

  SIMINT_som = list()
  #
  # Defining the system parameters locally 
  #
  <SYSTEM_PARAM>

  
  #
  # Defining the covariates
  #
  # This is an initialization function, and these should only use covariates
  # that are constant (like gender or race), so we just use the first value
  <COVARIATES_IC>

  #
  # Defining the secondary parameters locally
  #
  <SS_PARAM>


  #
  # Evaluating all of the input scales
  #
  SIMINT_scales = list()

  # Bolus scales
  if(!is.null(SIMINT_cfg$options$inputs$bolus$times$scale)){
    eval(parse(text=paste(sprintf(" SIMINT_scales$bolus =  %s", SIMINT_cfg$options$inputs$bolus$times$scale))))
  }

  # Infusion rate scales
  for(SIMINT_rname in names(SIMINT_cfg$options$inputs$infusion_rates)){
    SIMINT_scales$infusion_rates[[SIMINT_rname]] = eval(parse(text=paste(sprintf("%s", SIMINT_cfg$options$inputs$infusion_rates[[SIMINT_rname]]$times$scale))))
  }
  
  #
  # Identify all of the titration time points and creating an empty titration
  # history vector. To do this we loop through all of the rules.
  #
  SIMINT_thist_blank = list()
  SIMINT_ttimes = c()
  for(SIMINT_tname in names(SIMINT_cfg$titration$rules)){
    SIMINT_ttimes = c(SIMINT_ttimes, SIMINT_cfg$titration$rules[[SIMINT_tname]]$simtimes)
    SIMINT_thist_blank[[SIMINT_tname]]$value     = -1
    SIMINT_thist_blank[[SIMINT_tname]]$simtime   = -1
    SIMINT_thist_blank[[SIMINT_tname]]$timescale = -1
  }

  #
  # This holds the entire titration history and is NULL until the first event
  # is encountered
  #
  SIMINT_history = NULL

  # Pulling out any duplicates that may occur
  SIMINT_ttimes = sort(unique(SIMINT_ttimes))

  # Trimming off titration times that are beyond the simulation output times
  SIMINT_ttimes = SIMINT_ttimes[  SIMINT_ttimes < max(SIMINT_cfg$options$simulation_options$output_times)]

  # now we have all of the titration times, next we calculate the initial
  # conditions to start the first simulation
  SIMINT_IC = system_IC(SIMINT_cfg, SIMINT_p)

  # Now we loop through each of the titration time points



  for(SIMINT_ttidx in 1:length(SIMINT_ttimes)){
    # Pulling out the current bolus time
    SIMINT_ttime = SIMINT_ttimes[SIMINT_ttidx]

    #
    # Defining the time information for the current titration time. These are
    # the current titration times for each of the defined timescales in the
    # model
    #
    SIMINT_tt_ts = list()
    for(SIMINT_tsname in names(SIMINT_cfg$options$time_scales)){
      SIMINT_tt_ts[[SIMINT_tsname]] = SIMINT_cfg$options$time_scales[[SIMINT_tsname]]*SIMINT_ttime
    }

    # copying the cfg variable to use within the titration loop
    SIMINT_cfgtt = SIMINT_cfg
    # copying the titratino history variable to use within the titration loop
    SIMINT_thist = SIMINT_thist_blank

    #
    # Defining the state values
    #
    for(SIMINT_sname in names(SIMINT_IC)){
       eval(parse(text=paste(sprintf("%s = SIMINT_IC[[SIMINT_sname]]", SIMINT_sname))))
    }
    
    # JMH Apply titration rules twice
    #  (1) State assignments/resets
    #  (2) Other information

    SIMINT_rule_types = c('state', 'other')
    
    for(SIMINT_rule_type in SIMINT_rule_types){
      # Looping through all of the rules
      for(SIMINT_tname in names(SIMINT_cfgtt$titration$rules)){
        # if any of the rules are active at the current titration time
        # then we process those rules
        SIMINT_tcond = SIMINT_cfgtt$titration$rules[[SIMINT_tname]]$simtimes  == SIMINT_ttime
        if(any(SIMINT_tcond)){
      
          # Collecting the information for the current titration rule being
          # triggered
          SIMINT_ti_times = list()
          SIMINT_ti_times$simtime    = SIMINT_cfgtt$titration$rules[[SIMINT_tname]]$simtimes[SIMINT_tcond]
          SIMINT_ti_times$tstime     = SIMINT_cfgtt$titration$rules[[SIMINT_tname]]$times[SIMINT_tcond]
          SIMINT_ti_times$timescale  = SIMINT_cfgtt$titration$rules[[SIMINT_tname]]$timescale
      
          # now looping through each condition for the current rule to see if
          # there is a match
          for(SIMINT_tcond_name in names(SIMINT_cfgtt$titration$rules[[SIMINT_tname]]$conditions)){
            #Evaluating the boolean expression
            SIMINT_tcond_bool = eval(parse(text=paste(SIMINT_cfgtt$titration$rules[[SIMINT_tname]]$conditions[[SIMINT_tcond_name]]$cond)))

            if(SIMINT_tcond_bool){
               # if the Boolean expression is true then we
               # evaluate the action. To make sure that state actions occur
               # first we look to see if the rule type is state and the state
               # reassignment string is in the initial action. 
               
               if((SIMINT_rule_type == "state" &
                   grepl('SI_TT_STATE[', SIMINT_cfgtt$titration$rules[[SIMINT_tname]]$conditions[[SIMINT_tcond_name]]$action, fixed=TRUE)) |
                  (SIMINT_rule_type == "other" &
                   !grepl('SI_TT_STATE[', SIMINT_cfgtt$titration$rules[[SIMINT_tname]]$conditions[[SIMINT_tcond_name]]$action, fixed=TRUE))){

                 # Executing the action
                 eval(parse(text=paste(SIMINT_cfgtt$titration$rules[[SIMINT_tname]]$conditions[[SIMINT_tcond_name]]$action_parsed)))
                
                 # Next we store the titration history
                 SIMINT_thist[[SIMINT_tname]]$value     = eval(parse(text=paste(SIMINT_cfgtt$titration$rules[[SIMINT_tname]]$conditions[[SIMINT_tcond_name]]$value_parsed)))
                 SIMINT_thist[[SIMINT_tname]]$simtime   = SIMINT_ti_times$simtime 
                 SIMINT_thist[[SIMINT_tname]]$timescale = SIMINT_ti_times$tstime  
                
                 #
                 # Collecting information for _all_ of the history
                 #
                 if(is.null(SIMINT_history)){
                   SIMINT_history = data.frame(tname     = SIMINT_tname,
                                               value     = SIMINT_cfgtt$titration$rules[[SIMINT_tname]]$conditions[[SIMINT_tcond_name]]$value,
                                               simtime   = SIMINT_ti_times$simtime,
                                               timescale = SIMINT_ti_times$tstime, stringsAsFactors=FALSE)
                    
                   }
                 else{
                   SIMINT_history  = 
                       rbind(SIMINT_history, 
                             c(SIMINT_tname,                                                                       
                               SIMINT_cfgtt$titration$rules[[SIMINT_tname]]$conditions[[SIMINT_tcond_name]]$value, 
                               SIMINT_ti_times$simtime,                                                            
                               SIMINT_ti_times$tstime))
                 
                 
                  }
              }
            }
          }
        }
      }
    }

    #
    # Setting the initial condition
    #
    SIMINT_cfgtt = system_set_option(cfg    = SIMINT_cfgtt, 
                                     group  = "simulation", 
                                     option = "initial_conditions",
                                     value  = SIMINT_IC)
    

  # JMH defining the output times for this interval
  # If we're before the last titration time we keep everything between that
  # time and the current time
  if(SIMINT_ttidx < length(SIMINT_ttimes)){
    SIMINT_ottr_keep_idx = (SIMINT_cfg$options$simulation_options$output_times >= SIMINT_ttimes[SIMINT_ttidx])    &
                           (SIMINT_cfg$options$simulation_options$output_times <= SIMINT_ttimes[SIMINT_ttidx + 1])

    } 
  else {
    SIMINT_ottr_keep_idx = (SIMINT_cfg$options$simulation_options$output_times >= SIMINT_ttimes[SIMINT_ttidx]) 
    }



  # Output times vector subset of the current titration times
  SIMINT_ottr = SIMINT_cfg$options$simulation_options$output_times[SIMINT_ottr_keep_idx]

  # Adding a point just before the last point so that when we add the results
  # together before we will sample smoothly across titration points
  if(SIMINT_ttidx < length(SIMINT_ttimes)){
   SIMINT_pre  = (max(SIMINT_ottr) - 1e-8*(max(SIMINT_ottr) - min(SIMINT_ottr))) }
  
  # Making sure the titration time point is also included
  SIMINT_ottr = c(SIMINT_ottr, SIMINT_ttimes[SIMINT_ttidx])

  SIMINT_ottr = sort(unique(c(SIMINT_ottr, SIMINT_pre)))
  # Setting the output times
  SIMINT_cfgtt = system_set_option(SIMINT_cfgtt, 
                                   group  = "simulation", 
                                   option = "output_times", 
                                   SIMINT_ottr)


  # Simulating the system forward in time
  SIMINT_somtt = run_simulation_ubiquity(SIMINT_p, SIMINT_cfgtt)

  # adding the titration history information 
  # pulling out the number of observations
  SIMINT_nobs = length(SIMINT_somtt$simout$time)

  SIMINT_somtt$titration = data.frame(matrix(, nrow=SIMINT_nobs, ncol=0))
  for(SIMINT_tname in names(SIMINT_thist)){
    # Appending the value, simtime, and timescale to the som output
    SIMINT_somtt$titration[[sprintf('tt.%s.value',    SIMINT_tname)]] = rep(times=SIMINT_nobs,  SIMINT_thist[[SIMINT_tname]]$value)
    SIMINT_somtt$titration[[sprintf('tt.%s.simtime',  SIMINT_tname)]] = rep(times=SIMINT_nobs,  SIMINT_thist[[SIMINT_tname]]$simtime)
    SIMINT_somtt$titration[[sprintf('tt.%s.timescale',SIMINT_tname)]] = rep(times=SIMINT_nobs,  SIMINT_thist[[SIMINT_tname]]$timescale)
    }





  # Updating the initial condition to the last observation of som
  SIMINT_IC = as.numeric(SIMINT_somtt$simout[length(SIMINT_somtt$simout[,1]), names(SIMINT_cfg$options$mi$states)])
  names(SIMINT_IC) = names(SIMINT_cfg$options$mi$states)

  
  # Stripping the last observation off of som
  if(SIMINT_ttidx < length(SIMINT_ttimes)){
    SIMINT_somtt$simout    = SIMINT_somtt$simout[1:(SIMINT_nobs - 1),]
    SIMINT_somtt$titration = SIMINT_somtt$titration[1:(SIMINT_nobs - 1),]
    }
  
  #Appending the simulation results to the master set of results
  if(SIMINT_ttidx == 1){
    SIMINT_som = SIMINT_somtt }
  else{
     SIMINT_som$simout    = rbind(SIMINT_som$simout,    SIMINT_somtt$simout)
     SIMINT_som$titration = rbind(SIMINT_som$titration, SIMINT_somtt$titration)
    }

  }

  SIMINT_som$titration_history = SIMINT_history

# pulls out the last row of simout
# som$simout[length(som$simout[,1]),]

return(SIMINT_som)
}


system_DYDT = function(SIMINT_TIME,SIMINT_x,SIMINT_cfg){
#
# Evalutates the derivatives of the ODEs at time SIMINT_TIME
#

SIMINT_p = SIMINT_cfg$parameters$values

# System parameters
<SYSTEM_PARAM>

for(SIMINT_rate_name in names(SIMINT_cfg$options$inputs$infusion_rates)){
# Looping through each infusion rate and creating a variable in the current
# function with the rate at the value for the current time


  # plucking out the rate name
  SIMINT_my_rate = SIMINT_cfg$options$inputs$infusion_rates[[SIMINT_rate_name]]

  # scaling the times
  eval(parse(text=sprintf('SIMINT_my_rate$times$values = SIMINT_my_rate$times$values*%s',SIMINT_my_rate$times$scale)))

  # getting the covariate value at the given time
  SIMINT_rate_value = system_evaluate_input(SIMINT_my_rate$times$values,
                                            SIMINT_my_rate$levels$values,
                                            SIMINT_TIME, 
                                            'step')
  
  # creating the named value for the covariate
  # at the current time
  eval(parse(text=paste(sprintf("%s = SIMINT_rate_value*%s",SIMINT_rate_name, SIMINT_my_rate$levels$scale))))
}

for(SIMINT_cov_name in names(SIMINT_cfg$options$inputs$covariates)){
# Looping through each covariate and creating a variable in the current
# function with the covariate name at the value for the current time

  # plucking out the covariate
  SIMINT_my_cov = SIMINT_cfg$options$inputs$covariates[[SIMINT_cov_name]]

  # getting the covariate value at the given time
  SIMINT_cov_value = system_evaluate_input(SIMINT_my_cov$times$values,
                                           SIMINT_my_cov$values$values,
                                           SIMINT_TIME, 
                                           SIMINT_my_cov$cv_type)
                                        
  # creating the named value for the covariate
  # at the current time
  eval(parse(text=paste(sprintf("%s = SIMINT_cov_value",SIMINT_cov_name))))
}


# States
<STATES> 

# Static secondary parameters
<SS_PARAM>

# Dynamic secondary parameters
<DS_PARAM>

# ODEs
<ODES>

# ODE
SIMINT_DYDT = c(
<ODES_REMAP>)

#return(SIMINT_DYDT)

list(dy=SIMINT_DYDT,global=c())

}


system_map_output = function(SIMINT_cfg, SIMINT_simout, SIMINT_p,  SIMINT_eventdata){


# Pulling out the time vector
SIMINT_tts     = SIMINT_simout[,'time']

# Creating the matrix to store the simout
SIMINT_all_outputs = c(names(SIMINT_cfg$options$mi$states), names(SIMINT_cfg$options$mi$odes))

SIMINT_simoutmat = matrix(data=NA, nrow=length(SIMINT_tts), ncol=(length(SIMINT_all_outputs) +1) )
colnames(SIMINT_simoutmat) =   eval(parse(text= sprintf("c('time', '%s')", paste(SIMINT_all_outputs, collapse="', '"))))


# System parameters
<SYSTEM_PARAM>



for(SIMINT_cov_name in names(SIMINT_cfg$options$inputs$covariates)){
# Looping through each covariate and creating a variable in the current
# function with the covariate name (evaluated at the first instance)

  # plucking out the covariate
  SIMINT_my_cov = SIMINT_cfg$options$inputs$covariates[[SIMINT_cov_name]]

  # This is an initialization step, and these should only use covariates
  # that are constant (like gender or race), so we just use the first value
  SIMINT_cov_value = SIMINT_cfg$options$inputs$covariates[[SIMINT_cov_name]]$values$values[1]
  
  # creating the named value for the covariate
  # at the current time
  # This is used when calculating secondary parameters:
  eval(parse(text=paste(sprintf("%s = SIMINT_cov_value",SIMINT_cov_name))))

  # This will be stored in the output data frame
  eval(parse(text=paste(sprintf("SIMINT_CVIC_%s = SIMINT_cov_value",SIMINT_cov_name))))
}



# Static secondary parameters
<SS_PARAM>

for (SIMINT_tidx in seq(1,length(SIMINT_tts))){
  SIMINT_TIME = SIMINT_tts[SIMINT_tidx]
  <TIME_SCALES>

  # Creating the states here at a given time
  # (above a vector was created)
  for (SIMINT_sname in names(SIMINT_cfg$options$mi$states)){
    SIMINT_tmp_assignment = sprintf('%s =  SIMINT_simout[[SIMINT_tidx, SIMINT_sname]]', SIMINT_sname)  #JMH modify
    eval(parse(text=SIMINT_tmp_assignment))
  }


  for(SIMINT_rate_name in names(SIMINT_cfg$options$inputs$infusion_rates)){
  # Looping through each infusion rate and creating a variable in the current
  # function with the rate at the value for the current time
  
  
    # plucking out the rate name
    SIMINT_my_rate = SIMINT_cfg$options$inputs$infusion_rates[[SIMINT_rate_name]]
  
    # scaling the times
    eval(parse(text=sprintf('SIMINT_my_rate$times$values = SIMINT_my_rate$times$values*%s',SIMINT_my_rate$times$scale)))
  
    # getting the covariate value at the given time
    SIMINT_rate_value = system_evaluate_input(SIMINT_my_rate$times$values,
                                              SIMINT_my_rate$levels$values,
                                              SIMINT_TIME, 
                                              'step')
    
    # creating the named value for the covariate
    # at the current time
    eval(parse(text=paste(sprintf("%s = SIMINT_rate_value*%s",SIMINT_rate_name, SIMINT_my_rate$levels$scale))))
  }
  
  for(SIMINT_cov_name in names(SIMINT_cfg$options$inputs$covariates)){
  # Looping through each covariate and creating a variable in the current
  # function with the covariate name at the value for the current time
  
    # plucking out the covariate
    SIMINT_my_cov = SIMINT_cfg$options$inputs$covariates[[SIMINT_cov_name]]
  
    # getting the covariate value at the given time
    SIMINT_cov_value = system_evaluate_input(SIMINT_my_cov$times$values,
                                             SIMINT_my_cov$values$values,
                                             SIMINT_TIME, 
                                             SIMINT_my_cov$cv_type)
                                          
    # creating the named value for the covariate
    # at the current time
    eval(parse(text=paste(sprintf("%s = SIMINT_cov_value",SIMINT_cov_name))))
  }

  <DS_PARAM>
  
  # Outputs
  <OUTPUTS>  
  
  # With all of the variables defined we add a row to the simout matrix
  SIMINT_simoutmat[SIMINT_tidx,] =  eval(parse(text=sprintf("c(SIMINT_TIME, %s)", paste(SIMINT_all_outputs , collapse=", ")))) 
}


return(SIMINT_simoutmat) }


system_evaluate_input = function(tvals, lvals, etime, type){
#
# system_evaluate_input --- used to evaluate infusion rates and 
# covariates at etime
#  
#   tvals - time values where time-series is defined
#   lvals - corresponding values where the of the time series 
#   etime - time where the time-series is to be evaluated
#   type  - type of timeseries either: 'linear' or 'step' 
#  
  # initializing the return value 
  value = -1

  if(type == 'step'){
    if(length(tvals) == 1){
    # if there is only one element in tvals 
    # then we just take that value
      value = tail(lvals, 1)
    }
    else if(etime > max(tvals)){
    # the eval time is beyond the range of 
    # specified times then we carry the last
    # one forward
      value = tail(lvals, 1)
    }
    else if(etime < min(tvals)){
    # the eval time is before the range of 
    # specified times then we assign it to 
    # the first value
     value = lvals[1];
    }
    else{
    # this should return the portion of the 
    # lvals vector that is less than
    # the evaluation time (etime): 
    #
    # lvals[tvals <= etime]
    # and tail should pop off the last value
     value = tail(lvals[tvals <= etime], 1)
    
    }
  } 
  else if(type == 'linear'){
    if(length(tvals) == 1){
      # if there is only 1 value then there is no linear interpolation :)
      value = lvals
    }
    else{
    #linearly interpolating, values beyond boundary 
    # will take on the values at the boundary
    linear_interp = approx(tvals, lvals, etime, method="linear", rule=2, , , n=2)
    value = linear_interp$y
    }
  }
  return(value)
}

sample_around = function(tvals, ot){

# removing any duplicates
tvals = unique(tvals)
# calculating the total simulation time 
# and using that as a basis for simulations
tlength = abs(max(ot) - min(ot))
tsample = c()
delta   = 1e-8*tlength
ffollow = 0.10 # percent to follow effects of event
nfollow = 40   # number of sample times
vfollow = seq(0, tlength*ffollow, tlength*ffollow/nfollow)
for(tval in tvals){
  # This samples just before and just after the sample time
  tsample = c(tsample, (tval -delta), (tval + delta), (tval + 50*delta), (tval + 100*delta))

  # now adding ffolow percent of the total time to the end
  tsample = c(tsample, (vfollow + tval + 150*delta))
}

return(tsample)
}


#-------------------------------------------------------------------------

# Looping through each output to add the error
add_observation_errors = function(simout, parameters, cfg){
for(output in names(cfg$ve)){
   simout =  
   output_add_error(SIMINT_simout     = simout,          # simulation output without error
                    SIMINT_output     = output,          # output
                    SIMINT_em         = cfg$ve[[output]],# error model
                    SIMINT_parameters = parameters,      # current parameter values
                    SIMINT_cfg        = cfg) 

}

return(simout)}

#-------------------------------------------------------------------------

output_add_error = function(SIMINT_simout, SIMINT_output, SIMINT_em, SIMINT_parameters, SIMINT_cfg){

#browser()

# Defining the time
SIMINT_TIME = SIMINT_simout[,'time']

# Defining the pred values locally
PRED  = SIMINT_simout[, SIMINT_output]

# Pulling the variance parmeter names
SIMINT_VP_NAMES = as.vector(SIMINT_cfg$parameters$matrix$name[SIMINT_cfg$parameters$matrix$ptype == 'variance'])

# Evaluating the variance parameters locally
for(SIMINT_VP_NAME in SIMINT_VP_NAMES){
  eval(parse(text=sprintf('%s = as.numeric(SIMINT_parameters["%s"])', SIMINT_VP_NAME, SIMINT_VP_NAME)))
}

# calculating the error model value
eval(parse(text=sprintf("SIMINT_em_VARIANCE  =  %s", SIMINT_em)))
SIMINT_em_MEAN =  rep(0,length(PRED));
SIMINT_ERR     = rnorm(PRED, SIMINT_em_MEAN, sqrt(SIMINT_em_VARIANCE))

# adding the error to the prediction and storing it in SIMINT_simout
SIMINT_simout = eval(parse(text=sprintf('cbind(SIMINT_simout, SIOE_%s=c(PRED+SIMINT_ERR))', SIMINT_output)))

return(SIMINT_simout)}

#-------------------------------------------------------------------------

make_forcing_function = function(times, values, type, name, output_times){
#
# Inputs:
#
# times - time values for the forcing function
#
# values - magnitude for each time (same length of time)
#
# type - type of forcing function can be one of the following:
#         "step" for constant values that switch to new values at
#                the times
#         "linear" to linearly interpolate between the points
#
# cfg - System configuration variable generated in the following manner:
#


if("step" == type){
 counter = 1
 while( counter <= length(times)){
  if(counter == 1){
    myforce = matrix(ncol=2,byrow=TRUE,data=c(times[counter], values[counter]))
  } else{
    if(times[counter] == 0){
      delta         = 250*.Machine$double.eps
    } else {
      delta         = 250*.Machine$double.eps*times[counter]
    }
    delta         = 250000*.Machine$double.eps

   ## placing sample points in the constant region
   #if(counter ==2){
   # npts = 10
   # tmp_tstart = myforce[length(myforce[,1]), 1] + 2*delta
   # tmp_tstop  = times[counter] - 2*delta
   # stimes  = seq(tmp_tstart, tmp_tstop, (tmp_tstop - tmp_tstart)/npts)
   # svalues = rep(myforce[length(myforce[,1]), 2], npts)
   # myforce = rbind(myforce, cbind(stimes, svalues))
   #}
    # just before the switching time it takes the previous value
    myforce = (rbind(myforce, c((times[counter]-delta), values[counter-1])))
    # just afterwards it takes on the next value
    myforce = (rbind(myforce, c((times[counter]+delta), values[counter])))
  }
  counter = counter +1
 }

 # if the last switching time occurs before the end of the simulation
 # then we extend the last rate specified to the end of the simulation
 if(tail(myforce[,1], n=1) < tail(output_times, n=1)){
   myforce = (rbind(myforce, c((tail(output_times, n=1)), tail(values, n=1) )))
   }
}else  if("linear" == type){
   myforce = cbind(times, values)
 # if the last switching time occurs before the end of the simulation
 # then we extend the last rate specified to the end of the simulation
 if(tail(myforce[,1], n=1) < tail(output_times, n=1)){
   myforce = (rbind(myforce, c((tail(output_times, n=1)), tail(values, n=1) )))
 }
}

return(myforce);
}
