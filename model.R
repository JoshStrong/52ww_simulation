# name: discrete-time simulation for modelling inpatient waiting list dynamics


rm(list = ls())
library(parallel)
library(dplyr)
library(ggplot2)
library(tidyr)
library(truncdist)
library(timeDate)
source("simulation_function_logic2.R")
######################################################################################################################################################

##############
### INPUTS ###
##############

# Get initialisation date 
init_date <- as.Date("2021-09-27")

# Set end date
end_date = as.Date("2022-05-01")

# Calculate Horizon Length
h <- as.numeric(end_date - init_date)

# Parameters which drive the model
load("example_parameters.RData")
load("example_times_to_dta.RData")


#################
## Specialties ##
#################

# Get a list of the speciaities on the inpatient waiting list
specialties <- c("Urology")
divisions <- c("Surgery")


#######################################################
################### START #############################
#######################################################

type = c('current_rates')


# Run through each specialty
for (specialty_name in specialties){

  ########################
  # Current waiting list #
  ########################
  load("example_wl.Rdata")
  
  ## Notes
  # priority vs priority_definition - if a patient IS NOT planned then these two things are the same, but if a patient is planned then their priority_defintion is planned and then priority is their allocated priority code (P2,P3 or P4)
  # total days wait is days since their clock start (if they are on an open pathway, o/w it is the same as days_wait)
  # days_wait is days since their DTA
  
###################################
######## CURRENT RATES MODEL ######
###################################

if(any(type == 'current_rates')){
  
  ################
  # Times to DTA
  ###############
  # times until dta
  demand_waits <-
    dplyr::filter(times_to_dta, specialty == specialty_name)[c("priority", "days", "prob")]
  
  # Times to DTA
  times_to_dta_spec <-
    dplyr::filter(times_to_dta,
                  specialty == specialty_name,
                  time_period == "current")
  
  ##############
  # Capacity
  ##############
  capacity <-
    rep(
      filter(
        parameters,
        specialty == specialty_name,
        time_period == 'current',
        metric == 'capacity_mean'
      )$value,
      h
    )
  
  capacity_priority_splits <-
    dplyr::filter(
      parameters,
      specialty == specialty_name,
      metric == "capacity_priority_splits",
      time_period == "current",
      priority %in% c("Planned", "Unknown", "Priority 2", "Priority 3", "Priority 4")
    )[c("priority", "value")] %>%
    full_join(data.frame(priority = c("Priority 2","Priority 3", "Priority 4", "Unknown", "Planned"))) %>%
    mutate(value = ifelse(is.na(value), 0, value))#P2,P3,P4
  
  capacity_priority_splits$priority <-
    as.factor(x = as.character(capacity_priority_splits$priority))
  
  ##########
  # DTAs
  ###########
  demand <-
    rep(
      filter(
        parameters,
        specialty == specialty_name,
        time_period == 'current',
        metric == 'demand_mean'
      )$value,
      h
    )
  
  
  demand_priority_splits <-
    dplyr::filter(
      parameters,
      specialty == specialty_name &
        metric == "demand_priority_splits",
      time_period == "current",
      priority %in% c("Unknown", "Planned", "Priority 2", "Priority 3", "Priority 4")
    )[c("priority", "value")]%>%
    full_join(data.frame(priority = c("Priority 2","Priority 3", "Priority 4", "Unknown", "Planned"))) %>%
    mutate(value = ifelse(is.na(value), 0, value))#P2,P3,P4
  
  
  
  #############################
  #### Run the Simulation #####
  #############################
  
  if(dim(wl)[1] != 0){
    res <- data.frame(
      id = 1:dim(wl)[1],
      waits = as.numeric(wl$total_days_wait),
      inpatient_waits = as.numeric(wl$days_wait),
      priority = wl$priority,
      priority_definition = wl$priority_definition,
      stringsAsFactors = FALSE
    )
    
    initial.waiting.list<-dim(res)[1]
    
    
    
    
    n.runs <- 500 # number of runs of the simulation
    warm.up.period <- 0 # warm up period (discarded)
    
    
    cl<-makeCluster(detectCores()-1)
    clusterExport(cl=cl,
                  varlist=c("demand","capacity","capacity_priority_splits","demand_priority_splits","times_to_dta_spec","initial.waiting.list","res"),
                  envir=environment())
    clusterEvalQ(cl, library("dplyr"))
    res<-parLapply(cl,1:n.runs,simfn)
    stopCluster(cl)
    res<-do.call("rbind",res)
    
    #################
    ### OUTPUTS #####
    #################
    
    res.sum<-res %>%
      pivot_longer(cols=-c(day,ref),names_to="metric",values_to="value") %>%
      group_by(day,metric) %>%
      summarise(mean=mean(value, na.rm=T),q025=quantile(value,0.05,na.rm=TRUE),q975=quantile(value,0.95,na.rm=TRUE))
    
    res.sum$date <- as.character(init_date + res.sum$day, format = '%d/%m/%Y')
    res.sum$specialty = specialty_name
    res.sum$division = dplyr::filter(specialty_names, spec_desc == specialty_name)$division
    res.sum$creator  = 'system'
    res.sum$model    = 'current_rates'
    
    
    
    # Plot the results
    plot.res<-res.sum %>%
      dplyr::select(-date) %>%
      ggplot(aes(x=day)) +
      geom_vline(xintercept=warm.up.period,linetype="dashed",colour="darkgrey") +
      geom_ribbon(aes(ymin=q025,ymax=q975),fill="grey") +
      geom_line(aes(y=mean)) +
      facet_wrap(~metric,scales="free") +
      theme(axis.title.x=element_blank(),
            axis.title.y=element_blank())
    
    plot.res
    
    # Calculate clearence times
    get_clear <- function(data, metric_name = "rtt_nwm52_P2"){
      data_filtered <- filter(data, metric == metric_name)
      clear_dates <- rep(NA,3)
      names <- c("q025","mean","q975")
      results <- data.frame(type = metric_name, quantile = NA, date = NA)
      for (d in 1:3){
        #get first non-zero from the back
        data_filtered_type <- data_filtered[,names(data_filtered) %in% c("date",names[d])]
        # Reverse the data and get the location of the first non zero entry
        point <- which(rev(data_filtered_type[,1][[1]]) > 1)[1]
        
        if (is.na(point)){
          date = data_filtered_type[,2][[1]][1]
        }else if (point == 1){
          date = NA
        }else{
          date <- rev(data_filtered_type[,2][[1]])[point]
        }
        results <- results %>% rbind(data.frame(type = metric_name, quantile = names[d], date = date))
      }
      return(results[-1,])
    }
    
    clear_metrics <- c("rtt_nwm4_P2","rtt_nwm13_P3","rtt_nwm52_P2","rtt_nwm52_P3","rtt_nwm52_P4","rtt_nwm52_Unknown","rtt_nwm104_P2","rtt_nwm104_P3","rtt_nwm104_P4","rtt_nwm104_Unknown")
    
    results <- get_clear(data = res.sum, metric_name = clear_metrics[1])
    
    for(metric in clear_metrics[-1]){
      results <- rbind(results, get_clear(data = res.sum, metric_name = metric))
    }
    
    results$specialty = specialty_name
    results$division = dplyr::filter(specialty_names, spec_desc == specialty_name)$division
    results$creator  = 'system'
    results$model    = 'current_rates'
    
    print(paste0("Outputs updated for: ", specialty_name))
    closeAllConnections()
    log_con <- file("inpatient_wl_model.log")
  }
  
}

}
