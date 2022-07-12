###########################
### SIMULATION FUNCTION ###
###########################
simfn <-
  function(SEED, planned_patients_by_adm_date = FALSE) {
    set.seed(SEED)
    #initial conditions
    initial.waiting.list <- initial.waiting.list
    demand <- demand
    capacity <- capacity
    
    # Arrival and Service Rates
    rates <- data.frame(arr = demand, srv = capacity)
    
    ID <- initial.waiting.list + 1
    
    # Convert waits to time array
    res$time_arr = 0 - res$waits
    res$time_arr_inp = 0 - res$inpatient_waits
    res = res[c(
      "id",
      "time_arr",
      "time_arr_inp",
      "planned",
      "priority", # Order by this priority
      "priority_definition" # Define by this priority (can capture planned patients)
    )]
    
    # Output Data Frame
    outp <- data.frame(
      day = 0,
      rtt_nwl52_P2 = length(which(
        -res$time_arr <= 364 & res$priority_definition == "Priority 2"
      )),
      rtt_nwm52_P2 = length(which(
        -res$time_arr > 364 & res$priority_definition == "Priority 2"
      )),
      rtt_nwl52_P3 = length(which(
        -res$time_arr <= 364 & res$priority_definition ==  "Priority 3"
      )),
      rtt_nwm52_P3 = length(which(
        -res$time_arr > 364 & res$priority_definition ==  "Priority 3"
      )),
      rtt_nwl52_P4 = length(which(
        -res$time_arr <= 364 & res$priority_definition ==  "Priority 4"
      )),
      rtt_nwm52_P4 = length(which(
        -res$time_arr > 364 & res$priority_definition ==  "Priority 4"
      )),
      rtt_nwl52_Unknown = length(which(
        -res$time_arr <= 364 & res$priority_definition ==  "Unknown"
      )),
      rtt_nwm52_Unknown = length(which(
        -res$time_arr > 364 & res$priority_definition ==  "Unknown"
      )),
      rtt_nwm4_P2 = length(which(
        -res$time_arr > 28 & res$priority_definition == "Priority 2"
      )),
      rtt_nwm13_P3 = length(which(
        -res$time_arr > 91 & res$priority_definition == "Priority 3"
      )), 
      rtt_nwm104_P2 = length(which(
        -res$time_arr > 728 & res$priority_definition == "Priority 2"
      )),      
      rtt_nwm104_P3 = length(which(
        -res$time_arr > 728 & res$priority_definition == "Priority 3"
      )),
      rtt_nwm104_P4 = length(which(
        -res$time_arr > 728 & res$priority_definition == "Priority 4"
      )),
      rtt_nwm104_Unknown = length(which(
        -res$time_arr > 728 & res$priority_definition == "Unknown"
      )),
      wl_Deferred = length(which(res$priority_definition == "Deferred")),
      wl_Planned = length(which(res$priority_definition == "Planned")),
      act_demand = NA,
      act_activity = NA,
      rtt_maxwt = NA,
      rtt_meanwt = NA,
      rtt_medianwt = NA
    )
    
    #step through and simulate each day in considered period
    for (d in (1:nrow(rates))) {
      ################
      ### ARRIVALS ###
      ################
      
      #number of arrivals
      rate.arr <- rates$arr[d]
      #add noise
      d_arr <- rpois(1, lambda = rate.arr)
      # add new clock starts onto the waiting list
      if (d_arr > 0 & round(sum(demand_priority_splits$value), digits = 10) == 1.0) {
        res <- rbind(
          res,
          data.frame(
            id = ID:(ID + d_arr - 1),
            #Simulated time already waiting before converting to inpatient wl
            time_arr = NA ,
            time_arr_inp = d, # patient arrives at time d
            # Simulated priority
            priority_definition = as.character(
              sample(
                demand_priority_splits$priority,
                d_arr,
                replace = TRUE,
                demand_priority_splits$value
              )
            ),
            priority = NA,
            #Planned date
            planned = NA,
            stringsAsFactors = FALSE
          )
        )
        
        # DO the following:
        # 1. Simulate time already waiting before converting to inpatient wl (or if planned, set their planned admission date to be 2 years from now)
        # 2. If planned patient has arrived, set their priority for ordering to be 4 and then previous waiting time to be 0
        for (id in ID:(ID + d_arr - 1)) {
          id_priority = res[res$id == id, ]$priority_definition
          if (id_priority != "Planned") {
            # If NOT planned, set priority for ordering to be their priorty definition
            res[res$id == id, ]$priority <- res[res$id == id, ]$priority_definition
            
            # Now sample the time already waiting before arriving onto the list
            times_to_dta_priority = dplyr::filter(times_to_dta_spec, priority == id_priority)
            if (dim(times_to_dta_priority)[1] == 1){
              res[res$id == id, ]$time_arr = d - sample(
                size = 1,
                x = c(times_to_dta_priority$days,0),
                replace = TRUE,
                prob = c(times_to_dta_priority$prob,0)
              )
            }else{
              res[res$id == id, ]$time_arr = d - sample(
                size = 1,
                x = times_to_dta_priority$days,
                replace = TRUE,
                prob = times_to_dta_priority$prob
              )
            }
          } else{
            # If is planned, set priority for ordering to be priority 4
            res[res$id == id, ]$priority <- "Priority 4"
            
            # Set their time_arr to be d (as they have just arrived)
            res[res$id == id, ]$time_arr = d
            
            # And, if we are modeelling planned patients by planned admission date, set the planned addmision date to be 2 years for now
            if (planned_patients_by_adm_date) {
              res[res$id == id, ]$planned = d + 2 * 365
            }
            
          }
        }
      }
      ID <- ID + d_arr
      
      
      ################
      ## DEPARTURES ##
      ################
      #services
      rate.srv <- rates$srv[d]
      #d_srv <-
      #  min(floor(rate.srv) + rbinom(1, size = 1, prob = (rate.srv - floor(rate.srv))), nrow(res))
      d_srv<-rpois(1,lambda=rate.srv) # Choose this for random element to service capacity
      if (d_srv > 0 & dim(res)[1] > 0 & sum(capacity_priority_splits$value)==1) {
        
        if (planned_patients_by_adm_date) {
          # First deal with any planned patients:
          pri_planned_ls <- res$planned[res$priority_definition == "Planned"]
          names(pri_planned_ls) <-
            (1:nrow(res))[res$priority_definition == "Planned"]
          srv_planned_ls <-
            as.numeric(names(sort(pri_planned_ls[pri_planned_ls <= d], decreasing =
                                    FALSE)[1:d_srv]))
          srv_planned_ls <- srv_planned_ls[!is.na(srv_planned_ls)]
          number_planned_seen <- length(srv_planned_ls)
        } else{
          number_planned_seen = 0
        }
        
        
        if (d_srv > number_planned_seen) {
          ##############
          ## LOGIC RULES - All P2s are seen first and then any remaining slots are given to P3s and P4s prioritised by longest waiters
          #############
          
          # 1. Get waiting list by priority 2, priority 3+4 and unknown (NOTE: do not have planned as a priority)
          pri_unknown_ls <- d - res$time_arr[res$priority == "Unknown"]
          names(pri_unknown_ls) <-
            (1:nrow(res))[res$priority == "Unknown"]
          
          pri_2_ls <- d - res$time_arr[res$priority == "Priority 2"]
          names(pri_2_ls) <- (1:nrow(res))[res$priority == "Priority 2"]
          
          pri_34_ls <- d - res$time_arr[res$priority %in% c("Priority 3","Priority 4")]
          names(pri_34_ls) <- (1:nrow(res))[res$priority %in% c("Priority 3","Priority 4")]
          
          
          if (!planned_patients_by_adm_date) {
            pri_planned_ls <-
              d - res$time_arr_inp[res$priority_definition == "Planned"] # order planned patients by time on the inpatient WL list
            names(pri_planned_ls) <-
              (1:nrow(res))[res$priority_definition == "Planned"]
          }
          
          ##########
          # Get the number you can serve based on those waiting the longest within each priority
          ###########
          
          # P2
          if (d_srv > 0) {
            if (d_srv < length(pri_2_ls)){
              srv_2_ls_tmp <-
                as.numeric(names(sort(pri_2_ls, decreasing = TRUE)[1:d_srv]))
              left_over_p2 = 0
            } else{
              if(length(pri_2_ls) ==0){
                srv_2_ls_tmp = as.numeric()
                left_over_p2 = d_srv
              }else{
                srv_2_ls_tmp <-
                  as.numeric(names(sort(pri_2_ls, decreasing = TRUE)[1:d_srv]))
                left_over_p2 = d_srv - length(pri_2_ls)
              }
            }
          } else{
            srv_2_ls_tmp = as.numeric()
            left_over_p2 = 0
          }
          
          # P3 and p4
          d_srv_p34 <- left_over_p2
          if (d_srv_p34 > 0) {
            if (d_srv_p34 < length(pri_34_ls)) {
              srv_34_ls_tmp <-
                as.numeric(names(sort(pri_34_ls, decreasing = TRUE)[1:d_srv_p34]))
              left_over_p34 = 0
            } else{
              if(length(pri_34_ls) ==0){
                srv_34_ls_tmp = as.numeric()
                left_over_p34 = d_srv_p34
              }else{
                srv_34_ls_tmp <-
                  as.numeric(names(sort(pri_34_ls, decreasing = TRUE)[1:d_srv_p34]))
                left_over_p34 = d_srv_p34 - length(pri_34_ls)
              }
            }
          } else{
            srv_34_ls_tmp = as.numeric()
            left_over_p34 = 0
          }
          
          # Unknown
          d_srv_unknown <-  left_over_p34
          if (length(d_srv_unknown) > 0) {
            if (d_srv_unknown > 0) {
              if (d_srv_unknown < length(pri_unknown_ls)) {
                srv_unknown_ls_tmp <-
                  as.numeric(names(sort(
                    pri_unknown_ls, decreasing = TRUE
                  )[1:d_srv_unknown]))
                left_over_unknown = 0
              } else{
                if(length(pri_unknown_ls)==0){
                  srv_unknown_ls_tmp = as.numeric()
                  left_over_unknown = d_srv_unknown
                }else{
                  srv_unknown_ls_tmp <-
                    as.numeric(names(sort(pri_unknown_ls, decreasing = TRUE)[1:d_srv_unknown]))
                  left_over_unknown = d_srv_unknown - length(pri_unknown_ls)
                }
                
              }
            } else{
              srv_unknown_ls_tmp = as.numeric()
              left_over_unknown = 0
            }
          } else{
            srv_unknown_ls_tmp = as.numeric()
            left_over_unknown = 0
          }
          
          # Now we need to split back out the people we have served by their defined priorty
          srv_all = c(srv_2_ls_tmp, srv_34_ls_tmp, srv_unknown_ls_tmp)
          srv_all_priority = data.frame(index = srv_all, priority = res[srv_all,]$priority_definition)
          
          srv_2_ls <- srv_all_priority$index[which(srv_all_priority$priority == "Priority 2")]
          srv_3_ls <- srv_all_priority$index[which(srv_all_priority$priority == "Priority 3")]
          srv_4_ls <- srv_all_priority$index[which(srv_all_priority$priority == "Priority 4")]
          srv_unknown_ls <- srv_all_priority$index[which(srv_all_priority$priority == "Unknown")]
          srv_planned_ls <- srv_all_priority$index[which(srv_all_priority$priority == "Planned")]
          

          # Calculate waiting time metrics
          if (sum(c(length(srv_2_ls), length(srv_3_ls), length(srv_4_ls), length(srv_unknown_ls))) != 0) {
            rtt_maxwt <-
              max(
                c(
                  d - res$time_arr[srv_2_ls],
                  d - res$time_arr[srv_3_ls],
                  d - res$time_arr[srv_4_ls],
                  d - res$time_arr[srv_unknown_ls]
                ),
                na.rm = T
              )
            
            if (rtt_maxwt == -Inf){
              #stop()
            }
            rtt_meanwt <-
              mean(
                c(
                  d - res$time_arr[srv_2_ls],
                  d - res$time_arr[srv_3_ls],
                  d - res$time_arr[srv_4_ls],
                  d - res$time_arr[srv_unknown_ls]
                ),
                na.rm = T
              )
            rtt_medianwt <-
              median(
                c(
                  d - res$time_arr[srv_2_ls],
                  d - res$time_arr[srv_3_ls],
                  d - res$time_arr[srv_4_ls],
                  d - res$time_arr[srv_unknown_ls]
                ),
                na.rm = T
              )
          } else{
            rtt_maxwt <- NA
            rtt_meanwt <- NA
            rtt_medianwt <- NA
          }
          
          
          # Remove from the waiting list
          removals <-
            c(srv_planned_ls,
              srv_2_ls,
              srv_3_ls,
              srv_4_ls,
              srv_unknown_ls)
          removals <- removals[!is.na(removals)]
          res <- res[-c(removals), ]
        } else{
          # Remove from the waiting list
          removals <- c(srv_planned_ls)
          removals <- removals[!is.na(removals)]
          res <- res[-c(removals), ]
          
          rtt_maxwt <- NA
          rtt_meanwt <- NA
          rtt_medianwt <- NA
        }
        
        
      } else {
        rtt_maxwt <- NA
        rtt_meanwt <- NA
        rtt_medianwt <- NA
      }
      #output measures
      outp <-
        rbind(
          outp,
          data.frame(
            day = d,
            rtt_nwl52_P2 = length(which(
              (d - res$time_arr) <= 364 & res$priority_definition == "Priority 2"
            )),
            rtt_nwm52_P2 = length(which(
              (d - res$time_arr) > 364 & res$priority_definition == "Priority 2"
            )),
            rtt_nwl52_P3 = length(which(
              (d - res$time_arr) <= 364 & res$priority_definition ==  "Priority 3"
            )),
            rtt_nwm52_P3 = length(which(
              (d - res$time_arr) > 364 & res$priority_definition ==  "Priority 3"
            )),
            rtt_nwl52_P4 = length(which(
              (d - res$time_arr) <= 364 & res$priority_definition ==  "Priority 4"
            )),
            rtt_nwm52_P4 = length(which(
              (d - res$time_arr) > 364 & res$priority_definition ==  "Priority 4"
            )),
            rtt_nwl52_Unknown = length(which(
              (d - res$time_arr) <= 364 & res$priority_definition ==  "Unknown"
            )),
            rtt_nwm52_Unknown = length(which(
              (d - res$time_arr) > 364 & res$priority_definition ==  "Unknown"
            )),
            rtt_nwm4_P2 = length(which(
              (d - res$time_arr) > 28 & res$priority_definition == "Priority 2"
            )),
            rtt_nwm13_P3 = length(which(
              (d - res$time_arr) > 91 & res$priority_definition == "Priority 3"
            )),
            rtt_nwm104_P2 = length(which(
              (d - res$time_arr) > 728 & res$priority_definition == "Priority 2"
            )),      
            rtt_nwm104_P3 = length(which(
              (d - res$time_arr) > 728 & res$priority_definition == "Priority 3"
            )),
            rtt_nwm104_P4 = length(which(
              (d - res$time_arr) > 728 & res$priority_definition == "Priority 4"
            )),
            rtt_nwm104_Unknown = length(which(
              (d - res$time_arr) > 728 & res$priority_definition == "Unknown"
            )),
            wl_Deferred = length(which(res$priority_definition == "Deferred")),
            wl_Planned = length(which(res$priority_definition ==  "Planned")),
            act_demand = d_arr,
            act_activity = d_srv,
            rtt_maxwt = rtt_maxwt,
            rtt_meanwt = rtt_meanwt,
            rtt_medianwt = rtt_medianwt
          )
        )
    }
    #compile output measures over each simulation run
    #outp <- outp[-1,]
    outp$wl_size <-
      outp$rtt_nwl52_P2 + outp$rtt_nwm52_P2 + outp$rtt_nwl52_P3 + outp$rtt_nwm52_P3 +
      outp$rtt_nwl52_P4 + outp$rtt_nwm52_P4 + outp$rtt_nwl52_Unknown + outp$rtt_nwm52_Unknown + outp$wl_Planned + outp$wl_Deferred
    outp$ref <- SEED
    return(outp)
  }