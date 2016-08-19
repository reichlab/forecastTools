## read in data from CDC format forecast
read_flu_forecast <- function(filename) {
    require(data.table)
    require(dplyr)
    require(tidyr)
    require(stringr)

    ## read in metadata
    metadata <- fread(filename, nrows=5, skip=1, select=1:2)
        
        
    ## read in national data from flu submission
    seasonal_targets <- fread(filename, nrows=35, skip=9, select = c(1:4))  %>% 
        tbl_df() %>%
        
        ## organize and rename variables
        select(-V2) %>%
        rename(name = V1,
               season_onset = V3,
               peak_week = V4) %>%
        
        ## gather so one predicted value per row
        gather(key="target", value="value", season_onset, peak_week) %>%
        
        ## determine prediction type
        mutate(pred_type = ifelse(substr(name, start=1, stop=3) == "Pr(", "bin", "point")) %>%
        
        ## calculate bin limits
        mutate(bin_lwr = as.numeric(substr(name, 6, 7)),
               bin_upr_strict = (bin_lwr + 1)%%53)
    ## FIXME: does the 53 in above line need to be more flexible for a 53 week season?
        
    
    ## want to end up with value, pred.type, target, name, bin_lwr, bin_upr_strict
    
    incidence_targets <- fread(filename, nrows=29, skip=46, select=c(1:7)) %>%
        tbl_df() %>%
        
        ## organize and rename variables
        select(-V2) %>%
        rename(name = V1,
               week_ahead_1 = V3,
               week_ahead_2 = V4,
               week_ahead_3 = V5,
               week_ahead_4 = V6,
               peak_height = V7) %>%
        
        ## gather so one predicted value per row
        gather(key="target", value="value", week_ahead_1:peak_height) %>%
        
        ## determine prediction type
        mutate(pred_type = ifelse(substr(name, start=1, stop=3) == "Pr(", "bin", "point")) 
        
        ## calculate bin limits
        bin_lims <- str_extract_all(incidence_targets$name, "\\d+\\.*\\d*")
        incidence_targets$bin_lwr <- as.numeric(sapply(bin_lims, FUN = function(x) ifelse(length(x)==2, x[1], x[1])))
        incidence_targets$bin_upr_strict <- as.numeric(sapply(bin_lims, FUN = function(x) ifelse(length(x)==2, x[2], NA)))
        
        ## merge predictions
        preds_out <- bind_rows(seasonal_targets, incidence_targets)
        
        return(list(preds=preds_out, metadata))
}


