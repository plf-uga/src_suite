export_leg_data = function(working_directory, steps, timezone, outname, sep, keep_miss)
{
  if (!require(lubridate)) {
    install.packages("lubridate")  # Install the package if not installed
    library(lubridate)  # Load the package
  } else {
    library(lubridate)  # Load the package if already installed
  }
  
  if (missing(timezone)) timezone=Sys.timezone()
  if (missing(sep)) sep="\t"
  if (missing(keep_miss)) keep_miss=steps
  
  setwd(working_directory)
  csv_files <- list.files(working_directory, pattern = "\\.csv$", full.names = F)
  activity_ind = grep('legActivityData', csv_files)
  lying_ind = grep('lyingTimeData', csv_files)
  standing_ind = grep('standingTimeData', csv_files)
  
  ID <- read.csv(csv_files[activity_ind] , nrows=1, skip=3, h =F)
  transponder = read.csv(csv_files[activity_ind], nrows=1, skip=1, h =F)
  activity_data = read.csv(csv_files[activity_ind], skip=4, h =F)
  lying_data = read.csv(csv_files[lying_ind], skip=4, h =F)
  standing_data = read.csv(csv_files[standing_ind], skip=4, h =F)
  animals = data.frame("ID" = c(as.numeric(ID[,2:ncol(ID)])), "RFID" = c(unlist(transponder[,2:ncol(ID)])))
  
  colnames(activity_data) = c('date_time',animals$ID)
  colnames(lying_data) = c('date_time',animals$ID)
  colnames(standing_data) = c('date_time',animals$ID)
  rownames(animals) = animals$ID
  
  behav_count = data.frame("Animal" = NA, "RFID" = NA , "time_point" = NA, "date" = NA, "start_time" = NA, "end_time" = NA, 
                           "step_count" = NA, "walk_time" = NA,"lying_time" = NA, "standing_time" = NA, "missing_count" = NA)
  
  # Variable names
  variable_names <- c("Animal", "RFID", "time_point", "date", "start_time", "end_time", 
                      "step_count", "walk_time",  "lying_time", "standing_time", "missing_count")
  
  file_path <- paste(outname, ".out", sep = "")
  write.table(t(variable_names), file = file_path, append = F, col.names = FALSE, row.names = FALSE, quote = F, sep = sep)
  
  activity_data$date_time = ymd_hms(activity_data$date_time, tz = timezone)
  lying_data$date_time = ymd_hms(lying_data$date_time, tz = timezone)
  standing_data$date_time = ymd_hms(standing_data$date_time, tz = timezone)

  dates = activity_data$date_time
  dates = format(dates, "%Y-%m-%d")
  dates = data.frame(table(dates))
  
  for (k in 1:nrow(dates))
  {
    activity_temp = activity_data[which(format(activity_data$date_time,"%Y-%m-%d")==dates[k,1]),]
    stand_temp = standing_data[which(format(standing_data$date_time,"%Y-%m-%d")==dates[k,1]),]
    lying_temp = lying_data[which(format(lying_data$date_time,"%Y-%m-%d")==dates[k,1]),]
    
    steps = steps
    start = 1
    end = steps
    iteration = 1
    while(end < nrow(activity_temp))
    {
      tryCatch({
      for (i in 1:nrow(animals))
      {
        
        anim = animals$ID[i]
        col_name = as.character(animals$ID[anim])
        
        
        ind_stand = stand_temp[start:end,col_name]>=0
        ind_walk = activity_temp[start:end,col_name]>=0
        ind_rest = lying_temp[start:end,col_name]>=0
        ind_miss = lying_temp[start:end,col_name]==-1
        
        count_stand = sum(stand_temp[start:end,col_name][ind_stand])
        count_walk =  sum(activity_temp[start:end,col_name][ind_walk])
        count_rest = sum(lying_temp[start:end,col_name][ind_rest])
        count_miss = sum(abs(lying_temp[start:end,col_name][ind_miss]))*15
        walk_time = steps*15 - sum(count_stand,count_rest, count_miss)
        
        time_point = iteration
        anim = col_name
        rfid = animals[anim,2]
        
        start_time_f <- format(activity_temp$date_time[start], "%H:%M:%S")
        start_time_f = strptime(start_time_f,  format = "%H:%M:%S") - 14*60
        start_time_f = format(start_time_f,  "%H:%M:%S")
        end_time_f = format(activity_temp$date_time[end], "%H:%M:%S")
        current_date = as.character(dates[k,1])
        
        # Combine vectors into a data frame
        data <- data.frame(anim,gsub("'", "", rfid), time_point, current_date, start_time_f,end_time_f,
                           count_walk,walk_time, count_rest,count_stand,count_miss)
        
        if(data$count_miss <= keep_miss)
        {
          write.table(data, file = file_path, append = TRUE, col.names = FALSE, quote = F, row.names = FALSE, sep = sep)  
        }
        
      }
      }, error = function(e) {
        cat("Warning: skiping animal ", i,  ", timepoint ", start_time_f, " , cause: ",  conditionMessage(e), "\n")
        
        next
      })
      start = end+1
      end = min(end+steps, nrow(activity_temp))
      iteration <- iteration + 1
    }
    print(paste("Day:", current_date, " done ..."))
  }
  
  cat("file: ", file_path, " created with no errors.", "\n")
}

