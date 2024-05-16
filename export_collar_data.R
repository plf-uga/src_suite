#This first module will import the SCR collar data and export a count file according
#to the desired interval
library(lubridate)

export_collar_data = function(readname, min, timezone, outname, sep, keep_miss)
{
  min = min
  if (missing(timezone)) timezone="Europe/Amsterdam"
  if (missing(sep)) sep="\t"
  if (missing(keep_miss)) keep_miss=min
  start = 1
  end = min
  iteration <- 1
  day = 1
  
  ID <- read.csv(readname, nrows=1, skip=3, h =F)
  transponder = read.csv(readname, nrows=1, skip=4, h =F)
  beha_data = read.csv(readname, skip=4, h =T)
  
  animals = data.frame("ID" = c(as.numeric(ID[,3:ncol(ID)])), "RFID" = as.numeric(c(transponder[,3:ncol(ID)])))
  colnames(beha_data) = c(colnames(beha_data)[c(1,2)],animals$ID)
  
  behav_count = data.frame("Animal" = NA, "RFID" = NA , "time_point" = NA, "date" = NA, "start_time" = NA, "end_time" = NA, 
                           "inactive" = NA, "ruminating" = NA, "eating" = NA, "other" = NA, "missing" = NA)
  
  # Variable names
  variable_names <- c("Animal", "RFID",  "time_point", "date", "start_time", "end_time",
                      "inactive", "ruminating", "eating", "other", "missing")
  
  
  if(missing(outname)){filename = gsub("\\.csv$", "", readname)}
  else{filename = outname}
  
  file_path <- paste(outname, ".out", sep = "")
  
  
  write.table(t(variable_names), file = file_path, append = F, col.names = FALSE, row.names = FALSE, quote = F, sep = sep)
  
  rownames(animals) = animals$ID
  
  
  while(end < nrow(beha_data))
  {
    for (i in 1:nrow(animals))
    {
      anim = animals$ID[i]
      col_name = as.character(animals$ID[anim])
      count = data.frame(table(beha_data[start:end,col_name]))
      anim = col_name
      rfid = animals[anim,2]
      start_utc <- ymd_hms(beha_data$Starttime_utc[start], tz = "UTC")
      end_utc = ymd_hms(beha_data$Starttime_utc[end], tz = "UTC")
      start_time = with_tz(start_utc, tzone = timezone)
      end_time = with_tz(end_utc, tzone = timezone)
      
      
      inactive = max(0,count$Freq[which(count$Var1==1)])
      ruminating = max(0,count$Freq[which(count$Var1==2)])
      eating = max(0,count$Freq[which(count$Var1==3)])
      other = max(0,count$Freq[which(count$Var1==0)])
      missing = max(0,count$Freq[which(count$Var1==-1)])
      time_point = iteration
      
      
      start_time_formatted <- format(start_time, "%Y-%m-%d %H:%M:%S")
      end_time_formatted <- format(end_time, "%Y-%m-%d %H:%M:%S")
      date = format(end_time, "%Y-%m-%d")
      start_time_f = format(start_time, "%H:%M:%S")  
      end_time_f = format(end_time, "%H:%M:%S")
      
      # Combine vectors into a data frame
      data <- data.frame(anim,rfid, time_point, date, start_time_f,end_time_f,
        inactive,ruminating,eating,other,missing)
      
      if(data$missing <= keep_miss)
      {
        write.table(data, file = file_path, append = TRUE, col.names = FALSE, quote = F, row.names = FALSE, sep = sep)  
      }
      
    }
    start = end+1
    end = end+min
    iteration <- iteration + 1
    
    if (end %% 1440 == 0) {
      print(paste("Day:", day))
      day <- day+1
    }
  }
  cat("file: ", file_path, " created with no error.", "\n")
}

# Set the directory path
directory_path <- getwd()
# Get all CSV file names from the directory
csv_files <- list.files(directory_path, pattern = "\\.csv$", full.names = F)
# Print the list of CSV file names

export_collar_data(readname = csv_files[1], min = 60, 
                   timezone = "America/New_York", outname = "behavior_test", sep = " ",
                   keep_miss = 5)



