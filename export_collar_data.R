#This first module will import the SCR collar data and export a count file according
#to the desired interval

export_collar_data = function(readname, min, timezone, outname, sep, keep_miss)
{
  if (!require(lubridate)) {
    install.packages("lubridate")  # Install the package if not installed
    library(lubridate)  # Load the package
  } else {
    library(lubridate)  # Load the package if already installed
  }
  min = min
  if (missing(timezone)) timezone=Sys.timezone()
  if (missing(sep)) sep="\t"
  if (missing(keep_miss)) keep_miss=min
  
  
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
  
  beha_data$Starttime_utc = ymd_hms(beha_data$Starttime_utc, tz = "UTC")
  beha_data$Starttime_local = format(beha_data$Starttime_utc, tz = timezone)
  
  
  dates = beha_data$Starttime_local
  dates = as.Date(beha_data$Starttime_local)
  dates = data.frame(table(dates))
  
  
  for (k in 1:nrow(dates))
  {
    start = 1
    end = min
    iteration <- 1
    
    beha_temp = beha_data[which(as.Date(beha_data$Starttime_local)==as.Date(dates[k,1])),]
    while(end <= nrow(beha_temp))
    {
      tryCatch({
      for (i in 1:nrow(animals))
      {
        anim = animals$ID[i]
        col_name = as.character(animals$ID[anim])
        count = data.frame(table(beha_temp[start:end,col_name]))
        anim = col_name
        rfid = animals[anim,2]
        
        start_time <- beha_temp$Starttime_local[start]
        #end_utc = ymd_hms(beha_temp$Starttime_local[end], tz = "UTC")
        #start_time = with_tz(start_utc, tzone = timezone)
        end_time = beha_temp$Starttime_local[end]
        
        
        inactive = max(0,count$Freq[which(count$Var1==1)])
        ruminating = max(0,count$Freq[which(count$Var1==2)])
        eating = max(0,count$Freq[which(count$Var1==3)])
        other = max(0,count$Freq[which(count$Var1==0)])
        missing = max(0,count$Freq[which(count$Var1==-1)])
        time_point = iteration
        
        
        start_time_formatted <- format(ymd_hms(start_time, tz = timezone),"%Y-%m-%d %H:%M:%S") 
        end_time_formatted <- format(ymd_hms(end_time, tz = timezone),"%Y-%m-%d %H:%M:%S")
        date = as.Date(end_time)
        start_time_f = format(ymd_hms(start_time, tz = timezone), "%H:%M:%S")
        if(is.na(start_time_f)){stop("error during time conversion")}
        
        end_time_f = format(ymd_hms(end_time, tz = timezone), "%H:%M:%S")
        
        # Combine vectors into a data frame
        data <- data.frame(anim,rfid, time_point, date, start_time_f,end_time_f,
                           inactive,ruminating,eating,other,missing)
        
        if(data$missing <= keep_miss)
        {
          write.table(data, file = file_path, append = TRUE, col.names = FALSE, quote = F, row.names = FALSE, sep = sep)  
        }
        
      }
      }, error = function(e) {
        cat("Warning: skiping animal ", i,  ", timepoint ", start_time_f, " , cause: ",  conditionMessage(e), "\n")
        
        next
      })
      start = end+1
      end = min(end+min, nrow(beha_temp)+1)
      iteration <- iteration + 1
    }
    print(paste("Day:", dates[k,1], " done ..."))
  }
  
  cat("file: ", file_path, " created with no error.", "\n")
}
