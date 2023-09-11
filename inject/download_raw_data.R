# these functions automatically read in the raw data sets from the online form 
library(readr)


# function to find correct naming convention for each dataset -----------------
find_dataname <- function(download_links, col){
  # pattern type: only 1 task in fist study 
  if(colnames(entry[which(entry == download_links[2, col])]) == "Upload.data" |
     colnames(entry[which(entry == download_links[2, col])]) == "Link.to.data"){
    data_name <- "processed_data_study1"
    
  } else if(grepl("in", colnames(download_links)[col])){
    # pattern type: includes both study number and task number 
    split_name <- unlist(strsplit(colnames(download_links)[col], "[.]"))
    task_number <- split_name[10]
    study_number <- split_name[length(split_name)]
    data_name <- paste("processed_data_study",study_number,"_task",task_number, sep="")
    
  } else if(grepl("Upload.data.for.task.", colnames(entry[which(entry == download_links[2, col])])) |
            grepl("Link.to.data.for.task.", colnames(entry[which(entry == download_links[2, col])]))) {
    # name pattern if only one study submitted
    split_name <- unlist(strsplit(colnames(entry[which(entry == download_links[2, col])]), "[.]"))
    task_number <- split_name[length(split_name)]
    data_name <- paste("processed_data_study1_task", task_number, sep="")
    
  } else if(grepl("How.would.you.like.to.submit.data.for.study.", colnames(download_links)[col])){
    # name pattern if only one task in study 2-5
    split_name <- unlist(strsplit(colnames(download_links)[col], "[.]"))
    study_number <- split_name[length(split_name)]
    data_name <- paste("processed_data_study", study_number, sep="")
    
  } else {
    data_name <- "NAME MANUALLY"
    warning(paste("Assigning data frame names failed for data listed in column ", 
                  colnames(entry[which(entry == download_links[2, col])]), 
                  ". Please name the data set manually following the naming conventions"))
  }
  return(data_name)
}
  
 
# main function: download datasets, assign correct name and add to list --------
download_datasets <- function(entry, download_folder){
  # step 1: Get non-entry choices of how to provide data access
  datachoice_cols <- which(grepl("How", colnames(entry)))
  download_links <- entry %>%
    select(datachoice_cols[which(!is.na(entry[1, datachoice_cols]))])
  
  # step 2: append respective download link to each column of download_links df
  for(col in 1:ncol(download_links)){
    if(download_links[1,col] == "Upload data to this form"){
      # note: link to uploaded link is in column directly after choice column
      colnumber <- which(colnames(entry) == colnames(download_links)[col]) # find choice col index
      download_links[2,col] <- entry[1, colnumber + 1] 
    } else if(download_links[1,col] == "Submit link to OSF/ Github/ other"){
      # note: link to data is 2 columns next to choice column
      colnumber <- which(colnames(entry) == colnames(download_links)[col]) 
      download_links[2, col] <- entry[1, colnumber + 2] 
    }
  }
  
  # list will store all datasets
  dataframe_list <- list()
  
  # download each dataset 
  for(i in 1:ncol(download_links)){
    # define correct file name for saving 
    filepath <- paste(download_folder, "/data",i,".csv", sep="")
    
    # download data to local machine 
    # if data was uploaded to our website
    if(download_links[1, i] == "Upload data to this form"){
      download.file(download_links[2, i], filepath)
      # read data into list object, delete first row 
      dataframe_list[[i]] <- read.csv(filepath)
      dataframe_list[[i]] <- dataframe_list[[i]][, 2:ncol(dataframe_list[[i]])]
      # find correct name and assign to dataframe 
      data_name <- find_dataname(download_links, col = i)
      names(dataframe_list)[i] <- data_name 
      
      # if link to raw data on github  
    } else if(grepl("raw.githubusercontent.com", download_links[2, i])){
      dataframe_list[[i]] <- read_csv(url(download_links[2, i]))
      # find correct name and assign to dataframe 
      data_name <- find_dataname(download_links, col = i)
      names(dataframe_list)[i] <- data_name 
      
      # if download link to osf data
    } else if(grepl("osf.io", download_links[2, i]) & grepl("/download", download_links[2,i])){
            data <- read.csv(download_links[2,i], header=TRUE, na.strings="NA")
      if(ncol(data) == 1) {  # check if csv2 required instead
        data <- read.csv2(download_links[2,i], header=TRUE, na.strings="NA")
      }
      dataframe_list[[i]] <- data
      # find correct name and assign to dataframe 
      data_name <- find_dataname(download_links, col = i)
      names(dataframe_list)[i] <- data_name 
      
    } else {
      # if provided link cannot be downloaded automatically into R, give warning
      warning(paste("The following link to data for", names(download_links)[i], "can not be downloaded automatically.
                    Please check the following link:", download_links[2, i], "and download the data manually.", sep=" "))
    }
  }
  
  return(dataframe_list)
}