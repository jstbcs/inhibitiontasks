# these functions automatically read in the raw data sets from the online form 

# function to find correct naming convention for each dataset
find_dataname <- function(download_links, col){
  if(colnames(download_links)[col] == "Upload.data"){
    # name pattern type 1
    data_name <- "processed_data_study1"
    
  } else if(grepl("in", colnames(download_links)[col])){
    # name pattern type 3
    split_name <- unlist(strsplit(colnames(download_links)[col], "[.]"))
    task_number <- split_name[5]
    study_number <- split_name[length(split_name)]
    data_name <- paste("processed_data_study",study_number,"_task",task_number, sep="")
    
  } else if(grepl("Upload.data.for.task.", colnames(download_links)[col])){
    # pattern type 3 but study_number is 1
    split_name <- unlist(strsplit(colnames(download_links)[col], "[.]"))
    task_number <- split_name[length(split_name)]
    data_name <- paste("processed_data_study1_task", task_number, sep="")
    
  } else if(grepl("Upload.data.for.study.", colnames(download_links)[col])) {
    # name pattern type 2
    split_name <- unlist(strsplit(colnames(download_links)[col], "[.]"))
    study_number <- split_name[length(split_name)]
    data_name <- paste("processed_data_study", study_number, sep="")
    
  } else {
    data_name <- "NAME MANUALLY"
    warning(paste("Assigning data frame names failed for data listed in column ", colnames(download_links)[col], 
                  ". Please name the data set manually following the naming conventions"))
  }
  return(data_name)
}



# main function: download datasets, assign correct name and add to list

download_datasets <- function(entry, download_folder){
  # step 1: # 1 get non-empty columns containing download paths 
  upload_columns <- which(grepl("Upload", colnames(entry)))
  download_links <- entry %>%
    select(upload_columns[which(!is.na(entry[1, upload_columns]))])
  download_links
  
  # list will store all datasets
  dataframe_list <- list()
  
  # download each dataset 
  for(i in 1:ncol(download_links)){
    # define correct file name for saving 
    filepath <- paste(download_folder, "/data",i,".csv", sep="")
    
    # download data to local machine 
    download.file(download_links[1, i], filepath)
    
    # read data into list object, delete first row 
    dataframe_list[[i]] <- read.csv(filepath)
    dataframe_list[[i]] <- dataframe_list[[i]][, 2:ncol(dataframe_list[[i]])]
    
    # find correct name and assign to dataframe 
    data_name <- find_dataname(download_links, col = i)
    names(dataframe_list)[i] <- data_name 
  }
  
  return(dataframe_list)
}