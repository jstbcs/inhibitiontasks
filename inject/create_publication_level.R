# create publication level 
create_pub_level <- function(entry){
  pub <- list()
  
  pub$publication_table <- data.frame(
    authors = ifelse("Authors" %in% names(entry), entry$Authors, NA),
    conducted = ifelse("Year" %in% names(entry), entry$Year, NA), 
    added = Sys.Date(), 
    country = ifelse("Country" %in% names(entry), entry$Country, NA), 
    contact = ifelse("Email.for.contact" %in% names(entry), entry$Email.for.contact, NA),
    keywords = ifelse("Keywords" %in% names(entry), entry$Keywords, NA),
    APA_reference = entry$APA.reference, 
    publication_code = pub_code
  )
  
  return(pub)
}

