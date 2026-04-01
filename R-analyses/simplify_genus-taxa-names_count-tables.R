# Define a function to find the most specific taxon
extract_genus <- function(taxon_string) {
  # Split the string on "."
  parts <- unlist(strsplit(taxon_string, "\\."))
  
  # Find the part that starts with "g__"
  genus_part <- parts[grepl("^g__", parts)]
  
  # If no genus found, return NA
  if (length(genus_part) == 0 || genus_part == "g__") {
    return(NA)
  }
  
  # Remove "g__" prefix and any brackets or underscores
  genus <- gsub("^g__", "", genus_part)
  genus <- gsub("_", " ", genus)
  genus <- gsub("\\[|\\]", "", genus)
  genus <- trimws(genus)
  
  return(genus)
}


coda_balance_coeff_10p$Genus <- sapply(coda_balance_coeff_10p$Species, extract_genus)


coda_balance_coeff_10_twocup$Genus <- sapply(coda_balance_coeff_10_twocup$Species, extract_genus)


