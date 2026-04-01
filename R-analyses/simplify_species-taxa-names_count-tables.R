# Define a function to find the most specific taxon
find_most_specific_taxon <- function(taxon_string) {
  # Split the string into individual taxon
  split_taxon <- strsplit(taxon_string, "\\.")[[1]]

  # Remove the level identifiers
  named_taxon <- gsub("^.*__ ", "", split_taxon)

  # Reverse the order
  rev_taxon <- rev(named_taxon)

  # Find the first taxon that doesn't say 'uncultured' or 'metagenome'
  specific_taxon <- rev_taxon[!rev_taxon %in% c('uncultured',
                                                'unidentified',
                                                'uncultured_bacterium',
                                                'metagenome',
                                                'uncultured_organism',
                                                'human_gut',
                                                'gut_metagenome',
                                                '_bacterium',
                                                'Incertae_Sedis')][1]

  return(specific_taxon)
}

species_c4m <- coda_balance_coeff_10p
# species_c4m$Species <- coda_balance_coeff_meth_10p$Species

# View the updated data frame
head(species_c4m)

# Remove extra "__." 
species_c4m$Species <- gsub("\\.__$", "", species_c4m$Species)
species_c4m$Species <- gsub("\\.__$", "", species_c4m$Species)
species_c4m$Species <- gsub("\\.__$", "", species_c4m$Species)
species_c4m$Species <- gsub("\\.__$", "", species_c4m$Species)
species_c4m$Species <- gsub("\\.__$", "", species_c4m$Species)
head(species_c4m)

# Remove "__" where it's not followed by any more characters
# species_c4m$Species <- gsub("__$", "", species_c4m$Species)

# Remove "." where it's not followed by any more characters
species_c4m$Species <- gsub("\\.$", "", species_c4m$Species)

# Remove "." where followed by a single "_"
species_c4m$Species <- gsub("\\._(?!_)", "_", species_c4m$Species, perl=TRUE)

# Remove "." when preceded by two "__"
species_c4m$Species <- gsub("(?<=__)\\.", "", species_c4m$Species, perl=TRUE)

# Add a space following each ";" in Taxon, so same function can be used
# species_c4m$Species <- gsub(";", "; ", species_c4m$Species)

# Add a space after each of the specified patterns
species_c4m$Species <- gsub("(d__|p__|c__|o__|f__|g__|s__)", "\\1 ", species_c4m$Species)

# Remove "." from species/genera like UCG.003 so it reads UCG003
species_c4m$Species <- gsub("([a-zA-Z])\\.([0-9])", "\\1\\2", species_c4m$Species)

# Check
head(species_c4m$Species)

# Apply function to Species count table
species_c4m$Species <- sapply(species_c4m$Species, find_most_specific_taxon)

# Clean up special characters left in names
species_c4m$Species <- gsub("\\[|\\]", "", species_c4m$Species)

# Replace underscores with spaces
species_c4m$Species <- gsub("_", " ", species_c4m$Species)

# # Replace dashes with spaces
# species_c4m$Species <- gsub("-", " ", species_c4m$Species)

# Replace "." in sp.
species_c4m$Species <- gsub("sp$", "sp.", species_c4m$Species)

# # Remove spaces at the end of names
# species_c4m$Species <- gsub("\\s+$", "", species_c4m$Species)

# # Check for duplicates
# any(duplicated(species_c4m$Species)) # TRUE if present
# species[duplicated(species_c4m$Species), ]

coda_balance_coeff_10p <- species_c4m

