#' helpers
#'
#' @description A utils function
#'
#' @return The return value, if any, from executing the utility.
#'
#' @noRd


labelMandatory <- function(label) {
  tagList(
    label,
    span("*", class = "mandatory_star")
  )
}


# Function to convert input to character or NULL if not provided
convert_to_character_or_null <- function(input_variable) {
  if (is.null(input_variable)) {
    return(NULL)
  } else {
    return(as.character(input_variable))
  }
}


# Connect to MongoDB
mongo_visit <- mongo(collection = "visit", db = "drug_checking", url = "mongodb+srv://drthurain07:XtZXZxxgbNB3PT0q@cluster0.cmm67ct.mongodb.net/")
mongo_sample <- mongo(collection = "sample", db = "drug_checking", url = "mongodb+srv://drthurain07:XtZXZxxgbNB3PT0q@cluster0.cmm67ct.mongodb.net/")
mongo_technician <- mongo(collection = "technician", db = "drug_checking", url = "mongodb+srv://drthurain07:XtZXZxxgbNB3PT0q@cluster0.cmm67ct.mongodb.net/")
mongo_expected_sub <- mongo(collection = "expected_sub", db = "drug_checking", url = "mongodb+srv://drthurain07:XtZXZxxgbNB3PT0q@cluster0.cmm67ct.mongodb.net/")
mongo_ftir_sub <- mongo(collection = "ftir_sub", db = "drug_checking", url = "mongodb+srv://drthurain07:XtZXZxxgbNB3PT0q@cluster0.cmm67ct.mongodb.net/")


#function to get distinct list of a field in database table

fetch_distinct_values <- function(collection_name, field_name) {
  conn <- mongo(
    collection = collection_name,
    db = "drug_checking",
    url = "mongodb+srv://drthurain07:XtZXZxxgbNB3PT0q@cluster0.cmm67ct.mongodb.net/"
  )

  distinct_values <- conn$distinct(field_name)
  conn$disconnect()
  return(distinct_values)
}




