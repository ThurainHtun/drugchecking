#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
#'
#'


app_server <- function(input, output, session) {
  # Your application server logic
  mod_visit_form_server("visit_form_1")
  mod_sample_form_server("sample_form_1")
  mod_technician_list_server("technician_list_1")
  mod_Expected_Substance_List_server("Expected_Substance_List_1")
  mod_ftir_Substance_List_server("ftir_Substance_List_1")



}
