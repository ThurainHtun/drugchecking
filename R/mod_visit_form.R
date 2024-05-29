#' visit_form UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_visit_form_ui <- function(id){
  ns <- NS(id)
  tagList(

      shinyjs::useShinyjs(),
      fluidRow(id = ns("form"),
        layout_columns(
        column(width = 6,
        actionButton(ns("vs_add"), label = "ADD NEW", class = "btn-primary"),
        actionButton(ns("vs_import"), label = "Import Data", class = "btn-primary", style = "background-color:#1A5276;"),
        col_widths = c(2,2),
        )),
        shinyjs::hidden(div(id = ns("visit_form"),
        layout_columns(
          dateInput(ns("vs_date"),label = labelMandatory("Visit Date")),
          textInput(ns("vs_time_in"), label = labelMandatory("HH:MM:SS"), value = paste0(format(Sys.time(), "%H:%M:%S"))),
          textInput(ns("vs_id"), label = labelMandatory("Visit ID"), value = ""),
          textInput(ns("vs_city"), label = labelMandatory("City"), value = ""),
          textInput(ns("vs_site"), label = labelMandatory("Site"), value = ""),
          textInput(ns("vs_techname"), label = labelMandatory("Technician Name"), value = ""),
          radioButtons(ns("vs_gender"),
                       label = "Gender:",
                       choices = c(
                         "Female/woman",
                         "Male/man",
                         "Non-binary",
                         "Prefer not to say",
                         "Not Selected"),
                       selected = "Not Selected",
                       inline = TRUE),
          radioButtons(ns("vs_age"),
                       label = "Age:",
                       choices = c( "Under 19",
                                    "20 - 29",
                                    "30 - 39",
                                    "40 - 49",
                                    "50 - 59",
                                    "60+",
                                    "Prefer not to say",
                                    "Unknown",
                                    "Not Selected"),
                       selected = "Not Selected",
                       inline = TRUE),
        col_widths = c(2,2,2,2,2,2,6,6)),
        layout_columns(
          column(width = 6,
          actionButton(ns("vs_submit"), label = "Submit", class = "btn-primary"),
          actionButton(ns("vs_update"), label = "Update", class = "btn-primary"),
          actionButton(ns("vs_delete"), label = "Delete", class = "btn-primary", style = "background-color:#DE3163;"),
          col_widths = c(3,3,3)),
      )))),

      column(width = 6, DT::dataTableOutput(ns("visit_table")))




  )
}

#' visit_form Server Functions
#'
#' @noRd
mod_visit_form_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns


    #-----Setting up mandatory fields------
    mandatory_visit <- c( "vs_date" ,
                          "vs_time_in",
                          "vs_id",
                          "vs_city",
                          "vs_site",
                          "vs_techname",
                          "vs_gender",
                          "vs_age"
                          )

    ##---disable submit button if mandatory fields are not entered----
    observe({
      # check if all mandatory fields have a value
      mandatory_visit <-
        vapply(mandatory_visit,
               function(x) {
                 !is.null(input[[x]]) && input[[x]] != ""
               },
               logical(1))
      mandatory_visit <- all(mandatory_visit)

      # enable/disable the submit button
      shinyjs::toggleState(id = "vs_submit", condition = mandatory_visit)
    })


    # Connect to MongoDB
    mongo_visit <- mongo(collection = "visit", db = "drug_checking", url = "mongodb+srv://drthurain07:XtZXZxxgbNB3PT0q@cluster0.cmm67ct.mongodb.net/")

    # Function to fetch data from MongoDB
    fetch_data <- function() {
      data <- mongo_visit$find(fields = '{"_id": 1,"vs_date": 1, "vs_time_in": 1, "vs_id": 1,  "vs_city": 1, "vs_site": 1, "vs_techname": 1, "vs_gender": 1, "vs_age": 1}')
      data$`_id` <- as.character(data$`_id`) # Convert ObjectId to character
      return(data)
    }

    # Reactive value to store data
    myData <- reactiveVal(fetch_data())

    # Render DataTable
    output$visit_table <- DT::renderDataTable({
      DT::datatable(myData(), selection = "single", rownames = FALSE)
    })

    #show/hide form by clicking add button

    observeEvent(input$vs_add,{
       shinyjs::useShinyjs()
       shinyjs::show("visit_form")
       shinyjs::show("vs_submit")
       shinyjs::hide("vs_update")
       shinyjs::hide("vs_delete")
    })

#-------SUBMIT NEW DATA------
    observeEvent(input$vs_submit,{
      shinyjs::hide("visit_form")
      shinyjs::reset("visit_form")

      # Capture input values
      new_visit <- list(
        vs_date = as.character(unlist(input$vs_date)[1]),
        vs_time_in = as.character(unlist(input$vs_time_in)[1]),
        vs_id = as.character(unlist(input$vs_id)[1]),
        vs_city = as.character(unlist(input$vs_city)[1]),
        vs_site = as.character(unlist(input$vs_site)[1]),
        vs_techname = as.character(unlist(input$vs_techname)[1]),
        vs_gender = as.character(unlist(input$vs_gender)[1]),
        vs_age = as.character(unlist(input$vs_age)[1])
      )


      # Insert the new document into the collection
      mongo_visit$insert(new_visit)

      # Show popup message for successful deletion
      shinyalert::shinyalert(title = "Success", text = "Visit added successfully!", type = "success")

      # Close MongoDB connection
      mongo_visit$disconnect()

      # Refresh the data
      myData(fetch_data())
    })

#---- Action when table row is selected----

    observeEvent(input$visit_table_rows_selected,{
      shinyjs::show(id="visit_form")
      shinyjs::hide(id = "vs_submit")
      shinyjs::show(id="vs_update")
      shinyjs::show(id="vs_delete")
      shinyjs::hide(id = "vs_add")
      shinyjs::hide(id = "vs_import")

      selected <- input$visit_table_rows_selected
      if (length(selected)) {
        currentData <- myData()
        selectedID <- currentData[selected, "_id"]

        if (!is.null(selectedID)) {

          # Retrieve selected row's data
          selectedRowData <- currentData[selected, ]

          # Bind selected row's data to input variables
          updateDateInput(session,"vs_date",label = "Visit Date",value = selectedRowData$vs_date)
          updateTextInput(session,"vs_time_in", label = "HH:MM:SS", value = selectedRowData$vs_time_in)
          updateTextInput(session, "vs_id", label = "Visit ID", value = selectedRowData$vs_id)
          updateTextInput(session, "vs_city", label = "City", value = selectedRowData$vs_city)
          updateTextInput(session, "vs_site", label = "Site", value = selectedRowData$vs_site)
          updateTextInput(session, "vs_techname", label = "Technician Name", value = selectedRowData$vs_techname)
          updateRadioButtons(session,
                             "vs_gender",
                             label = "Gender:",
                             choices = c(
                               "Female/woman",
                               "Male/man",
                               "Non-binary",
                               "Prefer not to say",
                               "Not Selected"),
                             selected = selectedRowData$vs_gender,
                             inline = TRUE)
          updateRadioButtons(session,
                             "vs_age",
                             label = "Age:",
                             choices = c( "Under 19",
                                          "20 - 29",
                                          "30 - 39",
                                          "40 - 49",
                                          "50 - 59",
                                          "60+",
                                          "Prefer not to say",
                                          "Unknown",
                                          "Not Selected"),
                             selected = selectedRowData$vs_age,
                             inline = TRUE)


          # Close MongoDB connection
          mongo_visit$disconnect()
        } else {
          print("Selected ID is null for update")
        }
      } else {
        print("No row selected for update")
      }


    })



#------UPDATE DATA-----
    # Update selected technician
    observeEvent(input$vs_update, {

      shinyjs::reset("visit_form")
      shinyjs::hide("visit_form")
      shinyjs::hide("vs_update")
      shinyjs::hide("vs_delete")
      shinyjs::show("vs_add")
      shinyjs::show("vs_import")

      selected <- input$visit_table_rows_selected
      if (length(selected)) {
        currentData <- myData()
        selectedID <- currentData[selected, "_id"]

        if (!is.null(selectedID)) {

          # Construct the update document
          updated_visit <- list(
            "$set" = list(
              vs_date = as.character(unlist(input$vs_date)[1]),
              vs_time_in = as.character(unlist(input$vs_time_in)[1]),
              vs_id = as.character(unlist(input$vs_id)[1]),
              vs_city = as.character(unlist(input$vs_city)[1]),
              vs_site = as.character(unlist(input$vs_site)[1]),
              vs_techname = as.character(unlist(input$vs_techname)[1]),
              vs_gender = as.character(unlist(input$vs_gender)[1]),
              vs_age = as.character(unlist(input$vs_age)[1])
            )
          )

          # Convert the update document to JSON
          updated_visit_json <- jsonlite::toJSON(updated_visit, auto_unbox = TRUE)

          # Update the document
          mongo_visit$update(query = sprintf('{"_id": {"$oid": "%s"}}', selectedID), update = updated_visit_json)


          # Show popup message for successful deletion
          shinyalert::shinyalert(title = "Success", text = "visit updated successfully!", type = "success")

          # Close MongoDB connection
          mongo_visit$disconnect()

          # Refresh the data
          myData(fetch_data())
        } else {
          print("Selected ID is null for update")
        }
      } else {
        print("No row selected for update")
      }
    })

#-----DELETE DATA-----

    # Delete selected technician
    observeEvent(input$vs_delete, {
      shinyjs::reset("visit_form")
      shinyjs::hide("visit_form")
      shinyjs::hide("vs_update")
      shinyjs::hide("vs_delete")
      shinyjs::show("vs_add")
      shinyjs::show("vs_import")
      selected <- input$visit_table_rows_selected
      if (length(selected)) {
        currentData <- myData()
        selectedID <- currentData[selected, "_id"]

        # Debug print statements
        print(paste("Selected ID for deletion:", selectedID))
        print(str(currentData))

        if (!is.null(selectedID)) {
          mongo_visit$remove(paste0('{"_id": {"$oid": "', selectedID, '"}}'))
          myData(fetch_data())
          # Show popup message for successful deletion
          shinyalert::shinyalert(title = "Success", text = "Visit deleted successfully!", type = "success")
        } else {
          print("Selected ID is null for deletion")
        }
      } else {
        print("No row selected for deletion")
      }
    })



  })
}

## To be copied in the UI
# mod_visit_form_ui("visit_form_1")

## To be copied in the server
# mod_visit_form_server("visit_form_1")
