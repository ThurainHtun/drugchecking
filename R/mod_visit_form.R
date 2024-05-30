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
        actionButton(ns("vs_add"), label = tagList(
          icon("add"),"ADD NEW"), class = "btn-primary"),
        col_widths = c(2,2),
        )),
        shinyjs::hidden(div(id = ns("visit_form"),
        layout_columns(
          dateInput(ns("vs_date"),label = labelMandatory("Visit Date")),
          textInput(ns("vs_time_in"), label = labelMandatory("HH:MM:SS"), value = paste0(format(Sys.time(), "%H:%M:%S"))),
          textInput(ns("vs_id"), label = labelMandatory("Visit ID"), value = ""),
          selectInput(ns("vs_city"), label = labelMandatory("City"), choices = NULL),
          selectInput(ns("vs_site"), label = labelMandatory("Site"), choices = NULL),
          selectInput(ns("vs_techname"), label = labelMandatory("Technician Name"), choices = NULL),
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
          actionButton(ns("vs_submit"), label = tagList(icon("paper-plane"),"Submit"), class = "btn-primary"),
          actionButton(ns("vs_update"), label = tagList(icon("pen"),"Update"), class = "btn-primary"),
          actionButton(ns("vs_delete"), label = tagList(icon("trash-can"),"Delete"), class = "btn-primary", style = "background-color:#DE3163;"),
          col_widths = c(3,3,3)),
      )))),


       fluidRow(id = ns("table_box"),
       column(width = 12,
             h4("Table Output of Visits"),
             div("Please select a row in table if you wish to", tags$i(class="fa-solid fa-pen-to-square"), "UPDATE/EDIT or", tags$i(class="fa-solid fa-trash-can"), "DELETE record"),
         DT::dataTableOutput(ns("visit_table")))
       )



  )
}

#' visit_form Server Functions
#'
#' @noRd
mod_visit_form_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    #-----updating dropdown and option list ----

    observe({
      distinct_tech <- fetch_distinct_values("technician", "technician_name")
      updateSelectInput(session,"vs_techname", label = "Technician Name", choices = distinct_tech)
    })

    observe({
      distinct_city <- fetch_distinct_values("technician", "city")
      updateSelectInput(session,"vs_city", label = "City", choices = distinct_city)
    })

    observe({
      distinct_site <- fetch_distinct_values("technician", "site")
      updateSelectInput(session,"vs_site", label = "Site", choices = distinct_site)
    })


    #-----Setting up mandatory fields------
    mandatory_visit <- c( "vs_date" ,
                          "vs_time_in",
                          "vs_id",
                          "vs_city",
                          "vs_site",
                          "vs_techname"
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



      # Create a new visit document with NULL for missing values
      new_visit <- list(
        vs_date = convert_to_character_or_null(input$vs_date),
        vs_time_in = convert_to_character_or_null(input$vs_time_in),
        vs_id = convert_to_character_or_null(input$vs_id),
        vs_city = convert_to_character_or_null(input$vs_city),
        vs_site = convert_to_character_or_null(input$vs_site),
        vs_techname = convert_to_character_or_null(input$vs_techname),
        vs_gender = convert_to_character_or_null(input$vs_gender),
        vs_age = convert_to_character_or_null(input$vs_age)
      )

      # Convert the document to JSON
      new_visit_json <- toJSON(new_visit, auto_unbox = TRUE, na = "null")

      # Insert the new document into the collection
      mongo_visit$insert(new_visit_json)

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
          updateSelectInput(session, "vs_city", label = "City", selected =   selectedRowData$vs_city)
          updateSelectInput(session, "vs_site", label = "Site", selected =  selectedRowData$vs_site)
          updateSelectInput(session, "vs_techname", label = "Technician Name", selected =  selectedRowData$vs_techname)
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


      selected <- input$visit_table_rows_selected
      if (length(selected)) {
        currentData <- myData()
        selectedID <- currentData[selected, "_id"]

        if (!is.null(selectedID)) {

          # Construct the update document
          updated_visit <- list(
            "$set" = list(
              vs_date = as.character(input$vs_date),
              vs_time_in = as.character(input$vs_time_in),
              vs_id = as.character(input$vs_id),
              vs_city = as.character(input$vs_city),
              vs_site = as.character(input$vs_site),
              vs_techname = as.character(input$vs_techname),
              vs_gender = as.character(input$vs_gender),
              vs_age = as.character(input$vs_age)
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
