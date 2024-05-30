#' Expected_Substance_List UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_Expected_Substance_List_ui <- function(id){
  ns <- NS(id)
  tagList(
    fluidRow(
      shinyjs::useShinyjs(),
      style = "border: solid black 1px;",
      h6(""),
      column(width = 12,
             h6("Please update the Expected Substance drop-down list here"),
             style = "background-color:#EBF5FB;"),

      fluidRow(
        h6(" "),
        column(width = 3,
               actionButton(ns("add_esub"),
                            label = tagList(
                              icon("add"),
                              " ADD NEW"),
                            class = "btn-primary")),
        column(width = 3,
               actionButton(ns("import_esub"),
                            label = tagList(
                              tags$i(class = "fa-solid fa-file-csv"),
                              " Import from excel/csv"),
                            class = "btn-primary"))


      ),

      hidden(div (id = ns("import_form_esub"),
                  column(width = 3,
                         fileInput(ns("file"), "Choose CSV or Excel File",
                                   multiple = FALSE,
                                   accept = c(".csv", ".xlsx"))),

                  column(width = 3,actionButton(ns("file_upload_esub"), "Upload and Append")),
      )),


      hidden(div(id = ns("esub_form"),
                 fluidRow(

                   column(width = 4,
                          textInput(ns("expected_substance_name"),
                                    labelMandatory("Expected Substance Name:"),
                                    value = "",
                                    placeholder = "enter expected substance name"
                          )),
                   column(width = 4,
                          textInput(ns("expected_substance_synonym"),
                                    labelMandatory("Synonym:"),
                                    value = "",
                                    placeholder = "enter synonym"
                          )),




                 fluidRow(
                   column(width = 3,
                          actionButton(ns("submit_esub"),
                                       "Submit",
                                       class = "btn-success")),

                 )

      ))) ,

      layout_columns(

        hidden(
          actionButton(ns("update_esub"),
                       label = tagList(
                         icon("pen"),
                         " Update"),
                       class = "btn-primary")),

        hidden(
          actionButton(ns("delete_esub"),
                       label = tagList(
                         icon("trash-can"),
                         " Delete"),
                       class = "btn-danger")),

        col_widths = c(3,3)),



    fluidRow( style = "border: 1px solid black;",
              column(width = 12,
                     fluidRow(style = "background-color:#EBF5FB;",
                              h5("Table Output...")),
                     div("Please select a row in table if you wish to", tags$i(class="fa-solid fa-pen-to-square"), "UPDATE/EDIT or", tags$i(class="fa-solid fa-trash-can"), "DELETE record"),
                     DT::dataTableOutput(ns("esub_table"))),
              tableOutput(ns("excel")),

    ),

)
)}

#' Expected_Substance_List Server Functions
#'
#' @noRd
mod_Expected_Substance_List_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns


    #-----Setting up mandatory fields------
    mandatory_esub <- c("expected_substance_name")


    ##---hide submit button if mandatory fields are not entered----
    observe({
      # check if all mandatory fields have a value
      mandatory_esub <-
        vapply(mandatory_esub,
               function(x) {
                 !is.null(input[[x]]) && input[[x]] != ""
               },
               logical(1))
      mandatory_esub <- all(mandatory_esub)

      # enable/disable the submit button
      shinyjs::toggleState(id = "submit_esub", condition = mandatory_esub)
    })


    # Function to fetch data from MongoDB
    fetch_data <- function() {
      data <- mongo_expected_sub$find(fields = '{"_id": 1, "expected_substance_name": 1, "expected_substance_synonym": 1}')
      data$`_id` <- as.character(data$`_id`) # Convert ObjectId to character
      return(data)
    }

    # Reactive value to store data
    myData <- reactiveVal(fetch_data())

    # Render DataTable
    output$esub_table <- DT::renderDataTable({
      DT::datatable(myData(), selection = "single", rownames = FALSE)
    })

    # Show/hide form
    observeEvent(input$add_esub, {
      shinyjs::show("esub_form")
      shinyjs::show("submit_esub")
    })

    # Add new technician
    observeEvent(input$submit_esub, {
      shinyjs::hide("esub_form")
      shinyjs::reset("esub_form")
      # Create a new visit document with NULL for missing values
      new_esub <- list(
        expected_substance_name = convert_to_character_or_null(input$expected_substance_name),
        expected_substance_synonym = convert_to_character_or_null(input$expected_substance_synonym)

      )

      # Convert the document to JSON
      new_esub_json <- toJSON(new_esub, auto_unbox = TRUE, na = "null")

      # Insert the new document into the collection
      mongo_expected_sub$insert(new_esub_json)

      # Show popup message for successful deletion
      shinyalert::shinyalert(title = "Success", text = "Expected Substance added successfully!", type = "success")

      # Close MongoDB connection
      mongo_expected_sub$disconnect()

      # Refresh the data
      myData(fetch_data())
    })

    observeEvent(input$esub_table_rows_selected,{
      shinyjs::show(id="esub_form")
      shinyjs::hide(id = "submit_esub")
      shinyjs::show(id="update_esub")
      shinyjs::show(id="delete_esub")
      shinyjs::hide(id = "add_esub")

      selected <- input$esub_table_rows_selected
      if (length(selected)) {
        currentData <- myData()
        selectedID <- currentData[selected, "_id"]

        if (!is.null(selectedID)) {

          # Retrieve selected row's data
          selectedRowData <- currentData[selected, ]

          # Bind selected row's data to input variables
          updateTextInput(session, "expected_substance_name",
                            label = "Expected Substance Name:",
                            value = selectedRowData$expected_substance_name,
                            placeholder = "enter expected substance name")

          updateTextInput(session, "expected_substance_synonym",
                            label = "Synonym:",
                            value = selectedRowData$expected_substance_synonym,
                            placeholder = "enter synonym")

          # Close MongoDB connection
          mongo_expected_sub$disconnect()
        } else {
          print("Selected ID is null for update")
        }
      } else {
        print("No row selected for update")
      }


    })


    # Update selected substance
    observeEvent(input$update_esub, {

      shinyjs::reset("esub_form")
      shinyjs::hide("esub_form")
      shinyjs::hide("update_esub")
      shinyjs::hide("delete_esub")
      shinyjs::show("add_esub")

      selected <- input$esub_table_rows_selected
      if (length(selected)) {
        currentData <- myData()
        selectedID <- currentData[selected, "_id"]


        if (!is.null(selectedID)) {

          # Construct the update document
          updated_esub <- list(
            "$set" = list(
              expected_substance_name = input$expected_substance_name,
              expected_substance_synonym = input$expected_substance_synonym

            )
          )

          # Convert the update document to JSON
          updated_esub_json <- jsonlite::toJSON(updated_esub, auto_unbox = TRUE)

          # Update the document
          mongo_expected_sub$update(query = sprintf('{"_id": {"$oid": "%s"}}', selectedID), update = updated_esub_json)

          # Show popup message for successful deletion
          shinyalert::shinyalert(title = "Success", text = "Expected Substance updated successfully!", type = "success")

          # Close MongoDB connection
          mongo_expected_sub$disconnect()

          # Refresh the data
          myData(fetch_data())
        } else {
          print("Selected ID is null for update")
        }
      } else {
        print("No row selected for update")
      }
    })

    # Delete selected technician
    observeEvent(input$delete_esub, {
      shinyjs::reset("esub_form")
      shinyjs::hide("esub_form")
      shinyjs::hide("update_esub")
      shinyjs::hide("delete_esub")
      shinyjs::show("add_esub")
      selected <- input$esub_table_rows_selected
      if (length(selected)) {
        currentData <- myData()
        selectedID <- currentData[selected, "_id"]


        if (!is.null(selectedID)) {
          mongo_expected_sub$remove(paste0('{"_id": {"$oid": "', selectedID, '"}}'))
          myData(fetch_data())
          # Show popup message for successful deletion
          shinyalert::shinyalert(title = "Success", text = "Expected Substance deleted successfully!", type = "success")
        } else {
          print("Selected ID is null for deletion")
        }
      } else {
        print("No row selected for deletion")
      }
    })


    #----imported tech_data------
    uploaded_esub_data <- reactive({
      req(input$file)
      file <- input$file$datapath

      if (grepl("\\.csv$", input$file$name)) {
        read_csv(file)
      } else if (grepl("\\.xlsx$", input$file$name)) {
        read_excel(file)
      } else {
        NULL
      }
    })


    output$excel <- renderTable({
      req(uploaded_esub_data())
      uploaded_esub_data()
    })

    #---- function to validate the column names of imported tech data----
    validate_esub_data <- function(data, existing_colnames) {
      if (all(names(data) %in% existing_colnames)) {
        return(TRUE)
      } else {
        return(FALSE)
      }
    }




    observeEvent(input$import_esub,{

      shinyjs::reset("esub_form")
      shinyjs::hide("esub_form")
      shinyjs::hide("update_esub")
      shinyjs::hide("delete_esub")
      shinyjs::hide("add_esub")
      shinyjs::show("import_form_esub")

    })

    observeEvent(input$file_upload_esub,{

      shinyjs::reset("esub_form")
      shinyjs::hide("esub_form")
      shinyjs::hide("update_esub")
      shinyjs::hide("delete_esub")
      shinyjs::show("add_esub")
      shinyjs::hide("import_form_esub")

      req(uploaded_esub_data())

      existing_colnames <- c("_id", "expected_substance_name", "expected_substance_synonym")

      if (validate_esub_data(uploaded_esub_data(), existing_colnames)) {
        mongo_expected_sub$insert(uploaded_esub_data())
        mongo_expected_sub$disconnect()
        # Refresh the data
        myData(fetch_data())
        showModal(modalDialog(
          title = "Success",
          "Data has been successfully uploaded and appended to database.",
          easyClose = TRUE,
          footer = NULL
        ))
      }else {
        showModal(modalDialog(
          title = "Error",
          "Uploaded data does not match the required format.",
          easyClose = TRUE,
          footer = NULL
        ))



      }



    })



  })


}

## To be copied in the UI
# mod_Expected_Substance_List_ui("Expected_Substance_List_1")

## To be copied in the server
# mod_Expected_Substance_List_server("Expected_Substance_List_1")
