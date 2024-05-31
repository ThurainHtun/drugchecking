#' ftir_Substance_List UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_ftir_Substance_List_ui <- function(id){
  ns <- NS(id)
  tagList(

    fluidRow(
      shinyjs::useShinyjs(),
      style = "border: solid black 1px;",
      h6(""),
      column(width = 12,
             h6("Please update the FTIR result Substance drop-down list here"),
             style = "background-color:#EBF5FB;"),

      fluidRow(
        h6(" "),
        column(width = 3,
               actionButton(ns("add_sub"),
                            label = tagList(
                              icon("add"),
                              " ADD NEW"),
                            class = "btn-primary")),
        column(width = 3,
               actionButton(ns("import_sub"),
                            label = tagList(
                              tags$i(class = "fa-solid fa-file-csv"),
                              " Import from excel/csv"),
                            class = "btn-primary"))


      ),

      hidden(div (id = ns("import_form_sub"),
                  column(width = 3,
                         fileInput(ns("file"), "Choose CSV or Excel File",
                                   multiple = FALSE,
                                   accept = c(".csv", ".xlsx"))),

                  column(width = 3,actionButton(ns("file_upload_sub"), "Upload and Append")),
      )),


      hidden(div(id = ns("sub_form"),
                 fluidRow(

                   column(width = 4,
                          textInput(ns("substance_name"),
                                    labelMandatory("Substance Name:"),
                                    value = "",
                                    placeholder = "enter substance name"
                          )),
                   column(width = 4,
                          textInput(ns("substance_synonym"),
                                    labelMandatory("Synonym:"),
                                    value = "",
                                    placeholder = "enter synonym"
                          )),




                   fluidRow(
                     column(width = 3,
                            actionButton(ns("submit_sub"),
                                         "Submit",
                                         class = "btn-success")),

                   )

                 ))) ,

      layout_columns(

        hidden(
          actionButton(ns("update_sub"),
                       label = tagList(
                         icon("pen"),
                         " Update"),
                       class = "btn-primary")),

        hidden(
          actionButton(ns("delete_sub"),
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
                       DT::dataTableOutput(ns("sub_table"))),
                tableOutput(ns("excel")),

      ),

    )
  )
}

#' ftir_Substance_List Server Functions
#'
#' @noRd
mod_ftir_Substance_List_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    #-----Setting up mandatory fields------
    mandatory_sub <- c("substance_name")


    ##---hide submit button if mandatory fields are not entered----
    observe({
      # check if all mandatory fields have a value
      mandatory_sub <-
        vapply(mandatory_sub,
               function(x) {
                 !is.null(input[[x]]) && input[[x]] != ""
               },
               logical(1))
      mandatory_sub <- all(mandatory_sub)

      # enable/disable the submit button
      shinyjs::toggleState(id = "submit_sub", condition = mandatory_sub)
    })


    # Function to fetch data from MongoDB
    fetch_data <- function() {
      data <- mongo_ftir_sub$find(fields = '{"_id": 1, "substance_name": 1, "substance_synonym": 1}')
      data$`_id` <- as.character(data$`_id`) # Convert ObjectId to character
      return(data)
    }

    # Reactive value to store data
    myData <- reactiveVal(fetch_data())

    # Render DataTable
    output$sub_table <- DT::renderDataTable({
      DT::datatable(myData(), selection = "single", rownames = FALSE)
    })

    # Show/hide form
    observeEvent(input$add_sub, {
      shinyjs::show("sub_form")
      shinyjs::show("submit_sub")
    })

    # Add new technician
    observeEvent(input$submit_sub, {
      shinyjs::hide("sub_form")
      shinyjs::reset("sub_form")
      # Create a new visit document with NULL for missing values
      new_sub <- list(
        substance_name = convert_to_character_or_null(input$substance_name),
        substance_synonym = convert_to_character_or_null(input$substance_synonym)

      )

      # Convert the document to JSON
      new_sub_json <- toJSON(new_sub, auto_unbox = TRUE, na = "null")

      # Insert the new document into the collection
      mongo_ftir_sub$insert(new_sub_json)

      # Show popup message for successful deletion
      shinyalert::shinyalert(title = "Success", text = "Substance added successfully!", type = "success")

      # Close MongoDB connection
      mongo_ftir_sub$disconnect()

      # Refresh the data
      myData(fetch_data())
    })

    observeEvent(input$sub_table_rows_selected,{
      shinyjs::show(id="sub_form")
      shinyjs::hide(id = "submit_sub")
      shinyjs::show(id="update_sub")
      shinyjs::show(id="delete_sub")
      shinyjs::hide(id = "add_sub")

      selected <- input$sub_table_rows_selected
      if (length(selected)) {
        currentData <- myData()
        selectedID <- currentData[selected, "_id"]

        if (!is.null(selectedID)) {

          # Retrieve selected row's data
          selectedRowData <- currentData[selected, ]

          # Bind selected row's data to input variables
          updateTextInput(session, "substance_name",
                          label = "Substance Name:",
                          value = selectedRowData$substance_name,
                          placeholder = "enter substance name")

          updateTextInput(session, "substance_synonym",
                          label = "Synonym:",
                          value = selectedRowData$substance_synonym,
                          placeholder = "enter synonym")

          # Close MongoDB connection
          mongo_ftir_sub$disconnect()
        } else {
          print("Selected ID is null for update")
        }
      } else {
        print("No row selected for update")
      }


    })


    # Update selected substance
    observeEvent(input$update_sub, {

      shinyjs::reset("sub_form")
      shinyjs::hide("sub_form")
      shinyjs::hide("update_sub")
      shinyjs::hide("delete_sub")
      shinyjs::show("add_sub")

      selected <- input$sub_table_rows_selected
      if (length(selected)) {
        currentData <- myData()
        selectedID <- currentData[selected, "_id"]


        if (!is.null(selectedID)) {

          # Construct the update document
          updated_sub <- list(
            "$set" = list(
              substance_name = input$substance_name,
              substance_synonym = input$substance_synonym

            )
          )

          # Convert the update document to JSON
          updated_sub_json <- jsonlite::toJSON(updated_sub, auto_unbox = TRUE)

          # Update the document
          mongo_ftir_sub$update(query = sprintf('{"_id": {"$oid": "%s"}}', selectedID), update = updated_sub_json)

          # Show popup message for successful deletion
          shinyalert::shinyalert(title = "Success", text = "Substance updated successfully!", type = "success")

          # Close MongoDB connection
          mongo_ftir_sub$disconnect()

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
    observeEvent(input$delete_sub, {
      shinyjs::reset("sub_form")
      shinyjs::hide("sub_form")
      shinyjs::hide("update_sub")
      shinyjs::hide("delete_sub")
      shinyjs::show("add_sub")
      selected <- input$sub_table_rows_selected
      if (length(selected)) {
        currentData <- myData()
        selectedID <- currentData[selected, "_id"]


        if (!is.null(selectedID)) {
          mongo_ftir_sub$remove(paste0('{"_id": {"$oid": "', selectedID, '"}}'))
          myData(fetch_data())
          # Show popup message for successful deletion
          shinyalert::shinyalert(title = "Success", text = "Substance deleted successfully!", type = "success")
        } else {
          print("Selected ID is null for deletion")
        }
      } else {
        print("No row selected for deletion")
      }
    })


    #----imported tech_data------
    uploaded_sub_data <- reactive({
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
      req(uploaded_sub_data())
      uploaded_sub_data()
    })

    #---- function to validate the column names of imported tech data----
    validate_sub_data <- function(data, existing_colnames) {
      if (all(names(data) %in% existing_colnames)) {
        return(TRUE)
      } else {
        return(FALSE)
      }
    }




    observeEvent(input$import_sub,{

      shinyjs::reset("sub_form")
      shinyjs::hide("sub_form")
      shinyjs::hide("update_sub")
      shinyjs::hide("delete_sub")
      shinyjs::hide("add_sub")
      shinyjs::show("import_form_sub")

    })

    observeEvent(input$file_upload_sub,{

      shinyjs::reset("sub_form")
      shinyjs::hide("sub_form")
      shinyjs::hide("update_sub")
      shinyjs::hide("delete_sub")
      shinyjs::show("add_sub")
      shinyjs::hide("import_form_sub")

      req(uploaded_sub_data())

      existing_colnames <- c("_id", "substance_name", "substance_synonym")

      if (validate_sub_data(uploaded_sub_data(), existing_colnames)) {
        mongo_ftir_sub$insert(uploaded_sub_data())
        mongo_ftir_sub$disconnect()
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
# mod_ftir_Substance_List_ui("ftir_Substance_List_1")

## To be copied in the server
# mod_ftir_Substance_List_server("ftir_Substance_List_1")
