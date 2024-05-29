#' technician_list UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#' @import shiny
#' @import shinyjs
#' @import htmltools
#' @importFrom shiny NS tagList
#'


mod_technician_list_ui <- function(id){
  ns <- NS(id)
  tagList(


    fluidRow(
      shinyjs::useShinyjs(),
      style = "border: solid black 1px;",
              h6(""),
              column(width = 12,
                     h6("Please update the technician drop-down list here"),
                     style = "background-color:#EBF5FB;"),

              fluidRow(
                h6(" "),
                column(width = 3,
                       actionButton("add_tech",
                                    label = tagList(
                                      icon("add"),
                                      " ADD NEW"),
                                    class = "btn-primary")),
                column(width = 3,
                       actionButton("import_tech",
                                    label = tagList(
                                      tags$i(class = "fa-solid fa-file-csv"),
                                      " Import from excel/csv"),
                                    class = "btn-primary"))


              ),

               hidden(div (id = "import_form_tech",
                           column(width = 3,
                                  fileInput("file", "Choose CSV or Excel File",
                                            multiple = FALSE,
                                            accept = c(".csv", ".xlsx"))),

                           column(width = 3,actionButton("file_upload_tech", "Upload and Append")),
              )),


              hidden(div(id = "technician_form",
                          fluidRow(

                            column(width = 4,
                                   textInput("technician_name",
                                             labelMandatory("Technician Name:"),
                                             value = "",
                                             placeholder = "enter technician name"
                                   )),
                            column(width = 4,
                                   textInput("city",
                                             labelMandatory("City:"),
                                             value = "",
                                             placeholder = "enter city name"
                                   )),

                            column(width = 4,
                                   textInput("site",
                                             labelMandatory("Site:"),
                                             value = "",
                                             placeholder = "enter site name"
                                   ))),


                          fluidRow(
                            column(width = 3,
                                   actionButton("submit_tech",
                                                "Submit",
                                                class = "btn-success")),

                          )

              )) ,

              layout_columns(

                  hidden(
                  actionButton("update_tech",
                               label = tagList(
                                 icon("pen"),
                                 " Update"),
                               class = "btn-warning")),

                  hidden(
                  actionButton("delete_tech",
                               label = tagList(
                                 icon("trash-can"),
                                 " Delete"),
                               class = "btn-danger")),

                col_widths = c(3,3)),
    ),


    fluidRow( style = "border: 1px solid black;",
              column(width = 12,
                     fluidRow(style = "background-color:#EBF5FB;",
                              h5("Table Output...")),
                     div("Please select a row in table if you wish to", tags$i(class="fa-solid fa-pen-to-square"), "UPDATE/EDIT or", tags$i(class="fa-solid fa-trash-can"), "DELETE record"),
                     DT::dataTableOutput(ns("technician_table"))),
              tableOutput(ns("excel")),

  )
)}

#' technician_list Server Functions
#'
#' @noRd
mod_technician_list_server <- function(id){
  moduleServer( id, function(input, output, session){

    ns <- session$ns
    # Connect to MongoDB
    mongo <- mongo(collection = "technician", db = "drug_checking", url = "mongodb+srv://drthurain07:XtZXZxxgbNB3PT0q@cluster0.cmm67ct.mongodb.net/")


    #-----Setting up mandatory fields------
    mandatory_technician <- c("technician_name",
                              "city",
                              "site")



    ##---hide submit button if mandatory fields are not entered----
    observe({
      # check if all mandatory fields have a value
      mandatory_technician <-
        vapply(mandatory_technician,
               function(x) {
                 !is.null(input[[x]]) && input[[x]] != ""
               },
               logical(1))
      mandatory_technician <- all(mandatory_technician)

      # enable/disable the submit button
      shinyjs::toggleState(id = "submit_tech", condition = mandatory_technician)
    })


    # Function to fetch data from MongoDB
    fetch_data <- function() {
      data <- mongo$find(fields = '{"_id": 1, "technician_name": 1, "city": 1, "site": 1}')
      data$`_id` <- as.character(data$`_id`) # Convert ObjectId to character
      return(data)
    }

    # Reactive value to store data
    myData <- reactiveVal(fetch_data())

    # Render DataTable
    output$technician_table <- DT::renderDataTable({
      DT::datatable(myData(), selection = "single", rownames = FALSE)
    })

    # Show/hide form
    observeEvent(input$add_tech, {
      shinyjs::show("technician_form")
      shinyjs::show("submit_tech")
    })

    # Add new technician
    observeEvent(input$submit_tech, {
      shinyjs::hide("technician_form")
      shinyjs::reset("technician_form")
      # Capture input values
      new_technician_name <- input$technician_name
      new_city <- input$city
      new_site <- input$site

      # Debug print statements
      print(paste("New Technician Name:", new_technician_name))
      print(paste("New City:", new_city))
      print(paste("New Site:", new_site))


      # Construct the new document to insert
      new_technician <- list(
        technician_name = new_technician_name,
        city = new_city,
        site = new_site
      )

      # Insert the new document into the collection
      mongo$insert(new_technician)

      # Show popup message for successful deletion
      shinyalert::shinyalert(title = "Success", text = "Technician added successfully!", type = "success")

      # Close MongoDB connection
      mongo$disconnect()

      # Refresh the data
      myData(fetch_data())
    })

    observeEvent(input$technician_table_rows_selected,{
      shinyjs::show(id="technician_form")
      shinyjs::hide(id = "submit_tech")
      shinyjs::show(id="update_tech")
      shinyjs::show(id="delete_tech")
      shinyjs::hide(id = "add_tech")

      selected <- input$technician_table_rows_selected
      if (length(selected)) {
        currentData <- myData()
        selectedID <- currentData[selected, "_id"]

        # Debug print statements
        print(paste("Selected ID for update:", selectedID))
        print(str(currentData))

        if (!is.null(selectedID)) {

          # Retrieve selected row's data
          selectedRowData <- currentData[selected, ]

          # Bind selected row's data to input variables
          updateTextInput(session, "technician_name", value = selectedRowData$technician_name)
          updateTextInput(session, "city", value = selectedRowData$city)
          updateTextInput(session, "site", value = selectedRowData$site)

          # Close MongoDB connection
          mongo$disconnect()
        } else {
          print("Selected ID is null for update")
        }
      } else {
        print("No row selected for update")
      }


    })


    # Update selected technician
    observeEvent(input$update_tech, {

      shinyjs::reset("technician_form")
      shinyjs::hide("technician_form")
      shinyjs::hide("update_tech")
      shinyjs::hide("delete_tech")
      shinyjs::show("add_tech")

      selected <- input$technician_table_rows_selected
      if (length(selected)) {
        currentData <- myData()
        selectedID <- currentData[selected, "_id"]

        # Debug print statements
        print(paste("Selected ID for update:", selectedID))
        print(str(currentData))

        if (!is.null(selectedID)) {


          # Construct the update document
          updated_tech <- list(
            "$set" = list(
              technician_name = input$technician_name,
              city = input$city,
              site = input$site
            )
          )

          # Convert the update document to JSON
          updated_tech_json <- jsonlite::toJSON(updated_tech, auto_unbox = TRUE)

          # Update the document
          mongo$update(query = sprintf('{"_id": {"$oid": "%s"}}', selectedID), update = updated_tech_json)

          # Show popup message for successful deletion
          shinyalert::shinyalert(title = "Success", text = "Technician updated successfully!", type = "success")

          # Close MongoDB connection
          mongo$disconnect()

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
    observeEvent(input$delete_tech, {
      shinyjs::reset("technician_form")
      shinyjs::hide("technician_form")
      shinyjs::hide("update_tech")
      shinyjs::hide("delete_tech")
      shinyjs::show("add_tech")
      selected <- input$technician_table_rows_selected
      if (length(selected)) {
        currentData <- myData()
        selectedID <- currentData[selected, "_id"]

        # Debug print statements
        print(paste("Selected ID for deletion:", selectedID))
        print(str(currentData))

        if (!is.null(selectedID)) {
          mongo$remove(paste0('{"_id": {"$oid": "', selectedID, '"}}'))
          myData(fetch_data())
          # Show popup message for successful deletion
          shinyalert::shinyalert(title = "Success", text = "Technician deleted successfully!", type = "success")
        } else {
          print("Selected ID is null for deletion")
        }
      } else {
        print("No row selected for deletion")
      }
    })


    #----imported tech_data------
    uploaded_tech_data <- reactive({
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
      req(uploaded_tech_data())
      uploaded_tech_data()
    })

    #---- function to validate the column names of imported tech data----
    validate_tech_data <- function(data, existing_colnames) {
      if (all(names(data) %in% existing_colnames)) {
        return(TRUE)
      } else {
        return(FALSE)
      }
    }




    observeEvent(input$import_tech,{

      shinyjs::reset("technician_form")
      shinyjs::hide("technician_form")
      shinyjs::hide("update_tech")
      shinyjs::hide("delete_tech")
      shinyjs::hide("add_tech")
      shinyjs::show("import_form_tech")

    })

    observeEvent(input$file_upload_tech,{

      shinyjs::reset("technician_form")
      shinyjs::hide("technician_form")
      shinyjs::hide("update_tech")
      shinyjs::hide("delete_tech")
      shinyjs::show("add_tech")
      shinyjs::hide("import_form_tech")

      req(uploaded_tech_data())

      existing_colnames <- c("_id", "technician_name", "city", "site")

      if (validate_tech_data(uploaded_tech_data(), existing_colnames)) {
        mongo$insert(uploaded_tech_data())
        mongo$disconnect()
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
# mod_technician_list_ui("technician_list_1")

## To be copied in the server
# mod_technician_list_server("technician_list_1")
