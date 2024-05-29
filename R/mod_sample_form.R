#' sample_form UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
#'



mod_sample_form_ui <- function(id){
  ns <- NS(id)
  tagList(

    shinyjs::useShinyjs(),

    fluidRow(id = ns("search_bar"),
    layout_columns(
             textInput(ns("vs_id_input"), "Search by Visit ID:",value = ""),
             DT::dataTableOutput(ns("data_table")),
             col_widths = c(3,9),
      )),


    layout_columns(
     column(width = 6,
            shinyjs::hidden( actionButton(ns("sp_add"), label = tagList(
               icon("add"),"ADD NEW SAMPLE"), class = "btn-primary")),
             col_widths = c(2),
      )),

      shinyjs::hidden(

      div(id = ns("all_sample_form"),
      fluidRow(
      layout_columns(
        layout_columns(
          textInput(ns("sample_id"),
                    label = "Sample ID:",
                    value = ""),
          selectInput(ns("expected_sub"),
                      label = "Expected Substance:",
                      choices = NULL),
          textInput(ns("substance_name"),
                    label = "Name of Substance:",
                    value = ""),
          selectInput(ns("form"),
                      label = "Form of Substance:",
                      choices = NULL),
          selectInput(ns("color"),
                      label = "Color of Substance:",
                      choices = NULL),
          radioButtons(
            ns("used_substance"),
            label = "Have you already used/consumed this substance?",
            choices = c("Yes", "No", "Not Selected"),
            selected = "Not Selected",
            inline = TRUE
          ),
          selectInput(ns("unusual_effect"),
                      label = "If yes, could you comment on any unexpected or unusual effects it may have caused?",
                      choices = NULL),
          Toggle.shinyInput(
            ns("alert"),
            label = "Alert Recommended",
            value = FALSE
          ),
          Toggle.shinyInput(
            ns("flag"),
            label = "Flag for review",
            value = FALSE
          ),

          fluidRow(
            column(
              width = 12,
              radioButtons(
                ns("sample"),
                label = "Sample: Returning it or disposing of it?",
                choices = c(
                  "Client acknowledges sample is returned to client",
                  "Client acknowledges sample destroyed (process witnessed)",
                  "If the sample was secured by Team Manager for turning over to enforcement services/log of substances left behind for third-party disposal started",
                  "Not Selected"
                ),
                selected =  "Not Selected",
                width = "200%"
              )
            ),
          ),

          col_widths = c(12,3,3,3,3,6,6,6,6,12)
        ),



  layout_columns(

    Toggle.shinyInput(ns("ftir"),
                      label = "FTIR testing completed?",
                      value = FALSE),

    selectInput(ns("comp1_cat"),
                label = "Component Name",
                choices = NULL),

    textInput(ns("comp1_dat"),
              label = "Value",
              value = ""),


    h6("%"),

    selectInput(ns("comp2_cat"),

                label = NULL,
                choices = NULL),

    textInput("comp2_dat",
              label = NULL,
              value = ""),


    h6("%"),

    selectInput(ns("comp3_cat"),
                label = NULL,
                choices = NULL),

    textInput(ns("comp3_dat"),
              label = NULL,
              value = ""),


    h6("%"),

    selectInput(ns("comp4_cat"),

                label = NULL,
                choices = NULL),

    textInput(ns("comp4_dat"),
              label = NULL,
              value = ""),


    h6("%"),


    selectInput(ns("comp5_cat"),

                label = NULL,
                choices = NULL),

    textInput("comp5_dat",
              label = NULL,
              value = ""),


    h6("%"),


    selectInput(ns("comp6_cat"),

                label = NULL,
                choices = NULL),

    textInput("comp6_dat",
              label = NULL,
              value = ""),


    h6("%"),


    fluidRow(

      column(width = 12,

             radioButtons(
               ns("benzodiazepine"),
               label = "Benzodiazepine Test Strip*",
               choices = c("Positive", "Negative","Indeterminate", "Not Included"),
               selected = "Not Included",
               inline = TRUE
             )
      ),
    ),#Benzodiazepine Test Strip

    fluidRow(

      column(width = 12,

             radioButtons(
               ns("Fentanyl"),
               label = "Fentanyl Test Strip*	",
               choices = c("Positive", "Negative","Indeterminate", "Not Included"),
               selected = "Not Included",
               inline = TRUE
             )
      ),
    ),#Fentanyl Test Strip*


    col_widths = c(2,6,3,1,8,3,1,8,3,1,8,3,1,8,3,1,8,3,1,12,12)
  ),





  col_widths = c(6,6),
      ),

    ),

  fluidRow(

    layout_columns(

      layout_columns(
        h6(style = "font-weight: bold;",
           "Based on the result, what will you do with your drug? "),
        checkboxInput(ns("take_as_intended"), "Take as intended", FALSE),
        checkboxInput(ns("take_more"), "Take more", FALSE),
        checkboxInput(ns("take_less"), "Take less", FALSE),
        checkboxInput(ns("change_supplier"), "Change supplier", FALSE),
        checkboxInput(ns("dispose"), "Dispose of the drug", FALSE),
        checkboxInput(ns("use_with_fri"), "Use with a friend", FALSE),
        checkboxInput(ns("change_route"), "Change route (e.g. IV to inhalation)", FALSE),
        checkboxInput(ns("decision_other"), "Other: ", FALSE),
        checkboxInput(ns("decision_na"), "NA ", FALSE),
        col_widths = c(12,4,4,4,4,4,4,4,4,4)

      ),


      layout_columns(
        fileInput(
          ns("ftir_res_file"),
          "Please Upload FTIR result file here",
          multiple = FALSE,
          accept = NULL,
          width = NULL,
          buttonLabel = "Browse...",
          placeholder = "No file selected"
        ),

        textInput(ns("time_out"),
                  label = "Time Out (HH:MM:SS)",
                  value = paste0(format(Sys.time(), "%H:%M:%S"))),

        textAreaInput(
          ns("comments"),
          "Comments",
          width = "200%"
        ),
        col_widths = c(6,3,12)),



      col_widths = c(6,6)
    ),


    layout_columns(
      column(width = 6,
             actionButton(ns("sp_submit"), label = tagList(icon("paper-plane"),"Submit"), class = "btn-primary"),
             actionButton(ns("sp_update"), label =  tagList(icon("pen"),"Update"), class = "btn-primary"),
             actionButton(ns("sp_delete"), label = tagList(icon("trash-can"),"Delete"), class = "btn-primary", style = "background-color:#DE3163;"),
             col_widths = c(3,3,3)),
    )

  ), #end of fluid row



  )), # end of hidden sample form

  fluidRow(id = ns("table_box"),
           layout_columns(
                  h4("Table Output of Samples"),
                  div("Please select a row in table if you wish to", tags$i(class="fa-solid fa-pen-to-square"), "UPDATE/EDIT or", tags$i(class="fa-solid fa-trash-can"), "DELETE record"),
                  DT::dataTableOutput(ns("sample_table")),
           col_widths = c(12,12,12)
  )),


  )

}

#' sample_form Server Functions
#'
#' @noRd
mod_sample_form_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    shinyjs::useShinyjs()


    # refresh_visit <- function() {
    #
    #   # Retrieve data from MongoDB
    #   data <- mongo_visit$find('{}')
    #
    #   return(data)
    # }
    #
    # data <- reactiveVal(refresh_visit())
    #
    # # Function to automatically refresh data from MongoDB
    # auto_refresh <- function() {
    #   invalidateLater(5000, session)
    #   data(refresh_visit())
    # }
    #
    # # Refresh data every 5 seconds
    # observe({
    #   auto_refresh()
    # })
    #
    #
    # output$visit_table <- DT::renderDataTable({
    #   data()
    # })
    #

    data <- reactive({
      vs_id <- input$vs_id_input

      if (nchar(vs_id) == 0) {
        return(NULL)
      }


      # Query MongoDB collection
      query <- paste('{"vs_id": "', vs_id, '"}', sep = "")
      data <- mongo_visit$find(query)

      if (nrow(data) > 0) {
        shinyjs::show("sp_add")
      } else {
        shinyjs::hide("sp_add")
      }

      return(data)
    })



    # Render data table
    output$data_table <- DT::renderDataTable({
      data()
    },
    options = list(
      dom = 't',
      pageLength = 1
    ), selection = list(
      mode = 'single',       # Single row selection
      selected = 1           # Automatically select the first row

    ))




    observeEvent(input$sp_add,{
      shinyjs::show("all_sample_form")

    })


    # Function to fetch data from MongoDB
    fetch_data <- function() {
      data <- mongo_sample$find(fields = '{"_id": 1,
                                "vs_id_input": 1,
                                "sample_id": 1,
                                "expected_sub": 1,
                                "substance_name": 1,
                                "form": 1,
                                "color": 1,
                                "used_substance": 1,
                                "unusual_effect": 1}')
      data$`_id` <- as.character(data$`_id`)
      return(data)
    }

    # Reactive value to store data
    myData <- reactiveVal(fetch_data())

    # Render DataTable
    output$sample_table <- DT::renderDataTable({
      DT::datatable(myData(), selection = "single", rownames = FALSE,
                    options = list(scrollX = TRUE,
                    autoWidth = TRUE))
    })


    #-------SUBMIT NEW DATA------
    observeEvent(input$sp_submit,{
      shinyjs::hide("all_sample_form")
      shinyjs::reset("all_sample_form")



      # Create a new visit document with NULL for missing values
      new_sample <- list(
        vs_id_input = convert_to_character_or_null(input$vs_id_input),
        sample_id = convert_to_character_or_null(input$sample_id),
        expected_sub = convert_to_character_or_null(input$expected_sub),
        substance_name = convert_to_character_or_null(input$substance_name),
        form = convert_to_character_or_null(input$form),
        color = convert_to_character_or_null(input$color),
        used_substance = convert_to_character_or_null(input$used_substance),
        unusual_effect = convert_to_character_or_null(input$unusual_effect)
      )

      # Convert the document to JSON
      new_sample_json <- toJSON(new_sample, auto_unbox = TRUE, na = "null")

      # Insert the new document into the collection
      mongo_sample$insert(new_sample_json)

      # Show popup message for successful deletion
      shinyalert::shinyalert(title = "Success", text = "sample added successfully!", type = "success")

      # Close MongoDB connection
      mongo_sample$disconnect()

      # Refresh the data
      myData(fetch_data())
    })




  })
}

## To be copied in the UI
# mod_sample_form_ui("sample_form_1")

## To be copied in the server
# mod_sample_form_server("sample_form_1")
