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
             textInput(ns("vs_id_input"), "To add new sample, please enter Visit ID here:",value = ""),
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
          selectizeInput(ns("expected_sub"),
                      label = "Expected Substance:",
                      choices = NULL,
                      selected = NULL,
                      options = list(placeholder = 'Search for an option') ),
          textInput(ns("substance_name"),
                    label = "Name of Substance:",
                    value = ""),
          selectInput(ns("form"),
                      label = "Form of Substance:",
                      choices = c(
                        "--select--",
                        "Powder",
                        "Crystal(s)",
                        "Pill/tablet",
                        "Capsuels (Note Content)",
                        "Blotter",
                        "Liquid",
                        "Others (Specify)")),
          selectInput(ns("color"),
                      label = "Color of Substance:",
                      choices = c(
                        "--select--",
                        "Red",
                        "Pink",
                        "Yellow",
                        "Orange",
                        "Purple",
                        "Blue",
                        "Green",
                        "Grey",
                        "White",
                        "Beige",
                        "Others (Specify)")),
          radioButtons(
            ns("used_substance"),
            label = "Have you already used/consumed this substance?",
            choices = c("Yes", "No", "Not Selected"),
            selected = "Not Selected",
            inline = TRUE
          ),
          selectizeInput(ns("unusual_effect"),
                      label = "If yes, could you comment on any unexpected or unusual effects it may have caused?",
                      choices = c("Chest Pain", "Diziness", "Tremors", "Dissociated", "Sweating"),
                      multiple = TRUE,
                      options = list(
                        placeholder = 'Select tags...',
                        maxItems = NULL,
                        create = TRUE
                      )
          ),
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

          col_widths = c(4,4,4,6,6,6,6,6,6,12)
        ),



  layout_columns(

    Toggle.shinyInput(ns("ftir"),
                      label = "FTIR testing completed?",
                      value = FALSE),

    selectizeInput(ns("comp1_cat"),
                label = "Component Name",
                choices = NULL,
                selected = NULL,
                options = list(placeholder = 'Search for an option') ),

    textInput(ns("comp1_dat"),
              label = "Value",
              value = ""),


    h6("%"),

    selectizeInput(ns("comp2_cat"),

                label = NULL,
                choices = NULL,
                selected = NULL,
                options = list(placeholder = 'Search for an option') ),

    textInput(ns("comp2_dat"),
              label = NULL,
              value = ""),


    h6("%"),

    selectizeInput(ns("comp3_cat"),
                label = NULL,
                choices = NULL,
                selected = NULL,
                options = list(placeholder = 'Search for an option') ),

    textInput(ns("comp3_dat"),
              label = NULL,
              value = ""),


    h6("%"),

    selectizeInput(ns("comp4_cat"),

                label = NULL,
                choices = NULL,
                selected = NULL,
                options = list(placeholder = 'Search for an option') ),

    textInput(ns("comp4_dat"),
              label = NULL,
              value = ""),


    h6("%"),


    selectizeInput(ns("comp5_cat"),

                label = NULL,
                choices = NULL,
                selected = NULL,
                options = list(placeholder = 'Search for an option') ),

    textInput(ns("comp5_dat"),
              label = NULL,
              value = ""),


    h6("%"),


    selectizeInput(ns("comp6_cat"),

                label = NULL,
                choices = NULL,
                selected = NULL,
                options = list(placeholder = 'Search for an option') ),

    textInput(ns("comp6_dat"),
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

    #-----updating dropdown and option list ----

    observe({
      distinct_esub <- fetch_distinct_values("expected_sub", "expected_substance_name")
      updateSelectInput(session,
                        "expected_sub",
                        label = "Expected Substance:",
                        choices = distinct_esub,
                        selected = character(0))
    })


    observe({
      distinct_sub <- fetch_distinct_values("ftir_sub", "substance_name")
      updateSelectInput(session,
                        "comp1_cat",
                        label = NULL,
                        choices = distinct_sub,
                        selected = character(0))
      updateSelectInput(session,
                        "comp2_cat",
                        label = NULL,
                        choices = distinct_sub,
                        selected = character(0))
      updateSelectInput(session,
                        "comp3_cat",
                        label = NULL,
                        choices = distinct_sub,
                        selected = character(0))
      updateSelectInput(session,
                        "comp4_cat",
                        label = NULL,
                        choices = distinct_sub,
                        selected = character(0))
      updateSelectInput(session,
                        "comp5_cat",
                        label = NULL,
                        choices = distinct_sub,
                        selected = character(0))
      updateSelectInput(session,
                        "comp6_cat",
                        label = NULL,
                        choices = distinct_sub,
                        selected = character(0))
    })





    shinyjs::useShinyjs()



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
      shinyjs::show("sp_submit")
      # Reactive timer to update time every second
      observe({
        invalidateLater(1000, session)

        current_time <- format(Sys.time(), "%H:%M:%S")

        updateTextInput(session,"time_out",
                        label = "Time Out (HH:MM:SS)",
                        value = paste0(current_time))
      })

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
                                "unusual_effect": 1,
                                "alert": 1,
                                "flag": 1,
                                "sample": 1,
                                "ftir": 1,
                                "comp1_cat": 1,
                                "comp1_dat": 1,
                                "comp2_cat": 1,
                                "comp2_dat": 1,
                                "comp3_cat": 1,
                                "comp3_dat": 1,
                                "comp4_cat": 1,
                                "comp4_dat": 1,
                                "comp5_cat": 1,
                                "comp5_dat": 1,
                                "comp6_cat": 1,
                                "comp6_dat": 1,
                                "benzodiazepine": 1,
                                "Fentanyl": 1,
                                "take_as_intended": 1,
                                "take_more": 1,
                                "take_less": 1,
                                "change_supplier": 1,
                                "dispose": 1,
                                "use_with_fri": 1,
                                "change_route": 1,
                                "decision_other": 1,
                                "decision_na" : 1,
                                "time_out": 1,
                                "comments": 1
                                }')
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
        unusual_effect = convert_to_character_or_null(input$unusual_effect),
        alert=convert_to_character_or_null(input$alert),
        flag=convert_to_character_or_null(input$flag),
        sample=convert_to_character_or_null(input$sample),
        ftir=convert_to_character_or_null(input$ftir),
        comp1_cat=convert_to_character_or_null(input$comp1_cat),
        comp1_dat=convert_to_character_or_null(input$comp1_dat),
        comp2_cat=convert_to_character_or_null(input$comp2_cat),
        comp2_dat=convert_to_character_or_null(input$comp2_dat),
        comp3_cat=convert_to_character_or_null(input$comp3_cat),
        comp3_dat=convert_to_character_or_null(input$comp3_dat),
        comp4_cat=convert_to_character_or_null(input$comp4_cat),
        comp4_dat=convert_to_character_or_null(input$comp4_dat),
        comp5_cat=convert_to_character_or_null(input$comp5_cat),
        comp5_dat=convert_to_character_or_null(input$comp5_dat),
        comp6_cat=convert_to_character_or_null(input$comp6_cat),
        comp6_dat=convert_to_character_or_null(input$comp6_dat),
        benzodiazepine=convert_to_character_or_null(input$benzodiazepine),
        Fentanyl=convert_to_character_or_null(input$Fentanyl),
        take_as_intended=convert_to_character_or_null(input$take_as_intended),
        take_more=convert_to_character_or_null(input$take_more),
        take_less=convert_to_character_or_null(input$take_less),
        change_supplier=convert_to_character_or_null(input$change_supplier),
        dispose=convert_to_character_or_null(input$dispose),
        use_with_fri=convert_to_character_or_null(input$use_with_fri),
        change_route=convert_to_character_or_null(input$change_route),
        decision_other=convert_to_character_or_null(input$decision_other),
        decision_na =convert_to_character_or_null(input$decision_na ),
        time_out=convert_to_character_or_null(input$time_out),
        comments=convert_to_character_or_null(input$comments)

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


    #---- Action when table row is selected----

    observeEvent(input$sample_table_rows_selected,{
      shinyjs::show(id="all_sample_form")
      shinyjs::hide(id = "sp_submit")
      shinyjs::show(id="sp_update")
      shinyjs::show(id="sp_delete")
      shinyjs::hide(id = "sp_add")


      selected <- input$sample_table_rows_selected
      if (length(selected)) {
        currentData <- myData()
        selectedID <- currentData[selected, "_id"]

        if (!is.null(selectedID)) {

          # Retrieve selected row's data
          selectedRowData <- currentData[selected, ]

          # Bind selected row's data to input variables
          updateTextInput(session, "vs_id_input", "To add new sample, please enter Visit ID here:",value = selectedRowData$vs_id_input)
          updateTextInput(session, "sample_id",label = "Sample ID:",value = selectedRowData$sample_id)
          updateSelectizeInput(session, "expected_sub",
                         label = "Expected Substance:",
                         selected = selectedRowData$expected_sub)
          updateTextInput(session, "substance_name",
                    label = "Name of Substance:",
                    value = selectedRowData$substance_name)
          updateSelectInput(session,"form",
                      label = "Form of Substance:",
                      selected = selectedRowData$form)
          updateSelectInput(session,"color",
                            label = "Color of Substance:",
                            selected = selectedRowData$color)
          updateRadioButtons(
            session, "used_substance",
            label = "Have you already used/consumed this substance?",
            selected = selectedRowData$used_substance)
          updateSelectizeInput(session, "unusual_effect",
                         label = "If yes, could you comment on any unexpected or unusual effects it may have caused?",
                         selected = unlist(selectedRowData$unusual_effect))

          updateToggle.shinyInput(session,"alert",
            label = "Alert Recommended",
            value = as.logical(selectedRowData$alert))
          updateToggle.shinyInput(session,"flag",
            label = "Flag for review",
            value = as.logical(selectedRowData$flag ))
          updateRadioButtons(session,"sample",
            label = "Sample: Returning it or disposing of it?",
            selected =  selectedRowData$sample)
          updateToggle.shinyInput(session, "ftir",
                            label = "FTIR testing completed?",
                            value = as.logical(selectedRowData$ftir))
          updateSelectizeInput(session, "comp1_cat",
                         label = "Component Name",
                         selected = selectedRowData$comp1_cat)
          updateTextInput(session,"comp1_dat",
                    label = "Value",
                    value = selectedRowData$comp1_dat)
          updateSelectizeInput(session, "comp2_cat",

                               selected = selectedRowData$comp2_cat)
          updateTextInput(session,"comp2_dat",

                          value = selectedRowData$comp2_dat)
          updateSelectizeInput(session, "comp3_cat",

                               selected = selectedRowData$comp3_cat)
          updateTextInput(session,"comp3_dat",

                          value = selectedRowData$comp3_dat)
          updateSelectizeInput(session, "comp4_cat",

                               selected = selectedRowData$comp4_cat)
          updateTextInput(session,"comp4_dat",

                          value = selectedRowData$comp4_dat)
          updateSelectizeInput(session, "comp5_cat",

                               selected = selectedRowData$comp5_cat)
          updateTextInput(session,"comp5_dat",

                          value = selectedRowData$comp5_dat)
          updateSelectizeInput(session, "comp6_cat",

                               selected = selectedRowData$comp6_cat)
          updateTextInput(session,"comp6_dat",

                          value = selectedRowData$comp6_dat)
          updateRadioButtons(session,
            "benzodiazepine",
            label = "Benzodiazepine Test Strip*",
            selected = selectedRowData$benzodiazepine)
          updateRadioButtons(session,
            "Fentanyl",
            label = "Fentanyl Test Strip*	",
            selected = selectedRowData$Fentanyl)
          updateCheckboxInput(session,"take_as_intended", "Take as intended",  value = as.logical(selectedRowData$take_as_intended))
          updateCheckboxInput(session,"take_more", "Take more", value = as.logical(selectedRowData$take_more))
          updateCheckboxInput(session,"take_less", "Take less", value = as.logical(selectedRowData$take_less))
          updateCheckboxInput(session,"change_supplier", "Change supplier", value = as.logical(selectedRowData$change_supplier))
          updateCheckboxInput(session,"dispose", "Dispose of the drug", value = as.logical(selectedRowData$dispose))
          updateCheckboxInput(session,"use_with_fri", "Use with a friend", value = as.logical(selectedRowData$use_with_fri))
          updateCheckboxInput(session,"change_route", "Change route (e.g. IV to inhalation)", value = as.logical(selectedRowData$change_route))
          updateCheckboxInput(session,"decision_other", "Other: ", value = as.logical(selectedRowData$decision_other))
          updateCheckboxInput(session,"decision_na", "NA ", value = as.logical(selectedRowData$decision_na))
          updateTextInput(session,"time_out",
                   label = "Time Out (HH:MM:SS)",
                   value = selectedRowData$time_out)
          updateTextAreaInput(session,
            "comments",
            "Comments",
            value = selectedRowData$comments)

          # Close MongoDB connection
          mongo_sample$disconnect()
        } else {
          print("Selected ID is null for update")
        }
      } else {
        print("No row selected for update")
      }


    })



    #------UPDATE DATA-----
    # Update selected technician
    observeEvent(input$sp_update, {

      shinyjs::reset("all_sample_form")
      shinyjs::hide("all_sample_form")
      shinyjs::hide("sp_update")
      shinyjs::hide("sp_delete")
      shinyjs::show("sp_add")


      selected <- input$sample_table_rows_selected
      if (length(selected)) {
        currentData <- myData()
        selectedID <- currentData[selected, "_id"]

        if (!is.null(selectedID)) {

          # Construct the update document
          updated_sample <- list(
            "$set" = list(
              vs_id_input=as.character(input$vs_id_input),
              sample_id=as.character(input$sample_id),
              expected_sub=as.character(input$expected_sub),
              substance_name=as.character(input$substance_name),
              form=as.character(input$form),
              color=as.character(input$color),
              used_substance=as.character(input$used_substance),
              unusual_effect=as.character(input$unusual_effect),
              alert=as.character(input$alert),
              flag=as.character(input$flag),
              sample=as.character(input$sample),
              ftir=as.character(input$ftir),
              comp1_cat=as.character(input$comp1_cat),
              comp1_dat=as.character(input$comp1_dat),
              comp2_cat=as.character(input$comp2_cat),
              comp2_dat=as.character(input$comp2_dat),
              comp3_cat=as.character(input$comp3_cat),
              comp3_dat=as.character(input$comp3_dat),
              comp4_cat=as.character(input$comp4_cat),
              comp4_dat=as.character(input$comp4_dat),
              comp5_cat=as.character(input$comp5_cat),
              comp5_dat=as.character(input$comp5_dat),
              comp6_cat=as.character(input$comp6_cat),
              comp6_dat=as.character(input$comp6_dat),
              benzodiazepine=as.character(input$benzodiazepine),
              Fentanyl=as.character(input$Fentanyl),
              take_as_intended=as.character(input$take_as_intended),
              take_more=as.character(input$take_more),
              take_less=as.character(input$take_less),
              change_supplier=as.character(input$change_supplier),
              dispose=as.character(input$dispose),
              use_with_fri=as.character(input$use_with_fri),
              change_route=as.character(input$change_route),
              decision_other=as.character(input$decision_other),
              decision_na =as.character(input$decision_na ),
              time_out=as.character(input$time_out),
              comments=as.character(input$comments)

            )
          )

          # Convert the update document to JSON
          updated_sample_json <- jsonlite::toJSON(updated_sample, auto_unbox = TRUE)

          # Update the document
          mongo_sample$update(query = sprintf('{"_id": {"$oid": "%s"}}', selectedID), update = updated_sample_json)


          # Show popup message for successful deletion
          shinyalert::shinyalert(title = "Success", text = "sample updated successfully!", type = "success")

          # Close MongoDB connection
          mongo_sample$disconnect()

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
    observeEvent(input$sp_delete, {
      shinyjs::reset("all_sample_form")
      shinyjs::hide("all_sample_form")
      shinyjs::hide("sp_update")
      shinyjs::hide("sp_delete")
      shinyjs::show("sp_add")

      selected <- input$sample_table_rows_selected
      if (length(selected)) {
        currentData <- myData()
        selectedID <- currentData[selected, "_id"]

        # Debug print statements
        print(paste("Selected ID for deletion:", selectedID))
        print(str(currentData))

        if (!is.null(selectedID)) {
          mongo_sample$remove(paste0('{"_id": {"$oid": "', selectedID, '"}}'))
          myData(fetch_data())
          # Show popup message for successful deletion
          shinyalert::shinyalert(title = "Success", text = "sample deleted successfully!", type = "success")
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
# mod_sample_form_ui("sample_form_1")

## To be copied in the server
# mod_sample_form_server("sample_form_1")
