#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
#' @import fontawesome
#'
library(bslib)
library(fontawesome)
library("htmltools")
library(shinyjs)
library(mongolite)
library(rsconnect)
library(shinyTime)
library("htmltools")
library(shinyjs)
library("shinycssloaders")
library(lubridate)
library(dplyr)
library(bslib)
library(shiny)
library(shiny.fluent)
library(RSQLite)
library(DBI)
library(tidyverse)
library(openxlsx)
library(mongolite)
library(jsonlite)
library(shinyalert)
library(readr)
library(readxl)
library(golem)
library(DT)

app_ui <- function(request) {
  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    # Your application UI logic
    fluidPage(
      shinyjs::useShinyjs(),  # Initialize shinyjs

      theme = bs_theme(version =5,
                       "navbar-bg" = "#0072B2",
                       bg = "white", fg = "navy",
                       "progress-bar-bg" = "lightblue",
                       primary = "#0072B2",
                       secondary = "#0072B2",
                       success = "#009E73",
                       base_font = font_google("Inter"),
                       code_font = font_google("JetBrains Mono")
      ),

      navbarPage(
        title = "Drug Checking",
        underline = TRUE,
        tabPanel("Data Entry",

                 tabsetPanel(

                   tabPanel((tags$i(class = "fa-solid fa-street-view", style = "font-size: 4rem;")),"Client's Visit",
                            mod_visit_form_ui("visit_form_1"),
                            ),
                   tabPanel((tags$i(class = "fa-solid fa-vial", style = "font-size: 4rem;")), "Sample",
                            "sample form")
                 )
        ),# end of tabPanel Data Entry

        tabPanel("Dashboard",

                 tabsetPanel(

                   tabPanel((tags$i(class = "fa-solid fa-chart-column", style = "font-size: 4rem;")),"Charts",
                            "Charts"),
                   tabPanel((tags$i(class = "fa-solid fa-table", style = "font-size: 4rem;")),"Tables",
                            "Tables")
                 )
        ),# end of tabPanel Dashboard


        tabPanel("Admin Settings",

                 tabsetPanel(

                   tabPanel(tags$i(class = "fa-solid fa-hospital-user", style = "font-size: 4rem;"),"Technician List",
                            ""),
                   tabPanel((tags$i(class = "fa-solid fa-cannabis", style = "font-size: 4rem;")),"Substance List",
                            "Substance List"),
                   tabPanel((tags$i(class = "fa-solid fa-user-gear", style = "font-size: 4rem;")),"User Management",
                            "User List"),

                 )
        ),# end of tabPanel Admin Settings

      )# end of navbar page
      ) # end of fluid page

    )
}

#' Add external Resources to the Application
#'
#' This function is internally used to add external
#' resources inside the Shiny application.
#'
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function() {
  add_resource_path(
    "www",
    app_sys("app/www")
  )

  tags$head(
    favicon(),
    golem::activate_js(),
    shinyjs::useShinyjs(),
    bundle_resources(
      path = app_sys("app/www"),
      app_title = "drugchecking"
    ),
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert()
    tags$link(rel = "Stylesheet", type = "text/css", herf = "www.custom.css" ),
    tags$link(
      rel = "stylesheet",
      href = "https://cdnjs.cloudflare.com/ajax/libs/font-awesome/6.5.2/css/all.min.css",

    )
  )
}
