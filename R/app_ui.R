#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
#'
app_ui <- function(request) {
  tagList(
    golem_add_external_resources(),


    navbarPage(
      "MSinco",
      id = "navbar",
      selected = "Settings",

      tabPanel(
        "Settings",

        wellPanel(
          h5("Settings"),
          radioButtons("graphics",
            label = "Graphics",
            choices = list("lattice (faster)" = 1, "ggplot" = 2),
            selected = "1"
          ),
          checkboxGroupInput("settings", "Other settings",
            choiceNames = c("parallel processing"),
            choiceValues = c("parallel"), selected = c("parallel")
          )
        )
      ),

      navbarMenu(
        "Data",
        tabPanel(title = "Create experiment"),
        tabPanel(title = "Import experiment")
      ),
      # navbarMenu(
      #   "Export",
      #   tabPanel(title = "Active plots")
      # ),

      tabPanel(
        title = "Visualization",

        sidebarLayout(
          sidebarPanel(
            width = 3, style = "position:fixed;width:22%;border:1px solid #e0e0e0;border-radius:20px;", #
            h5("Parameters"),
            uiOutput("selectedFragment"),
            uiOutput("selectedFiles"),
            uiOutput("rtime"), # hr(),
            uiOutput("labelThreshold"),
            uiOutput("rtimeL"),
            uiOutput("rtimeR"),
            uiOutput("mass0"),
            uiOutput("N_atom"),
            uiOutput("mzd"),
            uiOutput("saveActivePlotsButton"),

            conditionalPanel(
              'input.tabs === "TIC" && output.selectedFiles',


              tagList(
                tagAppendAttributes(actionButton("run1", "run", width = "100%", style = "margin-bottom:8px"), `data-proxy-click` = "run1")
              )
            ),
            conditionalPanel(
              'input.tabs === "MSpectrum" && output.selectedFiles',

              tagList(
                tagAppendAttributes(actionButton("run2", "run", width = "100%", style = "margin-bottom:8px"), `data-proxy-click` = "run2")
              )
            ),
            conditionalPanel(
              'input.tabs === "SIM" && output.selectedFiles',


              actionButton("saveTotable", "Save parameters", width = "100%", style = "margin-bottom:8px"),
              tagList(
                tagAppendAttributes(actionButton("run3", "run", width = "100%", style = "margin-bottom:8px"), `data-proxy-click` = "run3")
              )
            ),
            br(),
            verbatimTextOutput("plot_hover_coord"),
          ),
          mainPanel(
            tabsetPanel(
              id = "tabs",


              tabPanel("TIC", fluidRow(column(6, uiOutput("plots_ticA")), column(6, uiOutput("plots_ticB")))),

              tabPanel("MSpectrum", fluidRow(column(6, uiOutput("plots_msA")), column(6, uiOutput("plots_msB")))),

              tabPanel("SIM", fluidRow(column(6, uiOutput("plots_simA")), column(6, uiOutput("plots_simB"))))
            )
          )
        )
      ),
      tabPanel(
        "Parameters & Analysis",
        sidebarLayout(
          sidebarPanel(
            width = 3, style = "position:fixed;width:22%;border:1px solid #e0e0e0;border-radius:20px;",
            h5("Parameters"),
            conditionalPanel(
              "output.selectedFiles",

              numericInput("mzd2", "Mass difference", value = 0.3),
              checkboxGroupInput("runParameters", NULL,
                choiceNames = c("Export all plots (TIC, SIM, MS)", "Baseline correction", "Isotope correction", "correctTracerImpurity"),
                choiceValues = c("savePlots", "baselineCorrection", "isotopeCorrection", "correctTracerImpurity"),
                selected = list("baselineCorrection", "isotopeCorrection")
              ),
              actionButton("saveButton", "Save table", width = "49%", style = "margin-bottom:8px"),
              actionButton("undoButton", "undo", width = "49%", style = "margin-bottom:8px"),
              actionButton("runButton2", "Save table & Run", width = "100%", style = "margin-bottom:8px", class = "btn-primary")
            )
          ),
          mainPanel(
            tabPanel("Parameters", fluidRow(column(6, rhandsontable::rHandsontableOutput("hot"))))
          )
        )
      ),
      tags$script(
        HTML("var header = $('.navbar > .container-fluid');
                              header.append('<div style=\"float:right; padding-top: 15px\"><button id=\"quitButton\" type=\"button\" class=\"btn btn-danger action-button\" >Quit</button></div>')")
      )
    )
  )
}


#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function() {
  add_resource_path(
    "www", app_sys("app/www")
  )

  tags$head(
    favicon(),
    bundle_resources(
      path = app_sys("app/www"),
      app_title = "MSinco"
    )
  )
}
