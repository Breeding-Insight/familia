#' Home UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shinyjs enable disable useShinyjs
#' @importFrom shiny NS tagList
#' @importFrom bs4Dash renderValueBox valueBox
#'
#'
mod_Home_ui <- function(id){
  ns <- NS(id)
  tagList(
    fluidPage(
      fluidRow(
        column(width = 4,
               box(
                 title = "Familia: Ancestry Estimation App", status = "info", solidHeader = FALSE, width = 12, collapsible = FALSE,
                 HTML(
              "<p>The BIGapp is a user-friendly tool for processing low to mid-density genotyping data for diploid and polyploid species. This R shiny app provides a web-based user friendly way for users to analyze genomic data without needing to use command-line tools.
              Additional analysis will be added, with the initial focus on a core set of features for supporting breeding decisions.</p>

              <p><b>Supported Analyses</b></p>
              Initial supported analyses includes the mature genomics/bioinformatics pipelines developed within Breeding Insight:
              <ul>
                <li>Genotype Processing</li>
                <li>Summary Metrics</li>
                <li>Population Structure</li>
                <li>GWAS</li>
                <li>Genomic Selection</li>
              </ul>"
                 ),
                 style = "overflow-y: auto; height: 500px"

               )
        ),
        column(width = 4,
               box(
                 title = "About Breeding Insight", status = "success", solidHeader = FALSE, width = 12, collapsible = FALSE,
                 HTML(
                   "We provide scientific consultation and data management software to the specialty crop and animal breeding communities.
            <ul>
              <li>Genomics</li>
              <li>Phenomics</li>
              <li>Data Management</li>
              <li>Software Tools</li>
              <li>Analysis</li>
            </ul>
            Breeding Insight is funded by the U.S. Department of Agriculture (USDA) Agricultural Research Service (ARS) through Cornell University.
            <div style='text-align: center; margin-top: 20px;'>
              <img src='www/BreedingInsight.png' alt='Breeding Insight' style='width: 85px; height: 85px;'>
            </div>"
                 ),
                 style = "overflow-y: auto; height: 500px"
               )
        ),
        # Right section: links + Try the Breedverse box
        shiny::column(width = 4,
                      shiny::tags$a(
                        href = "https://www.breedinginsight.org",
                        target = "_blank",
                        bs4Dash::valueBox(
                          value = NULL,
                          subtitle = "Learn More About Breeding Insight",
                          icon = shiny::icon("link"),
                          color = "purple",
                          gradient = TRUE,
                          width = 11
                        ),
                        style = "text-decoration: none; color: inherit;"
                      ),
                      shiny::tags$a(
                        href = "https://breedinginsight.org/contact-us/",
                        target = "_blank",
                        bs4Dash::valueBox(
                          value = NULL,
                          subtitle = "Contact Us",
                          icon = shiny::icon("envelope"),
                          color = "danger",
                          gradient = TRUE,
                          width = 11
                        ),
                        style = "text-decoration: none; color: inherit;"
                      ),
                      shiny::tags$a(
                       # href = "https://scribehow.com/viewer/...",
                        target = "_blank",
                        bs4Dash::valueBox(
                          value = NULL,
                          subtitle = "familia Tutorial",
                          icon = shiny::icon("compass"),
                          color = "info",
                          gradient = TRUE,
                          width = 11
                        ),
                        style = "text-decoration: none; color: inherit;"
                      ),
                      bs4Dash::box(
                        title = "Try the Breedverse!", status = "warning", solidHeader = TRUE, width = 11, collapsible = FALSE,
                        shiny::HTML(
                          "We developed an R shiny interface where you can use ALL of our Breeding Insight applications in a single location. This
                   includes applications like BIGapp, Qploidy, and GenoBrew, PLUS all of our newly released applications.
                   Learn more and see install instructions here
                    <div style='text-align: center; margin-top: 20px;'>
                      <img src='www/breedverse_logo.png' alt='Breedingverse' style='width: 120px; height: 140px;'>
                    </div>"
                        ),
                        style = "overflow-y: auto; height: 300px"
                      )
        )
      )
    )
  )
}

#' Home Server Functions
#'
#'
#' @noRd
mod_Home_server <- function(input, output, session, parent_session){

  ns <- session$ns

}

## To be copied in the UI
# mod_Home_ui("Home_1")

## To be copied in the server
# mod_Home_server("Home_1")
