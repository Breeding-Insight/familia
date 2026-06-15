#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @importFrom bs4Dash bs4Badge bs4DashSidebar bs4DashNavbar bs4DashPage sidebarMenu menuItem menuSubItem dashboardBody tabItems tabItem box dashboardFooter
#' @importFrom shinydisconnect disconnectMessage
#' @import shinyWidgets
#'
#' @noRd
app_ui <- function(request) {
  tagList(
    golem_add_external_resources(),
    tags$head(tags$style(HTML(sprintf(
      ":root { --sidebar-core: var(--%s-core); --sidebar-lite: var(--%s-lite); --sidebar-deep: var(--%s-deep); }",
      "green", "green", "green"
    )))),
    bs4DashPage(
      skin = "black",
      bs4DashNavbar(
        title = tagList(
          tags$img(src = 'www/Familia_logo.png', height = '40', width = '40'),
        ),
        rightUi = tags$li(
          class = "dropdown",
          tags$a(
            href          = "#",
            class         = "nav-link",
            `data-toggle` = "dropdown",
            icon("info-circle")
          ),
          tags$div(
            class = "dropdown-menu dropdown-menu-right",
            tags$a(
              class   = "dropdown-item",
              href    = "#",
              "Session Info",
              onclick = "Shiny.setInputValue('session_info_button', Math.random())"
            ),
            tags$a(
              class   = "dropdown-item",
              href    = "#",
              "Check for Updates",
              onclick = "Shiny.setInputValue('updates_info_button', Math.random())"
            )
          )
        ),
        help = NULL
      ),
      bs4DashSidebar(
        skin          = "light",
        status        = "warning",
        fixed         = TRUE,
        expandOnHover = TRUE,
        sidebarMenu(
          id   = "MainMenu",
          flat = FALSE,
          tags$li(class = "header", style = "color: grey; margin-top: 10px; margin-bottom: 10px; padding-left: 15px;", "Menu"),
          menuItem("Home", tabName = "welcome", icon = icon("house"), startExpanded = FALSE),
          tags$li(class = "header", style = "color: grey; margin-top: 18px; margin-bottom: 10px; padding-left: 15px;", "Pedigree & Parentage"),
          menuItem("Pedigree Cleaner",    tabName = "ped_cleaner",    icon = icon("sitemap")),
          menuItem("Validate Pedigree",   tabName = "validate_ped",   icon = icon("circle-check")),
          menuItem("Find Parentage",      tabName = "find_parentage", icon = icon("people-arrows")),
          tags$li(class = "header", style = "color: grey; margin-top: 18px; margin-bottom: 10px; padding-left: 15px;", "Breed/Line Composition"),
          menuItem(HTML("BreedTools<sup>poly</sup>"), tabName = "polybreedtools", icon = icon("chart-column")),
          menuItem("SNMF", tabName = "snmf", icon = icon("list-ol")),
          tags$li(class = "header", style = "color: grey; margin-top: 18px; margin-bottom: 10px; padding-left: 15px;", "Information"),
          menuItem("Source Code", icon = icon("circle-info"), href = "https://github.com/Breeding-Insight/Familia"),
          menuItem("Help", tabName = "help", icon = icon("circle-question"))
        )
      ),
      footer = dashboardFooter(
        right = div(
          style = "display: flex; align-items: center;",
          div(
            style = "display: flex; flex-direction: column; margin-right: 15px; text-align: right;",
            div("2026 Breeding Insight"),
            div("Funded by USDA through (UF|IFAS)")
          ),
          div(
            a(
              img(src = "www/usda-logo-color.png", height = "45px"),
              style = "margin-right: 15px;"
            ),
            a(
              img(src = "www/cornell_seal_simple_web_b31b1b.png", height = "45px")
            )
          )
        ),
        left = div(
          style = "display: flex; align-items: center; height: 100%;",
          sprintf("v%s", as.character(utils::packageVersion("Familia")))
        )
      ),
      dashboardBody(
        disconnectMessage(),
        tags$style(
          HTML(
            ".main-footer {
              background-color: white;
              color: grey;
              height: 65px;
              padding-top: 5px;
              padding-bottom: 5px;
            }
            .main-footer a {
              color: grey;
            }"
          )
        ),
        tabItems(
          tabItem(
            tabName = "welcome",       mod_Home_ui("Home_1")
          ),
          tabItem(
            tabName = "ped_cleaner",   mod_ped_cleaner_ui("ped_cleaner_1")
          ),
          tabItem(
            tabName = "validate_ped",  mod_validate_ped_ui("validate_ped_1")
          ),
          tabItem(
            tabName = "find_parentage", mod_find_parentage_ui("find_parentage_1")
          ),
          tabItem(
            tabName = "polybreedtools", mod_polybreedtools_ui("PolyBreedTools_1")
          ),
          tabItem(
            tabName = "snmf",          mod_SNMF_ui("SNMF_1")
          ),
          tabItem(
            tabName = "help",          mod_help_ui("help_1")
          )
        )
      )
    )
  )
}

#' Add external Resources to the Application
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
    bundle_resources(
      path      = app_sys("app/www"),
      app_title = "Familia"
    ),
    tags$style(HTML("
      /* Ensure box collapse/expand buttons are always on top */
      .card-tools { position: relative; z-index: 10; }
      /* Make collapse/expand icons visible on white box headers */
      .card-tools .btn-tool { color: #495057 !important; }
      .card-tools .btn-tool:hover { color: #212529 !important; }
    "))
  )
}
