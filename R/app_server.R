#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @importFrom httr GET content status_code
#' @importFrom curl new_handle curl_fetch_memory
#' @noRd
app_server <- function(input, output, session) {
  
  options(shiny.maxRequestSize = 1000000 * 1024^2)
  
  callModule(mod_polybreedtools_server,
             "PolyBreedTools_1",
             parent_session = session)
  
  callModule(mod_SNMF_server,
             "SNMF_1",
             parent_session = session)
  
  mod_ped_cleaner_server(
    "ped_cleaner_1",
    parent_session = session)
  
  mod_validate_ped_server(
    "validate_ped_1",
    parent_session = session)
  
  mod_find_parentage_server(
    "find_parentage_1",
    parent_session = session)
  
  mod_help_server(
    "help_1",
    parent_session = session)
  
  # Session info popup
  observeEvent(input$session_info_button, {
    showModal(modalDialog(
      title = "Session Information",
      size = "l",
      easyClose = TRUE,
      footer = tagList(
        modalButton("Close"),
        downloadButton("download_session_info", "Download")
      ),
      pre(
        paste(capture.output(sessionInfo()), collapse = "\n")
      )
    ))
  })
  
  # Version of the latest GitHub release, or NULL if it cannot be retrieved
  get_latest_github_release <- function(repo, owner) {
    url      <- paste0("https://api.github.com/repos/", owner, "/", repo, "/releases/latest")
    response <- tryCatch(GET(url, httr::timeout(10)), error = function(e) NULL)
    if (is.null(response) || status_code(response) != 200) return(NULL)
    tag_name <- content(response, "parsed")$tag_name
    if (is.null(tag_name)) return(NULL)
    clean_tag_name <- sub("-.*", "", tag_name)          # drop suffixes such as "-beta"
    clean_tag_name <- sub("^[vV]", "", clean_tag_name)  # drop the leading "v"
    clean_tag_name
  }
  
  is_internet_connected <- function() {
    handle  <- new_handle()
    success <- tryCatch({
      curl_fetch_memory("https://www.google.com", handle = handle)
      TRUE
    }, error = function(e) {
      FALSE
    })
    return(success)
  }
  
  observeEvent(input$updates_info_button, {
    if (!is_internet_connected()) {
      showModal(modalDialog(
        title = "No Internet Connection",
        easyClose = TRUE,
        footer = tagList(modalButton("Close")),
        "Please check your internet connection and try again."
      ))
      return()
    }
    
    package_name <- "Familia"
    repo_name    <- "Familia"
    repo_owner   <- "Breeding-Insight"

    installed_version <- as.character(packageVersion(package_name))
    latest_version    <- get_latest_github_release(repo_name, repo_owner)

    # Compare as versions, not text ("2.10.0" is newer than "2.9.0")
    is_newer <- !is.null(latest_version) && isTRUE(tryCatch(
      package_version(latest_version) > package_version(installed_version),
      error = function(e) FALSE
    ))

    if (is.null(latest_version)) {
      message_html <- paste(
        "Installed version:", installed_version, "<br>",
        "The latest release could not be retrieved from GitHub. Please try again later."
      )
    } else if (is_newer) {
      message_html <- paste(
        "Installed version:", installed_version, "<br>",
        "Latest version:", latest_version, "<br>",
        "<span>A new version is available on GitHub!</span><br>",
        "<span style='color: red;'>Please update your package.</span>"
      )
    } else {
      message_html <- paste(
        "Installed version:", installed_version, "<br>",
        "Your package is up-to-date!"
      )
    }

    showModal(modalDialog(
      title     = "Familia Updates",
      size      = "m",
      easyClose = TRUE,
      footer    = tagList(modalButton("Close")),
      HTML(message_html)
    ))
  })
  
  output$download_session_info <- downloadHandler(
    filename = function() paste0("session_info_", Sys.Date(), ".txt"),
    content  = function(file) {
      writeLines(paste(capture.output(sessionInfo()), collapse = "\n"), file)
    }
  )
}