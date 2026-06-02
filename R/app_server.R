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
  
  get_latest_github_commit <- function(repo, owner) {
    url      <- paste0("https://api.github.com/repos/", owner, "/", repo, "/releases/latest")
    response <- GET(url)
    content  <- content(response, "parsed")
    if (status_code(response) == 200) {
      tag_name       <- content$tag_name
      clean_tag_name <- sub("-.*", "", tag_name)
      clean_tag_name <- sub("v", "", clean_tag_name)
      return(clean_tag_name)
    } else {
      return(NULL)
    }
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
    
    package_name <- "BIGapp"
    repo_name    <- "BIGapp"
    repo_owner   <- "Breeding-Insight"
    
    installed_version <- as.character(packageVersion(package_name))
    latest_commit     <- get_latest_github_commit(repo_name, repo_owner)
    
    if (latest_commit > installed_version) {
      message_html <- paste(
        "Installed version:", installed_version, "<br>",
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
      title     = "BIGapp Updates",
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