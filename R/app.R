#' Launch BCG annotation app
#'
#' @param prh_path Path to PRH NetCDF file (optional)
#' @param ... Additional parameters to shinyApp
#'
#' @return Runs a Shiny app
#' @export
bcg_app <- function(prh_path = NULL, ...) {
  #### UI ####
  ui <- shinyUI(fluidPage(

    # Application title
    titlePanel("BCG Annotation"),

    # Sidebar with a slider input for number of bins
    sidebarLayout(
      sidebarPanel(
        tags$h2("Instructions"),

        tags$p("1: Load deployment"),
        tags$p("Note: This step is unnecessary if you specified a file path when calling bcg_app()"),
        shinyFilesButton("prhpath",
                         "Load PRH file",
                         "PRH file (*.nc)",
                         multiple = FALSE),

        tags$p("2: Using plot: drag to zoom, click to add/remove points, double-click to unzoom."),

        tags$p("3: Choose a directory to save results"),
        tags$p("Results will be saved as '[directory]/[whaleid]_beats.rds"),
        shinyDirButton("beatsdir",
                       "Save results",
                       "Choose results directory")
      ),

      # Show a plot of the generated distribution
      mainPanel(
        plotly::plotlyOutput("bcgplot",
                             width = "100%",
                             height = "600px")
      )
    )
  ))

  #### Server ####
  server <- function(input, output, session) {

    # Persistent data
    prh <- reactiveValues(
      file_path = prh_path,
      prhdata = NULL,
      timerange = NULL,
      prhzoom = NULL
    )
    beats <- reactiveValues(
      beatdata = NULL,
      beatidx = NULL
    )

    # File interaction
    volumes <- c(Home = fs::path_home(), getVolumes()())
    shinyFileChoose(input, "prhpath", root = volumes, filetypes = "nc")
    observeEvent(input$prhpath, {
      if (!is.integer(input$prhpath)) {
        file <- parseFilePaths(volumes, input$prhpath)

        prh$file_path <- file$datapath
      }
    })
    observeEvent(prh$file_path, {
      prh$prhdata <- read_nc(prh$file_path)
      prh$timerange <- NULL
      prh$prhzoom <- zoom_prh(prh$prhdata)

      beats$beatdata <- NULL
      beats$beatidx <- NULL
    })
    shinyDirChoose(input,
                   "beatsdir",
                   roots = volumes,
                   session = session)
    observeEvent(input$beatsdir, {
      if (!is.null(beats$beatdata) && !is.integer(input$beatsdir)) {
        beatsdir <- parseDirPath(volumes, input$beatsdir)
        beats_path <- file.path(
          beatsdir,
          paste0(attr(prh$prhdata, "whaleid"), "_beats.rds")
        )
        saveRDS(beats$beatdata, file = beats_path)
        showModal(modalDialog(
          title = "Saved!",
          paste0("Results saved as ", beats_path)
        ))
      }
    })

    # Add/remove beats with click
    observeEvent(plotly::event_data("plotly_click", source = "bcg"), {
      click_data <- plotly::event_data("plotly_click", source = "bcg")
      beat <- ymd_hms(click_data$x, tz = attr(prh$prhdata, "tz"))
      beatidx <- findInterval(beat, prh$prhdata$dt)

      # Add beat, unless within 2s of existing beat (then remove)
      tolerance <- 2.0 * attr(prh$prhdata, "fs")
      which_nearest <- which.min(abs(beatidx - beats$beatidx))

      if (length(beats$beatidx) == 0 || abs(beatidx - beats$beatidx[which_nearest]) > tolerance) {
        beats$beatidx <- sort(c(beatidx, beats$beatidx))
      } else {
        beats$beatidx <- beats$beatidx[-which_nearest]
      }
      if (length(beats$beatidx) == 0) {
        beats$beatdata <- NULL
      } else {
        beats$beatdata <- slice(prh$prhdata, beats$beatidx)
      }
    })

    # Plot BCG data
    output$bcgplot <- plotly::renderPlotly({
      prhdata <- prh$prhzoom
      beatsdata <- zoom_beats(beats$beatdata, range = prh$timerange)
      if (!inherits(prhdata, "data.frame"))
        return(NULL)
      withProgress(message = "Creating visualization...", value = 0, {
        plot_bcg(prhdata, beatsdata)
      })
    })
    observeEvent(plotly::event_data("plotly_relayout", source = "bcg"), {
      relayout <- plotly::event_data("plotly_relayout", source = "bcg")

      # On zoom-in
      if (any(stringr::str_detect(names(relayout), "xaxis.range"))) {
        prh$timerange <- ymd_hms(as.character(relayout),
                                 tz = attr(prh$prhdata, "tz"))
      }
      # On auto-zoom
      if (any(stringr::str_detect(names(relayout), "xaxis.autorange"))) {
        prh$timerange <- NULL
      }
      prh$prhzoom <- zoom_prh(prh$prhdata, range = prh$timerange)
    })
  }

  shinyApp(ui, server, ...)
}
