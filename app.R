suppressPackageStartupMessages({
  require(shiny)
  require(shinyjs)
  require(shinyDirectoryInput)
  require(DT)
  require(magrittr)
  require(exifr)
})

dtLang <- list(url = '//cdn.datatables.net/plug-ins/1.10.20/i18n/Chinese.json')
options(DT.options=list(autoWidth=F, pageLength=10, language = dtLang))


# Define UI for application that draws a histogram
ui <- fluidPage(
  # Application title
  titlePanel("DOP(DxO) Parsing and Renaming Utility"),

  fluidRow(column(6L, directoryInput('directory', label = 'Select a directory'))),
  fluidRow(
    column(9L, uiOutput('exifInfoBox'))
  ),
  br(),
  fluidRow(
    column(4L, DTOutput('dopListBox')),

    column(4L, DTOutput('fileListBox')),

    column(4L, htmlOutput('thumbnailImage')),
  ),
  fluidRow(
    column(12L, verbatimTextOutput('directory'))
  ),
  fluidRow(column(12L, imageOutput('previewImage')))
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {

  observeEvent(
    ignoreNULL = TRUE,
    eventExpr = {
      input$directory
    },
    handlerExpr = {
      if (input$directory > 0) {
        # condition prevents handler execution on initial app launch

        # launch the directory selection dialog with initial path read from the widget
        path = choose.dir(default = readDirectoryInput(session, 'directory'))

        # update the widget value
        updateDirectoryInput(session, 'directory', value = path)
      }
    }
  )

  output$directory = renderText({
    paste0(
      readDirectoryInput(session, 'directory'),'\n',
      dopList()[input$dopListBox_rows_selected], '\n',
      fileList()[input$fileListBox_rows_selected]
    )
  })

  dopList <- reactive({
    req(readDirectoryInput(session, 'directory'))
    dir(readDirectoryInput(session, 'directory'), pattern = '.+\\.dop', ignore.case = T)
  })

  fileList <- reactive({
    req(input$dopListBox_rows_selected)
    selectedDop <- dopList()[input$dopListBox_rows_selected] %>% tools::file_path_sans_ext() %>% paste0(.,'$')
    dir(readDirectoryInput(session, 'directory'), pattern = selectedDop, ignore.case = T)
  })

  output$dopListBox <- renderDT({
    # req(readDirectoryInput(session, 'directory'))
    # path <- readDirectoryInput(session, 'directory')
    # dopList <- dir(path, pattern = '.+\\.dop', ignore.case = T)
    datatable(data.frame('dop files' = dopList()), selection = 'single', style = 'bootstrap')
  })

  output$fileListBox <- renderDT({
    # req(input$dopListBox_rows_selected)
    datatable(data.frame('matches' = fileList()), selection = list(mode = 'single', selected = 1L), style = 'bootstrap')
  })





  # x <- read_exif('example/20181005150239-_MGL0212.CR2')
  # output$previewImage <- renderImage({list(src = paste0('data:image/jpeg;', gsub('base64:', 'base64,', x$PreviewImage)))}, deleteFile = T)
  # output$previewImage <- renderImage({list(src = paste0('data:image/jpeg;', gsub('base64:', 'base64,', x$ThumbnailImage)))}, deleteFile = T)

  # output$thumbnailImage <- renderUI({
  #   req(input$fileListBox_rows_selected)
  #   x <-paste0(readDirectoryInput(session, 'directory'), fileList()[input$fileListBox_rows_selected]) %>% read_exif()
  #   tags$img(src = paste0('data:image/jpeg;', gsub('base64:', 'base64,', x$ThumbnailImage)))
  # })

  observe({
    req(input$fileListBox_rows_selected)
    x <-paste0(readDirectoryInput(session, 'directory'), fileList()[input$fileListBox_rows_selected]) %>% read_exif()

    output$thumbnailImage <- renderUI({tags$img(src = paste0('data:image/jpeg;', gsub('base64:', 'base64,', x$ThumbnailImage)))})
    output$previewImage <- renderImage({
      tempImg <- tempfile(pattern = 'preview_', fileext = '.jpg')
      writeBin(x$PreviewImage %>% base64enc::base64decode(), tempImg)
      list(src = tempImg, contentType = 'image/jpeg')
    }, deleteFile = T)
  })
}

# Run the application
shinyApp(ui = ui, server = server)
