suppressPackageStartupMessages({
  require(shiny)
  require(shinyjs)
  require(shinyDirectoryInput)
  require(shinythemes)
  require(DT)
  require(data.table)
  require(magrittr)
  require(exifr)
  require(base64enc)
})

dtLang <- list(url = '//cdn.datatables.net/plug-ins/1.10.24/i18n/Chinese.json')
options(DT.options = list(autoWidth=F, pageLength=10, searching = F))#, language = dtLang


# Define UI for application that draws a histogram
ui <- fluidPage(
  theme = shinytheme('darkly'),
  tags$head(
    tags$style(HTML("
      div.DTS tbody tr.even {background-color: inherit !important;}
      #previewImage img, #thumbnailImage img {max-width: 100%;}
    "))
  ),

  # Application title
  titlePanel("DOP(DxO) Parsing and Renaming Utility"),

  fluidRow(column(6L, directoryInput('directory', label = NULL))),
  fluidRow(
    column(3L, htmlOutput('thumbnailImage')),
    column(9L, verbatimTextOutput('exifInfoBox'))
  ),
  br(),
  fluidRow(
    column(4L, DTOutput('dopListBox')),

    column(4L, DTOutput('imageListBox'))

    # column(4L, htmlOutput('thumbnailImage')),
  ),
  fluidRow(column(12L, uiOutput('diagnostics'))),
  fluidRow(column(12L, imageOutput('previewImage')))
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {

  observeEvent(
    ignoreNULL = TRUE,
    eventExpr = {input$directory},
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

  output$diagnostics <- renderUI({

    placeholderMsg <- 'STANDBY'

    wellPanel(
      fluidRow(
        column(6L, verbatimTextOutput('directoryMsg', T))
      ),
      fluidRow(
        column(3L, verbatimTextOutput('dopListSelectedMsg', T)),
        column(3L, verbatimTextOutput('imageListSelectedMsg', T)),
        column(3L, verbatimTextOutput('imageListCtMsg', T)),
        column(3L, verbatimTextOutput('imageExifCtMsg', T))
      )
    )
  })
  output$directoryMsg <- renderText({paste0(readDirectoryInput(session, 'directory'))})
  output$dopListSelectedMsg <- renderText({paste0(dopList()[input$dopListBox_rows_selected])})
  output$imageListSelectedMsg <- renderText({paste0(imageList()[input$imageListBox_rows_selected])})
  output$imageListCtMsg <- renderText({paste0('Image List: ', imageList()[,.N], ' items')})
  output$imageExifCtMsg <- renderText({paste0('Image Exif: ', length(imageExif()), ' variables')})

  # fileList ----
  fileList <- reactive({
    req(readDirectoryInput(session, 'directory'))
    data.table(
      files = dir(readDirectoryInput(session, 'directory'), ignore.case = T)
    )
  })

  # dopList ----
  dopList <- reactive({
    req(fileList())
    fileList()[files %like% '\\.dop$', .(`Sidecar Files` = files)]
  })

  # imageList ----
  imageList <- reactive({
    req(input$dopListBox_rows_selected)
    selected <- dopList()[input$dopListBox_rows_selected] %>% tools::file_path_sans_ext() %>% paste0(.,'$')
    fileList()[files %like% selected & !(files %like% '\\.dop$'), .(`Image Files` = files)]
  })

  # imageExif ----
  imageExif <- reactive({
    req(imageList()[,.N > 0L])
    req(input$imageListBox_rows_selected)
    paste0(readDirectoryInput(session, 'directory'), imageList()[input$imageListBox_rows_selected]) %>% read_exif()
  })

  # output$exifInfoBox <- renderText({
  #   req(imageList()[,.N > 0L])
  #   req(input$imageListBox_rows_selected)
  #   x <- imageExif()
  #   paste0(
  #     x$CreateDate, '\n',
  #     x$Model, '\n'
  #   )
  # })


  output$dopListBox <- renderDT({
    req(fileList())
    datatable(
      dopList(),# fileList()[files %like% '\\.dop$', .(`Sidecar files` = files)],
      selection = 'single', style = 'bootstrap', extensions = 'Scroller',
      options = list(scrollY = 370, scroller = list(loadingIndicator = T))
    )
  })

  output$imageListBox <- renderDT({
    req(input$dopListBox_rows_selected)
    datatable(
      imageList(),
      selection = list(mode = 'single', selected = 1L), style = 'bootstrap',
      options = list(dom = 'ti')
    )
  })






  observe({
    req(imageList()[,.N > 0L])
    req(input$imageListBox_rows_selected)
    x <- imageExif()

    tempThumb <- tempfile(pattern = 'thumb_', fileext = '.jpg')
    con <- file(tempThumb, 'wb')
    x$ThumbnailImage %>% substr(., 8, nchar(.)) %>% base64decode(., con)
    close(con)

    # tempPreview <- tempfile(pattern = 'preview_', fileext = '.jpg')
    # con <- file(tempPreview, 'wb')
    # x$PreviewImage %>% substr(., 8, nchar(.)) %>% base64decode(., con)
    # close(con)

    output$thumbnailImage <- renderImage({
      list(src = tempThumb, contentType = 'image/jpeg')
    }, deleteFile = T)

    # output$previewImage <- renderImage({
    #   list(src = tempPreview, contentType = 'image/jpeg')
    # }, deleteFile = T)
  })
}

# Run the application
shinyApp(ui = ui, server = server)
