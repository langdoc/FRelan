#' FRorpus Function
#'
#' This function launches a Shiny app. Please see the documentation in the address [github.com/langdoc/FRorpus-demo](http://github.com/langdoc/FRorpus-demo).
#' @param corpus The corpus file
#' @keywords ELAN
#' @export

FRorpus <- function(corpus, c1 = "before", c2 = "token", c3 = "after", c4 = FALSE) {

`%>%` <- dplyr::`%>%`

data <- corpus

# This is the function used to filter searches

filtering <- function(regex, env_after, env_before, ignore){
        data <- data %>% dplyr::filter(grepl(regex, token, perl = TRUE))
        data <- data %>% dplyr::filter(grepl(env_after, after, perl = TRUE))
        data <- data %>% dplyr::filter(grepl(env_before, before, perl = TRUE))
        data <- data %>% dplyr::filter(! grepl(ignore, token, perl = TRUE))
}

# This is the UI definition

shiny::shinyApp(ui = shiny::fluidPage(theme = "bootstrap.css",
        shiny::navbarPage(
                title = 'FRorpus',
                        shiny::tabPanel('Concordances',

                                shiny::tags$head(shiny::tags$style(".table .alignRight {color: black; text-align:right;}")),
                                shiny::tags$head(shiny::tags$style(".table .alignCenter {color: blue; text-align:center;}")),
                                shiny::tags$head(shiny::tags$style(".table .alignLeft {color: black; text-align:left;}")),

# There are the inputs

                        shiny::fluidRow(
                                shiny::column(3,
                                              shiny::textInput("text3", label = shiny::h4("Over left border:"), value = "^.+$")),
                                shiny::column(3,
                                              shiny::textInput("text1", label = shiny::h4("Search tokens:"), value = "^.+$")),
                                shiny::column(3,
                                              shiny::textInput("text2", label = shiny::h4("Over right border:"), value = "^.+$")),
                                shiny::column(3,
                                              shiny::textInput("text4", label = shiny::h4("Ignore:"), value = "$nothing")),

# This is the table for results

                        shiny::fluidRow(shiny::dataTableOutput(outputId="table")
                                                   ))
),

# This is another tab for different content -- there could be more of these

                        shiny::tabPanel('Information & Help',
                                 (shiny::fluidPage(
                                         shiny::h1("FRorpus application"),
                                         shiny::p(paste0("This search interface is designed to be used within FRelan R package and is integrated into it.")),
                                         shiny::p("The regular expressions have to be used in the searches. They are Perl compatible.")
                                 ))
),

# Just contact information

                        shiny::tabPanel('Contact',
                                (shiny::fluidPage(shiny::h1("Freiburg Research Group in Saami Studies"),
                                                  shiny::p("Please contact Niko Partanen (nikotapiopartanen@gmail.com) or Michael Rießler (michael.riessler@skandinavistik.uni-freiburg.de).")
                                           )))
                          )
                ),
# This is the server file. It is often as its own file, but can be also within the same file as it is here.

server = function(input, output) {

        # Filter data based on selections

        output$table <- shiny::renderDataTable({

                data <- filtering(input$text1, input$text2, input$text3, input$text4)

                 if (c4 == FALSE){
                data[,c(c1, c2, c3)]
                 } else {
                data[,c(c1, c2, c3, c4)]
                 }

        }, options = list(

                # These are the column settings

                aoColumnDefs = list(
                        list(targets = c(0, 1, 2), searchable = FALSE),
                        list(sClass="alignRight", aTargets=c(0)),
                        list(sClass="alignCenter", aTargets=c(1)),
                        list(sClass="alignLeft", aTargets=c(2)),
                        list(sWidth=c("200px"), aTargets=c(0)),
                        list(sWidth=c("100px"), aTargets=c(1)),
                        list(sWidth=c("200px"), aTargets=c(2))
                ),

                # These are data table settings

                searching = 0,  # global search box on/off
                lengthMenu = c(10, 50, 100), # length options
                pageLength = 20) # default rows
        )
})
}
