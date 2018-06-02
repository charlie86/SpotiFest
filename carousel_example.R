library(shiny)

df <- tibble(img = c('https://images.pexels.com/photos/248797/pexels-photo-248797.jpeg', 'https://www.w3schools.com/bootstrap/ny.jpg', 'https://www.w3schools.com/bootstrap/chicago.jpg'),
             title = c('LA', 'Chigaco', 'NYC'))

ui <- fluidPage(
    tags$head(
        # tags$link(rel = 'stylesheet', href = 'https://maxcdn.bootstrapcdn.com/bootstrap/3.3.7/css/bootstrap.min.css'),
        tags$script(src = 'https://ajax.googleapis.com/ajax/libs/jquery/3.3.1/jquery.min.js'),
        tags$script(src = 'https://maxcdn.bootstrapcdn.com/bootstrap/3.3.7/js/bootstrap.min.js')
    ),
    div(class = 'container',
        h2('Carousel Example'),
        div(id = 'myCarousel', class = 'carousel slide', 'data-ride' = 'carousel',
            tags$ol(class = 'carousel-indicators',
                    tagList(
                        map(1:nrow(df), function(x) {
                            class <- ifelse(x == 1, 'active', '')
                            tags$li('data-target'="#myCarousel", 'data-slide-to'=x-1, class = class)
                        })
                    )
            ),
            div(class = 'carousel-inner',
                tagList(
                    map(1:nrow(df), function(x) {
                        class <- ifelse(x==1, 'item active', 'item')
                        div(
                            class = class, img(src=df$img[x], alt=df$title[x], style="width:100%;")
                        )
                    })
                )
            ),
            a(class = 'left carousel-control', href="#myCarousel", 'data-slide'="prev",
              span(class = 'glyphicon glyphicon-chevron-left'),
              span(class = 'sr-only', 'Previous')
            ),
            a(class = 'right carousel-control', href="#myCarousel", 'data-slide'="next",
              span(class = 'glyphicon glyphicon-chevron-right'),
              span(class = 'sr-only', 'Next')
            )
        )
    )
)

server <- function(input, output, session) {
    
}

shinyApp(ui, server)