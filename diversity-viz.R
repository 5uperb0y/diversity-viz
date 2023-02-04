# A Shiny app to demonstrate diversity difference with free-hand drawn distribution curves
# Drawing board in this script was modified from agenis's code, see https://stackoverflow.com/questions/41701807/way-to-free-hand-draw-shapes-in-shiny
library(shiny)
ui <- fluidPage(
  fluidRow(
    # setting panel
    column(2,
      wellPanel(
        sliderInput("mywidth", "width of the pencil", min = 1, max = 10, step = 1, value = 4),
        radioButtons("lcol", label = "line color", choices = list("blue" = "blue", "orange" = "orange")),
        actionButton("reset", "reset")
      ) 
    ),
    # drawing board
    column(4,
      plotOutput(
        "plot",
        width = "500px", height = "500px",
        hover = hoverOpts(id = "hover", delay = 100, delayType = "throttle", clip = TRUE, nullOutside = TRUE),
        click = "click"
        )
    ),
    # output plots
    column(3,
      plotOutput("box", width = "500px", height = "500px"),
    ),
  )
)

server <- function(input, output, session) {
  vals <- reactiveValues(x = numeric(), y = numeric(), g = integer())
  idx <- reactiveValues(n = integer(), shannon = numeric(), simpson = numeric()) 
  isDraw <- reactiveVal(FALSE)
  isLineHead <- reactiveVal(FALSE)
  gN <- reactiveVal(0)
  
  observeEvent(input$click, handlerExpr = {
    if(isDraw() == FALSE) {
      vals$x <- c(vals$x, NA)
      vals$y <- c(vals$y, NA)
      isLineHead(!isLineHead())
    } else {
      isLineHead(!isLineHead())
    }
    isDraw(!isDraw())
    if (isLineHead() == FALSE) {
      #[TODO]: simplify diversity calculation code with function
      #[TODO]: rename unclear variables
      gx <- vals$x[vals$g == gN()]
      gx <- gx[!is.na(gx)]
      gy <- vals$y[vals$g == gN()]
      gy <- gy[!is.na(gy)]
      idx$n <- c(idx$n, sum(gx, na.rm = TRUE))
      shannon <- sum(gy * (gx / sum(gx)) * log(gx / sum(gx)) * -1, na.rm = TRUE)
      idx$shannon <- c(idx$shannon, shannon)
    }
    gN(gN() + 1)
    })

  observeEvent(input$reset, handlerExpr = {
    vals$x <- numeric()
    vals$y <- numeric()
    idx$n <- numeric()
    vals$g <- integer()
    idx$shannon <- numeric()
    gN <- 0
    isLineHead(FALSE)
  })
  observeEvent(input$hover, {
    if (isDraw()) {
      vals$x <- c(vals$x, input$hover$x)
      vals$y <- c(vals$y, input$hover$y)
      vals$g <- c(vals$g, gN())
    }})
  output$plot <-  renderPlot({
    plot(x = vals$x, y = vals$y, xlim = c(0, 28), ylim = c(0, 28), col = input$lcol, ylab = "frequency", xlab = "population size", type = "l", lwd = input$mywidth)
  })
  output$box  <-  renderPlot({
    par(mfrow = c(1, 2))
    boxplot(idx$n, ylim = c(0, max(c(50, idx$n), na.rm = TRUE)), ylab = "taxa number", col = "white")
    stripchart(idx$n, method = "jitter", pch = 19, col = 4, add = TRUE, vertical = TRUE)
    boxplot(idx$shannon, ylim = c(0, max(c(1, idx$shannon), na.rm = TRUE)), ylab = "shannon's index", col = "white")
    stripchart(idx$shannon, method = "jitter", pch = 19, col = 4, add = TRUE, vertical = TRUE)
  })
  }

shinyApp(ui, server)


