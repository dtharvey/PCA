ui = navbarPage("AC 3.0: Visualizing a Principal Component Analysis",
                theme = shinytheme("journal"),
                header = tags$head(
                  tags$link(rel = "stylesheet",
                            type = "text/css",
                            href = "style.css") 
                ),
                
# introduction
  tabPanel("Introduction",
    fluidRow(
      withMathJax(),
        column(width = 6, 
          wellPanel(
            class = "scrollable-well",
            div(
              class = "html-fragment",
              includeHTML("text/introduction.html")
                 ))),
           column(width = 6,
                  align = "center",
                  img(src = "introduction-figure.png", height = "700px")
            ))),

# first activity
  tabPanel("Original Axes",
    fluidRow(
      column(width = 6,
        wellPanel(
          class = "scrollable-well",
          div(
              class = "html-fragment",
              includeHTML("text/activity1.html")
            ))),
        column(
          width = 6,
          align = "center",
          plotlyOutput("plotlyactivity1a", height = "600px")
        ))),
      
# second activity
tabPanel("PCA Axes",
         fluidRow(
           column(width = 6,
                  wellPanel(
                    class = "scrollable-well",
                    div(
                      class = "html-fragment",
                      includeHTML("text/activity2.html")
                    ))),
           column(
             width = 6,
             align = "center",
             plotlyOutput("plotlyactivity2a", height = "600px")
           ))),

# third activity
tabPanel("Scores",
         fluidRow(
           column(width = 6,
                  wellPanel(
                    class = "scrollable-well",
                    div(
                      class = "html-fragment",
                      includeHTML("text/activity3.html")
                    ))),
           column(
             width = 6,
             align = "center",
             sliderInput(inputId = "rotangle", 
                         label = "rotation angle",
                         min = 0, max = 90, value = 0,
                         step = 1),
             plotOutput("activity3a", height = "500px")
           ))),

# fourth activity
tabPanel("Variance",
         fluidRow(
           column(width = 6,
                  wellPanel(
                    class = "scrollable-well",
                    div(
                      class = "html-fragment",
                      includeHTML("text/activity4.html")
                    ))),
           column(
             width = 6,
             align = "center",
             sliderInput(inputId = "rotangle_load", 
                         label = "rotation angle",
                         min = 0, max = 90, value = 0,
                         step = 1),
             plotOutput("activity4a", height = "500px")
           ))),


# wrapping up

tabPanel("Wrapping Up",
         fluidPage(
           column(width = 6,
                  wellPanel(
                    class = "scrollable-well",
                    div(
                      class = "html-fragment",
                      includeHTML("text/wrapup.html")
                    ))),
           column(width = 6,
                  align = "center",
                  img(src = "3D.png", height = "300px"),
                  img(src = "2Da.png", height = "300px")
           )))            

) # closing for user interface
