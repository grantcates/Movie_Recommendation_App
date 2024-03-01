library(shiny)
library(DT)
library(arules)
library(shinythemes)


load("MOVIERECSpring2022.RData")

POPULARITY <- POPULARITY[POPULARITY$percentSeen * 100 >= 0.1, ]
ALL_MOVIE_TITLES <- sort(unique(POPULARITY$title))


ui <- fluidPage(
  theme = shinytheme("lumen"),
  titlePanel("Hidden Gems: Movie Recommender"),
  sidebarLayout(
    sidebarPanel(

      selectInput(
        inputId = "selected_movies",
        label = "What movies do you like ?",
        choices = ALL_MOVIE_TITLES,
        selected = c("Shawshank Redemption, The (1994)","Godfather, The (1972)","Dark Knight, The (2008)",
                     "Pulp Fiction (1994)","Schindler's List (1993)","12 Angry Men (1957)",
                     "Inception (2010)","Fight Club (1999)"),
        multiple = TRUE

      ),


      sliderInput(
        inputId ="min_confidence",
        label = "Minimum confidence in recommendation",
        min = 0.01,
        max = 1,
        value = .01,
        step = .01
      ),


      sliderInput(
        inputId ="max_popularity",
        label = "Maximum popularity",
        min = 0,
        max = 50,
        value = 3,
        step = .1
      ),

      numericInput(
        inputId = "number_of_recs",
        label = "How many recommedations?",
        min = 1,
        max = 100,
        value = 10
      ),
      # TODO: Add a way for a user to select how many recs there should be
      submitButton(
        text = "Recommend movies!",

      )
    ),
    mainPanel(
      DT::dataTableOutput(outputId = "recommendation_table"),
    )
  )
)


server <- function(input, output) {
  output$recommendation_table <- DT::renderDataTable({
    # REC CODE START   ----------------------------------------
    RECS <- NULL
    if (isTruthy(input$selected_movies)) {
      # Rule out too popular movies early on
      too_popular <- POPULARITY$title[which(100 * POPULARITY$percentSeen > input$max_popularity)]

      # Keep popular movies that the user input
      too_popular <- setdiff(too_popular, input$selected_movies)

      min_support <- 4
      max_time <- 0

      RULES <- apriori(
        TRANS,
        parameter = list(
          supp = min_support / length(TRANS),
          conf = input$min_confidence,
          minlen = 2,
          maxtime = max_time
        ),
        appearance = list(
          none = too_popular,
          lhs = input$selected_movies,
          default = "rhs"
        ),
        control = list(
          verbose = FALSE
        )
      )

      if (length(RULES) > 0) {
        RULES <- RULES[!is.redundant(RULES)]
        RULES <- RULES[is.significant(RULES, TRANS)]

        RULESDF <- DATAFRAME(RULES, itemSep = " + ", setStart = "", setEnd = "")
        names(RULESDF)[1:2] <- c("BasedOn", "title")

        # Remove recs that the user gave as input
        RULESDF <- RULESDF[!(RULESDF$title %in% input$selected_movies), ]
        if (nrow(RULESDF) > 0) {
          RECS <- aggregate(confidence ~ title, data = RULESDF, FUN = max)

          RECS <- merge(RECS, POPULARITY, by = "title")

          RECS$item_id <- NULL
          RECS$countSeen <- NULL
          RECS$Year <- NULL
          names(RECS) <- c("Movie", "Confidence", "PercentSeen", "imdbRating")

          # Order the recommendations by confidence
          RECS <- RECS[order(RECS$Confidence, decreasing = TRUE), ]
          RECS <- head(RECS, input$number_of_recs)

          # Take out confusing row names
          row.names(RECS) <- NULL

          RECS$Confidence <- round(RECS$Confidence * 100, 2)
          RECS$PercentSeen <- round(RECS$PercentSeen * 100, 2)
        }
      }
    }

    if (is.null(RECS)) {
      RECS <- data.frame(
        Error = "No recommendations with these parameters.  Add more movies, decrease confidence, or increase popularity!"
      )
    }

    RECS
    # REC CODE END     ----------------------------------------
  })
}
shinyApp(ui = ui, server = server)
