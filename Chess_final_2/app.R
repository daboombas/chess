library(shiny)
library(plotly)
library(shinythemes)
library(tidyverse)
library(ggplot2)
library(ggthemes)
library(vembedr)
library(rchess)
library(gt)

# Reading in the data

join <- read.csv("join.csv")
win_pct <- read.csv("win_pct.csv")

# Changing op to character because
# it's needed later

join$op <- as.character(join$op)

# Define UI for application 

ui <- fluidPage(theme = shinytheme("lumen"),
                
                # Using a break for aesthetics
                
                br(),
                
                # Defining the Navigation bar
                # Defining the Introduction Tab
                
                navbarPage("Chess: An Analysis", 
                           tabPanel("Introduction",  
                                    h3("Welcome!"),
                                    strong("To an introductory analysis of chess."),
                                    h6("My name is Jason Yoo (Hwo Sun Yoo), and I am a 
                                    junior in Leverett House studying Social Studies with a 
                                    secondary in Statistics. In my later teenage years,
                                       I developed a bordering-unhealthy love of chess."),
                                       h6("Although I am not a great player, 
                                       I have always been interested in studying the relationship 
                                       between variables like opening moves, time spent, and differences 
                                       in rating that might potentially affect the probability of 
                                       winning a game of chess. For the project, I am using three 
                                       different datasets. The first, named lichess, is a public 
                                       dataset from the popular online chess site, Lichess. 
                                       It contains games between humans, regardless of playing level. 
                                       The second and third are datasets containing information 
                                       about computer versus computer or computer versus human games 
                                       of varying playing levels. I have combined these two datasets 
                                       into one, and named it internet."),
                                       h6("Through these datasets, 
                                       I hope to study the significance of opening moves, color, 
                                       moves spent per game, and rating differences between players 
                                          on the probability of winning."),
                                    br(),
                                    strong("What is chess?"),
                                    h6("Chess is a game that features 6 different pieces: 
                                    namely, pawns, rooks, knights, bishops, queens, and kings. 
                                    The goal of the game is to corner the enemy king. Some important
                                    concepts that players have in their heads while playing are:
                                    the center of the board should be controlled for future advantages,
                                    the king should be safe from attacks,
                                       and opening moves matter greatly in shaping the game, itself."),
                                    br()
                                    ),
                           
                           # Defining the second tab
                           
                           tabPanel("A Cursory Glance",
                                    
                                    # Creating the first tab to deal with
                                    # Ratings and Turns, the first graph
                                    
                                    tabsetPanel(
                                    tabPanel("Ratings and Turns",
                                             h3("How do rating differences correlate with the number of turns in a game?"),
                                             strong("There is a significant, observable correlation between rating differences and turns."),
                                             br(),
                                             h6("I used a bar plot here to observe the effect of ratings on the 
                                             number of turns in a game. As rating differences become larger and larger 
                                             (either white having a greater rating than black, represented by positive 
                                             difference values, or black being rated higher than white, 
                                             represented by negative difference values), the number of turns decrease."),
                                             h6("This is probably because, as two players diverge in rating and thus in skill level, 
                                             the stronger player will be able to end the game much quicker than if the two players 
                                             were evenly matched."),
                                             h6("At the center, or where the difference converges to 0, the number of turns 
                                             increases dramatically -- which makes a lot of sense.
                                                As two players get closer in rating, and the rating 
                                                difference between them gets closer to 0, they are 
                                                probably more likely to be evenly matched. In a game
                                                with two evenly matched or closely matched players, 
                                                it will take more turns to end the game since a decisive 
                                                advantage will be harder to find."),
                                             br(),
                                             
                                             # Plotting the graph here
                                             
                                             mainPanel(
                                                 plotlyOutput("win_dist"),
                                                 br()
                                                 )
                                             ),
                                    
                                    # Creating the second tab in this part of the app
                                    
                                    tabPanel("Victory Status",
                                             
                                             # This part deals with how games concluded.
                                             
                                             h3("Breakdown of Game Conclusions"),
                                             
                                             # I use checkboxes to let the user choose
                                             # which victory outcomes to display on the graph
                                             
                                             sidebarPanel(
                                                 checkboxGroupInput("vic",
                                                                    "Status", 
                                                                    choices = as.character(unique(join$victory_status)), selected = c("mate", "resign", "outoftime", "draw")),
                                                 h6("Most games ended in either a resignation or a forced checkmate.")),
                                             
                                             # Placing the bar graph here
                                             
                                             mainPanel(
                                                 plotlyOutput("pie"),
                                                 br()
                                                 )
                                             )
                                    )
                                    ),

                           tabPanel("Summary of Starts",
                                    h3("Openings"),
                                    strong("Explore the impact of openings on game outcomes!"),
                                    br(),
                                    br(),
                                    tabsetPanel(
                                        tabPanel("Distribution of Openings",
                                                 sidebarPanel(
                                                     radioButtons("Category",
                                                                  "Winner:",
                                                                  unique(join$winner)),
                                                     h6("For either a win for black, white, or a draw, pawn to e4 was the most popular opening move, closely followed by pawn to d4.
                                                        Indeed, pawn moves that jump 2 spaces were the most popular, followed by knight moves toward the center of the board.")
                                                     ),
                                                 mainPanel(
                                                     plotlyOutput("histPlot"),
                                                     br()
                                                     )
                                                 ),
                                        tabPanel("Openings and Turns",
                                                 sidebarPanel(
                                                    checkboxGroupInput("yes",
                                                                       "Opening Move:", 
                                                                       choices = as.character(unique(join$op)), 
                                                                       selected = c("e4", "d4", "c4")
                                                                       )
                                                    ),
                                                 br(),
                                                 mainPanel(
                                                     plotlyOutput("dist"),
                                                     br(),
                                                     h6("The trendlines mostly slope downward because as rating differences increase, 
                                                     the number of turns should also decrease.
                                                        This is probably because a higher rated player will end a game more quickly.")
                                                     )
                                                 ),
                                        tabPanel("Openings and Turns Combined",
                                                 sidebarPanel(
                                                     checkboxGroupInput("yes2",
                                                                        "Select Opening Moves:", 
                                                                        choices = as.character(unique(join$op)),
                                                                        selected = c("e4", "d4", "c4")
                                                                        )
                                                     ),
                                                 mainPanel(h3("A noticeable downward trend"),
                                                           plotlyOutput("dist2"),
                                                           br()
                                                           )
                                                 ),
                                        tabPanel("Opening Advantages for White",
                                                 h3("How much more likely is white to win?"),
                                                 sidebarPanel(h6("Well... It matters depending on the opening move."),
                                                              br(),
                                                              h6("It seems that most moves that start with a pawn push forward 2 spaces (any move
                                                                 that ends with a 4) give white a small advantage, maxing out at 5% for e4, c4, and d4, the most
                                                                 common opening moves. Their popularity makes sense -- although players
                                                                 can't instinctly know the statistical virtues of playing these two moves, 
                                                                 they can probably feel some advantage from playing them!"),
                                                              h6("3 outliers were b4, c3, and h3. b4 is a move that does not take control of the center,
                                                                 an important part of chess, and c3 and h3 are passive moves. Still, they gave respectable advantages to white,
                                                                 6%, 5%, and 5% respectively. "),
                                                              h6("The worst openings for white were h4 and Na3, resulting in an 11% and 12% disadvantage. 
                                                                 h4 is an aggressive move that aims a pawn towards where the black king will castle, but also exposes white's kingside.
                                                                 Na3 is a knight move that does not take control of the center of the board, and the knight in the new position has very little
                                                                 influence in the game. Thus, these findings make some sense.")),
                                                 tableOutput("winpct"))
                                        )
                                    ),
                           tabPanel("Castling",
                                    h3("Castling: is it important?"),
                                    sidebarPanel(
                                        selectInput("type",
                                                    "Opening Move:",
                                                    c("All", as.character(unique(join$op)))),
                                                    h6("Castling has a great effect on the number of turns in a game.
                                                    As observable in each graph, the trendline corresponding to no castling in a game
                                                    results in a noticeable lesser number of turns in a game. Castling is a move
                                                    that puts one's king in a safer position, thus making the game last longer by preventing
                                                    an early checkmate. In games where there was no castling, the number of turns were drastically reduced. 
                                                    In some cases, the trendline for no castling dropped drastically as the rating difference increased.
                                                    The common sense interpretation is that the higher rated player is probably much more likely to checkmate
                                                       an opponent who has not castled their king, exposing it.")),
                                    mainPanel(
                                        plotlyOutput("reg"),
                                        br(),
                                        br(),
                                        tableOutput("castlingreg"),
                                        h3("What does this mean?"),
                                        h6("The estimate of castling's effect on the number of turns is 61.56 -- out of the games played, the 
                                                     games where either player castled lasted 60 turns longer, on average! For every rating point difference,
                                                     the number of turns dropped by 0.016, not taking into account castling. If a player castled, a rating point
                                                     difference amounts to an estimated decrease of 0.02 turns.")
                                        )
                                    ),
                           tabPanel("Game Database", 
                                    mainPanel("Explore the games!",
                                              h6("This is the database I used for the project. It comprises more than one hundred thousand observations..."),
                                              h6("It was a pain to clean, but it was still super interesting to sift through random games!"),
                                              dataTableOutput("table2")
                                              )
                                    )
                           )
                )


# Define server logic

server <- function(input, output) {
    
    # This plots the distribution of 
    # turns according to the difference
    # in ratings
    
    output$dist <- renderPlotly({
        join %>% 
            group_by(op) %>%
            filter(op %in% input$yes) %>%
            filter(difference >= 0) %>%
            
            # Slicing the data because
            # otherwise it takes too long
            
            slice(1:100) %>%
            ggplot(aes(difference, turns, color = op)) +
            geom_point() +
            
            # Adding a trendline
            
            geom_smooth(method = "glm", se = FALSE, color = "black") +
            theme_classic() +
            xlab("") +
            scale_color_discrete(name = "Openings") +
            
            # Facet wrapping to see each opening
            # on each graph
            
            facet_wrap(~op) +
            ggtitle("Openings and Turns")
        
    })
    
    # This is a duplicate of 'dist' but
    # without the facet wrapping
    # so that each opening can be
    # plotted on a single graph
    
    output$dist2 <- renderPlotly({
        join %>% 
            group_by(op) %>%
            filter(op %in% input$yes) %>%
            filter(difference >= 0) %>%
            slice(1:100) %>%
            ggplot(aes(difference, turns, color = op)) +
            geom_point() +
            geom_smooth(method = "glm", se = FALSE, color = "black") +
            theme_classic() +
            scale_color_discrete(name = "Openings") +
            xlab("Rating Difference") +
            ylab("Turns") +
            ggtitle("Openings and Turns")
    })

    # This table portrays win
    # percentages for colors
    
    output$winpct <- renderTable({
        win_pct %>%
            
            # Mutating new variables to
            # find percentages of winning
            # per color and draw
            
            mutate(`White Percentage` = white/total, 
                   `Black Percentage` = black/total, 
                   `Draw Percentage` = draw/total) %>%
            mutate(`White Advantage` = `White Percentage` - `Black Percentage`) %>%
            
            # Selecting only the relevant variables
            # I named them in two strings for aesthetics
            
            select(op, `White Percentage`, `Black Percentage`, `Draw Percentage`, `White Advantage`)

    })
    
    # This table helps the user
    # explore the data
    
    output$table2 <- renderDataTable({
            join %>% 
            select(!realmoves) %>%
            select(!X.1) %>%
            select(!X) %>%
            select(!difference) %>%
            select(!real_difference) %>%
            select(!turns)
    },
    
    # Using filter to allow
    # custom filterings in the table
    
    filter = 'top',
    rownames = FALSE)

    # This plot shows the win distribution
    # according to the winner and the difference
    # in player ratings
    
    output$win_dist <- renderPlotly({
        win_dist <- ggplotly(join %>% 
                                 
                                 # Grouping by winner and 
                                 # difference to see how the two
                                 # variables relate
                                 
                                 group_by(winner, real_difference) %>%
                                 summarize(average_turns = mean(turns)) %>%
                                 ggplot(aes(x = real_difference, y = average_turns)) +
                                 geom_bar(stat = "identity") +
                                 theme_classic() +
                                 xlab("Rating Difference") +
                                 ylab("Number of Turns") +
                                 ggtitle("Rating Differences Effect on Turns in a Game", 
                                         subtitle = "As rating difference converges to 0, the number of turns increases."))
            
    })
    
    # This histogram shows the popularity
    # of opening moves
    
    output$histPlot <- renderPlotly({
        hist <- ggplotly(join %>%
                             filter(winner == input$Category) %>%
                             
                             # Grouping by winner to see
                             # popularity of moves according to
                             # color of winner
                             
                             group_by(winner) %>%
                             ggplot(aes(x = op)) +
                             geom_histogram(stat = "count") +
                             ylim(0, 40000) +
                             theme_classic() +
                             xlab("Opening Moves") +
                             ylab("Number of Games") +
                             ggtitle("Opening Popularity"))
    })
    
    # Although not a pie chart,
    # this chart shows the distribution
    # of victories and how games ended
    
    output$pie <- renderPlotly({
        victory <- join %>%
            
            # This allows the user to choose
            # the status of the victory
            
            filter(victory_status %in% input$vic) 
        victory %>%
            ggplot(aes(x = victory_status, fill = victory_status)) +
            geom_bar(stat = "count") +
            theme_classic() +
            xlab("Status of Victory") +
            ylab("Number of Ggames") +
            ggtitle("Distribution of How Games Ended",
                    subtitle = "Most games ended in either a resignation or a forced checkmate.") +
            scale_fill_discrete(labels = c("Draw/Tie", "Checkmate", "Out of Time", "Resignation"), name = "Victory Status")
            
        })
    
    # This plot shows the correlation
    # between difference and turns, colored
    # by whether users castled or not.
    
    output$reg <- renderPlotly({
        
        # This if statement only happens if
        # the user selects 'All'
        
        if(input$type == "All"){
            reg <- ggplotly(join %>%
                                slice(1:1000) %>%
                                ggplot(aes(x = difference, y = turns, group = factor(castling), color = factor(castling))) +
                                geom_point() +
                                geom_smooth(method = "glm", se = FALSE, color = 'black') +
                                theme_classic() +
                                xlab("Rating Difference") +
                                ylab("Turns") +
                                ggtitle("Castling's Role in Number of Turns") +
                                scale_color_discrete(name = "Castling"))
            
        }else{
            reg <- ggplotly(join %>%
                                filter(op %in% input$type) %>%
                                slice(1:1000) %>%
                                ggplot(aes(x = difference, y = turns, group = factor(castling), color = factor(castling))) +
                                geom_point() +
                                geom_smooth(method = "glm", se = FALSE, color = 'black') +
                                theme_classic() +
                                xlab("Rating Difference") +
                                ylab("Turns") +
                                ggtitle("Castling's Role in Number of Turns") +
                                scale_color_discrete(name = "Castling"))
        }
    })
    
    # Showing the regression output for
    # difference in rating and castling on turns
    
    output$castlingreg <- renderTable({
        lm(turns ~ difference * castling, data = join) %>%
            tidy(conf.int = TRUE) %>% 
            
            # Selecting the appropriate variables
            
            select(term, estimate, conf.low, conf.high) %>%
            gt()
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
