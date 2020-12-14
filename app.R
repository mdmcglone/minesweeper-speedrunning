library(shiny)
library(tidyverse)
library(ggthemes)
library(shinythemes)
library(rstanarm)
library(gt)
#load relevant libraries

mines <- read.csv('data/minesweeper.csv')
mines_pro <- read.csv('data/minesweeper_pro.csv')
mines_pp_data <- readRDS('mines_pp_data.rds')
mines_pp_pro <- readRDS('mines_pp_pro.rds')
#reads in raw data and posterior predictions from stan_glm


# Define UI 
ui <- navbarPage(
    "Minesweeper!",
    tabPanel("About",
        tabsetPanel(
            #creates first sub-panel for project intro
         tabPanel("Introduction", 
             titlePanel("Introduction"),
             tags$iframe(width="560", height="315", src="https://www.youtube.com/embed/Kt5kDRNowGQ", 
                         frameborder="0", allow="accelerometer; autoplay; encrypted-media; gyroscope; 
                         picture-in-picture", allowfullscreen=NA),
             #places my youtube video of minesweeper win for hook
             h3("The Game"),
             p("Minesweeper is a single-player puzzle game originally from the 60s. 
               The game begins with a board of square cells in the covered state.
               A certain number of mines (depending on difficulty) are placed in cells randomly at the 
               start of the game. The player can left-click to uncover a cell, but if they click a mine, 
               they lose. Uncovered cells without mines are blank or display a number. 
               These numbers represent how many mines are in all neighboring cells, including diagonals.
               The player can also right-click to place flags on covered cells which they believe 
               have mines in them. The goal is to uncover all non-mine cells as fast as possible,
               using the numbers and flags as a guide. Flagging all mines is not required.
               The first click is never a mine. The player may also simultaneously right-
               and left-click on a number to uncover all adjacent unflagged covered tiles, if 
               that number borders an equal number of flagged tiles. See the tutorial tab for 
               an overview of a short game"),
             #description of minesweeper
             h3("The Data"),
             p("I created two spreadsheets for this project: one that records my data for many games, 
               and another for a pro player. Pro is a bit of a misnomer since no one makes money off 
               this, but he's better than me at least. Both played on the free website, 
               minesweeperonline.com The data I recorded include: win or loss, time, difficulty, 
               cause of loss, and the number of mines remaining at loss.
               Due to scarcity, I was only able to find pro games for the expert difficulty, 
               but that's the most important one. I recorded the pro data from saved streams of a player 
               called eXtine, on his Youtube channel eXtelevision. The goal of this project is to compare
               my skills to that of the pro player, as well as to predict a theoretical limit for my scores."),
             #description of data scraping 
             h3("The Matt"),
             p("My name is Matt McGlone and I've been playing Minesweeper on and off since high school.
               It's something I like to do passively, it relieves stress and lets me think while my 
               fingers and eyes do something else. Apparently during that time I got pretty good at the 
               game too - my record on expert is 85 seconds. Here's the Github repo for this project: 
               https://mdmcglone.shinyapps.io/minesweeper/. Oh, I'm also a physics concentrator, and
               a junior at Harvard, I think?")
             #description of me with github link
    ),
        tabPanel("Tutorial",
                 #whole chunk below writes step-by-step tutorial
            titlePanel("Tutorial"),
            p("Let's practice a game on the beginner difficulty. The board below is 9x9 with 10 mines. 
              The upper left number represents how many mines remain, and the upper right number is the 
              number of seconds elapsed."),
            imageOutput('t1'),
            br(), br(), br(), br(), br(), br(),
            #pictures kept overlapping, br() makes white space
            p("This is what the game looks like after a single click in the middle. Observe that each of 
              the numbers I've circled in red borders exactly same number of uncovered tiles. 
              The number represents how many mines it borders, therefore all tiles adjacent to those 
              numbers are mines, so we are free to flag them all."),
            imageOutput('t2'),
            br(), br(), br(), br(), br(), br(),
            p("Viola! Only three more mines to find. The numbers circled now each border the same 
              number of flags - meaning they border no more mines. So, we're free to uncover all other 
              adjacent tiles."),
            imageOutput('t3'),
            br(), br(), br(), br(), br(), br(),
            p("The whole bottom left has opened up because no mines remained there - blank tiles uncover
              automatically when an adjacent tile is uncovered Now, let's repeat the same processes. 
              The 2 circled in red borders exactly two tiles, so we can flag them both. The 2 circled 
              in purple already borders two flags/mines, so we can uncover the remaining adjacent tile."),
            imageOutput('t4'),
            br(), br(), br(), br(), br(), br(),
            p("We're almost there, two mines left! Our only obvious path is that the circled 3 borders 
            three flagged tiles, meaning we can clear everything else around it."),
            imageOutput('t5'),
            br(), br(), br(), br(), br(), br(),
            p("Again, the circled 4 borders four tiles, and the circled 3 borders three tiles,
              so everything they border can be flagged.
              Notice that these are the last two mines in the game!"),
            imageOutput('t6'),
            br(), br(), br(), br(), br(), br(),
            p("We've marked all ten mines, so we're free to clear everything else, and win!"),
            imageOutput('t7')   
        )
    )),
    tabPanel("Statistics",
        tabsetPanel(
            tabPanel('My Stats',
                #this page displays non-model stats for me
             fluidPage(
                 titlePanel("My Minesweeper Stats"),
                 sidebarLayout(
                     sidebarPanel( 
                         selectInput(inputId = 'menu', label = 'Select Difficulty (for time graph)',
                                     choices = mines$difficulty, selected = 1),
                         #difficulty selection input for actual time distribution graph
                         h3('Interpretation'),
                         p('A few trends are obvious to see; as difficulty increases, 
                           the proportion of wins decreases. The loss category that expands 
                           the most is "guess", which seems natural, as the larger the board is,
                           the higher the chance that the game will force the player into 
                           guessing and losing at some point. This manifests most often in situations 
                           where the player can only say that a mine is in one of two tiles, 
                           forcing a 50/50 guess.'),
                         p('For clarification - "guess" means the game did not provide enough
                           information to determine where a mine is, forcing a guess that may end 
                           the game. "misclick" means a player error where they mistakenly clicked a 
                           mine, wheareas "misflag" means a player error where they mistakenly
                           placed a flag, causing them to later click a mine.')
                     ),
                     mainPanel(plotOutput("timePlot", height = "300px"), br(),
                               plotOutput("colorPlot", height = "300px"))))),
                    #places actual time distribution and cause of loss plots
            tabPanel('Pro Stats',
                     #this page displays non-model stats for eXtine
                     fluidPage(
                      titlePanel('eXtine\'s Minesweeper Stats'),
                      sidebarLayout(
                          sidebarPanel(
                              h3('Intepretation'),
                              p('eXtine\'s play style is visibly more aggressive than mine. 
                                Most of his runs are over in the first 25 seconds, losing to
                                mistakes made in haste, but beyond that he appears to play more
                                careful in order to clinch wins. My runs fall in a much more even 
                                distribution of times. His faster, more optimized play leads 
                                to more misclicks and less guessing - and a lower win rate. 
                                In the table, we can see that he flags about .5 mines per second
                                more than I do, at the cost of halving his winrate and
                                game length.')
                          ),
                          mainPanel(gt_output('statsTable'), br(),
                                    plotOutput('timePlot_pro', height = '300px'), br(),
                                    plotOutput('colorPlot_pro', height = '300px'))), 
                      #places pro actual time distribtuion and cause of loss plots
                      
                        
                            
                            
                        )))
            ),
    tabPanel("Model",
        tabsetPanel(
            tabPanel('My Model',
                    #this page displays the results of my stan_glm model
             fluidPage(
                 titlePanel("My Win Times Model"),
                 sidebarLayout(
                     sidebarPanel(
                         sliderInput(inputId = 'time', label = 'Select Time (for table)',
                                     min = 0, max = 125, value = 60, step = 2),
                         #slider input for win time probability table
                         h3('Interpretation'),
                         p('My possible expert scores range from 90 to 125 seconds. This is a 
                            little strange, because my personal best is 85 seconds! This does 
                            encapsulate the vast majority of my runs, though. Intermediate difficulty 
                            has a range from 24 to 54 seconds, and beginner from 0 to 24.
                           It also says it\'s possible that I score below 0 on beginner difficulty - 
                           can\'t wait for that day to come!'),
                         p('The model was generated using stan_glm, with time as the output and
                           difficulty as the predictor.')
                     ),
                     mainPanel(gt_output('probTable'), br(),
                               plotOutput('posteriorPlot', height = '300px'))))),
                    #places graph with posterior predictions for times in each difficulty 
             tabPanel('Pro Model',
                      fluidPage(
                          titlePanel('eXtine\'s Win Times Model'),
                          #this page displays the stan_glm for the pro
                          sidebarLayout(
                              sidebarPanel(
                                  sliderInput(inputId = 'time_pro', label = 'Select Time (for table)',
                                              min = 50, max = 100, value = 75),
                                  #slider input for win time probability table
                                  h3('Interpretation'),
                                  p('eXtine\'s slowest possible times just skim my fastest - 
                                    I\'ll count that as a personal victory. His range of scores
                                    is much wider, from 50 to 100 seconds,
                                    likely because he had so few wins in his sample.
                                    Theoretically, though, he could score a time at least in the 50s
                                    with a perfect run. The world record, by Kamil Muranski,
                                    is 31 seconds, so eXtine remains far from glory.')
                              ),
                        mainPanel(gt_output('probTable_pro'), br(),
                                  plotOutput('posteriorPlot_pro', height = '300px')))))
                    #places posterior predictions for pro's times from stan_glm
             )),
    tabPanel("Conditional Prediction",
        fluidPage(
            titlePanel("Conditional Victory Probability Comparison"),
                sidebarLayout(
                    sidebarPanel( 
                        sliderInput(inputId = 'conditional_pred', label = 'Select Time (for table)',
                                    min = 0, max = 150, value = 75),
                        h3('Interpretation'),
                        p('This model was made using stan_glm with the binomial family to predict
                          the chance of victory using time as an indicator. As such, inputting a time
                          will output the odds of victory for each player when they reach that time.
                          eXtine\'s aggressive play is again visible; his odds of victory jump from 5% 
                          to 55% as time increases from 50 to 75 seconds. My inflection point is much later,
                          with my victory odds increasing fastest around 80 to 120 seconds.' )
                                     ),
                    mainPanel(gt_output('condTable'), br(),
                              plotOutput('condPlot', height = '300px')
                        
                    ))),
    
    ),
    
    theme = shinytheme("cerulean")
    #sets theme to cerulean
)

# Define server logic 
server <- function(input, output) {
    
    #creates end cause plot with stacked bars for difficulty
    output$colorPlot <- renderPlot({
        mines %>% 
            drop_na() %>% 
            group_by(difficulty, end_cause) %>% 
            summarise(n = n(), mean_time = mean(time)) %>%
            mutate(freq = n / sum(n)) %>% 
            ggplot(aes(fill = end_cause, y = freq,
                       x = fct_reorder(difficulty, mean_time))) + 
            geom_bar(position= "stack", stat="identity") + 
            theme_classic() +
            labs(x = 'Difficulty', y = 'Percent',
                 title = 'Distribution of Game End Causes by Difficulty',
                 fill = 'End Cause') +
            scale_fill_discrete(name = 'End Cause', labels = c('Guess','Misclick','Misflag','Win'))
        
    })
    
    #creates unstacked end cause plot for pro
    output$colorPlot_pro <- renderPlot({
        mines_pro %>% 
            drop_na() %>% 
            group_by(end_cause) %>% 
            summarise(n = n(), mean_time = mean(time)) %>%
            mutate(freq = n / sum(n)) %>% 
            ggplot(aes(fill = end_cause, y = freq, x = fct_reorder(end_cause, freq))) + 
            geom_bar(stat="identity") + 
            theme_classic() +
            labs(x = 'End Cause', y = 'Percent',
                 title = 'Distribution of Game End Causes',
                 fill = 'End Cause') +
            scale_y_continuous(labels = scales::percent) +
            theme(legend.position = 'none') +  
            scale_x_discrete(labels = c('Win','Misclick','Misflag','Guess'))
        
    })
    
    #creates actual time distribution plot for me with difficulty selecetion
    output$timePlot <- renderPlot({
        mines %>%
            drop_na() %>% 
            filter(difficulty == input$menu) %>% 
            ggplot(aes(x = time)) +
            geom_histogram(aes(y = after_stat(count/sum(count)), fill = win), bins = 15) + 
            theme_classic() +
            labs(x = 'Time (seconds)', y = 'Percent', 
                 title = 'Actual Distribution of Minesweeper Times at Game Over') +
            scale_fill_discrete(name = 'Win or Loss', labels = c('Loss', 'Win')) +
            scale_y_continuous(labels = scales::percent)
        
        
    })
    
    #creates actual time distribution plot for pro
    output$timePlot_pro <- renderPlot({
        mines_pro %>%
            drop_na() %>% 
            ggplot(aes(x = time)) +
            geom_histogram(aes(y = after_stat(count/sum(count)), fill = win), bins = 15) + 
            theme_classic() +
            labs(x = 'Time (seconds)', y = 'Percent', 
                 title = 'Actual Distribution of Minesweeper Times at Game Over') +
            scale_fill_discrete(name = 'Win or Loss', labels = c('Loss', 'Win')) + 
            scale_y_continuous(labels = scales::percent)
        
        
    })
    
    #creates time probability table with slider input for me
    output$probTable <- render_gt({
        mines_pp_data %>% 
            mutate(split = ifelse(time < input$time, 1, 0)) %>% 
            group_by(diff) %>% 
            summarize(mean = mean(split)) %>% 
            gt() %>% 
            tab_header(title = 'Probability of Getting a Time Faster than Selected Time') %>% 
            cols_label(
                diff = "Difficulty",
                mean = "Probability"
            ) %>%
            fmt_number(
                columns = vars(mean),
                decimals = 4)
    })
    #creates combined stats table
    output$statsTable <- render_gt({
        
        #finds mines per sec, winrate, avg time/mines remaining for pro
        stats_pro <- mines_pro %>% 
            filter(difficulty == 'expert') %>% 
            mutate(minerate = (99 - mines_left)/time) %>% 
            summarize(minerate = mean(minerate), winrate = sum(win)/n(), 
                      time = mean(time), mines_left = mean(mines_left)) %>% 
            mutate(player = 'eXtine')
        
        #finds mines per sec, winrate, avg time/mines remaining for me
        stats <- mines %>% 
            filter(difficulty == 'expert') %>% 
            mutate(minerate = (99 - mines_left)/time) %>% 
            summarize(minerate = mean(minerate), winrate = sum(win)/n(), 
                      time = mean(time), mines_left = mean(mines_left)) %>% 
            mutate(player = 'Matt')
        
        #unifies/formats stats table
        full_join(stats, stats_pro) %>% 
            gt() %>% 
            tab_header(title = 'Player Stats Compared') %>% 
            cols_label(
                minerate = "Mines Per Second",
                winrate = "Proportion of Wins",
                time = "Average Game Length",
                mines_left = "Average Mines Remaining",
                player = 'Player'
            ) %>% 
            fmt_number(
                columns = vars(minerate, winrate, time),
                decimals = 3)
        
    })
    
    
    #creates density plot from stan_glm posterior for my times in different difficulties
    output$posteriorPlot <- renderPlot({
        mines_pp_data %>% 
            ggplot(aes(x = time)) +
            geom_density() +
            facet_wrap(~diff) +
            labs(x = 'Win Time', y = 'Probability',
                 title = 'Predicted Win Time Distribution In Each Difficulty') + 
            theme_classic()
        
        
    })
    
    #creates density plot from stan_glm posterior for pro times
    output$posteriorPlot_pro <- renderPlot({
        mines_pp_pro %>% 
            ggplot(aes(x = time)) +
            geom_density() +
            labs(x = 'Win Time (seconds)', y = 'Probability',
                 title = 'Predicted Win Times In Expert Difficulty') + 
            theme_classic()
    })
    
    #creates time probability table with slider input for pro
    output$probTable_pro <- render_gt({
        mines_pp_pro %>% 
            mutate(split = ifelse(time < input$time_pro, 1, 0)) %>% 
            summarize(mean = mean(split)) %>% 
            gt() %>% 
            tab_header(title = 'Probability of Getting a Time Faster than Selected Time') %>% 
            cols_label(
                mean = "Probability"
            ) %>%
            fmt_number(
                columns = vars(mean),
                decimals = 4)
    })
    
    #creates table predicting win chance conditioned on reaching a certain time
    output$condTable <- render_gt({
        
        time_pred_pro <- tibble(time = 1:150) %>% 
            mutate(probability = map_dbl(time, ~plogis(-8.426 + .*.115))) %>% 
            mutate(player = 'eXtine')
        
        time_pred <- tibble(time = 1:150) %>% 
            mutate(probability = map_dbl(time, ~plogis(-9.924 + .*.093))) %>% 
            mutate(player = 'Matt')
        
        full_join(time_pred_pro, time_pred) %>%
            filter(time == input$conditional_pred) %>% 
            gt() %>%
            tab_header(title = 'Probability of A Game Ending in Victory ',
                       subtitle = 'Conditioned on reaching the selected time') %>%
            fmt_number(
                columns = vars(probability),
                decimals = 4)  
    })
    
    output$condPlot <- renderPlot({
        time_pred_pro <- tibble(time = 1:150) %>% 
            mutate(probability = map_dbl(time, ~plogis(-8.426 + .*.115))) %>% 
            mutate(player = 'eXtine')
        
        time_pred <- tibble(time = 1:150) %>% 
            mutate(probability = map_dbl(time, ~plogis(-9.924 + .*.093))) %>% 
            mutate(player = 'Matt')
        
        full_join(time_pred_pro, time_pred) %>% 
            ggplot(aes(x = time, y = probability, color = player)) +
            geom_line() +
            theme_classic() +
            labs(title = 'Odds of Victory Conditioned on Reaching the Corresponding Time',
                 x = 'Time', y = 'Probability of Victory')
    })
    
    #all remaining outputs load images for tutorial
    output$t1 <- renderImage({
        
        list(src = "tutorial/t1.PNG",
             width = 400,
             height = 500,
             alt = "")
    }, deleteFile = FALSE)
    
    output$t2 <- renderImage({
        
        list(src = "tutorial/t2.PNG",
             width = 400,
             height = 500,
             alt = "")
    }, deleteFile = FALSE)
    
    output$t3 <- renderImage({
        
        list(src = "tutorial/t3.PNG",
             width = 400,
             height = 500,
             alt = "")
    }, deleteFile = FALSE)
    
    output$t4 <- renderImage({
        
        list(src = "tutorial/t4.PNG",
             width = 400,
             height = 500,
             alt = "")
    }, deleteFile = FALSE)
    
    output$t5 <- renderImage({
        
        list(src = "tutorial/t5.PNG",
             width = 400,
             height = 500,
             alt = "")
    }, deleteFile = FALSE)
    
    output$t6 <- renderImage({
        
        list(src = "tutorial/t6.PNG",
             width = 400,
             height = 500,
             alt = "")
    }, deleteFile = FALSE)
    
    output$t7 <- renderImage({
        
        list(src = "tutorial/t7.PNG",
             width = 400,
             height = 500,
             alt = "")
    }, deleteFile = FALSE)
    
}

# Run the application 
shinyApp(ui = ui, server = server)




