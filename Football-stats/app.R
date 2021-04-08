library(shiny)
library(dplyr)
library(RColorBrewer)

#Upload the data and adjust them
df <- read.csv("data/I1.csv")
df <- select(df, c("Date", "HomeTeam", "AwayTeam", 
                   "FTHG", "FTAG", "FTR"))
df$Date <- as.Date(df$Date, format = "%d/%m/%Y")

#Creates new columns for the data to be plotted
df$TG <- df$FTHG + df$FTAG #Number of total goal in each match
df$EvsO <- ifelse(df$TG%%2==1, "Odd", "Even")
df$Over <- ifelse(df$TG > 2, "At least 3 goals", "Less than 3 goals") 

#Create a list of the teams 
t <- sort(unique(df$HomeTeam))
names(t) <- t

#Create a list for categories 
cat <- list("Wins/Losses/Draws" = "WDL", 
            "Even vs Odd" = "EvsO",
            "Matches with at least 3 goals" = "Over")

#Create a function to fill the column of Wins/Losses/Draws according to the selected team
col_filler <- function(df, team) {
    for (i in 1:length(df$FTR)) {
        if (df$FTR[i] == "H" & df$HomeTeam[i] == team) {
            df$WDL[i] <- "Wins"
        } else if (df$FTR[i] == "H" & df$AwayTeam[i] == team) {
            df$WDL[i] <- "Losses"
        } else if (df$FTR[i] == "A" & df$HomeTeam[i] == team) {
            df$WDL[i] <- "Losses"
        } else if (df$FTR[i] == "A" & df$AwayTeam[i] == team) {
            df$WDL[i] <- "Wins"
        } else {
            df$WDL[i] <- "Draws"
        }
    }
    return(df)
}

# Define UI 
ui <- fluidPage(
    
    titlePanel("Football stats"),
    
    sidebarLayout(
        sidebarPanel(
            helpText("For the selected team and the selected day range, 
                     returns a plot bar showing the distribution of the matches
                     according to the selected category."),
            
            selectInput("team", "Select a team:", 
                        choices = t, 
                        selected = t[1]),
            
            dateRangeInput("days",
                           "Select a day range:",
                           start = min(df$Date),
                           end = max(df$Date), 
                           min = min(df$Date),
                           max = max(df$Date)),
            
            radioButtons("stat", "Select a category:",
                         choices = cat,
                         selected = "WDL")
        ),
        
        # Show a plot of the generated distribution
        mainPanel(
            plotOutput("PlotBar")
        )
    )
)

# Define server logic required to draw the plot
server <- function(input, output) {
    output$PlotBar <- renderPlot({
        
        #Select the team and the day range
        finalData <- subset(df, subset = (HomeTeam == input$team | AwayTeam == input$team)
                            & Date >= input$days[1] & Date <= input$days[2])
        
        #If needed, fill the Wins/Draws/Losses column according to the selected team
        if (input$stat == "WDL"){
            finalData <- col_filler(df=finalData, team=input$team)
        }
        
        #Create the vector to be plot
        l <- vector()
        finalData <- finalData[[input$stat]]
        labels <- sort(unique(finalData), decreasing = TRUE)
        #Count the occurrences for each unique value
        for (i in labels) {
            l[i] <- sum(finalData==i)
        }
        
        #Create a customized title
        title <- names(cat)[match(input$stat, cat)]
        
        #Plot
        my_bar <- barplot(l, col=brewer.pal(length(l), "Set2"), 
                ylab = "Frequencies", xlab = "Categories", cex.lab=1.4,
                ylim = c(0,max(l)+3), main=title, cex.main=2, font.main=4,
                col.main="#FF0000")
        text(my_bar, l+1.5, l, cex=1.5)
    })
    }
    
    # Run the application 
    shinyApp(ui = ui, server = server)
    
