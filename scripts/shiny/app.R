library(shiny)
library(qlcData)
library(qlcVisualize)


word <- "wurst"
aligned <- paste0("../", word, "/", word, "_aligned.txt")
recoding <- paste0("../", word, "/", word, "_recoding.yml")
data_to_view <- "RECODE"
aligned_column <- 3
map_main <- paste(word, data_to_view, "column:", aligned_column)

# load basemap
load("../../data/KDSAvoronoi.Rdata")

# load manually corrected data
aligned_data <- read.table(aligned, header = TRUE, sep = "\t")

# sounds in "berg"
ALIGN <- as.character(aligned_data$ALIGN)
ALIGN[aligned_data$COGID != 1] <- NA
ALIGN[grep("NA",ALIGN)] <- NA
ALIGN <- sapply(ALIGN, strsplit, split = " ")
ALIGN <- do.call(rbind,ALIGN)
dimnames(ALIGN) <- NULL
ALIGN <- as.data.frame(ALIGN)

# recode data according to edited recoding template
RECODE <- recode(recoding, data = ALIGN)

if (data_to_view == "ALIGN") {
	data <- as.factor(ALIGN[,aligned_column])
} else {
	data <- as.factor(RECODE[,aligned_column])
}

userChoice <- levels(data)

choiceNames <- paste(userChoice, " (", table(data), ")", sep = "")
names(userChoice) <- choiceNames

# Define UI for app ----
ui <- fluidPage(

  # App title ----
  titlePanel("KDSA testing"),

  # Sidebar layout with input and output definitions ----
  sidebarLayout(
  	position = "left",
  	fluid = TRUE,

    # Sidebar panel for inputs ----
    sidebarPanel(

      # Input: Slider for the number of bins ----
      checkboxGroupInput(
      	inputId = "selectToShow",
      	label = "Show:",
      	choices = as.list(userChoice)
      ),
      width = 2
    ),

    # Main panel for displaying outputs ----
    mainPanel(

      # Output: Map ----
      plotOutput(outputId = "map")

    )
  )
)

# Define server logic ----
server <- function(input, output) {

  output$map <- renderPlot({

		cols <- rep("grey", times = length(userChoice))
		options <- c("red", "blue", "green", "purple", "yellow")
		
		for (sel in input$selectToShow) {
			cols[which(userChoice == sel)] <- options[1]
			options <- tail(options, -1)
		}
		
		vmap(v, col = cols[data], border = NA)
		legend("bottomright"
				, legend = userChoice
				, fill = cols
				, cex = .7
				)
		title(main = map_main)
		
    })

}

shinyApp(ui = ui, server = server)
