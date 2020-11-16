#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.

#install and load necessary packages
#if packages already installed and loaded, skip.
#But if packages installed and not loaded then load
#If packages not installed, install and load.
packages = c("igraph", "dplyr","shiny","readxl")

## Now load or install&load all
package.check <- lapply(
    packages,
    FUN = function(x) {
        if (!require(x, character.only = TRUE)) {
            install.packages(x, dependencies = TRUE)
            library(x, character.only = TRUE)
        }
    }
)


#filePath<-"C:/Users/luolaw/Desktop/AutoCoev/raftCoev/final/raft_0_00001.xlsx"
data<-read_excel(filePath)

data$Best.P.value<-as.numeric(data$P_value)


# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Predicted coevolution network"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            sliderInput("bins",
                        "Filter with P-values:",
                        min = min(data$Best.P.value),
                        max = max(data$Best.P.value),
                        value = (max(data$Best.P.value))*100,
                        step = (max(data$Best.P.value))/100,
                        width = "400px"),
            
        selectInput(inputId="shape",
                    label = "Select node shape:",
                    choices=c('sphere', 'circle', 'csquare', 'square','rectangle', 'pie' ),
                    selected='sphere',
                    width = "400px"),
        selectInput(inputId="nodeColour",
                    label = "Select node colour:",
                    choices=c('cyan', 'red', 'blue', 'green','yellow', 'black','magenta','pink','gold','orange' ),
                    selected='cyan',
                    width = "400px"),
        
        selectInput(inputId="labelColour",
                    label = "Select label colour:",
                    choices=c('cyan', 'red', 'blue', 'green','yellow', 'black', 'magenta','pink','gold','orange' ),
                    selected='red',
                    width = "400px"),
        
        numericInput("nodeSize", "Node size:", 50, min = 10, max = 100),
        numericInput("edgeSize", "Edge size:", 2, min = 1, max = 100),
        numericInput("labelFont", "Label Font:", 1, min = 1, max = 10)
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("distPlot",
                      width = "auto",
                      height = "400px")
        )
    )
)
# Define server logic required to draw a histogram
server <- function(input, output) {

    output$distPlot <-renderPlot({
        data1<-filter(data,data$P_value<=input$bins)
        vert<- data.frame(data1$source)
        vert2<-data.frame(data1$target)%>%dplyr::rename("data1.source"="data1.target")
        vert3<-unique(rbind(vert,vert2))
        netg <- graph_from_data_frame(data1, directed=FALSE, vertices=vert3)
        coords <- layout.auto(netg)
        plot(netg, vertex.label.cex = input$labelFont, vertex.color=input$nodeColour, vertex.size=input$nodeSize, vertex.frame="NA",edge.width=input$edgeSize,
             vertex.shape=input$shape,vertex.label.color = input$labelColour, layout=coords,
             rescale=FALSE, xlim=range(coords[,1]), ylim=range(coords[,2]))
    },height = 1000, width = 1000)
}

# Run the application 
shinyApp(ui = ui, server = server)


