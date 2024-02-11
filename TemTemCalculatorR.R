library(shiny)

ui <- fluidPage(
  

  

  titlePanel("TemTem Catch Calculator"),

  sidebarLayout(
    

    sidebarPanel(
      

      sliderInput(inputId = "perHP",
                  label = "Percentage of HP left",
                  min = 1,
                  max = 100,
                  value = 100),
      selectInput("cardc",
                  "TemCard", 
                  choices=c("TemCard", "TemCard+", "TemCard++"), 
                  selected = "TemCard"),
     # StatusCONDITIONS <- c("Asleep", "Frozen", "Burned", "Poisoned", "Cold", "Trapped", "Exhausted", "Seized", "Alerted", 
      #                      "Invigorated", "Evading", "None")
      selectInput("statusC",
                  "Status Condition 1", 
                  choices=c("Asleep", "Frozen", "Burned", "Poisoned", "Cold", "Trapped", "Exhausted", "Seized", "Alerted", 
                            "Invigorated", "Evading", "None"), 
                  selected = "None"),
      selectInput("statusC2",
                  "Status Condition 2", 
                  choices=c("Asleep", "Frozen", "Burned", "Poisoned", "Cold", "Trapped", "Exhausted", "Seized", "Alerted", 
                            "Invigorated", "Evading", "None"), 
                  selected = "None"),
      checkboxInput("flcbin", 
                    label = "FourLeafClover Catchratebonus", 
                    value = FALSE),
    
    sliderInput("baseHP", label = "BaseHP Stat of species", min = 22, max = 105, value = 50),
    sliderInput("TemTemLevel", label = "Level of Target", min = 1, max = 100, value = 50),
    sliderInput("SV", label = "SV Stats on HP", min = 0, max = 50, value = 20),
    numericInput("TameChance", label = "TemtemCaptureRate ( 30 - 200 )", value = 80),
    numericInput("attemptsin", label = "Number of attempts (Positive Integers)", value = 10),
    if ("statusC" == "statusC2")
      {updateSelectInput(inputId = "statusC2", choices = c("Asleep", "Frozen", "Burned", "Poisoned", "Cold", "Trapped", "Exhausted", "Seized", "Alerted", 
                                                           "Invigorated", "Evading", "None"), 
                         selected = "None")},
    
    p("Necessary Data taken from the official TemTem Wiki"),
    a("Click here for the Source", href="https://temtem.wiki.gg/wiki/Taming#Capture_formula"),
    br(),br(),
    p("Das Offizielle Temtem Wiki war die Quelle für die nötigen Daten, um die Berechnungen durchzuführen"),
    a("Hier klicken für die Quelle", href="https://temtem.wiki.gg/wiki/Taming#Capture_formula"),
  
    fluidRow(column(2, verbatimTextOutput("value")))
    
    
    
    ),
  
    mainPanel(
      

      plotOutput(outputId = "distPlot")
      
    )
  )
)



server <- function(input, output) {
  

  output$distPlot <- renderPlot({
    
  
    statusCondition1 <- 1.0
    if (input$statusC == "Asleep" || input$statusC == "Frozen")
    {statusCondition1 <- 1.5}
    else if (input$statusC == "Burned" || input$statusC == "Poisened")
    {statusCondition1 <- 1.25}
    else if (input$statusC == "Cold" || input$statusC == "Trapped")
    {statusCondition1 <- 1.2}
    else if (input$statusC == "Exhausted" || input$statusC == "Seized")
    {statusCondition1 <- 1.1}
    else if (input$statusC == "Alerted" || input$statusC == "Invigorated")
    {statusCondition1 <- 0.8}
    else if (input$statusC == "Evading")
    {statusCondition1 <- 0.5}
    
    statusCondition2 <- 1.0
    if (input$statusC == input$statusC2)
    {{updateSelectInput(inputId = "statusC2", selected = "None")}}
    else{
    if (input$statusC2 == "Asleep" || input$statusC2 == "Frozen")
    {statusCondition2 <- 1.5}
    else if (input$statusC2 == "Burned" || input$statusC2 == "Poisened")
    {statusCondition2 <- 1.25}
    else if (input$statusC2 == "Cold" || input$statusC2 == "Trapped")
    {statusCondition2 <- 1.2}
    else if (input$statusC2 == "Exhausted" || input$statusC2 == "Seized")
    {statusCondition2 <- 1.1}
    else if (input$statusC2 == "Alerted" || input$statusC2 == "Invigorated")
    {statusCondition2 <- 0.8}
    else if (input$statusC2 == "Evading")
    {statusCondition2 <- 0.5}
    }
    
    if (input$TameChance < 30 || is.na(input$TameChance) )
    {updateSelectInput(inputId = "TameChance", selected = 30)}
    if (input$attemptsin < 1 || is.na(input$attemptsin))
    {updateSelectInput(inputId = "attemptsin", selected = 1)}
    
    attempts <- input$attemptsin
    baseV <- input$baseHP
    ttLevel <- input$TemTemLevel
    sv <- input$SV
    catchRate <- input$TameChance
    flcb <- 1.0
    if (input$flcbin)
    {
      flcb <- 1.1
    }
    card <- 1
    if (input$cardc == "TemCard+")
    {
      card <- 1.5
    }
    if (input$cardc == "TemCard++")
    {
      card <- 2.5
    }
    maxHP <- ceiling((((1.5*baseV) + sv)*ttLevel)/80 + (sv*baseV*ttLevel)/20000 + ttLevel + 15)
    curHP <- floor(input$perHP * maxHP / 100)
    aValue <- ((((4*maxHP)-(3*curHP)) * catchRate * card) / ((2*maxHP) + (10*ttLevel)) * flcb * statusCondition1 * statusCondition2 )
    bValue <- 1000000/sqrt(sqrt(21000000/aValue))
    shakeChance <- bValue / 50000
    fourShake <- shakeChance ^ 4

    if (shakeChance < 1)
    {mydata <- data.frame(xcord = seq(1,as.numeric(attempts)),ycord=1 - ((1 - fourShake) ^ seq(1,as.numeric(attempts))))
    ggplot(mydata, aes(x=xcord))+geom_point(aes(y=ycord), color="black")+scale_x_continuous(breaks=seq(0,attempts,by=1))+xlab("Number of Attempts")+ylab("Chance of Succesful Catch (1 means 100%)")+
      ggtitle("Probability of successfully Catching the Temtem")}
    else 
    {mydata <- data.frame(xcord = seq(1,as.numeric(attempts)),ycord=1 ^ seq(1,as.numeric(attempts)))
    ggplot(mydata, aes(x=xcord))+geom_point(aes(y=ycord), color="black")+scale_x_continuous(breaks=seq(0,attempts,by=1))+xlab("Guaranteed Success on each Attempt")+ylab("Chance of Succesful Catch (1 means 100%)")+
      ggtitle("Probability of successfully Catching the Temtem")}
    
    
    
    
  })
  
}

shinyApp(ui, server)