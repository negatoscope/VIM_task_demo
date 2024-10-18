library(shiny)

ui <- fluidPage(
  titlePanel("Dream Color Saturation Study"),
  
  verbatimTextOutput("debugInfo"),
  
  conditionalPanel(
    condition = "output.currentPhase == 1",
    textInput("participantID", "Participant ID:"),
    actionButton("startButton", "Start Experiment")
  ),
  
  conditionalPanel(
    condition = "output.currentPhase == 2 || output.currentPhase == 3",
    h3("Select the image that most resembles your dream's color saturation:"),
    uiOutput("imageSelection"),
    sliderInput("progressBar", "", min = 1, max = 15, value = 1, step = 1),
    actionButton("selectButton", "Select")
  ),
  
  conditionalPanel(
    condition = "output.currentPhase == 4",
    radioButtons("dreamFrequency", "How often do you remember your dreams when you wake up?",
                 choices = c(
                   "Never" = 0,
                   "Less than once a month" = 1,
                   "About once a month" = 2,
                   "2-3 times a month" = 3,
                   "About once a week" = 4,
                   "2-3 times a week" = 5,
                   "Almost every morning" = 6
                 )),
    radioButtons("dreamVividness", "How vivid was the visual scene of the dream you just recalled?",
                 choices = c(
                   "Not at all vivid (no visual detail)" = 1,
                   "Slightly vivid (some vague images)" = 2,
                   "Moderately vivid (some clear images, but not very detailed)" = 3,
                   "Very vivid (most details are clear)" = 4,
                   "Extremely vivid (all details are sharp and clear, as if seeing it in real life)" = 5
                 )),
    actionButton("submitButton", "Submit")
  )
)

server <- function(input, output, session) {
  currentPhase <- reactiveVal(1)
  selectedImage1 <- reactiveVal(NULL)
  selectedImage2 <- reactiveVal(NULL)
  
  output$debugInfo <- renderText({
    paste("Current Phase:", currentPhase(), "Current Image:", input$progressBar)
  })
  
  output$currentPhase <- reactive({ currentPhase() })
  outputOptions(output, "currentPhase", suspendWhenHidden = FALSE)
  
  observeEvent(input$startButton, {
    print("Start button clicked")
    currentPhase(2)
    updateSliderInput(session, "progressBar", value = 1)
  })
  
  observeEvent(input$selectButton, {
    print("Select button clicked")
    if (currentPhase() == 2) {
      selectedImage1(input$progressBar)
      currentPhase(3)
      updateSliderInput(session, "progressBar", value = 1)
    } else if (currentPhase() == 3) {
      selectedImage2(input$progressBar)
      currentPhase(4)
    }
  })
  
  output$imageSelection <- renderUI({
    imageName <- if (currentPhase() == 2) {
      paste0("saturation_", sprintf("%02d", input$progressBar), ".jpg")
    } else {
      paste0("saturation_", sprintf("%02d", input$progressBar), "_2.jpg")
    }
    img(src = imageName, width = "100%", height = "auto")
  })
  
  observeEvent(input$submitButton, {
    print("Submit button clicked")
    results <- data.frame(
      ParticipantID = input$participantID,
      DreamFrequency = input$dreamFrequency,
      DreamVividness = input$dreamVividness,
      Image1 = paste0("saturation_", sprintf("%02d", selectedImage1()), ".jpg"),
      Image2 = paste0("saturation_", sprintf("%02d", selectedImage2()), "_2.jpg")
    )
    write.table(results, file = "dream_study_results.csv", append = TRUE, sep = ",",
                col.names = !file.exists("dream_study_results.csv"), row.names = FALSE)
    
    currentPhase(1)
    selectedImage1(NULL)
    selectedImage2(NULL)
    updateTextInput(session, "participantID", value = "")
    updateRadioButtons(session, "dreamFrequency", selected = character(0))
    updateRadioButtons(session, "dreamVividness", selected = character(0))
    updateSliderInput(session, "progressBar", value = 1)
  })
}

shinyApp(ui, server)