library(shiny)

shinyUI(navbarPage("Survival Analysis",
                   tabPanel("Exploratory Analysis",
                            sidebarLayout(
                              sidebarPanel(
                                
                                checkboxGroupInput(inputId="fillvalue",
                                                   label = "Filter by",
                                                   choices = levels(BMT$Patient.s.Sex),selected = c("F","M")
                                ),
                                selectInput(inputId="xvalue",
                                            label = "Value for x-axis",
                                            choices = c("TypeOfMM","RenalFailure",
                                                        "Plasmacytoma","Therapy")
                                ),
                                width=2
                                ),
                              
                              # Show a plot of the generated distribution
                              mainPanel(
                                plotlyOutput("Age"),
                                plotlyOutput("bar") 
                                )
                              )
                            ),
                   tabPanel("Survival Analysis",
                            sidebarLayout(
                              sidebarPanel(
                                selectInput(inputId="event",
                                            label = "Event",
                                            choices = c("RenalFailure","Plasmacytoma",
                                                        "RemissionStatusPreTx","RemissionStatusPostTx",
                                                        "TotalNoTherapy","ISS")
                              ),
                              selectInput(inputId="analysis",
                                          label="Period for analysis",
                                          choices=c("From date of transplant"="TD",
                                                    "From date of relapse"="TD_R")
                                
                              ),
                              width=3
                              ),
                              mainPanel(
                               plotOutput("overall"),
                               plotOutput("events")
                              )
                            )
                             
                            )
                   )
)