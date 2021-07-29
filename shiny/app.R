source('helperFunctions.R')

library(shiny)


ui <- fluidPage(

    titlePanel("Smoothing procedure as used by the classification algorithm."),

    # Sidebar with a slider input for number of bins 
    fluidRow(
        column(12, plotOutput('orgPlot'))
    ),
    fluidRow(
        column(4,fileInput(inputId = "dataPath",
                            'path to the excel file containing data'),align = 'center'),
        column(4, tableOutput('crossTable')),
        column(4, textOutput('interrater'))
    ),
    fluidRow(
        column(4,sliderInput("breakVal",
                             'weight of break-frames',
                             min = 0,
                             max = 4,
                             value = 2,
                             step = .1),align = 'center'),
        column(4,sliderInput("betwVal",
                             HTML('weigth of frames coded as "overlap"'),
                             min = 0,
                             max = 4,
                             value = 2,
                             step = .1), align = 'center'),
        column(4,sliderInput("kernel",
                             'width of the gaussian-weighted-mean filter',
                             min = 1,
                             max = 10000,
                             value = 100,
                             step = 1), align = 'center')
        
    )#,
    # fluidRow(
    #     column(4,#selectInput('classifier',
    #               #           'Entscheidungsalgorithmus:',
    #                #          choices = list('Local Mean'='lmean'),
    #                 #         selected = 'lmean'),
    #            sliderInput("kernel",
    #                        'Wie viele Werte sollen für das local mean genutzt werden?',
    #                        min = 1,
    #                        max = 10000,
    #                        value = 100,
    #                        step = 1))
    #     
    # )
    
)



server <- function(input, output) {
    dataGen <- reactive({
      if(is.null(input$dataPath$datapath)){
        path <- 'P24_P.xlsx'
      }else{
        path <- input$dataPath$datapath
      }
      a <- makeDataAccessible(path)
        if(is.null(a)){
            a
        }else{
            a %>% mutate(code = ifelse(code == 0, input$breakVal, code),
                         code = ifelse(code == 2, input$betwVal, code))  
        }
    })
    smoothedData <- reactive({
        dataGen() %>%  
            smoothIt(alg = 'lmean',pars =  list(kernel = input$kernel))
            })
    output$orgPlot <- renderPlot({
        makeGraph(smoothedData())
        
    })
    output$crossTable <- renderTable({
        a <- smoothedData()
        if(is.null(a)){
            NULL
        }else{
            a %>% rename('Algorithm' = matches('filCode'),
                         'Human' = matches('phase')) %>% 
                count(Algorithm, Human) %>% 
                mutate(across(where(is.character), ~recode(.,
                                                           'Transit' = 'Transition',
                                                           'Aktion' = 'Action')))
        }
    },
    rownames = T,
    colnames = T)
    output$interrater <- renderText({
        a <- smoothedData()
        if(!is.null(a)){
            a <- a %>% 
                filter(name == 'filtered')
            paste0('Krippendorffs alpha: ',round(irr::kripp.alpha(matrix(c(as.numeric(factor(a$filCode)), as.numeric(factor(a$phase))), nrow = 2,byrow = T),method = 'nominal')$value,3), '\n\n',
                   'Cohens Kappa: ',round(irr::kappa2(matrix(c(as.numeric(factor(a$filCode)), as.numeric(factor(a$phase))), ncol = 2,byrow = F))$value,3)) 
        }else{
            NULL
        }
        
    })
}



smoothIt <- function(data,alg,pars){
    if(alg == 'none'){
        return(x)
    }else{
        if(alg=='lmean'){
            out <- localMeaner(data,kernel = pars$kernel)
        }
    }
}


# Run the application 
shinyApp(ui = ui, server = server)
