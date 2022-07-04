library("shiny")
library("dplyr")
library("koscrap")
library("reactable")
library("htmltools")

# UI Side
ui <- fluidPage(
    # Application title
    titlePanel("네이버 뉴스 검색 애플리케이션"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            textInput("client_id", 
                      label = h4("Client ID:"), 
                      value = ""),
            textInput("client_secret", 
                      label = h4("Client Secret:"), 
                      value = ""),            
            textInput("keyword", 
                      label = h4("검색 키워드:"), 
                      value = ""),
            radioButtons("sort", label = h4("정렬 옵션:"),
                         choices = list("날짜순" = "date", "유사도순" = "sim"), 
                         selected = "date"),           
            sliderInput("max_record", label = h4("검색 건수:"), min = 0, 
                        max = 500, value = 100, step = 100),
            actionButton("search_keyword", label = "뉴스 검색", 
                         icon = icon("newspaper")),
            width = 3
        ),

        # Reactable에 검색 결과 리스트업
        mainPanel(
            reactableOutput("news_list"),
            width = 9
        )
    )
)

# Server Side
server <- function(input, output) {
    newsList <- reactiveValues(
        list = data.frame(
            title = character(0),
            description = character(0),
            publish_date = character(0),
            link = character(0),
            stringsAsFactors = FALSE
        ) %>% 
            reactable(
                defaultColDef = colDef(
                    align = "left"
                ),
                columns = list(
                    title = colDef(
                        name = "타이틀",
                        width = 250,
                    ),
                    description = colDef(name = "뉴스내용 요약"),
                    publish_date = colDef(
                        name = "뉴스 계시시간",
                        width = 150,
                    ),
                    link = colDef(
                        name = "뉴스 링크",
                        width = 250,
                        html = TRUE,
                        cell = function(url) {
                            htmltools::tags$a(href = as.character(url), target = "_blank", as.character(url))
                        }
                    )    
                ),
                showPageSizeOptions = TRUE,
                pageSizeOptions = c(5, 10, 15), 
                defaultPageSize = 5,
                bordered = TRUE,
                highlight = TRUE
            )    
    )
    
    output$news_list <- renderReactable({
        newsList$list
    })
    
    observeEvent(input$search_keyword, {
        # 3개의 텍스트는 반드시 입력해야 함
        req(input$keyword)
        req(input$client_id)
        req(input$client_secret)

        result <- koscrap::search_naver(
                query = input$keyword,
                sort  = input$sort,
                chunk = min(input$max_record, 100),
                max_record = input$max_record,
                do_done = TRUE,
                client_id = input$client_id,
                client_secret = input$client_secret) %>%
                mutate(title = title_text) %>%
                mutate(description = description_text) %>%
                mutate(publish_date = stringr::str_remove_all(publish_date,
                                                              "[[:alpha:]]")) %>%
                select(title, description, publish_date, link) %>% 
        reactable(
            defaultColDef = colDef(
                align = "left"
            ),
            columns = list(
                title = colDef(
                    name = "타이틀",
                    width = 250,
                ),
                description = colDef(name = "뉴스내용 요약"),
                publish_date = colDef(
                    name = "뉴스 계시시간",
                    width = 150,
                ),
                link = colDef(
                    name = "뉴스 링크",
                    width = 250,
                    html = TRUE,
                    cell = function(url) {
                        htmltools::tags$a(href = as.character(url), target = "_blank", as.character(url))
                    }
                )    
            ),
            showPageSizeOptions = TRUE,
            pageSizeOptions = c(5, 10, 15), 
            defaultPageSize = 5,
            bordered = TRUE,
            highlight = TRUE
        )
        
        newsList$list <- result
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
