library(shiny)
library(DT)
library(data.table)


#Input data and create excel: https://stackoverflow.com/questions/68864368/copying-cells-from-excel-and-pasting-them-in-a-r-shiny-application-using-text-ar

ui <-fluidPage(
  titlePanel("Pooled Sample Data"),
  textAreaInput("pasted", "Paste your data here"), 
  dataTableOutput("table"),
  titlePanel("Pooled Sample Data Output"),
  dataTableOutput("pooled_table")
)

server <- function(input, output, session) {
  titlePanel("Pooling Data")
  
  input_df <- reactive({ 
    
    if (input$pasted != '') {
      
      input_df <- fread(paste(input$pasted, collapse = "\n"))
      input_df <-as.data.frame(input_df)
      input_df
      
    }
    
  })
  
  output$table <- renderDataTable({
    input_df()},
    filter="top", class = 'hover cell-border stripe', editable= TRUE,extensions= 'Buttons', options = list(dom = 'Bfrtip',pageLength =3,buttons = c('copy','csv','excel','pdf','print'), scrollX=TRUE),server=FALSE)
  
  df_pooled <- reactive({ 
    
    if (input$pasted != '') {
      df <- input_df()
      print(head(df))
      df_no_concentrations <- df[,seq(2,ncol(df))]
      df_no_concentrations[is.na.data.frame(df_no_concentrations)] <- 0

      total_replicates <- rowSums(df_no_concentrations[, seq(3, ncol(df_no_concentrations), 3)],na.rm = T)

      n_replicates <- df_no_concentrations[, seq(3, ncol(df_no_concentrations), 3)]

      means <- df_no_concentrations[,seq(1, ncol(df_no_concentrations), 3)]
      stdev <-df_no_concentrations[,seq(2, ncol(df_no_concentrations), 3)]

      # Compute weighted mean by row
      weighted_mean <- rowSums(means*n_replicates)/total_replicates

      # Combine Standard deviation
      ##q1, q2, etc =(n1-1)*var(x1)^2 + n1*mean(x1)^2
      ##qc = q1 + q2 + ....
      qc <- rowSums((n_replicates-1)*stdev^2 + n_replicates*means^2)

      #sd=sqrt((qc-(n1+n2)*mean(x)^2/(n1+n2-1))
      pooled_stdev <- sqrt((qc - total_replicates*weighted_mean^2)/(total_replicates-1))

      df_pooled <- cbind(df[,1], weighted_mean, pooled_stdev, total_replicates)
      colnames(df_pooled) <- c(colnames(df)[1], "Weighted Mean", "Pooled Standard Deviation", "Total Number of Replicates")
      df_pooled

    }
    
  })

  
  output$pooled_table <- renderDataTable({
    df_pooled()},
    filter="top", class = 'hover cell-border stripe', editable= TRUE,extensions= 'Buttons', options = list(dom = 'Bfrtip',pageLength =10,buttons = c('copy','csv','excel','pdf','print'), scrollX=TRUE),server=FALSE)
  
  
  
}

# Run the application 
shinyApp(ui = ui, server = server)

  