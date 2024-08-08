#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)
library(dplyr)
library(DT)
library(tidyr)
library(lubridate)
library(ggplot2)
library(formattable)
library(readr)
library(RColorBrewer)

########################################################
########################################################

options(shiny.maxRequestSize = 900*1024^2)  # limit to 900MB

########################################################
########################################################

# fba <- read_csv("~/Documents/repeat purchase for clients/repeatPurchase Willas Kitchen/WillasKitchenRPR_18Months.csv")
# fbm <- read_csv("~/Downloads/fbmWP.csv")
#
# colnames(fba)
# colnames(fbm)
# cleanDF <- clean_data(testDF)
# monthDF <- month_group(cleanDF, N = 1)
# groupDF <- group_orders(cleanDF, N = 1)

dspl_tbl_clean <- function(df){
  df <- df %>%
    dplyr::rename_with(~stringr::str_replace_all(., " ", "_")) %>%
    dplyr::rename_with(~stringr::str_replace_all(., "[:punct:]", "")) %>%
    dplyr::rename_with(~stringr::str_replace_all(., "[:space:]", ""))


  cleaned_data <- df %>%
    dplyr::select(c(PurchaseDate, BuyerEmail, ShippedQuantity, ItemPrice, AmazonOrderId, ShippingPostalCode, MerchantSKU)) %>%
    # copilot give the monthYr as follows "july 2024" instead of "2024-07"
    dplyr::mutate(monthYr = format(PurchaseDate, "%B %Y"))
  return(cleaned_data)
}

clean_data <- function(df, dropSameOrders = TRUE) {
  required_columns <- c("Amazon Order Id", "Reporting Date", "Buyer Email", "Item Price", "Shipping Postal Code", "Shipped Quantity", "Merchant SKU")

  # Check if all required columns are present
  missing_columns <- setdiff(required_columns, names(df))
  if (length(missing_columns) > 0) {
    stop(paste("Missing required columns:", paste(missing_columns, collapse = ", ")))
  }

  df %>%
    select(all_of(required_columns)) %>%
    mutate(
      `Buyer Email` = trimws(`Buyer Email`),
      `Reporting Date` = lubridate::ymd_hms(`Reporting Date`, tz = "UTC"),
      `Reporting Month` = format(`Reporting Date`, "%Y-%m")
    ) %>%
    filter(`Buyer Email` != "") %>%
    {if(dropSameOrders) distinct(., `Amazon Order Id`, `Buyer Email`, .keep_all = TRUE) else .} %>%
    arrange(`Reporting Date`, `Buyer Email`)
}

month_group <- function(df, N = 3) {
  grouped_orders <- df %>%
    arrange(`Reporting Date`) %>%
    group_by(`Buyer Email`) %>%
    mutate(
      cumCount = row_number(),
      cumSpend = cumsum(`Item Price`),
      purchase_category = case_when(
        cumCount <= N ~ as.character(cumCount),
        TRUE ~ paste(N + 1, "or more")
      )
    ) %>%
    ungroup() %>%
    mutate(`Reporting Month` = as.character(`Reporting Month`))

  month_grouped <- grouped_orders %>%
    group_by(`Reporting Month`, purchase_category) %>%
    summarise(
      count = n(),
      spendPerMonth = sum(`Item Price`),
      .groups = "drop"
    )

  uniqueMonths <- unique(month_grouped$`Reporting Month`)
  categories <- c(as.character(1:N), paste(N + 1, "or more"))
  full_data <- expand.grid(
    `Reporting Month` = uniqueMonths,
    purchase_category = categories,
    stringsAsFactors = FALSE
  )

  merged_data <- full_data %>%
    left_join(month_grouped, by = c("Reporting Month", "purchase_category")) %>%
    tidyr::replace_na(list(count = 0, spendPerMonth = 0))

  pivoted_data <- merged_data %>%
    pivot_wider(
      id_cols = `Reporting Month`,
      names_from = purchase_category,
      values_from = c(count, spendPerMonth),
      names_glue = "{purchase_category}_{.value}"
    )

  # Calculate totals
  result <- pivoted_data %>%
    mutate(
      Orders_Total = rowSums(select(., ends_with("_count"))),
      Spend_Total = rowSums(select(., ends_with("_spendPerMonth")))
    )

  # Calculate percentages
  for (cat in categories) {
    result <- result %>%
      mutate(
        !!paste0(cat, "_Orders%") := round(!!sym(paste0(cat, "_count")) / Orders_Total * 100, 1),
        !!paste0(cat, "_Spend%") := round(!!sym(paste0(cat, "_spendPerMonth")) / Spend_Total * 100, 1)
      )
  }

  # Rename columns for clarity
  for (cat in categories) {
    result <- result %>%
      rename(
        !!paste0(cat, "_Orders") := !!sym(paste0(cat, "_count")),
        !!paste0(cat, "_Spend") := !!sym(paste0(cat, "_spendPerMonth"))
      )
  }

  # Ensure all expected columns are present
  expected_columns <- c(
    "Reporting.Month",
    paste0(rep(categories, each = 4), c("_Orders", "_Spend", "_Orders%", "_Spend%")),
    "Orders_Total", "Spend_Total"
  )

  missing_columns <- setdiff(expected_columns, names(result))
  if (length(missing_columns) > 0) {
    result[missing_columns] <- 0
  }

  return(result)
}


plot_monthly_metrics <- function(data, metric = "Orders", type = "value", N = 3, chart_type = "bar") {
  # Define category names
  categories <- c(as.character(1:N), paste(N + 1, "or more"))

  long_data <- if(type == "value") {
    data %>%
      pivot_longer(cols = ends_with(c("_Orders", "_Spend")),
                   names_to = c("category", "measure"),
                   names_pattern = "(.*)_(.*)",
                   values_to = "value") %>%
      filter(measure == metric) %>%
      mutate(`Reporting Month` = ym(`Reporting Month`))
  } else {
    data %>%
      pivot_longer(cols = ends_with(c("_Orders%", "_Spend%")),
                   names_to = c("category", "measure"),
                   names_pattern = "(.*)_(.*%)",
                   values_to = "value") %>%
      filter(measure == paste0(metric, "%")) %>%
      mutate(`Reporting Month` = ym(`Reporting Month`))
  }

  # Create a custom factor level for category
  long_data <- long_data %>%
    mutate(category = factor(category, levels = categories, ordered = TRUE))

  # Define a color palette
  colors <- c(colorRampPalette(c("skyblue", "mediumpurple2","mediumpurple4"))(N), "indianred2")

  # Base plot
  p <- ggplot(long_data, aes(x = `Reporting Month`, y = value, fill = category, color = category)) +
    scale_x_date(date_breaks = "1 month", date_labels = "%Y-%m") +
    scale_fill_manual(values = colors, breaks = categories) +
    scale_color_manual(values = colors, breaks = categories) +
    labs(
      x = "Month",
      y = if(type == "percent") "Percentage" else metric,
      fill = "Purchase Number",
      color = "Purchase Number"
    ) +
    theme_minimal() +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1, size = 12),  # Increase tick label size
      axis.text.y = element_text(size = 12),  # Increase y-axis tick label size
      axis.title.x = element_text(size = 14),  # Increase x-axis label size
      axis.title.y = element_text(size = 14),  # Increase y-axis label size
      legend.text = element_text(size = 12),  # Increase legend text size
      legend.title = element_text(size = 14)  # Increase legend title size
    )

  # Add geom based on type and chart_type
  if (type == "percent") {
    p <- p + geom_bar(stat = "identity", position = "stack")
  } else {
    p <- p + case_when(
      chart_type == "area" ~ list(geom_area(position = "stack")),
      chart_type == "bar" ~ list(geom_bar(stat = "identity", position = "dodge")),
      chart_type == "line" ~ list(geom_line(size = 1), geom_point(size = 2)),
      TRUE ~ list(geom_area(position = "stack"))  # default to area if invalid type
    )
  }

  # Add comma formatting for non-percentage y-axis
  if(type != "percent") {
    p <- p + scale_y_continuous(labels = scales::comma)
  }

  return(p)
}

group_orders <- function(df, N) {
  # Check if avg_time_diff column exists
  has_avg_time_diff <- "avg_time_diff" %in% names(df)

  df %>%
    group_by(`Buyer Email`) %>%
    summarize(
      order_group = n(),
      `Shipped Quantity` = sum(`Shipped Quantity`, na.rm = TRUE),
      `Item Price` = sum(`Item Price`, na.rm = TRUE),
      avg_time_diff = if(has_avg_time_diff) mean(avg_time_diff, na.rm = TRUE) else NA
    ) %>%
    mutate(grouped_order = case_when(
      order_group <= N ~ as.character(order_group),
      order_group > N ~ paste0(">", N)
    )) %>%
    group_by(grouped_order) %>%
    summarise(
      count = n(),
      total_orders = sum(order_group, na.rm = TRUE),
      total_shipped_quantity = sum(`Shipped Quantity`, na.rm = TRUE),
      total_item_price = sum(`Item Price`, na.rm = TRUE),
      Average_Number_of_Days_Between_Each_Purchase = if(has_avg_time_diff) mean(avg_time_diff, na.rm = TRUE) else NA
    ) %>%
    mutate(
      Lifetime_Value_of_the_Customer = round(total_item_price/count,2),
      avg_Units_Per_Customer = round(total_shipped_quantity/count,2)
    ) %>%
    arrange(grouped_order) %>%
    # Remove NA column if avg_time_diff wasn't present
    select_if(~!all(is.na(.)))
}

create_bar_chart <- function(data, bar_chart_type) {

  #data <- group_orders(data, N = repeat_Purchase_Groups)
  # Validate y_axis input
  valid_y_axes <- c("count", "total_orders", "total_shipped_quantity", "total_item_price",
                    "Average_Number_of_Days_Between_Each_Purchase",
                    "Lifetime_Value_of_the_Customer", "avg_Units_Per_Customer")
  if (!bar_chart_type %in% valid_y_axes) {
    stop("Invalid y_axis. Choose from: ", paste(valid_y_axes, collapse = ", "))
  }

  # Create a new column for numerical order
  data <- data %>%
    mutate(numeric_order = as.numeric(ifelse(grepl("^>", grouped_order), Inf, grouped_order))) %>%
    arrange(numeric_order)

  # Determine the number of groups (excluding the last ">N" group)
  n_groups <- sum(!grepl("^>", data$grouped_order))

  # Create a color palette
  colors <- colorRampPalette(c("skyblue", "mediumpurple2","mediumpurple4"))(n_groups)
  colors <- c(colors, "indianred2")

  # Create named vector for color mapping
  color_mapping <- setNames(colors, c(as.character(1:n_groups), tail(data$grouped_order, 1)))

  # Create the plot
  p <- ggplot(data, aes(x = grouped_order, y = .data[[bar_chart_type]], fill = grouped_order)) +
    geom_bar(stat = "identity") +
    scale_fill_manual(values = color_mapping) +
    scale_x_discrete(limits = data$grouped_order) +  # Preserve the original order
    theme_minimal() +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1, size = 12),  # Increase tick label size
      axis.text.y = element_text(size = 12),  # Increase y-axis tick label size
      axis.title.x = element_text(size = 14),  # Increase x-axis label size
      axis.title.y = element_text(size = 14),  # Increase y-axis label size
      legend.position = "none"
    ) +
    labs(
      x = "# of Purchases",
      y = gsub("_", " ", (bar_chart_type))
    )

  return(p)
}


skuRPR <- function(df, N = Inf, other_threshold = 0.01) {
  df <- df %>%
    dplyr::rename_with(~stringr::str_replace_all(., " ", "_")) %>%
    dplyr::rename_with(~stringr::str_replace_all(., "[:punct:]", "")) %>%
    dplyr::rename_with(~stringr::str_replace_all(., "[:space:]", "")) %>%
    dplyr::select(-c(AmazonOrderItemId, ItemTax, ShippingPrice, ShippingTax, GiftWrapTax,
                     GiftWrapPrice, ItemPromoDiscount, ShipmentPromoDiscount))

  df_SkuNames <- unique(df$MerchantSKU)

  resultDF <- df %>%
    tidyr::pivot_wider(names_from = MerchantSKU, values_from = ShippedQuantity, values_fill = 0) %>%
    dplyr::group_by(AmazonOrderId) %>%
    dplyr::summarise(dplyr::across(tidyselect::where(is.numeric), sum),
                     BuyerEmail = unique(BuyerEmail),
                     PurchaseDate = unique(PurchaseDate),
                     orderCount = n()) %>%
    dplyr::group_by(BuyerEmail) %>%
    dplyr::summarise(dplyr::across(tidyselect::where(is.numeric), sum)) %>%
    dplyr::arrange(dplyr::desc(BuyerEmail)) %>%
    dplyr::filter(!is.na(BuyerEmail)) %>%
    dplyr::mutate(orderCount = pmin(orderCount, N)) %>%
    dplyr::group_by(orderCount) %>%
    dplyr::summarize(dplyr::across(tidyselect::where(is.numeric), sum)) %>%
    tidyr::pivot_longer(cols = unique(df$MerchantSKU), names_to = "MerchantSKU", values_to = "Quantity")

  # Calculate total quantity
  total_quantity <- sum(resultDF$Quantity)

  # Group small quantity SKUs into "Other"
  resultDF <- resultDF %>%
    dplyr::group_by(orderCount) %>%
    dplyr::mutate(
      MerchantSKU = ifelse(Quantity / sum(Quantity) < other_threshold, "Other", MerchantSKU)
    ) %>%
    dplyr::group_by(orderCount, MerchantSKU) %>%
    dplyr::summarise(Quantity = sum(Quantity), .groups = "drop")

  return(resultDF)
}

skuRPR_FillPlot <- function(df, N = 5, other_threshold = 0.1){
  df <- skuRPR(df, N = N, other_threshold = other_threshold)
  resultPlot <- ggplot(data = df) +
    geom_bar(aes(x = orderCount, y = Quantity, fill = MerchantSKU),
             stat = "identity", position = "fill") +
    scale_fill_brewer(palette = "Set2") +
    scale_x_continuous(breaks = function(x) seq(floor(min(x)), ceiling(max(x)), by = 1)) +
    labs(x = "# of Purchases",
         y = "Percentage",
         fill = "SKU") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1),
          legend.position = "right")+
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1, size = 12),  # Increase tick label size
      axis.text.y = element_text(size = 12),  # Increase y-axis tick label size
      axis.title.x = element_text(size = 14),  # Increase x-axis label size
      axis.title.y = element_text(size = 14),  # Increase y-axis label size
      legend.text = element_text(size = 12),  # Increase legend text size
      legend.title = element_text(size = 14)  # Increase legend title size
    )

  return(resultPlot)
}

get_unique_months <- function(data) {
  months <- unique(format(data$`Reporting Date`, "%B %Y"))
  months <- as.Date(paste0("01 ", months), format = "%d %B %Y")
  sorted_dates <- sort(months, decreasing = TRUE)
  # Convert back to original format
  result <- format(sorted_dates, "%B %Y")
}




# Define UI for application that draws a histogram
ui <- fluidPage(
  # Application title
  titlePanel("Repeat Purchase Analysis"),
  br(),
  sidebarLayout(
    sidebarPanel(
      fileInput("files", "Choose CSV File(s)",
                multiple = TRUE,
                accept = c(
                  "text/csv",
                  "text/comma-separated-values,text/plain",
                  ".csv")
      )
      # ),
      # tags$hr(),
      # checkboxInput("header", "Header", TRUE)
    ),
    mainPanel(
      fluidRow(
        column(6,
               uiOutput("sku_filter_ui"),
               ),
        column(3,
               uiOutput("start_month_ui")
               ),
        column(3,
               uiOutput("end_month_ui")
               )
        # column(3,
        #        tableOutput("filtered_data")
        #        ),
        # column(3,
        #        verbatimTextOutput("debug_info")  # Add this for debugging
        #        )
      )

    )
  ),
  br(),
  tabsetPanel(
    tabPanel("A) Monthly Metrics",
             br(),
             br(),
             fluidRow(
               column(8,
                      br(),
                      fluidRow(
                        column(1),
                        column(2,
                               selectInput("chart_type", "Chart Type", choices = c("bar", "line", "area"), selected = "bar")
                        ),
                        column(2,
                               radioButtons("metric", "Metric",
                                            choices = c("Customers", "Revenue"),
                                            selected = "Customers",
                                            inline = TRUE)
                        ),
                        column(2,
                               radioButtons("type", "Type",
                                            choices = c("Total", "Percent"),
                                            selected = "Total",
                                            inline = TRUE)
                        ),
                        column(2,
                               numericInput("N1", "# Repeat Purchases",
                                            value = 1, min = 1, max = 10)
                        ),
                        column(3)

                      ),
                      br(),
                      br(),
                      fluidRow(
                        column(1),
                        column(10,
                               plotOutput("monthly_metrics_plot")
                        ),
                        column(1)
                      )
               ),
               column(4,
                      br(),
                      fluidRow(
                        column(10,
                               formattable::formattableOutput("monthly_metrics_table")
                        ),
                        column(2)
                      )

                      # formattable output
               )
             )
    ),
    tabPanel("B) Lifetime Summary",
             br(),
             br(),
             br(),
             column(8,
               fluidRow(
                 column(1),
                 column(4,
                        numericInput("repeat_purchase_groups", "# of Repeat Purchases:",
                                     value = 1, min = 1, max = 10)
                 ),
                 column(3,
                        selectInput("bar_chart_type", "Bar Chart Metric:",
                                    choices =   c("# of Customers", "# of Orders Sold", "Total Revenue",
                                                  "Days Between Purchases",
                                                  "Value of Customer ($'s)", "Units per Customer"),
                                    selected = "total_orders")
                 ),
                 column(4)
               ),
               br(),
               br(),
               fluidRow(
                 column(1),
                 column(10,
                        plotOutput("bar_chart")
                 ),
                 column(1)
               )
             ),
             column(3,
                    br(),
                    formattable::formattableOutput("lifetime_summary_table")
                    # formattable output

             ),
             column(1)
    ),
    tabPanel("C) SKU Analysis",
             br(),
             sidebarLayout(
               sidebarPanel(
                 numericInput("repeat_purchases", "# of Repeat Purchases",
                              value = 2, min = 1, max = 10, step = 1),
                 sliderInput("sku_threshold", "Remaining SKU's Threshold %",
                             min = 0.01, max = 0.3, value = 0.1, step = 0.01)
               ),
               mainPanel(
                 plotOutput("skuPlot")
               )
             )
    )

  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  userData <- reactive({
    req(input$files)
    # Read all uploaded files

    process_chunk <- function(chunk, pos) {
      # Process each chunk as needed
      chunk
    }

    # all_data <- lapply(input$files$datapath, function(path) {
    #   #readr::read_csv_chunked(path, callback = process_chunk, chunk_size = 1000)
    #   data.table::fread(path, header = input$header)
    # })

    all_data <- lapply(input$files$datapath , function(path) {
      vroom::vroom(path, col_names = TRUE)
    })
    # Combine all data frames
    do.call(rbind, all_data)
  })

  unique_months <- reactive({
    req(userData())
    get_unique_months(userData())
  })

  output$sku_filter_ui <- renderUI({
    req(userData())
    selectInput("sku_filter", "Select SKU:",
                choices = c("ALL SKU's", unique(userData()$`Merchant SKU`)))
  })

  output$start_month_ui <- renderUI({
    req(unique_months())
    selectInput("start_month", "Start Month:",
                choices = unique_months(),
                selected = tail(unique_months(), 1))  # Select the first month
  })

  output$end_month_ui <- renderUI({
    req(unique_months())
    selectInput("end_month", "End Month:",
                choices = unique_months(),
                selected = head(unique_months(), 1))  # Select the last month
  })


  filtered_sku_data <- reactive({
    req(userData(), input$sku_filter)
    filtered <- userData()

    if (input$sku_filter != "ALL SKU's") {
      filtered <- userData() %>%
        filter(`Merchant SKU` == input$sku_filter)
    }

    filtered
  })


  output$filtered_data <- renderTable({
    head(filtered_data(), 10)  # Display first 10 rows of filtered data
  })

  output$filtered_data <- renderTable({
    data <- filtered_data() %>%
      select(c(`Merchant SKU`, `Reporting Date`))
    head(data, 10)  # Display first 10 rows of filtered data
  })

  cleanData <- reactive({
    req(input$files, filtered_sku_data())
    clean_data(filtered_sku_data())
  })

  monthData <- reactive({
    req(cleanData(), input$N1)
    month_group(cleanData(), N = input$N1)
  })

  monthDataN1 <- reactive({
    req(cleanData())
    month_group(cleanData(), N = 1)
  })


  filtered_month_data <- reactive({
    req(monthData(), input$start_month, input$end_month)

    start_date <- floor_date(parse_date_time(input$start_month, "B Y"), "month")
    end_date <- ceiling_date(parse_date_time(input$end_month, "B Y"), "month") + months(1) - days(1)

    data <- monthData()
    data$ReportingDate <- as.Date(paste0(data$`Reporting Month`, "-01"))

    filtered <- data %>%
      filter(ReportingDate >= start_date,
             ReportingDate <= end_date)

    filtered
  })

  filtered_month1_data <- reactive({
    req(monthDataN1(), input$start_month, input$end_month)

    start_date <- floor_date(parse_date_time(input$start_month, "B Y"), "month")
    end_date <- ceiling_date(parse_date_time(input$end_month, "B Y"), "month") + months(1) - days(1)

    data <- monthDataN1()
    data$ReportingDate <- as.Date(paste0(data$`Reporting Month`, "-01"))

    filtered <- data %>%
      filter(ReportingDate >= start_date,
             ReportingDate <= end_date)

    filtered
  })

  # output$table <- DT::renderDT(
  #   dspl_tbl_clean(userData()),
  #   selection = "none",
  #   options = list(pageLength = 3),
  #   server = FALSE,
  #   editable = TRUE
  # )

  output$monthly_metrics_plot <- renderPlot({
    req(filtered_month_data(), input$metric, input$type, input$N1, input$chart_type)

    if (input$metric == "Customers") {
      metricTechnical = "Orders"
    } else if (input$metric == "Revenue") {
      metricTechnical = "Spend"
    }

    if (input$type == "Total") {
      typeTechnical = "value"
    } else if (input$type == "Percent") {
      typeTechnical = "percent"
    }


    plot_monthly_metrics(
      data = filtered_month_data(),
      metric = metricTechnical,
      type = typeTechnical,
      N = input$N1,
      chart_type = input$chart_type
    )
  })

  output$monthly_metrics_table <- formattable::renderFormattable({
    req(filtered_month1_data(), input$metric, input$type)
    #browser()
    data <- filtered_month1_data()

    if (input$metric == "Customers") {
      metricTechnical = "Orders"
    } else if (input$metric == "Revenue") {
      metricTechnical = "Spend"
    }

    if (input$type == "Total") {
      typeTechnical = "value"
    } else if (input$type == "Percent") {
      typeTechnical = "percent"
    }

    if (metricTechnical == "Orders" && typeTechnical == "value") {
      columns <- c("Reporting Month", "2 or more_Orders", "1_Orders")
      names <- c("Reporting Month", "# of Orders Repeat Customers",  "# of Orders from New Customers")
    } else if (metricTechnical == "Orders" && typeTechnical == "percent") {
      columns <- c("Reporting Month","2 or more_Orders%",  "1_Orders%")
      names <- c("Reporting Month",  "% of All Orders from Repeat Customers", "% of All Orders from New Customers")
    } else if (metricTechnical == "Spend" && typeTechnical == "value") {
      columns <- c("Reporting Month", "2 or more_Spend", "1_Spend")
      names <- c("Reporting Month", "Revenue from Repeat Customers", "Revenue from New Customers")
    } else if (metricTechnical == "Spend" && typeTechnical == "percent") {
      columns <- c("Reporting Month",  "2 or more_Orders%", "1_Orders%")
      names <- c("Reporting Month", "% of Revenue from Repeat Customers", "% of Revenue from New Customers")
    }
    table_data <- data[, columns]

    table_data <- table_data %>%
      mutate(`Reporting Month` = ym(`Reporting Month`) %>%
               format("%b %y"))

    # Change the column names
    new_names <- c("Month", "Repeat Customers", "New Customers")
    colnames(table_data) <- new_names

    formattable(
      table_data,
      align = c("l", "r", "r"),
      list(
        area(col = 2:3) ~ color_tile("#f7f7f7", "#e0e0e0")
      )
    )
  })

  groupData <- reactive({
    req(cleanData())
    group_orders(cleanData(), N = input$repeat_purchase_groups)
  })


  output$bar_chart <- renderPlot({
    req(groupData(), input$bar_chart_type)

    if (input$bar_chart_type == "# of Customers") {
      bar_chart_technical <- c("count")

    } else if (input$bar_chart_type == "# of Orders Sold") {
      bar_chart_technical <- c("total_orders")

    } else if (input$bar_chart_type == "Total Revenue") {
      bar_chart_technical <- c("total_item_price")

    } else if (input$bar_chart_type == "Days Between Purchases") {
      bar_chart_technical <- c("Average_Number_of_Days_Between_Each_Purchase")

    } else if (input$bar_chart_type == "Value of Customer ($'s)") {
      bar_chart_technical <- c("Lifetime_Value_of_the_Customer")

    } else if (input$bar_chart_type == "Units per Customer") {
      bar_chart_technical <- c("avg_Units_Per_Customer")
    }
    #browser()
    create_bar_chart(data = groupData(), bar_chart_type = bar_chart_technical)
  })

  output$lifetime_summary_table <- formattable::renderFormattable({
    req(groupData())

    data <- groupData()

    newNames <- c("# Purchases", "Customers", "Total Orders", "Total Units", "Total Revenue", "$ Value of Customer", "Units Sold per Customer")
    colnames(data) <- newNames

    # choices = c("total_orders", "total_shipped_quantity", "total_item_price",
    #             "Average_Number_of_Days_Between_Each_Purchase",
    #             "Lifetime_Value_of_the_Customer", "avg_Units_Per_Customer"),

    if (input$bar_chart_type == "# of Customers") {
      columns <- c("# Purchases", "Customers")

    } else if (input$bar_chart_type == "# of Orders Sold") {
      columns <- c("# Purchases", "Total Orders")

    } else if (input$bar_chart_type == "Total Revenue") {
      columns <- c("# Purchases", "Total Revenue")

    } else if (input$bar_chart_type == "Days Between Purchases") {
      columns <- c("# Purchases", "Average_Number_of_Days_Between_Each_Purchase")

    } else if (input$bar_chart_type == "Value of Customer ($'s)") {
      columns <- c("# Purchases", "$ Value of Customer")

    } else if (input$bar_chart_type == "Units per Customer") {
      columns <- c("# Purchases", "Units Sold per Customer")

    }

    table_data <- data[, columns]

    #table_data %>%
    #  dplyr::mutate(`Reporting Month` = format(`Reporting Month`, "%B %Y"))

    #colnames(table_data) <- names

    formattable(
      table_data,
      align = c("l", "r"),
      list(
        `grouped_order` = formatter("span"),#, style = ~ style(font.weight = "bold")),
        area(col = 2:2) ~ color_tile("#f7f7f7", "#e0e0e0")
      )
    )
  })

  output$skuPlot <- renderPlot({
    # Assuming you have a reactive data frame called `reactive_df()`
    skuRPR_FillPlot(userData(),
                    N = input$repeat_purchases,
                    other_threshold = input$sku_threshold)
  })


}

# Run the application
shinyApp(ui = ui, server = server)
