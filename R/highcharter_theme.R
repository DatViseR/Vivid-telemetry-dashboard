
library(highcharter)


# Define a proper Highcharts theme object
theme_vivid_dark <- hc_theme(

  colors = c("#4F46E5", "#10B981", "#F59E0B", "#3B82F6"),
  
  # Chart settings (keep very similar to example)
  chart = list(
    backgroundColor = NULL,  
    style = list(
      fontFamily = "Inter, Segoe UI, sans-serif"
    )
  ),
  
  # Title styling (match example structure exactly)
  title = list(
    style = list(
      color = "#E0E0E0",
      fontFamily = "Inter, Segoe UI, sans-serif"
    )
  ),
  
  # Subtitle styling (match example structure exactly)
  subtitle = list(
    style = list(
      color = "#C0C0C0",
      fontFamily = "Inter, Segoe UI, sans-serif"
    )
  ),
  
  # Legend styling (match example structure exactly)
  legend = list(
    itemStyle = list(
      fontFamily = "Inter, Segoe UI, sans-serif",
      color = "#E0E0E0"
    ),
    itemHoverStyle = list(
      color = "#FFFFFF"
    )
  ),
  
  # Tooltip styling
  tooltip = list(
    backgroundColor = "#1F2937",
    style = list(
      color = "#E0E0E0"
    )
  )
)
