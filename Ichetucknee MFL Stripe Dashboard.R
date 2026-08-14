library(dataRetrieval)
library(dplyr)
library(plotly)
library(scico)
library(htmlwidgets)
# ---- Data pull ----
data <- read_waterdata_daily(
  monitoring_location_id = "USGS-02322700",
  parameter_code = "00060",
  time = c("2015-01-01", "today"),
  skipGeometry = T
  ) %>%
  rename(`Flow (cfs)` = value,
         Date = time,
         Qualifier = qualifier) %>%
  select(Date, `Flow (cfs)`, Qualifier)

# ---- Threshold Establishment Dates ----
mfl_established_date <- as.Date("2015-01-01") #Approximate, need official date when available
mfl_2021_change_date <- as.Date("2021-01-01") #Approximate, need official date when available

threshold <- case_when(
  data$Date < mfl_established_date ~ NA_real_,
  data$Date < mfl_2021_change_date ~ 343,
  TRUE ~ 346
)
pre_mfl <- is.na(threshold)
# ---- Calculating percent difference from legal min flow each day ----
pct <- ((data$`Flow (cfs)` - threshold) / threshold * 100)

# ---- Classifying directions and magnitude of MFL compliance ----
magnitude <- case_when(
  abs(pct) < 10 ~ "marginally",
  abs(pct) < 30 ~ "moderately",
  TRUE ~ "substantially"
)
direction <- if_else(pct >= 0, "above", "below")

explanation <- if_else(
  pct>= 0,
  "This meets the legally required minimum flow, helping protect the river's health and ecology.",
  "This falls below the legally required minimum flow, signalling harm to the river's health and ecology."
)

# ---- Creating hover text with layperson oriented interpretation ----
hover_text <- sprintf(
  "%s<br>Flow: %s cfs <br> Legal Minimum: %s cfs (%+.1f%% difference)<br><br>Flow is %s %s the legal minimum.<br>%s",
  format(data$Date, "%B %d, %Y"),
  round(data$`Flow (cfs)`),
  threshold,
  pct,
  magnitude,
  direction,
  explanation
)

# ---- Establishing color scale ----
vik_colors <- scico(11, palette = "vik", direction = -1)
vik_colorscale <- Map(function(pos, col) list(pos, col),
                      seq(0, 1, length.out = 11), vik_colors)
# ---- Calculating number of days legal minimum has been met since original MFL ----
 n_total <- sum(!pre_mfl & !is.na(data$`Flow (cfs)`))
n_met <- sum(data$`Flow (cfs)` >= threshold & !pre_mfl, na.rm =T)
pct_met <- round(100 * n_met / n_total, 1)
#---- Plot generation ----

p <- plot_ly(
  x = data$Date,
  z = matrix(pct, nrow = 1),
  text = matrix(hover_text, nrow = 1),
  hoverinfo = "text",
  type = "heatmap",
  colorscale = vik_colorscale,
  zmid = 0, zmin = -75, zmax = 75,
  colorbar = list(
    title = "<b>Flow vs. Legal Minimum",
    tickvals = c(-50, -30, -10, 0, 10, 30, 50),
    tickmode = "array",
    ticktext = c("Substantially Below", "Moderately Below", "Marginally Below",
                 "At Minimum",
                 "Marginally Above", "Moderately Above", "Substantially Above")
  )
) %>%
  layout(
    title = list(
      text = "<b>Ichetucknee River - Flow Relative to the Legal Minimum Flow</b><br><sup>Daily discharge compared to the established legal minimum (343 cfs 2015-2021; 346 cfs 2021-present)",
      x = 0.05,
      xanchor = "left"),
    xaxis = list(title = "Date"),
    yaxis = list(
      title = "",
      showgrid = F, 
      showticklabels = F,
      zeroline = F,
      ticks = ""
    ),
    margin = list(l = 40, r = 40, t = 80, b = 100),
    annotations = list(
      text = sprintf("<b>Legal minimum met on %s of %s days (%s%%) since establishment of first minimum.",
                     format(n_met, big.mark = ","), format(n_total, big.mark = ","), pct_met),
      xref = "paper",
      yref = "paper",
      x = 0.5,
      y = -0.20,
      showarrow = F,
      xanchor = "center",
      font = list(size = 15, color = "black")
    )
    )

p<- p %>%
  add_annotations(
    text = "Data: USGS-02322700",
    xref = "paper",
    yref = "paper",
    x = 1.2,
    y = -0.2,
    showarrow = F,
    xanchor = "right",
    font = list(size = 10, color = "black")
  )
    

dir.create("docs", showWarnings = F)
saveWidget(p, "docs/ichetucknee_legal_minimum_flow.html", selfcontained = T)
