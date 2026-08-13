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

# ---- Establish Threshold ----
threshold <- 346 #The legal MFL, the 50th percentile of flow MUST exceed this to be in compliance

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
    title = "Flow vs. Legal Minimum",
    tickvals = c(-50, -30, -10, 0, 10, 30, 50),
    tickmode = "array",
    ticktext = c("Substantially Below", "Moderately Below", "Marginally Below",
                 "At Minimum",
                 "Marginally Above", "Moderately Above", "Substantially Above")
  )
) %>%
  layout(
    title = "Ichetucknee River - Flow Relative to the Legal Minimum Flow<br><sup>Daily discharge compared to the established legal minimum (346 cfs)",
    xaxis = list(title = "Date"),
    yaxis = list(
      title = "",
      showgrid = F, 
      showticklabels = F,
      zeroline = F,
      ticks = ""
    ),
    margin = list(l = 150, r = 40, t =60, b = 60)
    )
p
saveWidget(p, "ichetucknee_legal_minimum_flow.html", selfcontained = T)
