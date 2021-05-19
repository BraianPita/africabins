context("basic functionality")
test_that("we can do something", {

  require(ggplot2)
  require(africabins)
  require(tidyverse)
  require(countrycode)

  # edges: create random connections between countries (nodes)
  edges <- read.csv(file = "countries_africa.csv")


  edges <- edges %>% filter((Target == "NGA" | Source == "NGA") & value != 0.0) %>%
    mutate(category = case_when(value > median(value) + (2 * sd(value)) ~ "High",
                                TRUE ~ "Low")) %>%
    arrange(value) %>%
    select(Target, value, category) %>%
    mutate(category = case_when(category == "High" ~ 1,
                                category == "Low" ~ 2)) %>%
    filter(!duplicated(Target)) %>%
    mutate(Target = countrycode::countrycode(Target, "iso3c", "iso2c"))

  rownames(edges) <- edges$Target

  ggplot(edges, aes(country=Target, fill=value)) +
    geom_africabins() +
    coord_equal() -> gg

  gb <- ggplot_build(gg)

  gb

  africabins(edges, country_type = "iso2", country_col = "Target", value_col = "value")

})
