# ----------------------------------------------------------------------- #
# Project: Chinook Analysis
# Author: Manuel Mena
# Repo: github.com/alexismenanieves/Chinook_Project
# Date: 04/28/21
# Scope: Machine Learning, Databases, Analysis
# ----------------------------------------------------------------------- #

# ----------------------------------------------------------------------- #
# Business questions
# Which customers are similar?
# What products are similar?
# What relates to customer's repurchasing within 90 days?
# ----------------------------------------------------------------------- #

# Step 0 - Load libraries and functions -----------------------------------
library(DBI)
library(RMySQL)
library(dplyr)
library(magrittr)
library(lubridate)
library(stringi)

library(ggplot2)
library(plotly)
library(skimr)
library(GGally)

library(tidymodels)
library(textrecipes)
library(embed)
library(vip)

# Step 1 - Connect to the database ----------------------------------------
con <- DBI::dbConnect(MySQL(),
                      dbname = Sys.getenv("DBNAME_CHINOOK"),
                      host = Sys.getenv("HOST_CHINOOK"),
                      user = Sys.getenv("USER_CHINOOK"),
                      password = Sys.getenv("PASSWORD_CHINOOK"),
                      port = 3306)

dbListTables(con)
# dbListTables(con) %>% purrr::map(~ tbl(con, .))

invoices_tbl <- tbl(con, "Invoice") %>%
  collect()

invoices_tbl %<>% 
  mutate(InvoiceDate = as.Date(InvoiceDate))

customers_tbl <- tbl(con, "Customer") %>%
  collect()

invoice_lines_tbl <- tbl(con, "InvoiceLine") %>%
  left_join(tbl(con, "Track") %>%
              select(-UnitPrice) %>%
              rename(TrackName = Name), 
            by = "TrackId") %>%
  left_join(tbl(con, "Genre") %>% 
              rename(GenreName = Name),
            by = "GenreId") %>%
  left_join(tbl(con, "Album") %>%
              rename(AlbumTitle = Title), 
            by = "AlbumId") %>%
  left_join(tbl(con, "Artist") %>%
              rename(ArtistName = Name),
            by = "ArtistId") %>%
  left_join(tbl(con, "Invoice") %>%
              select(InvoiceId, CustomerId),
            by = "InvoiceId") %>%
  select(-ends_with("Id"), 
         starts_with("Invoice"), 
         starts_with("Customer")) %>%
  collect() %>%
  relocate(contains("Id"), .before = 1)

DBI::dbDisconnect(con)

summary(invoice_lines_tbl)

invoice_lines_tbl %<>% 
  mutate(CustomerId = gsub("[^[:alnum:]]","", CustomerId),
         CustomerId = gsub("([a-z])([A-Z])","\\1 \\2",CustomerId, perl = TRUE),
         ArtistName = gsub("[^[:alnum:]]","", ArtistName),
         ArtistName = gsub("([a-z])([A-Z])","\\1 \\2",ArtistName, perl = TRUE)) 

customer_artists_tbl <- invoice_lines_tbl %>%
  select(CustomerId, ArtistName) %>%
  count(CustomerId, ArtistName) %>%
  pivot_wider(names_from = ArtistName,
              values_from = n,
              values_fill = 0,
              names_prefix = "artist_",
              names_sep = "_")

customer_artists_tbl

# Dimensionality reduction with UMAP

recipe_spec_umap <- recipe(~., customer_artists_tbl) %>%
  step_umap(
    -CustomerId,
    num_comp = 20,
    retain = FALSE,
    seed = c(123,123))

customer_artists_umap_tbl <- recipe_spec_umap %>%
  prep() %>%
  juice()

customer_artists_umap_tbl

# Which customers are buying from similar artists?

p <- customer_artists_umap_tbl %>%
  ggplot(aes(umap_01, umap_02, text = CustomerId)) +
  geom_point(alpha = 0.5)

ggplotly(p)

# dataset <- data.frame(CustomerId = c(1,2),
#                       umap_01 = c(0.0198, -0.319),
#                       umap_02 = c(0.336, -0.321))
# 
# p <- dataset %>%
#   ggplot(aes(umap_01,umap_02, text = CustomerId)) +
#   geom_point(alpha = 0.5)
# 
# ggplotly(p)

customer_artists_umap_tbl %>%
  plot_ly(x = ~umap_01, 
          y = ~umap_02, 
          z = ~ umap_03, 
          color = ~umap_04, 
          text = ~CustomerId) %>%
  add_markers()

invoice_lines_tbl %>%
  filter(CustomerId %in% c(37,57,43)) %>%
  count(CustomerId, ArtistName) %>%
  group_by(CustomerId) %>%
  arrange(-n, .by_group = TRUE) %>%
  slice(1:5)

customer_song_len_tbl <- invoice_lines_tbl %>%
  select(CustomerId, Milliseconds) %>%
  group_by(CustomerId) %>%
  summarise(enframe(quantile(Milliseconds, probs = c(0,0.25,0.5,0.75,1)))) %>%
  ungroup() %>%
  mutate(name = str_remove_all(name, "%")) %>%
  pivot_wider(names_from= name,
              values_from = value,
              names_prefix = "song_len_q")

customer_song_len_tbl %>%
  arrange(-song_len_q100)

max 