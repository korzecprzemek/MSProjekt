library(dplyr)
library(stringr)
oblicz_kwantyl <- function(df, q_name, q_pos_formula){
  df %>%
    group_by(Group) %>%
    mutate(right_cum = cumsum(n),
           q_pos = eval(rlang::parse_expr(q_pos_formula)),
           left_cum = tidyr::replace_na(lag(right_cum,1),0) +1,
           cond = q_pos <= right_cum & q_pos >= left_cum,
           n_lag1 = lag(right_cum, 1),
           {{ q_name }} := bin_min + (q_pos - n_lag1)/n*bin_length) %>%
    filter(cond)
}
draw_hist <- function(df, grupa, kolor){
  df %>%
    filter(Group == grupa) %>%
    ggplot(aes(x=bin, y=n)) + 
    geom_col(fill = kolor) +
    labs(title = "Histogram dla szeregu rozdzielczego",
         subtitle = paste0("Dane dla grupy ", grupa, "."),
         x="", y="Częstość") +
    theme_minimal()
}
# Definicja szeregu rozdzielczego
n <- 6
szereg_rozdzielczy <- wyniki %>%
  group_by(Group) %>%
  mutate(bin = cut(Score, breaks = n, right = FALSE, dig.lab = 4)) %>%
  count(Group, bin) %>%
  ungroup()

draw_hist(szereg_rozdzielczy, grupa = 1, kolor = "lightgreen") 
draw_hist(szereg_rozdzielczy, grupa = 2, kolor = "lightblue")

# dodanie informacji o przedzialach (bins)
szereg_rozdzielczy2 <- szereg_rozdzielczy %>%
  rowwise() %>%
  mutate(bins = bin %>% 
           as.character() %>% 
           stringr::str_remove_all(pattern = "\\[|\\)") %>% 
           stringr::str_split_fixed(pattern = ",", n=2) %>% 
           as.numeric() %>% list(),
         bin_min = unlist(bins) %>% min(),
         bin_max = unlist(bins) %>% max(),
         bin_length = bin_max - bin_min,
         bin_avg = unlist(bins) %>% mean())  
#srednia, mediana, wariancja, odchylenie, moment 4 i inne
szereg_rozdzielczy2 %>% 
  group_by(Group) %>%
  summarise(srednia_wazona = sum(n*bin_avg) / sum(n),
            wariancja = sum(((bin_avg- srednia_wazona)^2)*n) /sum(n),
            odch_std = sqrt(wariancja),
            moment_4 = sum(((bin_avg- srednia_wazona)^4)*n) /sum(n),
            kurtoza = moment_4 / wariancja^2,
            `skośność` = sum(((bin_avg- srednia_wazona)^3)*n) /sum(n),
            `rozstęp` =  max(bin_max) - min(bin_min),
            `wsp. zmienności` = odch_std/srednia_wazona) %>%
  left_join(
    oblicz_kwantyl(szereg_rozdzielczy2, q_name = "mediana", q_pos_formula = "(sum(n) +1)/2") %>%
      select(Group, mediana)
    ) %>% 
  left_join(
    oblicz_kwantyl(szereg_rozdzielczy2, q_name = "q1", q_pos_formula = "sum(n)/4") %>%
      select(Group, q1)
    ) %>%
  left_join(
    oblicz_kwantyl(szereg_rozdzielczy2, q_name = "q3", q_pos_formula = "3*sum(n)/4") %>%
      select(Group, q3)
    ) %>%
  left_join(
    szereg_rozdzielczy2 %>%
      group_by(Group) %>%
      mutate(n_prev = lag(n,1),
             n_next = lead(n, 1)) %>%
             filter(n == max(n)) %>%
             mutate(dominanta = bin_min + (n - n_prev)/(n - n_prev + n - n_prev)*bin_length) %>% 
      select(Group, Dominanta = dominanta)
  ) %>%
  mutate(`odchylenie ćwiartkowe` = (q3 - q1)/2,
         `pozycyjny wsp. zmiennosci` = `odchylenie ćwiartkowe` / mediana) %>% 
  readr::write_csv2("data/zadanie_1_rozdzielczy_miary.csv")








            