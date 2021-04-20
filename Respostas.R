
# INÍCIO
#==============================================================================
# Pacotes -----------------------------------------------------------------
library(seasonal)
library(readxl)
library(parsedate)
library(tidyverse)
library(mFilter)
library(lubridate)
library(ggpubr)
library(ggrepel)
library(modelr)
library(xts)
library(tsibble)
library(forecast)


# Cores RGB ---------------------------------------------------------------

paletavermelha <- c("#660000", "#990000", "#CC0000", "#FF0000", "#FF3333", "#FF6666", "#FF9999", "#FFCCCC")
paletaazul     <- c("#003366", "#004C99", "#0066CC", "#0080FF", "#3399FF", "#66B2FF", "#99CCFF", "#CCE5FF")
paletaverde    <- c("#336600", "#4C9900", "#66CC00", "#80FF00", "#99FF33", "#82FF66", "#CCFF99", "#E5FFCC")
paletacinza    <- c("#000000", "#202020", "#404040", "#606060", "#808080", "#A0A0A0", "#C0C0C0", "#E0E0E0")
fundo          <- theme(plot.background  = element_rect(fill = "#f4f5f6", color = NA), 
                        panel.background = element_rect(fill = "#f4f5f6", color = NA))


# Temas GGPLOT -------------------------------------------------------------------

temasggplot <- function(fonte         = "Arial", 
                        cor           = paletacinza[1],
                        titlesize     = 11, 
                        subtitlesize  = 10, 
                        captionsize   = 8,
                        axistextsize  = 9, 
                        axistitlesize = 9) {
  
  g1 <<- theme(panel.grid.major     = element_blank(), 
               panel.grid.minor     = element_blank(), 
               panel.background     = element_rect(fill = "#FFFFFF", color = cor, size = 0.5), 
               plot.title           = element_text(family = fonte, color = cor, size = titlesize, face = "bold", margin = margin(t = 5, b = 2, unit = "pt")),
               plot.subtitle        = element_text(family = fonte, color = cor, size = subtitlesize),
               plot.caption         = element_text(family = fonte, color = cor, size = captionsize, margin = margin(t = 2, b = 5, unit = "pt")),
               axis.line.x.bottom   = element_blank(), 
               axis.line.x.top      = element_blank(), 
               axis.line.y.left     = element_blank(),
               axis.line.y.right    = element_blank(), 
               axis.text.y.left     = element_text(family = fonte, color = cor, size = axistextsize,  margin = margin(l = 5, unit = "pt")),
               axis.text.x.bottom   = element_text(family = fonte, color = cor, size = axistextsize,  margin = margin(b = 5, unit = "pt")),
               axis.title.y         = element_text(family = fonte, color = cor, size = axistitlesize, hjust = 0.9, margin = margin(l = 3, unit = "pt")),
               axis.title.x         = element_text(family = fonte, color = cor, size = axistitlesize, hjust = 0.1, margin = margin(b = 3, unit = "pt")),
               legend.background    = element_blank(), 
               legend.key           = element_rect(fill = "#FFFFFF", size= 0.5), 
               legend.position      = c(0,1), 
               legend.justification = c(0,1)); g_1 <<- g1
               
               g2 <<- g1 + theme(panel.background     = element_blank(), 
                                 axis.line.y.left     = element_line(color = cor, size = 0.5), 
                                 axis.line.x.bottom   = element_line(color = cor, size = 0.5)); g_2 <<- g2
               
               g3 <<- g2 + theme(panel.grid.major.x = element_line(color = paletacinza[5], linetype = 3, size = 0.2), 
                                 panel.grid.major.y = element_line(color = paletacinza[5], linetype = 3, size = 0.2),
                                 panel.grid.minor.x = element_line(color = paletacinza[5], linetype = 3, size = 0.2),
                                 panel.grid.minor.y = element_line(color = paletacinza[5], linetype = 3, size = 0.2)); g_4 <<- g3
               
               # plot.margin = margin(c(l = 20,12,0,0), unit = "pt")
               
}; temasggplot()

leg01 <- theme(legend.justification = c(0,1), legend.position = c(0,1))
leg00 <- theme(legend.justification = c(0,0), legend.position = c(0,0))
leg11 <- theme(legend.justification = c(1,1), legend.position = c(1,1))
leg10 <- theme(legend.justification = c(1,0), legend.position = c(1,0))


g_3 <- theme_bw() + theme(panel.border      = element_rect(size = 1.3, color = "#000000"),
                          panel.grid.major  = element_blank(), 
                          panel.grid.minor  = element_blank(), 
                          plot.title        = element_text(size = 14, hjust = 0, face = "bold"),
                          plot.subtitle     = element_text(size = 11, hjust = 0),
                          panel.grid        = element_line(size = 1, color = "#000000"),
                          axis.ticks        = element_line(colour = "black"), 
                          axis.line         = element_line(color = "#000000"),
                          axis.title.y      = element_text(colour = "black", size = 10, hjust = 0.5, face = "bold"), 
                          axis.title.x      = element_blank(), 
                          axis.text.y       = element_text(size = 10, colour = "black", face = "bold"), 
                          axis.text.x       = element_text(size = 10, colour = "black", face = "bold", angle = 0, vjust = 0.5, hjust = 0.5),
                          legend.title      = element_text(size = 13, colour = "black", face = "bold"),
                          legend.background = element_blank(),
                          legend.key        = element_rect(colour = "black"),
                          legend.text       = element_text(size = 11, face = "bold"), 
                          plot.margin       = margin(l = 5, t = 5, b = 5, r = 20, unit = "pt")) 

g4 <- g_4 <- theme_bw() + theme(panel.border      = element_blank(),
                                panel.grid.major  = element_line(size = 0.5, color = "#C0C0C0", linetype = 3),
                                panel.grid.minor  = element_line(size = 0.5, color = "#C0C0C0", linetype = 3),
                                plot.title        = element_text(size = 10, hjust = 0, face = "bold"),
                                plot.subtitle     = element_text(size = 10, hjust = 0),
                                plot.caption      = element_text(size = 9,  color = "#202020", hjust = 1, face = "bold"),
                                axis.ticks        = element_line(colour = "black"), 
                                axis.line         = element_line(color = "#000000"),
                                axis.title.y      = element_text(colour = "#000000", size = 9, hjust = 0.1, face = "bold"), 
                                axis.title.x      = element_text(colour = "#808080", size = 9, hjust = 0.1, face = "bold"), 
                                axis.text.y       = element_text(size = 9, colour = "black", face = "bold"), 
                                axis.text.x       = element_text(size = 9, 
                                                                 colour = "black", 
                                                                 face = "bold", 
                                                                 angle = 0, 
                                                                 vjust = 0.5, 
                                                                 hjust = 0.5, 
                                                                 margin = margin(b = 5)),
                                legend.title         = element_text(size = 9, colour = "black", face = "bold"),
                                legend.background    = element_blank(),
                                legend.key           = element_rect(colour = "black"),
                                legend.text          = element_text(size = 9.2, face = "bold"), 
                                legend.justification = c(0,1), 
                                legend.position      = c(0,1),
                                plot.margin       = margin(l = 5, t = 5, b = 5, r = 20, unit = "pt")) 

g5 <- g_5 <- theme_bw() + theme(panel.border      = element_blank(),
                                panel.grid.major  = element_blank(),
                                panel.grid.minor  = element_blank(),
                                plot.title        = element_text(size = 10, hjust = 0, face = "bold"),
                                plot.subtitle     = element_text(size = 10, hjust = 0),
                                plot.caption      = element_text(size = 9,  color = "#202020", hjust = 1, face = "bold"),
                                axis.ticks        = element_line(colour = "black"), 
                                axis.line         = element_line(color = "#000000"),
                                axis.title.y      = element_blank(),
                                axis.title.x      = element_blank(),
                                axis.text.y       = element_text(size = 9, colour = "black", face = "bold"), 
                                axis.text.x       = element_text(size = 9, 
                                                                 colour = "black", 
                                                                 face = "bold", 
                                                                 angle = 0, 
                                                                 vjust = 0.5, 
                                                                 hjust = 0.5, 
                                                                 margin = margin(b = 5)),
                                legend.title         = element_text(size = 9, colour = "black", face = "bold"),
                                legend.background    = element_blank(),
                                legend.key           = element_rect(colour = "black"),
                                legend.text          = element_text(size = 9.2, face = "bold"), 
                                legend.justification = c(0,1), 
                                legend.position      = c(0,1),
                                plot.margin       = margin(l = 5, t = 5, b = 5, r = 20, unit = "pt")) 


# Regiões, Estados -----------------------------------------------------------------

Estados <- 
  tibble(UF = c("RO", "AC", "AM", "RR", "PA", "AP", "TO",
                "MA", "PI", "CE", "RN", "PB", "PE", "AL", "SE", "BA", 
                "MG", "ES", "RJ", "SP", 
                "PR", "SC", "RS", 
                "MS", "MT", "GO", "DF"), 
         Nome = c("Rondônia", "Acre", "Amazonas", "Roraima", "Pará", "Amapá", "Tocantins", 
                  "Maranhão", "Piauí", "Ceará", "Rio Grande do Norte", "Paraíba", "Pernambuco", "Alagoas", "Sergipe", "Bahia", 
                  "Minas Gerais", "Espírito Santo", "Rio de Janeiro", "São Paulo",
                  "Paraná", "Santa Catarina", "Rio Grande do Sul", 
                  "Mato Grosso do Sul", "Mato Grosso", "Goiás", "Distrito Federal"), 
         CodUF = c(c(11:17), 
                   c(21:29),
                   c(31:33, 35), 
                   c(41:43), 
                   c(50:53))) %>%
  mutate(Reg = fct_collapse(UF, 
                            Norte    = c("RO", "AC", "AM", "RR", "PA", "AP", "TO"), 
                            Nordeste = c("MA", "PI", "CE", "RN", "PB", "PE", "AL", "SE", "BA"),
                            Sudeste  = c("MG", "ES", "RJ", "SP"),
                            Sul      = c("PR", "SC", "RS"), 
                            Centro   = c("MS", "MT", "GO", "DF"))) %>%
  select(Reg,UF, CodUF, Nome)


Norte    <- Estados %>% filter(Reg == "Norte")
Nordeste <- Estados %>% filter(Reg == "Nordeste")
Sudeste  <- Estados %>% filter(Reg == "Sudeste")
Sul      <- Estados %>% filter(Reg == "Sul")
Centro   <- Estados %>% filter(Reg == "Centro")
Regioes  <- c("Norte", "Nordeste", "Sudeste", "Sul", "Centro")



# Base de dados PIM -------------------------------------------------------

pim_categ <- c("Indústria geral", "Indústria extrativas", "Indústria de transformação")
pim <- 
  read_excel("Tabela 3651.xlsx", range = "A6:D235", col_names = c("t", pim_categ)) %>%
  mutate(t = parse_date(t, format = "%B %Y", locale = locale("pt"))) %>%
  pivot_longer(`Indústria geral`:`Indústria de transformação`, names_to = "Categ", values_to = "y") %>%
  mutate(y = if_else(t == as_date("2002-01-01"), NA_real_, y), 
         Categ = factor(Categ, levels = pim_categ), 
         Setor = "Indústria") %>%
  select(Setor, everything())
  

# Base de dados da PMC ----------------------------------------------------

pmc_categ <- c("Combustíveis e lubrificantes", "Alimentos (geral)", "Têxtil", "Móveis e eletrodomésticos", "Fármacos e outros", 
               "Impresso", "Escritório e informática", "Pessoal e doméstico", "Veículos e peças", "Material de construção")
pmc <- 
  read_excel("tabela3419.xlsx", range = "A7:K260", col_names = c("t", pmc_categ), na = "-") %>%
  mutate(t = parse_date(t, format = "%B %Y", locale = locale("pt"))) %>%
  pivot_longer(`Combustíveis e lubrificantes`:`Material de construção`, names_to = "Categ", values_to = "y") %>%
  mutate(Categ = factor(Categ, levels = c(pmc_categ)), Setor = "Comércio") %>%
  select(Setor, everything()) %>%
  arrange(Categ, t)

x <- 
  read_excel("tabela3417.xlsx", range = "A7:B223", col_names = c("t", "y"), na = "-") %>%
  mutate(t = parse_date(t, format = "%B %Y", locale = locale("pt")), 
         Categ = "Comércio total", 
         Setor = "Comércio") %>%
  select(Setor,t, Categ,y)

pmc_categ <- c("Comércio total", pmc_categ)
pmc <- rbind(x, pmc) %>% mutate(Categ = factor(Categ, levels = pmc_categ)); rm(x)
  

# Base de dados da PMS ----------------------------------------------------

pms_categ <- c("Serviços total", "Serviços às famílias", "Informação e comunicação", 
               "Profissionais, adm. e outros", "Transportes e auxiliares e correio", "Outros serviços")
pms <- 
  read_excel("tabela6443.xlsx", range = "A7:G128", col_names = c("t", pms_categ), na = "-") %>%
  mutate(t = parse_date(t, format = "%B %Y", locale = locale("pt"))) %>%
  pivot_longer(`Serviços total`:`Outros serviços`, names_to = "Categ", values_to = "y") %>%
  mutate(Categ = factor(Categ, levels = c(pms_categ)), Setor = "Serviços") %>%
  select(Setor, everything()) %>%
  arrange(Categ, t)


# Base de dados PNAD -----------------------------------------------------------

pnad <- read.csv("pnad.csv", head = TRUE, na.strings = "NA")

# n_1  <- pnad %>% dim() %>% first()
# n_2 <- 
#   pnad %>%
#   as_tibble() %>%
#   filter(V2008 != 99) %>%
#   dim() %>% first()

# x <- 
#   pnad %>%
#   as_tibble() %>%
#   filter(V2008 != 99) %>%
#   mutate(Nasc_ano = as.character(V20082), 
#          Nasc_mes = as.character(V20081), 
#          Nasc_dia = as.character(V2008), 
#          Nasc_mes = if_else(nchar(Nasc_mes) == 1, paste0(0, Nasc_mes), Nasc_mes), 
#          Nasc_dia = if_else(nchar(Nasc_dia) == 1, paste0(0, Nasc_dia), Nasc_dia), 
#          Nasc = as_date(paste(Nasc_ano, Nasc_mes, Nasc_dia, sep = "-")), 
#          Qtr  = as.yearqtr(paste(Ano, Trimestre, sep = "-"), format = "%Y-%q")) %>%
#   rename(Dom = V1008, Painel = V1014, Sex = V2007, Trim = Trimestre) %>%
#   select(-Nasc_ano, -Nasc_mes, -Nasc_dia) %>%
#   select(X, Ano, Trim, UPA, Dom, Painel, Sex, Nasc, Qtr, everything()) %>%
#   group_by(Qtr) %>%
#   mutate(Dp_UPA = if_else(duplicated(UPA)    == TRUE | duplicated(UPA,    fromLast = T), 1,0), 
#          Dp_Dom = if_else(duplicated(Dom)    == TRUE | duplicated(Dom,    fromLast = T), 1,0), 
#          Dp_Pai = if_else(duplicated(Painel) == TRUE | duplicated(Painel, fromLast = T), 1,0), 
#          Dp_Sex = if_else(duplicated(Sex)    == TRUE | duplicated(Sex,    fromLast = T), 1,0), 
#          Dp_Nas = if_else(duplicated(Nasc)   == TRUE | duplicated(Nasc,   fromLast = T), 1,0), 
#          Dp     = if_else(Dp_UPA == 1 & Dp_Pai == 1 & Dp_Sex == 1 & Dp_Nas == 1, 1, 0))
# 
# x %>% select(X, Ano, Trim, UPA, Dom, Painel, Sex, Nasc, Qtr, Dp) %>% filter(Dp == 0) %>% print(n=50)

# Retirando dia 99 e Gêmeos

pnad2 <- 
  pnad %>%
  as_tibble() %>%
  filter(V2008 != 99) %>%
  mutate(Nasc_ano = as.character(V20082), 
         Nasc_mes = as.character(V20081), 
         Nasc_dia = as.character(V2008), 
         Nasc_mes = if_else(nchar(Nasc_mes) == 1, paste0(0, Nasc_mes), Nasc_mes), 
         Nasc_dia = if_else(nchar(Nasc_dia) == 1, paste0(0, Nasc_dia), Nasc_dia), 
         Nasc = as_date(paste(Nasc_ano, Nasc_mes, Nasc_dia, sep = "-")), 
         Qtr  = as.yearqtr(paste(Ano, Trimestre, sep = "-"), format = "%Y-%q")) %>%
  select(-Nasc_ano, -Nasc_mes, -Nasc_dia) %>%
  rename(Dom      = V1008, 
         Painel   = V1014, 
         Sex      = V2007,
         Cor      = V2010,
         Trim     = Trimestre, 
         Ftrab    = VD4001,
         Ocup     = VD4002,
         Rend_h   = VD4019,
         Rend_e   = VD4020, 
         Hora_h   = VD4031, 
         Hora_e   = VD4035, 
         Afastado = V4005, 
         Estudo   = VD3004) %>%
  select(X, Ano, Trim, UPA, Dom, Painel, Sex, Nasc, Qtr, everything()) %>%
  group_by(Qtr,UPA,Dom, Painel,Sex) %>%
  mutate(Dp = if_else(duplicated(Nasc) == TRUE | duplicated(Nasc, fromLast = T), 1,0)) %>%
  filter(Dp == 0)


# Índice de atividade -----------------------------------------------------

ibcbr <- read_csv2("STP-20210418231005110.csv", skip = 1, col_names = c("t", "y"))
ibcbr <- ibcbr[-dim(ibcbr)[1],]
ibcbr <- 
  ibcbr %>%
  mutate(t = parse_date(t, format = "%m/%Y"), 
         t = yearmonth(t),
         y = parse_double(y, locale = locale(decimal_mark = ",")))
  
  
#==============================================================================


# QUESTÃO 2
#==============================================================================
# Dinâmica: indústria (mensal) -----------------------------------------------------

graf <- function(ANO = 2014) { 
  x <- 
    pim1 %>%
    group_by(Setor, Categ) %>%
    mutate(t = yearmonth(t,format = "%b %Y")) %>%
    filter(year(t) >= ANO) %>%
    mutate(y = Ind(y/100, 100))
  
  format(first(x$t), "%b-%Y")
  xmin <- min(x$t)
  xmax <- max(x$t)
  ymin <- min(x$y, na.rm = TRUE) 
  ymax <- max(x$y, na.rm = TRUE)
  
  x %>%
    ggplot(aes(t,y, color = Categ)) + 
    geom_line() + 
    geom_point() + 
    scale_x_yearmonth(NULL,  breaks = seq(xmin, xmax, length.out = 10), date_labels = "%b-%y", expand = c(0,0)) + 
    scale_y_continuous(NULL, breaks = round(seq(ymin, ymax, length.out = 7), digits = 0)) + 
    scale_color_hue(NULL) + 
    labs(title    = "DINÂMICA DA INDÚSTRIA (PMI)", 
         subtitle = paste0("Número-Índice: ", format(first(x$t), "%B %Y"), " = 100")) + 
    g1 + leg00
  }; graf(2012)



# Dinâmica: comércio (mensal) ---------------------------------------------

graf <- function(ANO = 2014) { 
  x <- 
    pmc %>%
    group_by(Setor, Categ) %>%
    mutate(t = yearmonth(t,format = "%b %Y")) %>%
    filter(year(t) >= ANO) %>%
    mutate(y = Ind(y/100, 100))
  
  xmin <- min(x$t)
  xmax <- max(x$t)
  ymin <- min(x$y, na.rm = TRUE) 
  ymax <- max(x$y, na.rm = TRUE)
  
  x <<- x
  
  x %>%
    ggplot(aes(t,y, color = Categ)) + 
    geom_line(alpha = 0.85) + 
    geom_point() + 
    scale_x_yearmonth(NULL,  breaks = seq(xmin, xmax, length.out = 10), date_labels = "%b-%y", expand = c(0,0)) + 
    scale_y_continuous(NULL, breaks = round(seq(ymin, ymax, length.out = 7), digits = 0)) + 
    scale_color_hue(NULL) + 
    labs(title    = "DINÂMICA DA INDÚSTRIA (PMI)", 
         subtitle = paste0("Número-Índice: ", format(first(x$t), "%B %Y"), " = 100")) + 
    g1 + leg00
  
}; graf(2019)


# Dinâmica: serviços (mensal) --------------------------------------------

graf <- function(ANO = 2019) { 
  x <- 
    pms %>%
    group_by(Setor, Categ) %>%
    mutate(t = yearmonth(t,format = "%b %Y")) %>%
    filter(year(t) >= ANO) %>%
    mutate(y = Ind(y/100, 100))
  
  xmin <- min(x$t)
  xmax <- max(x$t)
  ymin <- min(x$y, na.rm = TRUE) 
  ymax <- max(x$y, na.rm = TRUE)
  
  x <<- x

  x %>%
    ggplot(aes(t,y, color = Categ)) + 
    geom_line(alpha = 0.85) + 
    geom_point() + 
    scale_x_yearmonth(NULL,  breaks = seq(xmin, xmax, length.out = 10), date_labels = "%b-%y", expand = c(0,0)) + 
    scale_y_continuous(NULL, breaks = round(seq(ymin, ymax, length.out = 7), digits = 0)) + 
    scale_color_hue(NULL) + 
    labs(title    = "DINÂMICA DOS SERVIÇOS (PMI)", 
         subtitle = paste0("Número-Índice: ", format(first(x$t), "%B %Y"), " = 100")) + 
    g1 + leg00
  

  
}; graf(2018)


# Indústria: magnitude da queda -------------------------------------------

graf <- function(d1 = 0, d2 = 0) { 
  x <- 
    pim %>%
    group_by(Setor, Categ) %>%
    mutate(t = yearmonth(t)) %>%
    filter(year(t) >= 2019) %>%
    mutate(y = Ind(y/100,100)) %>%
    summarise(y = ((min(y)/first(y))-1)*100)
  
  ymin <- min(x$y) - d1
  ymax <- max(x$y)
  
  x <<- x
  
  x %>% 
    ggplot(aes(fct_reorder(Categ, -y), y, fill = Categ)) +
    geom_bar(stat = "identity", position = "dodge", width = 1) + 
    geom_text(aes(label = paste0(round(y, digits = 1), "%")), vjust = d2, color = paletacinza[1], size = 3.5, fontface = "bold") + 
    scale_y_continuous(NULL, limits = c(ymin, 0), expand = c(0,0), labels = function(y) paste0(y,"%")) + 
    scale_x_discrete(NULL) + 
    scale_fill_brewer(NULL, palette = "Paired") + 
    labs(title    = "MAGNITUDE DAS QUEDAS DOS SETORES DA INDÚSTRIA", 
         subtitle = "Variação percentual entre Jan/2019 e vale") + 
    g1 + leg00 + 
    theme(panel.background = element_blank(),
          axis.ticks.x     = element_blank(), 
          axis.text.x      = element_blank())
  
}; graf(d1 = 2, d2 = 1.2)
x

# Indústria: magnitude da recuperação ------------------------------------------------

graf <- function(d1 = 0, d2 = 0) { 
  x <- 
    pim %>%
    group_by(Setor, Categ) %>%
    mutate(t = yearmonth(t)) %>%
    filter(year(t) >= 2019) %>%
    mutate(y = Ind(y/100,100)) %>%
    summarise(y = ((last(y)/min(y))-1)*100)
  
  ymin <- min(x$y)
  ymax <- max(x$y) + d1
  
  x <<- x
  
  x %>% 
    ggplot(aes(fct_reorder(Categ, -y), y, fill = Categ)) +
    geom_bar(stat = "identity", position = "dodge", width = 1) + 
    geom_text(aes(label = paste0(round(y, digits = 0), "%")), vjust = d2, color = paletacinza[1], size = 3.5, fontface = "bold") + 
    scale_y_continuous(NULL, limits = c(0, ymax), expand = c(0,0), labels = function(y) paste0(y,"%")) + 
    scale_x_discrete(NULL) + 
    scale_fill_brewer(NULL, palette = "Paired") + 
    labs(title    = "MAGNITUDE DAS QUEDAS DOS SETORES DA INDÚSTRIA", 
         subtitle = "Variação percentual entre Jan/2019 e vale") + 
    g1 + leg11 + 
    theme(panel.background = element_blank(),
          axis.ticks.x     = element_blank(), 
          axis.text.x      = element_blank(), 
          plot.margin      = margin(b = 5, unit ="pt"))
  
}; graf(d1 = 7, d2 = -1)



# Comércio: magnitude da queda --------------------------------------------

graf <- function(d1 = 0, d2 = 0) { 
  x <- 
    pmc %>%
    group_by(Setor, Categ) %>%
    mutate(t = yearmonth(t)) %>%
    filter(year(t) >= 2019) %>%
    mutate(y = Ind(y/100,100)) %>%
    summarise(y = ((min(y)/first(y))-1)*100) 
  
  ymin <- min(x$y) - d1
  ymax <- max(x$y)
  
  x <<- x
  x %>% 
    ggplot(aes(fct_reorder(Categ, -y), y, fill = Categ)) +
    geom_bar(stat = "identity", position = "dodge", width = 1) + 
    geom_text(aes(label = paste0(round(y, digits = 1), "%")), vjust = d2, color = paletacinza[1], size = 3.5, fontface = "bold") + 
    scale_y_continuous(NULL, limits = c(ymin, 0), expand = c(0,0), labels = function(y) paste0(y,"%")) + 
    scale_x_discrete(NULL) + 
    scale_fill_brewer(NULL, palette = "Paired") + 
    labs(title    = "MAGNITUDE DAS QUEDAS DOS SETORES DO COMÉRCIO", 
         subtitle = "Variação percentual entre Jan/2019 e vale") + 
    g1 + leg00 + 
    theme(panel.background = element_blank(),
          axis.ticks.x     = element_blank(), 
          axis.text.x      = element_blank())
  
}; graf(d1 = 5, d2 = 1.2)


# Comércio: magnitude da recuperação --------------------------------------------

graf <- function(d1 = 0, d2 = 0) { 
  x <- 
    pmc %>%
    group_by(Setor, Categ) %>%
    mutate(t = yearmonth(t)) %>%
    filter(year(t) >= 2019) %>%
    mutate(y = Ind(y/100,100)) %>%
    summarise(y = ((last(y)/min(y))-1)*100) 
  
  ymin <- min(x$y)
  ymax <- max(x$y) + d1
  
  x <<- x
  
  x %>% 
    ggplot(aes(fct_reorder(Categ, -y), y, fill = Categ)) +
    geom_bar(stat = "identity", position = "dodge", width = 1) + 
    geom_text(aes(label = paste0(round(y, digits = 0), "%")), vjust = d2, color = paletacinza[1], size = 3.5, fontface = "bold") + 
    scale_y_continuous(NULL, limits = c(0, ymax), expand = c(0,0), labels = function(y) paste0(y,"%")) + 
    scale_x_discrete(NULL) + 
    scale_fill_brewer(NULL, palette = "Paired") + 
    labs(title    = "MAGNITUDE DAS RECUPERAÇÕES DOS SETORES DO COMÉRCIO", 
         subtitle = "Variação percentual entre vale e Fev/2021") + 
    g1 + leg11 + 
    theme(panel.background = element_blank(),
          axis.ticks.x     = element_blank(), 
          axis.text.x      = element_blank(), 
          plot.margin      = margin(b = 5, unit = "pt"))
  
}; graf(d1 = 20, d2 = -1)


# Serviços: magnitude da queda --------------------------------------------

graf <- function(d1 = 0, d2 = 0) { 
  x <- 
    pms %>%
    group_by(Setor, Categ) %>%
    mutate(t = yearmonth(t)) %>%
    filter(year(t) >= 2019) %>%
    mutate(y = Ind(y/100,100)) %>%
    summarise(y = ((min(y)/first(y))-1)*100)
  
  ymin <- min(x$y) - d1
  ymax <- max(x$y)
  
  x <<- x

  x %>% 
    ggplot(aes(fct_reorder(Categ, -y), y, fill = Categ)) +
    geom_bar(stat = "identity", position = "dodge", width = 1) + 
    geom_text(aes(label = paste0(round(y, digits = 1), "%")), vjust = d2, color = paletacinza[1], size = 3.5, fontface = "bold") + 
    scale_y_continuous(NULL, limits = c(ymin, 0), expand = c(0,0), labels = function(y) paste0(y,"%")) + 
    scale_x_discrete(NULL) + 
    scale_fill_brewer(NULL, palette = "Paired") + 
    labs(title    = "MAGNITUDE DAS QUEDAS DOS SETORES DE SERVIÇOS", 
         subtitle = "Variação percentual entre Jan/2019 e vale") + 
    g1 + leg00 + 
    theme(panel.background = element_blank(),
          axis.ticks.x     = element_blank(), 
          axis.text.x      = element_blank())
  
}; graf(d1 = 3, d2 = 1.2)




# Serviços: magnitude da recuperação --------------------------------------------

graf <- function(d1 = 0, d2 = 0) { 
  x <- 
    pms %>%
    group_by(Setor, Categ) %>%
    mutate(t = yearmonth(t)) %>%
    filter(year(t) >= 2019) %>%
    mutate(y = Ind(y/100,100)) %>%
    summarise(y = ((last(y)/min(y))-1)*100)
  
  ymin <- min(x$y)
  ymax <- max(x$y) + d1
  
  x <<- x
  
  x %>% 
    ggplot(aes(fct_reorder(Categ, -y), y, fill = Categ)) +
    geom_bar(stat = "identity", position = "dodge", width = 1) + 
    geom_text(aes(label = paste0(round(y, digits = 1), "%")), vjust = d2, color = paletacinza[1], size = 3.5, fontface = "bold") + 
    scale_y_continuous(NULL, limits = c(0, ymax), expand = c(0,0), labels = function(y) paste0(y,"%")) + 
    scale_x_discrete(NULL) + 
    scale_fill_brewer(NULL, palette = "Paired") + 
    labs(title    = "MAGNITUDE DAS QUEDAS DOS SETORES DE SERVIÇOS", 
         subtitle = "Variação percentual entre Jan/2019 e vale") + 
    g1 + leg11 + 
    theme(panel.background = element_blank(),
          axis.ticks.x     = element_blank(), 
          axis.text.x      = element_blank())
  
}; graf(d1 = 5, d2 = -1)





# IBC-Br ------------------------------------------------------------------

graf <- function(ANO = 2014, d1 = 0, d2 = 0) {
  x <- 
    ibcbr %>%
    mutate(y = ((y/lag(y))-1)) %>%
    filter(year(t) >= ANO) %>%
    mutate(y = Ind(y, 100))
  
  xmin <- min(x$t) 
  xmax <- max(x$t) 
  ymin <- min(x$y, na.rm = TRUE) + d1
  ymax <- max(x$y, na.rm = TRUE) + d2
  
  x %>%
    ggplot(aes(t,y)) + 
    geom_line() + 
    geom_point() + 
    scale_y_continuous(NULL, limits = c(ymin,ymax), expand = c(0,0)) + 
    scale_x_yearmonth(NULL, date_labels ="%b-%y", expand = c(0,0)) + 
    labs(title = "BRASIL: ÍNDICE DE ATIVIDADE: IBC-BR", 
         subtitle = paste0("Número-Índice: ", format(first(x$t),"%B/%Y"), " = 100")) + 
    g1
  
}; graf(ANO = 2003)



# IBC-Br: correlações -----------------------------------------------------

pim %>%
  mutate(t = yearmonth(t)) %>%
  inner_join(rename(transmute(ibcbr, t,  y = ((y/lag(y)) -1)*100), IBC = y), by = "t") %>%
  group_by(Categ) %>%
  filter(!is.na(IBC)) %>%
  summarise(Cor = cor(y,IBC))

pmc %>%
  mutate(t = yearmonth(t)) %>%
  inner_join(rename(transmute(ibcbr, t,  y = ((y/lag(y)) -1)*100), IBC = y), by = "t") %>%
  group_by(Categ) %>%
  filter(year(t) >= 2003) %>%
  filter(!is.na(y), !is.na(IBC)) %>%
  summarise(Cor = cor(y,IBC))

pms %>%
  mutate(t = yearmonth(t)) %>%
  inner_join(rename(transmute(ibcbr, t,  y = ((y/lag(y)) -1)*100), IBC = y), by = "t") %>%
  group_by(Categ) %>%
  filter(!is.na(y), !is.na(IBC)) %>%
  summarise(Cor = cor(y,IBC))


# Dinâmica: indústria (trimestral) ----------------------------------------

graf <- function(ANO = 2014) {
  x <- 
    pim %>%
    group_by(Categ) %>%
    transmute(Setor, t, y = Ind(y/100, 100)) %>%
    mutate(t = yearmonth(t), Ano = year(t), Qtr = quarter(t)) %>%
    group_by(Setor, Categ, Ano, Qtr) %>%
    summarise(y = sum(y, na.rm = TRUE)) %>%
    mutate(t = yearquarter(paste0(Ano," Q",Qtr))) %>%
    ungroup() %>%
    select(Setor, t, Categ, y) %>%
    group_by(Setor, Categ) %>%
    filter(year(t) >= ANO, t < last(t)) %>%
    mutate(y = (y/lag(y))-1, y = Ind(y, 100))
    
  xmin <- min(x$t)
  xmax <- max(x$t)
  ymin <- min(x$y, na.rm = TRUE) 
  ymax <- max(x$y, na.rm = TRUE)
  
  x %>%
    ggplot(aes(t,y, color = Categ)) + 
    geom_line() + 
    geom_point() + 
    g1 + leg00
  
}; graf(ANO = 2019)



# Dinâmica: comércio (trimestral) ----------------------------------------------------

graf <- function(ANO = 2014) {
x <- 
  pmc %>%
  mutate(t = yearmonth(t)) %>%
  filter(year(t) >= 2003) %>%
  group_by(Categ) %>%
  transmute(Setor, t, y = Ind(y/100, 100)) %>%
  mutate(Ano = year(t), Qtr = quarter(t)) %>%
  group_by(Setor, Categ, Ano, Qtr) %>%
  summarise(y = sum(y, na.rm = TRUE)) %>%
  mutate(t = yearquarter(paste0(Ano," Q",Qtr))) %>%
  ungroup() %>%
  select(Setor, t, Categ, y) %>%
  group_by(Setor, Categ) %>%
  filter(year(t) >= ANO, t < last(t)) %>%
  mutate(y = (y/lag(y))-1, y = Ind(y, 100)) 

xmin <- min(x$t)
xmax <- max(x$t)
ymin <- min(x$y, na.rm = TRUE) 
ymax <- max(x$y, na.rm = TRUE)

x %>%
  ggplot(aes(t,y, color = Categ)) + 
  geom_line() + 
  geom_point() + 
  g1 + leg00

}; graf(ANO = 2019)

# Dinâmica: serviços (trimestral) -----------------------------------------

graf <- function(ANO = 2014) {
x <- 
  pms %>%
  mutate(t = yearmonth(t)) %>%
  group_by(Categ) %>%
  transmute(Setor, t, y = Ind(y/100, 100)) %>%
  mutate(Ano = year(t), Qtr = quarter(t)) %>%
  group_by(Setor, Categ, Ano, Qtr) %>%
  summarise(y = sum(y, na.rm = TRUE)) %>%
  mutate(t = yearquarter(paste0(Ano," Q",Qtr))) %>%
  ungroup() %>%
  select(Setor, t, Categ, y) %>%
  group_by(Setor, Categ) %>%
  filter(year(t) >= ANO, t < last(t)) %>%
  mutate(y = (y/lag(y))-1, y = Ind(y, 100)) 

xmin <- min(x$t)
xmax <- max(x$t)
ymin <- min(x$y, na.rm = TRUE) 
ymax <- max(x$y, na.rm = TRUE)

x %>%
  ggplot(aes(t,y, color = Categ)) + 
  geom_line() + 
  geom_point() + 
  g1 + leg00

}; graf(ANO = 2019)

# Dinâmica: PIB e setores I -------------------------------------------------

graf <- function(ANO = 2020) { 
  x <- 
    pim %>%
    filter(Categ == "Indústria geral") %>%
    select(Setor, t, y) %>%
    rbind(select(filter(pmc, Categ == "Comércio total"), Setor,t,y)) %>%
    rbind(select(filter(pms, Categ == "Serviços total"), Setor,t,y)) %>%
    rbind(select(mutate(ibcbr, t = yearmonth(t), y = ((y/lag(y))-1)*100, Setor = "IBC-Br"), Setor, t, y)) %>%
    mutate(Setor = factor(Setor, levels = c("IBC-Br", "Indústria", "Comércio", "Serviços"))) %>%
    arrange(Setor) %>%
    group_by(Setor) %>%
    filter(year(t) >= ANO) %>%
    mutate(y = Ind(y/100, 100), l = if_else(t == last(t), y, NA_real_))
  
  xmin <- min(x$t)
  xmax <- max(x$t)
  ymin <- min(x$y, na.rm = TRUE)
  ymax <- max(x$y, na.rm = TRUE)
  
  x <<- x
  
  x %>%
    ggplot(aes(t,y, color = Setor)) + 
    geom_line() + 
    geom_point() + 
    geom_text_repel(aes(label = round(l, digits = 0)), size = 3.5, face = "bold", nudge_x = c(12,0,15,-3), nudge_y = c(1,0,-4,-2)) + 
    scale_y_continuous(NULL, breaks = round(seq(ymin,ymax, length.out = 10), digits = 0)) + 
    scale_x_yearmonth(NULL,  limits = c(xmin,xmax), breaks = seq(xmin,xmax, length.out = 8), date_labels = "%b-%y", expand = c(0,3)) + 
    scale_color_manual(NULL, breaks = unique(x$Setor), values = c(paletacinza[1], paletaazul[2], paletavermelha[3], paletaverde[2])) +
    g1 + leg00 +
    labs(title = "(a)") + 
    theme(plot.margin = margin(r = 18, unit = "pt"), 
          plot.title = element_text(hjust = 0.5, size = 12))
  
}; graf(ANO = 2014)

x %>% summarise(y = ((min(y)/first(y))-1)*100)
x %>% summarise(y = ((last(y)/min(y))-1)*100)
x %>% summarise(y = ((last(y)/first(y)))*100)


x <- 
  pim %>%
  filter(Categ == "Indústria geral") %>%
  select(Setor, t, y) %>%
  rbind(select(filter(pmc, Categ == "Comércio total"), Setor,t,y)) %>%
  rbind(select(filter(pms, Categ == "Serviços total"), Setor,t,y)) %>%
  rbind(select(mutate(ibcbr, t = yearmonth(t), y = ((y/lag(y))-1)*100, Setor = "PIB (IBC-Br)"), Setor, t, y)) %>%
  mutate(Setor = factor(Setor, levels = c("PIB (IBC-Br)", "Indústria", "Comércio", "Serviços"))) %>%
  arrange(Setor) %>%
  group_by(Setor) %>%
  filter(year(t) >= 2020) %>%
  mutate(y = Ind(y/100, 100), l = if_else(t == last(t), y, NA_real_))

x %>% filter(Setor == "PIB (IBC-Br)") %>% print(n=Inf)
x %>% filter(y == min(y))
x %>% summarise(y = ((min(y)/first(y))-1)*100)
x %>% summarise(y = ((last(y)/min(y))-1)*100)
x %>% summarise(y = ((last(y)/first(y)))*100)


  
# Dinâmica: PIB e setores II ---------------------------

graf <- function(ANO = 2020) { 
  pim %>%
    rbind(pmc) %>%
    rbind(pms) %>%
    rbind(select(mutate(ibcbr, Categ = "IBC-Br", Setor = "PIB", y = ((y/lag(y))-1)*100), Setor, t, Categ, y)) %>%
    mutate(Categ = factor(Categ, levels = c("IBC-Br", pim_categ, pmc_categ, pms_categ)), 
           Setor = factor(Setor, levels = c("PIB", "Indústria", "Comércio", "Serviços")), 
           t     = yearmonth(t)) %>%
    arrange(Setor, Categ) %>%
    filter(year(t) >= ANO) %>%
    group_by(Setor, Categ) %>%
    mutate(y = Ind(y/100, 100)) -> x
  
  xmin <- min(x$t)
  xmax <- max(x$t)
  ymin <- min(x$y, na.rm = TRUE)
  ymax <- max(x$y, na.rm = TRUE)
  
  x <<- x
  
  x %>%
    ggplot(aes(t,y, color = Setor, group = Categ)) + 
    geom_line(alpha = 0.7) + 
    geom_point(alpha = 0.7) + 
    scale_y_continuous(NULL, breaks = round(seq(ymin,ymax, length.out = 10), digits = 0)) + 
    scale_x_yearmonth(NULL,
                      expand = c(0,0), 
                      breaks = seq(xmin,xmax, length.out = 10), 
                      date_labels = "%b-%y") + 
    scale_color_manual(NULL, breaks = unique(x$Setor), values = c(paletacinza[1], paletaazul[2], paletavermelha[3], paletaverde[2])) +
    g1 + leg00 + 
    labs(title = "(b)") + 
    theme(plot.margin = margin(r = 18, unit = "pt"), 
          plot.title = element_text(hjust = 0.5, size = 12))
  
  }; graf(ANO = 2014)

x %>% summarise(y = (last(y)/first(y))*100) %>% print(n=Inf)


# Dinâmica: PIB e setores (trimestral) ------------------------------------

pim %>%
  filter(Categ == "Indústria geral") %>%
  select(Setor, t, y) %>%
  rbind(select(filter(pmc, Categ == "Comércio total"), Setor,t,y)) %>%
  rbind(select(filter(pms, Categ == "Serviços total"), Setor,t,y)) %>%
  rbind(select(mutate(ibcbr, t = yearmonth(t), y = ((y/lag(y))-1)*100, Setor = "PIB (IBC-Br)"), Setor, t, y)) %>%
  mutate(Setor = factor(Setor, levels = c("PIB (IBC-Br)", "Indústria", "Comércio", "Serviços"))) %>%
  arrange(Setor) %>%
  group_by(Setor) %>%
  filter(year(t) >= 2011) %>%
  mutate(Ano = year(t), 
         Qtr = quarter(t), 
         y = Ind(y/100, 100)) %>%
  group_by(Setor, Ano, Qtr) %>%
  summarise(y = sum(y, na.rm = T)) %>%
  mutate(t = yearquarter(paste0(Ano," Q", Qtr))) %>%
  ungroup() %>%
  select(Setor, t, y) %>%
  group_by(Setor) %>%
  filter(t >= yearquarter("2019 Q4"), year(t) <= 2020) -> x

x %>% summarise(y = ((min(y)/first(y))-1)*100)
x %>% filter(y == min(y))




# Modelos ARIMA(12,0,0) -----------------------------------------------------------------

x <- 
  ibcbr %>%
  mutate(y = log(y),
         d0  = if_else(year(t) == 2020 & month(t) == 3, 1, 0), 
         d1  = lag(d0,1), 
         d2  = lag(d0,2), 
         d3  = lag(d0,3), 
         d4  = lag(d0,4), 
         d5  = lag(d0,5), 
         d6  = lag(d0,6), 
         d7  = lag(d0,7), 
         d8  = lag(d0,8), 
         d9  = lag(d0,9), 
         d10 = lag(d0,10), 
         d11 = lag(d0,11)) %>%
  replace(is.na(.), 0) %>% 
  select(y,d0:d11) %>%
  ts(start = c(2003,1), frequency = 12) %>%
  window(start = c(2004,1), end = c(2021,1))

f1 <- 
  data.frame(d0 = rep(0,11)) %>% 
  mutate(d1 = d0, d2 = d1, d3 = d2, d4 = d3, d5  = d4, 
         d6 = d5, d7 = d6, d8 = d7, d9 = d8, d10 = d9, d11 = d10)
f2 <- 
  data.frame(d0 = c(rep(0.1,4), rep(0,7))) %>%
  mutate(d1  = lag(d0,1), 
         d2  = lag(d0,2), 
         d3  = lag(d0,3), 
         d4  = lag(d0,4), 
         d5  = lag(d0,5), 
         d6  = lag(d0,6), 
         d7  = lag(d0,7), 
         d8  = lag(d0,8), 
         d9  = lag(d0,9), 
         d10 = lag(d0,10), 
         d11 = lag(d0,11)) %>%
  replace(is.na(.),0)

dummies1 <- colnames(x[,-c(1, 5:13)])
dummies2 <- colnames(x[,-c(1, 8:13)])
dummies3 <- colnames(x[,-c(1, 11:13)])
dummies4 <- colnames(x[,-c(1, 13)])
# auto.arima(x[,"y"], ic = "bic", xreg = x[,dummies1], stationary = TRUE)
# auto.arima(x[,"y"], ic = "bic", xreg = x[,dummies2], stationary = TRUE)
# auto.arima(x[,"y"], ic = "bic", xreg = x[,dummies3], stationary = TRUE)
# auto.arima(x[,"y"], ic = "bic", xreg = x[,dummies4], stationary = TRUE)

mod <- auto.arima(x[,"y"], ic = "bic", xreg = x[,dummies1], stationary = FALSE)
fct1 <- predict(mod, n.ahead = 11, newxreg = f1[,dummies1])
fct2 <- predict(mod, n.ahead = 11, newxreg = f2[,dummies1])


x1 <- 
  cbind(y = mod$x, fit = mod$fitted, fct1 = fct1$pred, fct2 = fct2$pred) %>%
  as_tibble() %>%
  mutate(t = seq(as_date("2004-01-01"), by = "month", length = n()), t = yearmonth(t), 
         fct1 = case_when(year(t) <= 2020 ~ y, year(t) == 2021 & month(t) == 1 ~ y, TRUE ~ fct1), 
         fct2 = case_when(year(t) <= 2020 ~ y, year(t) == 2021 & month(t) == 1 ~ y, TRUE ~ fct2), 
         y = exp(y), 
         fit = exp(fit), 
         fct1 = exp(fct1), 
         fct2 = exp(fct2)) %>%
  select(t, everything())

x <- 
  x1 %>%
  pivot_longer(y:fct2, names_to = "k", values_to = "y") %>% 
  mutate(k = factor(k, levels = c("y", "fit", "fct1", "fct2")), 
         k =  fct_recode(k, 
                         "IBC-Br" = "y", 
                         "Modelo ARIMA" = "fit",
                         "Previsão ARIMA sem restrição" = "fct1", 
                         "Previsão ARIMA com restrição" = "fct2")) %>%
  arrange(k,t) %>%
  group_by(k)

xmin <- min(x$t)
xmax <- max(x$t)
ymin <- min(x$y, na.rm = TRUE)
ymax <- max(x$y, na.rm = TRUE)

x %>% 
  filter(!(k %in% c("Previsão ARIMA sem restrição", "Previsão ARIMA com restrição"))) %>%
  group_by(k) %>%
  ggplot(aes(t,y, color = k)) + 
  geom_line() + 
  scale_y_continuous(NULL) + 
  scale_x_yearmonth(NULL, breaks = seq(xmin, xmax, length.out = 10), date_labels = "%b %y", expand = c(0,0)) + 
  scale_color_manual(NULL, values = c(paletacinza[1], paletaazul[2], paletavermelha[2])) + 
  g1 + leg10 + 
  labs(title = "(a)") + 
  theme(plot.margin = margin(t = 5, r = 18, unit = "pt"), 
        plot.title  = element_text(size = 12, hjust = 0.5))

x %>% 
  filter(k != "Modelo ARIMA") %>%
  group_by(k) %>%
  mutate(y  = ((y/lag(y))-1)*100) %>% 
  filter(year(t) >= 2020) %>%
  mutate(y = Ind(y/100,100)) %>%
  mutate(y = case_when(year(t) <= 2020 & k != "IBC-Br" ~ NA_real_, TRUE ~ y)) %>%
  ggplot(aes(t,y, color = k)) + 
  geom_line() + 
  geom_point() + 
  scale_y_continuous(NULL) + 
  scale_x_yearmonth(NULL, breaks = seq(yearmonth("2020 1"), yearmonth("2021 12"), length.out = 10), date_labels = "%b %y", expand = c(0,0)) + 
  scale_color_manual(NULL, values = c(paletacinza[1], paletaazul[2], paletavermelha[2])) + 
  g1 + leg10 + 
  labs(title = "(b)") + 
  theme(plot.margin = margin(t = 5, r = 18, unit = "pt"), 
        plot.title  = element_text(size = 12, hjust = 0.5))


#==============================================================================

# QUESTÃO 3
#==============================================================================
# Pessoas ocupadas, emprego e desemprego ---------------------------------
x <- pnad2
x %>%
  group_by(Ano, Trim) %>%
  filter(Ftrab == 1) %>%
  summarise(Ftrab = sum(Ftrab)) %>%
  left_join(summarise(filter(group_by(x, Ano), Ocup == 1), Ocup = sum(Ocup)), by = "Ano") %>%
  left_join(summarise(group_by(x, Ano), n = n()), by = "Ano") %>% 
  mutate(Desocup = Ftrab - Ocup, 
         tx_ocup = (Ocup/Ftrab)*100, 
         tx_desocup = 100-tx_ocup, 
         tx_part    = (Ftrab/n)*100) %>%
  select(Ano, Trim, Ocup, Desocup, Ftrab, n, everything())


# Nível de instrução: pessoal ocupado -------------------------------------
x <- 
  pnad2 %>%
  ungroup() %>%
  filter(Ocup == 1) %>%
  select(Ano, Trim, Estudo) %>%
  mutate(Estudo = replace_na(Estudo,0), 
         E0 = if_else(Estudo == 0, 1,0),    # dummy não aplicável
         E1 = if_else(Estudo == 1, 1,0),     # dummy s/ instrução
         E2 = if_else(Estudo == 2, 1,0),     # dummy ensino fundamental incompleto
         E3 = if_else(Estudo == 3, 1,0),     # dummy ensino fundamental completo
         E4 = if_else(Estudo == 4, 1,0),     # dummy ensinmo médio incompleto
         E5 = if_else(Estudo == 5, 1,0),     # dummy ensino médio completo
         E6 = if_else(Estudo == 6, 1,0),     # dummy ensino superior incompleto
         E7 = if_else(Estudo == 7, 1,0),     # dummy ensino superior completo
         Qtr = paste(Ano, Trim, sep = ":")) %>% 
  group_by(Ano) %>%
  summarise(E0 = sum(E0),
            E1  = sum(E1), 
            E2  = sum(E2), 
            E3  = sum(E3), 
            E4  = sum(E4), 
            E5  = sum(E5), 
            E6  = sum(E6), 
            E7  = sum(E7), 
            Tot = n()) %>%
  pivot_longer(c(E0:E7), names_to = "E", values_to = "y") %>% 
  mutate(E = factor(E), E = fct_recode(E, "não aplicável" = "E0",
                                          "s/ instrução"  = "E1", 
                                          "EF incompleto" = "E2", 
                                          "EF completo"   = "E3", 
                                          "EM incompleto" = "E4", 
                                          "EM completo"   = "E5", 
                                          "ES incompleto" = "E6", 
                                          "ES completo"   = "E7")) %>%
  mutate(y = (y/Tot)*100) %>%
  arrange(Ano, E)

ymin <- min(x$y)
ymax <- max(x$y)
d1   <- 0.2
d2   <- 2.5

x %>%
  ggplot(aes(fct_rev(E), y)) +
  geom_bar(aes(fill = -y), stat = "identity", position = "dodge", alpha = 0.8) + 
  geom_text(aes(label = paste0(round(y, digits = 1), "%")), hjust = -d1, color = paletacinza[1], size = 3, fontface = "bold") + 
  scale_x_discrete(NULL, expand = c(0,0)) + 
  scale_y_continuous(NULL, 
                     limits = c(0,ymax+d2), 
                     breaks = round(seq(0, ymax, length.out = 10), digits = 0),
                     labels = function(y) paste0(y,"%"),
                     expand = c(0,0)) + 
  scale_fill_gradient(NULL) + 
  labs(title = "NÍVEL DE INSTRUÇÃO: PESSOAL OCUPADO", 
       subtitle = "Proporções em relação ao pessoal ocupado", 
       caption = "Fonte: PNAD/IBGE") + 
  coord_flip() + 
  facet_grid(Ano ~.) + 
  g1 + 
  theme(legend.position = "none", 
        axis.line.x.bottom = element_blank(), 
        axis.line.y.left   = element_blank(), 
        axis.ticks.x       = element_blank(), 
        axis.text.x        = element_blank())


# Nível de instrução: pessoal desocupado ----------------------------------
x <- 
  pnad2 %>%
  ungroup() %>%
  filter(Ocup == 2) %>%
  select(Ano, Trim, Estudo) %>%
  mutate(Estudo = replace_na(Estudo,0), 
         E0 = if_else(Estudo == 0, 1,0),    # dummy não aplicável
         E1 = if_else(Estudo == 1, 1,0),     # dummy s/ instrução
         E2 = if_else(Estudo == 2, 1,0),     # dummy ensino fundamental incompleto
         E3 = if_else(Estudo == 3, 1,0),     # dummy ensino fundamental completo
         E4 = if_else(Estudo == 4, 1,0),     # dummy ensinmo médio incompleto
         E5 = if_else(Estudo == 5, 1,0),     # dummy ensino médio completo
         E6 = if_else(Estudo == 6, 1,0),     # dummy ensino superior incompleto
         E7 = if_else(Estudo == 7, 1,0),     # dummy ensino superior completo
         Qtr = paste(Ano, Trim, sep = ":")) %>% 
  group_by(Ano) %>%
  summarise(E0 = sum(E0),
            E1  = sum(E1), 
            E2  = sum(E2), 
            E3  = sum(E3), 
            E4  = sum(E4), 
            E5  = sum(E5), 
            E6  = sum(E6), 
            E7  = sum(E7), 
            Tot = n()) %>%
  pivot_longer(c(E0:E7), names_to = "E", values_to = "y") %>% 
  mutate(E = factor(E), E = fct_recode(E, "não aplicável" = "E0",
                                       "s/ instrução"  = "E1", 
                                       "EF incompleto" = "E2", 
                                       "EF completo"   = "E3", 
                                       "EM incompleto" = "E4", 
                                       "EM completo"   = "E5", 
                                       "ES incompleto" = "E6", 
                                       "ES completo"   = "E7")) %>%
  mutate(y = (y/Tot)*100) %>%
  arrange(Ano, E)

ymin <- min(x$y)
ymax <- max(x$y)
ymin <- min(x$y)
ymax <- max(x$y)
d1   <- 0.2
d2   <- 2.5

x %>%
  ggplot(aes(fct_rev(E), y)) +
  geom_bar(aes(fill = -y), stat = "identity", position = "dodge", alpha = 0.8) + 
  geom_text(aes(label = paste0(round(y, digits = 1), "%")), hjust = -d1, color = paletacinza[1], size = 3, fontface = "bold") + 
  scale_x_discrete(NULL, expand = c(0,0)) + 
  scale_y_continuous(NULL, 
                     limits = c(0,ymax+d2), 
                     breaks = round(seq(0, ymax, length.out = 10), digits = 0),
                     labels = function(y) paste0(y,"%"),
                     expand = c(0,0)) + 
  scale_fill_gradient(NULL) + 
  labs(title    = "NÍVEL DE INSTRUÇÃO: POPULAÇÃO DESOCUPADA", 
       subtitle = "Proporções em relação ao total de desocupados", 
       caption  = "Fonte: PNAD/IBGE") + 
  coord_flip() + 
  facet_grid(Ano ~.) + 
  g1 + 
  theme(legend.position = "none", 
        axis.line.x.bottom = element_blank(), 
        axis.line.y.left   = element_blank(), 
        axis.ticks.x       = element_blank(), 
        axis.text.x        = element_blank())


# Nível de instrução: população geral -------------------------------------

x <- 
  pnad2 %>%
  ungroup() %>%
  select(Ano, Trim, Estudo) %>%
  mutate(Estudo = replace_na(Estudo,0), 
         E0 = if_else(Estudo == 0, 1,0),    # dummy não aplicável
         E1 = if_else(Estudo == 1, 1,0),     # dummy s/ instrução
         E2 = if_else(Estudo == 2, 1,0),     # dummy ensino fundamental incompleto
         E3 = if_else(Estudo == 3, 1,0),     # dummy ensino fundamental completo
         E4 = if_else(Estudo == 4, 1,0),     # dummy ensinmo médio incompleto
         E5 = if_else(Estudo == 5, 1,0),     # dummy ensino médio completo
         E6 = if_else(Estudo == 6, 1,0),     # dummy ensino superior incompleto
         E7 = if_else(Estudo == 7, 1,0),     # dummy ensino superior completo
         Qtr = paste(Ano, Trim, sep = ":")) %>% 
  group_by(Ano) %>%
  summarise(E0  = sum(E0),
            E1  = sum(E1), 
            E2  = sum(E2), 
            E3  = sum(E3), 
            E4  = sum(E4), 
            E5  = sum(E5), 
            E6  = sum(E6), 
            E7  = sum(E7), 
            Tot = n()) %>%
  pivot_longer(c(E0:E7), names_to = "E", values_to = "y") %>% 
  mutate(E = factor(E), E = fct_recode(E, "não aplicável" = "E0",
                                       "s/ instrução"  = "E1", 
                                       "EF incompleto" = "E2", 
                                       "EF completo"   = "E3", 
                                       "EM incompleto" = "E4", 
                                       "EM completo"   = "E5", 
                                       "ES incompleto" = "E6", 
                                       "ES completo"   = "E7")) %>%
  mutate(y = (y/Tot)*100) %>%
  arrange(Ano, E)

ymin <- min(x$y)
ymax <- max(x$y)
ymin <- min(x$y)
ymax <- max(x$y)
d1   <- 0.2
d2   <- 2.2

x %>%
  ggplot(aes(fct_rev(E), y)) +
  geom_bar(aes(fill = -y), stat = "identity", position = "dodge", alpha = 0.8) + 
  geom_text(aes(label = paste0(round(y, digits = 1), "%")), hjust = -d1, color = paletacinza[1], size = 3, fontface = "bold") + 
  scale_x_discrete(NULL, expand = c(0,0)) + 
  scale_y_continuous(NULL, 
                     limits = c(0,ymax+d2), 
                     breaks = round(seq(0, ymax, length.out = 10), digits = 0),
                     labels = function(y) paste0(y,"%"),
                     expand = c(0,0)) + 
  scale_fill_gradient(NULL) + 
  labs(title    = "NÍVEL DE INSTRUÇÃO: POPULAÇÃO DESOCUPADA (2019/2020)", 
       subtitle = "Proporções em relação ao total de desocupados", 
       caption  = "Fonte: PNAD/IBGE") + 
  coord_flip() + 
  facet_grid(Ano ~.) + 
  g1 + 
  theme(legend.position = "none", 
        axis.line.x.bottom = element_blank(), 
        axis.line.y.left   = element_blank(), 
        axis.ticks.x       = element_blank(), 
        axis.text.x        = element_blank())


# Nível de instrução: força de trabalho -----------------------------------

x <- 
  pnad2 %>%
  ungroup() %>%
  select(Ano, Trim, Estudo, Ftrab) %>%
  filter(Ftrab == 1) %>%
  mutate(Estudo = replace_na(Estudo,0), 
         E0 = if_else(Estudo == 0, 1,0),    # dummy não aplicável
         E1 = if_else(Estudo == 1, 1,0),     # dummy s/ instrução
         E2 = if_else(Estudo == 2, 1,0),     # dummy ensino fundamental incompleto
         E3 = if_else(Estudo == 3, 1,0),     # dummy ensino fundamental completo
         E4 = if_else(Estudo == 4, 1,0),     # dummy ensinmo médio incompleto
         E5 = if_else(Estudo == 5, 1,0),     # dummy ensino médio completo
         E6 = if_else(Estudo == 6, 1,0),     # dummy ensino superior incompleto
         E7 = if_else(Estudo == 7, 1,0),     # dummy ensino superior completo
         Qtr = paste(Ano, Trim, sep = ":")) %>% 
  group_by(Ano) %>%
  summarise(E0  = sum(E0),
            E1  = sum(E1), 
            E2  = sum(E2), 
            E3  = sum(E3), 
            E4  = sum(E4), 
            E5  = sum(E5), 
            E6  = sum(E6), 
            E7  = sum(E7), 
            Tot = n()) %>%
  pivot_longer(c(E0:E7), names_to = "E", values_to = "y") %>% 
  mutate(E = factor(E), E = fct_recode(E, "não aplicável" = "E0",
                                       "s/ instrução"  = "E1", 
                                       "EF incompleto" = "E2", 
                                       "EF completo"   = "E3", 
                                       "EM incompleto" = "E4", 
                                       "EM completo"   = "E5", 
                                       "ES incompleto" = "E6", 
                                       "ES completo"   = "E7")) %>%
  mutate(y = (y/Tot)*100) %>%
  arrange(Ano, E)

ymin <- min(x$y)
ymax <- max(x$y)
ymin <- min(x$y)
ymax <- max(x$y)
d1   <- 0.2
d2   <- 2.2

x %>%
  ggplot(aes(fct_rev(E), y)) +
  geom_bar(aes(fill = -y), stat = "identity", position = "dodge", alpha = 0.8) + 
  geom_text(aes(label = paste0(round(y, digits = 1), "%")), hjust = -d1, color = paletacinza[1], size = 3, fontface = "bold") + 
  scale_x_discrete(NULL, expand = c(0,0)) + 
  scale_y_continuous(NULL, 
                     limits = c(0,ymax+d2), 
                     breaks = round(seq(0, ymax, length.out = 10), digits = 0),
                     labels = function(y) paste0(y,"%"),
                     expand = c(0,0)) + 
  scale_fill_gradient(NULL) + 
  labs(title    = "NÍVEL DE INSTRUÇÃO: POPULAÇÃO NA FORÇA DE TRABALHO (2019/2020)", 
       subtitle = "Proporções em relação ao total de desocupados", 
       caption  = "Fonte: PNAD/IBGE") + 
  coord_flip() + 
  facet_grid(Ano ~.) + 
  g1 + 
  theme(legend.position = "none", 
        axis.line.x.bottom = element_blank(), 
        axis.line.y.left   = element_blank(), 
        axis.ticks.x       = element_blank(), 
        axis.text.x        = element_blank())


# Nível de instrução: ocupados vs desocupados ---------------------------

x1 <- x1 %>% mutate(Ocup = "Ocupados")    %>% filter(Ano == 2020) %>% select(Ocup, E,y)
x2 <- x2 %>% mutate(Ocup = "Desocupados") %>% filter(Ano == 2020) %>% select(Ocup, E,y)

x <- rbind(x1,x2)

d1 <- 0.1
d2 <- 2.5
geom_text(aes(label = paste0(round(y, digits = 1), "%")), hjust = -d, color = "#000000", size = 3, fontface = "bold")
  
x %>%
  ggplot(aes(fct_rev(E), y)) +
  geom_bar(aes(fill = -y), stat = "identity", position = "dodge", alpha = 0.8) + 
  geom_text(aes(label = paste0(round(y, digits = 1), "%")), hjust = -d1, color = paletacinza[1], size = 3, fontface = "bold") + 
  scale_x_discrete(NULL, expand = c(0,0)) + 
  scale_y_continuous(NULL, 
                     limits = c(0,ymax+d2), 
                     breaks = round(seq(0, ymax, length.out = 10), digits = 0),
                     labels = function(y) paste0(y,"%"),
                     expand = c(0,0)) + 
  scale_fill_gradient(NULL) + 
  labs(title    = "NÍVEL DE INSTRUÇÃO: POPULAÇÃO OCUPADA E DESOCUPADA (2019/2020)", 
       subtitle = "Proporções em relação ao total de desocupados", 
       caption  = "Fonte: PNAD/IBGE") + 
  coord_flip() + 
  facet_grid(Ocup ~.) + 
  g1 + 
  theme(legend.position    = "none", 
        axis.line.x.bottom = element_blank(), 
        axis.line.y.left   = element_blank(), 
        axis.ticks.x = element_blank(), 
        axis.text.x = element_blank())


# Rendimento médio: ocupados ----------------------------------------------

x <- x1 <-
  pnad2 %>%
  ungroup() %>%
  filter(Ocup == 1) %>%
  select(Qtr, Rend_h, Rend_e) %>%
  pivot_longer(c(Rend_h, Rend_e), names_to = "Rend", values_to = "y") %>%
  mutate(Rend = if_else(Rend == "Rend_h", "Habitual", "Efetivo"), 
         Qtr = paste(year(Qtr),quarter(Qtr), sep = ":")) %>%
  arrange(Qtr, Rend) %>%
  group_by(Qtr, Rend) %>%
  summarise(y = mean(y, na.rm = TRUE)) %>%
  mutate(Grupo = "Ocupados") %>%
  select(Grupo, Qtr, Rend, y)

ymin <- min(x$y, na.rm = T) 
ymax <- max(x$y, na.rm = T) + 80

x %>%
  ggplot(aes(Qtr,y, fill = Rend)) +
  geom_bar(stat = "identity", position = "dodge", alpha = 0.8) + 
  geom_text(aes(y = y + 40, label = round (y, digits = 0)), 
            size = 3.2, 
            fontface = "bold",
            nudge_x = c(-0.2,0.2)) + 
  scale_x_discrete(NULL) + 
  scale_y_continuous(NULL, limits = c(0,ymax), breaks = round(seq(0, ymax, length.out = 10), digits = 0), expand = c(0,0)) + 
  scale_fill_manual(NULL, values = c(paletacinza[2], paletacinza[5])) + 
  labs(title = "RENDIMENTO MÉDIO: PESSOAL OCUPADO (2019/2020)", 
       subtitle = "Média dos rendimentos mensais (R$)", 
       caption = "Fonte: PNAD/IBGE") + 
  g2 + theme(legend.position  = "top",
             legend.justification = "center", 
             legend.title = element_blank(), 
             axis.text.x.bottom = element_text(size = 10), 
             axis.text.y.left = element_text(size = 10))


# Rendimento médio: ocupados com redução de jornada -----------------------

x <- x2 <- 
  pnad2 %>%
  group_by(Ano) %>%
  select(Ano, Trim, Rend_h, Rend_e, Hora_h, Hora_e, Ocup) %>%
  mutate(Red = if_else((Hora_e/Hora_h) <= 0.75, 1,0)) %>%
  filter(Ocup == 1, Red == 1) %>%
  select(-Hora_h, -Hora_e, -Red, -Ocup) %>%
  pivot_longer(c(Rend_h, Rend_e), names_to = "Rend", values_to = "y") %>%
  mutate(Rend = if_else(Rend == "Rend_h", "Habitual", "Efetivo"), 
         Qtr  = paste(Ano,Trim, sep = ":")) %>%
  group_by(Qtr, Rend) %>%
  summarise(y = mean(y, na.rm = T)) %>%
  mutate(Grupo = "Reduzidos") %>%
  select(Grupo, everything())

ymin <- min(x$y, na.rm = T) 
ymax <- max(x$y, na.rm = T) + 80

x %>%
  ggplot(aes(Qtr,y, fill = Rend)) +
  geom_bar(stat = "identity", position = "dodge", alpha = 0.8) + 
  geom_text(aes(y = y + 40, label = round (y, digits = 0)), 
            size = 3.2, 
            fontface = "bold",
            nudge_x = c(-0.2,0.2)) + 
  scale_x_discrete(NULL) + 
  scale_y_continuous(NULL, limits = c(0,ymax), breaks = round(seq(0, ymax, length.out = 10), digits = 0), expand = c(0,0)) + 
  scale_fill_manual(NULL, values = c(paletacinza[2], paletacinza[5])) + 
  labs(title    = "RENDIMENTO MÉDIO: PESSOAL OCUPADO COM REDUÇÃO DE JORNADA (2019/2020)", 
       subtitle = "Média dos rendimentos mensais (R$); Amostra: grupo com horas efetivas ao menos 25% menores que habituais", 
       caption  = "Fonte: PNAD/IBGE") + 
  g2 + theme(legend.position  = "top",
             legend.justification = "center", 
             legend.title = element_blank(), 
             axis.text.x.bottom = element_text(size = 10), 
             axis.text.y.left = element_text(size = 10))


# Rendimento médio: ocupados e Afastados ----------------------------------------------

x <- x3 <-
  pnad2 %>%
  ungroup() %>%
  select(Ano, Trim, Rend_h, Rend_e, Ocup, Afastado) %>%
  filter(Ocup == 1, Afastado == 1) %>%
  select(-Afastado, - Ocup) %>%
  pivot_longer(c(Rend_h, Rend_e), names_to = "Rend", values_to = "y") %>%
  mutate(Rend = if_else(Rend == "Rend_h", "Habitual", "Efetivo"), 
         Qtr  = paste(Ano,Trim, sep = ":")) %>%
  group_by(Qtr, Rend) %>%
  summarise(y = mean(y, na.rm = T)) %>%
  mutate(Grupo = "Afastados") %>%
  select(Grupo,  everything())

ymin <- min(x$y, na.rm = T) 
ymax <- max(x$y, na.rm = T) + 80

x %>%
  ggplot(aes(Qtr,y, fill = Rend)) +
  geom_bar(stat = "identity", position = "dodge", alpha = 0.8) + 
  geom_text(aes(y = y + 40, label = round (y, digits = 0)), 
            size = 3.2, 
            fontface = "bold",
            nudge_x = c(-0.2,0.2)) + 
  scale_x_discrete(NULL) + 
  scale_y_continuous(NULL, limits = c(0,ymax), breaks = round(seq(0, ymax, length.out = 10), digits = 0), expand = c(0,0)) + 
  scale_fill_manual(NULL, values = c(paletacinza[2], paletacinza[5])) + 
  labs(title = "RENDIMENTO MÉDIO: PESSOAL AFASTADO DO TRABALHO (2019/2020)", 
       subtitle = "Média dos rendimentos mensais (R$)", 
       caption = "Fonte: PNAD/IBGE") + 
  g2 + theme(legend.position  = "top",
             legend.justification = "center", 
             legend.title = element_blank(), 
             axis.text.x.bottom = element_text(size = 10), 
             axis.text.y.left = element_text(size = 10))

# Rendimento médio: conjunção dos subgrupos -----------------------------

x <- 
  rbind(x1,x2,x3) %>%
  separate(Qtr, into = c("Qtr", NA), sep = ":")  %>%
  mutate(Grupo = factor(Grupo, levels = c("Ocupados", "Reduzidos", "Afastados")), 
         Qtr   = factor(Qtr,   levels = c("2019", "2020")), 
         Rend  = factor(Rend,  levels = c("Efetivo", "Habitual"))) %>%
  arrange(Qtr) %>%
  mutate(y_2019 = if_else(Qtr == "2019", y, NA_real_), 
         y_2020 = if_else(Qtr == "2020", y, NA_real_))

d1   <- -0.3
d2   <- 150
ymin <- min(x$y)
ymax <- max(x$y) + d2

x %>%
  ggplot() + 
  geom_bar(aes(Rend, y,  fill = fct_rev(Qtr)), stat = "identity", position = "dodge", alpha = 0.8) +
  geom_text(aes(Rend, y, fill = fct_rev(Qtr), label = round(y_2019, digits = 0)), 
            hjust    = d1, 
            color    = paletacinza[1], 
            size     = 3, 
            fontface = "bold", 
            nudge_x  = 0.2) + 
  geom_text(aes(Rend, y, fill = fct_rev(Qtr), label = round(y_2020, digits = 0)), 
            hjust    = d1, 
            color    = paletacinza[1], 
            size     = 3, 
            fontface = "bold", 
            nudge_x  = -0.2) + 
  scale_y_continuous(NULL, limits = c(0,ymax), expand = c(0,0)) +
  scale_fill_manual(NULL, breaks = unique(x$Qtr), values = c(paletaazul[4], paletaazul[2])) +
  coord_flip() + 
  facet_grid(Grupo ~ .) + 
  labs(title = "RENDIMENTO MÉDIO: SUBGRUPOS SELECIONADOS (2019/2020)", 
       subtitle = "Média de rendimento mensal (R$)") + 
  g1 + theme(legend.position      = "top", 
             legend.justification = "center", 
             axis.ticks.x         = element_blank(), 
             axis.text.x          = element_blank(), 
             axis.title.y         = element_blank()) -> g; g

# g + theme(plot.title = element_blank(), plot.subtitle = element_blank()) + leg11


# Horas: pessoas ocupadas -------------------------------------------------
x <-  
  pnad2 %>%
  ungroup() %>%
  filter(Ocup == 1) %>%
  select(Qtr, Hora_h, Hora_e) %>%
  pivot_longer(c(Hora_h, Hora_e), names_to = "Horas", values_to = "y") %>%
  mutate(Horas = if_else(Horas == "Hora_h", "Habituais", "Efetivas"), 
         Qtr   = paste(year(Qtr),quarter(Qtr), sep = ":")) %>%
  arrange(Qtr, Horas) %>%
  group_by(Qtr, Horas) %>%
  summarise(y = mean(y, na.rm = TRUE))

ymin <- min(x$y, na.rm = T) 
ymax <- max(x$y, na.rm = T) + 2

x %>%
  ggplot(aes(Qtr,y, fill = Horas)) +
  geom_bar(stat = "identity", position = "dodge", alpha = 0.8) + 
  geom_text(aes(y = y + 1, label = round (y, digits = 0)), 
            size = 3.2, 
            fontface = "bold",
            nudge_x = c(-0.22,0.22)) + 
  scale_x_discrete(NULL) + 
  scale_y_continuous(NULL, limits = c(0,ymax), breaks = round(seq(0, ymax, length.out = 10), digits = 0), expand = c(0,0)) + 
  scale_fill_manual(NULL, values = c(paletacinza[2], paletacinza[5])) + 
  labs(title = "HORAS TRABALHADAS: PESSOAL OCUPADO (2019/2020)", 
       subtitle = "Média das horas trabalhadas", 
       caption = "Fonte: PNAD/IBGE") + 
  g2 + theme(legend.position  = "top",
             legend.justification = "center", 
             legend.title = element_blank(), 
             axis.text.x.bottom = element_text(size = 10), 
             axis.text.y.left = element_text(size = 10))


# Horas: ocupadas com redução de jornadas ---------------------------------
x<-  
  pnad2 %>%
  group_by(Ano) %>%
  mutate(Red = if_else((Hora_e/Hora_h) <= 0.75, 1,0)) %>%
  filter(Ocup == 1, Red == 1) %>%
  select(Ano, Trim, Hora_h, Hora_e) %>%
  pivot_longer(c(Hora_h, Hora_e), names_to = "Horas", values_to = "y") %>%
  mutate(Horas = if_else(Horas == "Hora_h", "Habituais", "Efetivas"), 
         Qtr   = paste(Ano, Trim, sep = ":")) %>%
  arrange(Qtr, Horas) %>%
  group_by(Qtr, Horas) %>%
  summarise(y = mean(y, na.rm = TRUE))

ymin <- min(x$y, na.rm = T) 
ymax <- max(x$y, na.rm = T) + 2

x %>%
  ggplot(aes(Qtr,y, fill = Horas)) +
  geom_bar(stat = "identity", position = "dodge", alpha = 0.8) + 
  geom_text(aes(y = y + 1, label = round (y, digits = 0)), 
            size = 3.2, 
            fontface = "bold",
            nudge_x = c(-0.22,0.22)) + 
  scale_x_discrete(NULL) + 
  scale_y_continuous(NULL, limits = c(0,ymax), breaks = round(seq(0, ymax, length.out = 10), digits = 0), expand = c(0,0)) + 
  scale_fill_manual(NULL, values = c(paletacinza[2], paletacinza[5])) + 
  labs(title = "HORAS TRABALHADAS: PESSOAL OCUPADO COM REDUÇÃO DE JORNADA (2019/2020)", 
       subtitle = "Média das horas trabalhadas (grupo com ao menos 25% de redução de horas)", 
       caption = "Fonte: PNAD/IBGE") + 
  g2 + theme(legend.position  = "top",
             legend.justification = "center", 
             legend.title = element_blank(), 
             axis.text.x.bottom = element_text(size = 10), 
             axis.text.y.left = element_text(size = 10))


# Horas: ocupados e afastados ---------------------------------------------
x<-  
  pnad2 %>%
  group_by(Ano) %>%
  filter(Ocup == 1, Afastado == 1) %>%
  select(Ano, Trim, Hora_h, Hora_e) %>%
  pivot_longer(c(Hora_h, Hora_e), names_to = "Horas", values_to = "y") %>%
  mutate(Horas = if_else(Horas == "Hora_h", "Habituais", "Efetivas"), 
         Qtr   = paste(Ano, Trim, sep = ":")) %>%
  arrange(Qtr, Horas) %>%
  group_by(Qtr, Horas) %>%
  summarise(y = mean(y, na.rm = TRUE))

ymin <- min(x$y, na.rm = T) 
ymax <- max(x$y, na.rm = T) + 2

x %>%
  ggplot(aes(Qtr,y, fill = Horas)) +
  geom_bar(stat = "identity", position = "dodge", alpha = 0.8) + 
  geom_text(aes(y = y + 1, label = round (y, digits = 0)), 
            size = 3.2, 
            fontface = "bold",
            nudge_x = c(-0.22,0.22)) + 
  scale_x_discrete(NULL) + 
  scale_y_continuous(NULL, limits = c(0,ymax), breaks = round(seq(0, ymax, length.out = 10), digits = 0), expand = c(0,0)) + 
  scale_fill_manual(NULL, values = c(paletacinza[2], paletacinza[5])) + 
  labs(title = "HORAS TRABALHADAS: PESSOAL OCUPADO E AFASTADO (2019/2020)", 
       subtitle = "Média das horas trabalhadas (grupo com afastamento do trabalho)", 
       caption = "Fonte: PNAD/IBGE") + 
  g2 + theme(legend.position  = "top",
             legend.justification = "center", 
             legend.title = element_blank(), 
             axis.text.x.bottom = element_text(size = 10), 
             axis.text.y.left = element_text(size = 10))


# Regiões: rendimento  ----------------------------------------------------
x <- 
  pnad2 %>%
  ungroup() %>%
  select(Ano, Trim, UF, Ocup, Rend_e) %>%
  filter(Ocup == 1) %>%
  select(-Ocup) %>%
  group_by(Ano) %>%
  rename(CodUF = UF) %>%
  left_join(select(Estados, Reg, UF, CodUF), by = "CodUF") %>%
  transmute(Ano, Trim,Reg, UF, Qtr = paste(Ano, Trim, sep = ":"), y = Rend_e) %>%
  mutate(Reg = case_when(Reg == "Norte" ~ "N", 
                         Reg == "Nordeste" ~ "NE", 
                         Reg == "Sudeste" ~ "SD", 
                         Reg == "Sul" ~ "S", 
                         TRUE ~ "CO")) %>%
  group_by(Reg, Qtr) %>%
  summarise(y = mean(y, na.rm = TRUE)) %>%
  group_by(Reg) %>%
  mutate(med = median(y, na.rm = TRUE)) %>%
  arrange(Qtr,Reg)
  
ymin <- min(x$y, na.rm = T) 
ymax <- max(x$y, na.rm = T) + 80

x %>%
  ggplot(aes(reorder(Reg,med), y, fill = fct_rev(factor(Qtr)))) + 
  geom_bar(stat = "identity", position = "dodge", alpha = 0.7) + 
  scale_x_discrete(NULL) + 
  scale_y_continuous(NULL, expand = c(0,0)) + 
  scale_fill_manual(NULL, values = c("2019:4" = paletaazul[3], "2020:4" = paletacinza[1])) + 
  coord_flip() + 
  labs(title    = "RENDIMENTO MÉDIO POR REGIÕES GEOPOLÍTICAS (2019//2020)", 
       subtitle = "Rendimento médio mensal do pessoal ocupado (R$)", 
       caption  = "Fonte: PNAD/IBGE") + 
  g2 + leg10 + 
  theme(panel.border       = element_blank(),
        axis.line.x.bottom = element_blank(), 
        axis.line.y.left   = element_blank(), 
        axis.ticks.x       = element_blank(), 
        axis.text.x        = element_blank())


# Regiões: desemprego -----------------------------------------------------

x <- 
  pnad2 %>%
  ungroup() %>%
  select(Ano, Trim, UF, Ocup, Ftrab) %>%
  filter(Ftrab == 1) %>%
  mutate(Ocup = if_else(Ocup == 2, 0, 1)) %>%
  rename(CodUF = UF) %>%
  left_join(select(Estados, Reg, UF, CodUF), by = "CodUF") %>%
  transmute(Ano, Trim, Qtr = paste(Ano, Trim, sep = ":"), Reg, UF, Ocup, Ftrab) %>%
  mutate(Reg = case_when(Reg == "Norte" ~ "N", 
                         Reg == "Nordeste" ~ "NE", 
                         Reg == "Sudeste" ~ "SD", 
                         Reg == "Sul" ~ "S", 
                         TRUE ~ "CO")) %>%
  group_by(Reg, Qtr) %>%
  summarise(Ocup = sum(Ocup), Ftrab = sum(Ftrab)) %>%
  mutate(y = 100 - ((Ocup/Ftrab)*100)) %>%
  group_by(Reg) %>%
  mutate(med = median(y))

x %>%
  ggplot(aes(reorder(Reg,med), y, fill = fct_rev(factor(Qtr)))) + 
  geom_bar(stat = "identity", position = "dodge", alpha = 0.7) + 
  scale_x_discrete(NULL) + 
  scale_y_continuous(NULL, expand = c(0,0)) + 
  scale_fill_manual(NULL, values = c("2019:4" = paletaazul[3], "2020:4" = paletacinza[1])) + 
  coord_flip() + 
  labs(title    = "RENDIMENTO MÉDIO POR REGIÕES GEOPOLÍTICAS (2019//2020)", 
       subtitle = "Rendimento médio mensal do pessoal ocupado (R$)", 
       caption  = "Fonte: PNAD/IBGE") + 
  g2 + leg10 + 
  theme(panel.border       = element_blank(),
        axis.line.x.bottom = element_blank(), 
        axis.line.y.left   = element_blank(), 
        axis.ticks.x       = element_blank(), 
        axis.text.x        = element_blank())


#==============================================================================