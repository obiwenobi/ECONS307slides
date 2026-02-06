n <- 5000
puissance <- rnorm(n,300,300)
poids <- 1.5*puissance+ rnorm(n,150,300)
u <- rnorm(n,50,50)
prix <- 600 + 1.5*puissance-poids + u

dta <- tibble(prix,puissance,poids)

reg <- dta %>% lm(formula=prix ~ puissance+poids)  
ggplot(dta,aes(x=puissance,y=prix))+
  geom_point()+
  geom_smooth(method="lm",formula = y~x)

plot(residuals(reg),u)

reg %>% residuals()
plot(residuals(reg),u)


ggplot(dta, aes(x = poids, y = prix)) + 
  geom_point(alpha = 0.3) +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  geom_smooth(method = "lm", formula = y ~ x - 1, se = FALSE, color = "red") +
  xlim(c(0, NA)) + ylim(c(0, NA))  # Extend to see origin



mod <- lm(prix ~ poids, data = dta)


dta$res <- resid(mod)

ggplot(dta, aes(poids, res2)) +
  geom_point() +
  geom_smooth()

data <- tibble(poids=dta$poids,res1=reg$residuals,res2=reg2$residuals) 


# Cleaning dataphones dataset
df <- read.csv("C:/Users/user/PyCharmMiscProject/dataphones.csv")


list_var <- c('price',
              "Qualité de la caméra","Capacité totale de stockage",
              "Année d'introduction","Produit",
              "Marque","Garantie","Poids","Couleur","Mémoire vive (RAM)",
              "Processeur","Nombre total de mégapixels","Densité de pixels"
              )
phones <- df %>% select(-X) %>% 
  filter(Variable %in% list_var) %>% 
  group_by(model) %>% distinct() %>% 
  pivot_wider(names_from = "Variable",values_from = "Value",id_cols = "model") %>% 
  ungroup %>% 
  select(-model) %>% 
  relocate(Produit,.before=price) %>% 
  tibble

phonesdf <- phones %>% 
  mutate(price=str_remove_all(price, "[€\\-\\,\\.]")) %>% 
  mutate(price=as.numeric(price)) %>% 
  mutate(Poids=str_remove_all(Poids, "g"),Poids=as.numeric(Poids)) %>% 
  mutate(RAM=str_remove_all(`Mémoire vive (RAM)`, "Go"),RAM=as.numeric(RAM)) %>% select(-`Mémoire vive (RAM)`) %>% 
  mutate(stockage=str_replace_all(`Capacité totale de stockage`,"1 To","1000 Go"),
         stockage=str_remove_all(stockage, "Go"),
         stockage=as.numeric(stockage)) %>% select(-`Capacité totale de stockage`) %>% 
  mutate(megapixels=str_remove_all(`Nombre total de mégapixels`, "Mpx"),megapixels=as.numeric(megapixels)) %>%
  select(-`Nombre total de mégapixels`) %>% 
  mutate(age=2026-as.numeric(`Année d'introduction`)) %>% 
  mutate(Marque=factor(Marque),Marque=relevel(Marque,"Apple"),
         `Qualité de la caméra`=factor(`Qualité de la caméra`),
         `Qualité de la caméra`=relevel(`Qualité de la caméra`,"Bon")) %>% 
  filter(Marque!="Refurbished") %>% mutate(meanprice=mean(price)) 

phonesdf %>% tibble
phonesdf %>% colnames


preg <- phonesdf %>% 
  lm(formula = price~ Poids + 
       megapixels + factor(age) + RAM + 
       stockage + (`Qualité de la caméra`)+(Marque))
preg %>% summary()


preg2 <- linear_reg() %>% 
  set_engine("lm") %>% 
  fit( price~ Poids + 
         megapixels + factor(age) + RAM + 
         stockage + (`Qualité de la caméra`)+(Marque), data = phonesdf) 

tidy(preg2)%>% 
  mutate(term = reorder(term, estimate),
    conf.low  = estimate - 1.96 * std.error,
    conf.high = estimate + 1.96 * std.error) %>%
  ggplot(aes(x = estimate, y = term)) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey50") +
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high),
    height = 0.2,
    color = "grey40") +
  geom_point(size = 3, color = "#0072B2") +
  labs(x = "Coefficient estimate",
    y = NULL,
    title = "Coefficient plot with 95% confidence intervals") +
  theme_minimal(base_size = 13)
