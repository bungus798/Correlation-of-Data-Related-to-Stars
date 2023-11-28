library(h5ls)
library(tidyverse)

header <- h5ls("STA130_APOGEE.h5") 
header

wavelength <- "STA130_APOGEE.h5" %>% h5read("wavelength") %>% as_tibble()

wavelength %>% glimpse()

ggplot(data=wavelength) + aes(x=value) + geom_histogram()

teff <- "STA130_APOGEE.h5" %>% h5read("teff") %>% as_tibble()
glimpse(teff)
ggplot(data = teff) + aes(x = value) +geom_histogram(bins = 50) + xlab("Effective Temperature (k)")

teff_sample <- teff %>% 
  sample_n(7514, replace = FALSE)

teff_wave <- lm(teff_sample$value ~ wavelength$value) %>% summary()
teff_wave


ggplot(data = teff_sample, aes(x = wavelength$value, y = value)) + 
  geom_point() + 
  geom_smooth(method = 'lm', se = FALSE,) +
  xlab("Wavelength") +
  ylab("Effective Temperature (k)")

logg<- "STA130_APOGEE.h5" %>% h5read("logg") %>% as_tibble()
logg %>% glimpse()


samp_size <- 99705
n_trial <- 1000

set.seed(911)
sim_mean_logg <- rep(NA,n_trial)
for (i in 1:n_trial){
  bootstrap_sample <- logg %>% slice_sample(n=samp_size,replace = TRUE)
  sim_mean = bootstrap_sample %>% summarise(mean(value)) %>% as.numeric()
  sim_mean_logg[i] = sim_mean
}
sim_mean_logg_data<- tibble(sim_mean_logg)
#sim_mean_logg_data %>% glimpse()

sim_mean_logg_data %>% ggplot(aes(x=sim_mean_logg)) + 
  geom_histogram(color = "black",fill ="grey",bins = 25) +
  labs(title="The Distribution of Simulated Mean Values of logg",
       x="Mean Value(cenetimetres-grams-seconds)")


X1=c((1-0.99)/2,(1+0.99)/2)
X2=c((1-0.95)/2,(1+0.95)/2)
X3=c((1-0.90)/2,(1+0.90)/2)
X4=c((1-0.85)/2,(1+0.85)/2)

quantile(sim_mean_logg,X1)
quantile(sim_mean_logg,X2)
quantile(sim_mean_logg,X3)
quantile(sim_mean_logg,X4)

sim_mean_logg_data %>% ggplot(aes(x=sim_mean_logg)) + geom_boxplot() + labs(x="Mean Value(cenetimetres-grams-seconds)")

feh <- "STA130_APOGEE.h5" %>% h5read("fe_h") %>% as_tibble()

ggplot(data=feh) + aes(x=value) + geom_histogram() + xlab("Levels of Iron in the Stars relative to the Sun")

cah <- "STA130_APOGEE.h5" %>% h5read("ca_h") %>% as_tibble()


oh <- "STA130_APOGEE.h5" %>% h5read("o_h") %>% as_tibble()
ch <- "STA130_APOGEE.h5" %>% h5read("c_h") %>% as_tibble()
mgh <- "STA130_APOGEE.h5" %>% h5read("mg_h") %>% as_tibble()
alh <- "STA130_APOGEE.h5" %>% h5read("al_h") %>% as_tibble()
nh <- "STA130_APOGEE.h5" %>% h5read("n_h") %>% as_tibble()
cah <- "STA130_APOGEE.h5" %>% h5read("ca_h") %>% as_tibble()

not_fe_elements <- bind_cols(oh, ch, mgh, alh, nh, cah) %>% rename(oh = value...1, ch = value...2, mgh = value...3, alh = value...4, nh = value...5, cah = value...6)
glimpse(not_fe_elements)

oh <- "STA130_APOGEE.h5" %>% h5read("o_h") %>% as_tibble()
ch <- "STA130_APOGEE.h5" %>% h5read("c_h") %>% as_tibble()
mgh <- "STA130_APOGEE.h5" %>% h5read("mg_h") %>% as_tibble()
alh <- "STA130_APOGEE.h5" %>% h5read("al_h") %>% as_tibble()
nh <- "STA130_APOGEE.h5" %>% h5read("n_h") %>% as_tibble()
cah <- "STA130_APOGEE.h5" %>% h5read("ca_h") %>% as_tibble()
feh <- "STA130_APOGEE.h5" %>% h5read("fe_h") %>% as_tibble()

not_iron <- oh + ch +mgh + alh + nh + cah


elements <- bind_cols(not_iron, feh) %>% rename(not_iron = value...1, feh = value...2)
glimpse(elements)


iron_elements_regression <- lm(feh ~ not_iron, data = elements) %>% summary()
iron_elements_regression


iron_elements_correlation <- cor(elements$feh, elements$not_iron)
iron_elements_correlation

ggplot(data = elements) + aes(x = feh, y = not_iron) + geom_point() + geom_smooth(method=lm, se=FALSE)

