library(dplyr)
library(magrittr)
library(ggplot2)


df <- readxl::read_xlsx("../data/9May2021_diachronic_CORRECTED.xlsx")

# head(df) %>% as.data.frame()

df$troughFromVstart <- ifelse(is.na(df$corrected_troughFromVstart), df$troughFromVstart, df$corrected_troughFromVstart)
df$troughFromVend <- ifelse(is.na(df$corrected_troughFromVend), df$troughFromVend, df$corrected_troughFromVend)
df$troughtime <- ifelse(is.na(df$corrected_troughtime), df$troughtime, df$corrected_troughtime)

#df %<>% dplyr::select(-`...24`, -`...25`, -`...26`, -`...27`)
df %<>% dplyr::rename(generation4 = `generation4...19`, generation4_numeric = `generation4...18`)

#xtabs(~df$generation4+df$generation4_numeric)
# df %<>% subset(generation4 %in% c("gen1","gen2","gen3","gen4") )

# there are still errors in the character representation of the generation: Use the numeric one instead
# x <- df %>% subset( paste0("gen", generation4_numeric) != generation4 )
# x %>% dplyr::select(filename, generation4, generation4_numeric)
df$generation4 <- paste("generation", df$generation4_numeric)

# plot a histogram of number of observations per speaker
length(unique(df$participant))
df %>% group_by(participant, generation4, dialectname) %>% dplyr::summarise(N=n()) %>% ggplot(aes(N)) + geom_histogram() + ylab("Number of speakers") + xlab("Number of observations") + facet_grid(dialectname~generation4)

# # by-observation histogram of the variable with the highest degree of bi-modality 
# df %>% subset(troughFromVend < 50 & troughFromVend > -50) %>% ggplot(aes(troughFromVend)) +geom_histogram()+facet_grid(dialectname~generation4)

## no need for by-speakers histograms, since speakers exhibit substantial variation from utterance to utterance, and the assumption we're modeling
## is that *utterances* come from a bimodal distribution
## # by-participant histogram of the averages of the variable with the highest degree of bi-modality 
## df %>% group_by(participant, generation4, dialectname) %>% dplyr::summarise(troughFromVend = mean(troughFromVend)) %>% 
##         ggplot(aes(troughFromVend)) +geom_histogram()+facet_grid(dialectname~generation4)

# set group ids for Athenian (-1), AMG (0), Turkish (1) to be used in the mixture analysis 
df %<>% dplyr::select(-group)
contr_group <- data.frame(dialectname=c('Athenian','AMG','Turkish'), group=c(-1, 0, 1))
df %<>% left_join(contr_group)

# df_contr_generation <- data.frame(generation4 = c("generation 1", "generation 2", "generation 3", "generation 4"),
#                                   cGen_2m1 = c(-3/4,  1/4,  1/4,  1/4),
#                                   cGen_3m2 = c(-2/4, -2/4,  2/4,  2/4),
#                                   cGen_4m3 = c(-1/4, -1/4, -1/4,  3/4))
# df %<>% left_join( df_contr_generation )
# 
# df_contr_dialect <- data.frame(dialectname = c('Athenian', 'AMG', 'Turkish'),
#                                cDialect_AMGmATH = c(-2/3,  1/3,  1/3),
#                                cDialect_TRmAMG = c(-1/3,  -1/3,  2/3)
#                                )
# df %<>% left_join( df_contr_dialect )

# # set generation contrasts
# df %<>% dplyr::select(-cdGen1, -cdGen2, -cdGen3, -cdGen4)
# df_contr_generation_dummy <- data.frame(generation4 = c("generation 1", "generation 2", "generation 3", "generation 4"),
#                                         cdGen1 = c(1, 0, 0, 0),
#                                         cdGen2 = c(0, 1, 0, 0),
#                                         cdGen3 = c(0, 0, 1, 0),
#                                         cdGen4 = c(0, 0, 0, 1)
#                                         )
# df %<>% left_join( df_contr_generation_dummy )

df$participant %<>% as.factor()

df$isAMG <- as.integer(df$dialectname == "AMG")
df$cAMG <- (df$dialectname == "AMG") - 0.5

head(df) %>% as.data.frame()
df_clean <- df %>% select( filename, rec_year, speaker=participant, gender=`gender...15`, 
                           dialect = dialectname, generation=generation4, 
                           tau = troughFromVend, c1, c2, c3, c4)

saveRDS(df_clean, file = "../data/data_mix.rds")
