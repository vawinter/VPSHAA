# renaming columns for easier readability

dat2 <- dat %>% 
  rename(m_SC_a.cos = m__SC_a.cos,
         m_SC_a.sin = m__SC_a.sin,
         m_SC_bio = m__SC_bio,
         m_SC_rough = m__SC_rough,
         m_SC_herb = m__SC_herb,
         m_SC_shrub = m__SC_shrub)

write.csv(dat2, "Data/Outputs/RSF_outputs/20221017_mem-prep-v2.csv", row.names = T)
