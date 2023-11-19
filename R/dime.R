
library(tidyverse)

dime <- read_csv("data/dime_recipients_1979_2020.csv") |> 
  filter(str_detect(election, "2020")) |> 
  select(-bonica.cid, -cand.gender, -dwnom1, -dwnom2, -ps.dwnom1, -ps.dwnom2,
         -irt.cfscore, -num.givers:-ind.exp.oppose, -recipient.type,
         -igcat, -comtype, -ICPSR:-nimsp.candidate.status) |> 
  drop_na(lname, fname)


