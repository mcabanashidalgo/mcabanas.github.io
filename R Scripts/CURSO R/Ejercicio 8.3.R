
library(tidyverse)
censals <- read.csv("CENSALS_CAT.csv",
                    sep = "\t",
                    fileEncoding = "latin1")

censals <- as.tibble(censals)
cens <- select(censals, Comarca, pob.2000,pob.2001,pob.2002,	pob.2003	,pob.2004	,pob.2005	,pob.2006	,pob.2007	,pob.2008	,pob.2009)

pob <- pob.com%>%gather(key=ANY, value = POBLACIO,-Comarca)


pob.com <- cens%>%group_by(Comarca)%>%
  dplyr::summarise(p0=sum(pob.2000),
                   p01=sum(pob.2001), 
                   p02=sum(pob.2002),
                   p03=sum(pob.2003),
                   p04=sum(pob.2004), 
                   p05=sum(pob.2005),
                   p06=sum(pob.2006),
                   p07=sum(pob.2007), 
                   p08=sum(pob.2008),
                   p09=sum(pob.2009))

                

ggplot(pob, aes(y=POBLACIO, x=ANY, group=5))+
  geom_line()+
  facet_wrap(~Comarca)

##2. Evolució del cens electoral per a las comarque de Catalunya entre 1999 i 2009
electoral <- select(censals, Comarca, cens.2000,	cens.2001,	cens.2002,	cens.2003,	cens.2004,	cens.2005,	cens.2006,	cens.2007,	cens.2008,	cens.2009)
el <- electoral%>%group_by(Comarca)%>%
  dplyr::summarise("2000"=sum(cens.2000),
                   "2001"=sum(cens.2001), 
                   "2002"=sum(cens.2002),
                   "2003"=sum(cens.2003),
                   "2004"=sum(cens.2004), 
                   "2005"=sum(cens.2005),
                   "2006"=sum(cens.2006),
                   "2007"=sum(cens.2007), 
                   "2008"=sum(cens.2008),
                   "2009"=sum(cens.2009))
censE <- el%>%gather(key=Any, value = Cens, -Comarca)

ggplot(censE, aes(x=Any, y=Cens, group=1))+
  geom_line()+
  facet_wrap(~Comarca)+
  scale_y_continuous(breaks = 100000, 300000, 50000)

## 3. Evolució del percentatge de població immigrant, sobre el total de població de les comarques de Catalunya entre 1999 i 2009
