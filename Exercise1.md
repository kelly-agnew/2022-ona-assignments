Exercise1
================

## Imports

``` r
# graph analysis
library(ggraph)
```

    ## Warning: package 'ggraph' was built under R version 4.1.3

    ## Loading required package: ggplot2

``` r
library(igraph)
```

    ## Warning: package 'igraph' was built under R version 4.1.3

    ## 
    ## Attaching package: 'igraph'

    ## The following objects are masked from 'package:stats':
    ## 
    ##     decompose, spectrum

    ## The following object is masked from 'package:base':
    ## 
    ##     union

``` r
# tidy data analysis and visualziation
library(readr)
library(dplyr)
```

    ## 
    ## Attaching package: 'dplyr'

    ## The following objects are masked from 'package:igraph':
    ## 
    ##     as_data_frame, groups, union

    ## The following objects are masked from 'package:stats':
    ## 
    ##     filter, lag

    ## The following objects are masked from 'package:base':
    ## 
    ##     intersect, setdiff, setequal, union

``` r
library(skimr)
```

    ## Warning: package 'skimr' was built under R version 4.1.2

## Read Data

``` r
connections = read.csv("Data\\Connections.csv")
```

## Cleaning

``` r
connections = subset(connections, select=-c(Email.Address))
connections %>% skim() #good to go
```

|                                                  |            |
|:-------------------------------------------------|:-----------|
| Name                                             | Piped data |
| Number of rows                                   | 340        |
| Number of columns                                | 5          |
| \_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_   |            |
| Column type frequency:                           |            |
| character                                        | 5          |
| \_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_ |            |
| Group variables                                  | None       |

Data summary

**Variable type: character**

| skim\_variable | n\_missing | complete\_rate | min | max | empty | n\_unique | whitespace |
|:---------------|-----------:|---------------:|----:|----:|------:|----------:|-----------:|
| First.Name     |          0 |              1 |   0 |  21 |     1 |       302 |          0 |
| Last.Name      |          0 |              1 |   0 |  30 |     1 |       309 |          0 |
| Company        |          0 |              1 |   0 |  95 |     5 |       260 |          0 |
| Position       |          0 |              1 |   0 |  97 |     5 |       313 |          0 |
| Connected.On   |          0 |              1 |  11 |  11 |     0 |       183 |          0 |

## Count of total connections

``` r
connections %>% count()
```

    ##     n
    ## 1 340

## Count of connections by company

``` r
connections %>% 
  count(Company) %>% 
  arrange(-n)
```

    ##                                                                                             Company
    ## 1                                               McGill University - Desautels Faculty of Management
    ## 2                                                                                 McGill University
    ## 3                                                                          Brentwood College School
    ## 4                                                                             Rogers Communications
    ## 5                                                                                        Scotiabank
    ## 6                                                                                                  
    ## 7                                                                                       Air Transat
    ## 8                                                                                   McKesson Canada
    ## 9                                                                                          Novartis
    ## 10                                                                                             Bell
    ## 11                                                                          Keurig Dr Pepper Canada
    ## 12                                                                                        Metrolinx
    ## 13                                               RI-MUHC | Research Institute of the MUHC | #rimuhc
    ## 14                                                                                        Accenture
    ## 15                                                                                         Autodesk
    ## 16                                                                                       BOMBARDIER
    ## 17                                                                                       CloudRaker
    ## 18                                                                                            Coveo
    ## 19                                                                                         Deloitte
    ## 20                                                                          DHL eCommerce Solutions
    ## 21                                                                            Keurig Dr Pepper Inc.
    ## 22                                                           McGill University - Faculty of Science
    ## 23                                                                                         McKesson
    ## 24                                                                                              MNP
    ## 25                                                                                              RBC
    ## 26                                                                                          Shopify
    ## 27                                                                           Teck Resources Limited
    ## 28                                                                        University Health Network
    ## 29                                                                                             Vaco
    ## 30                                                                Ã‰cole de technologie supÃ©rieure
    ## 31                                                                Ã‰cole Polytechnique de MontrÃ©al
    ## 32                                                                      Ã‰cole Vision Victoriaville
    ## 33                                                                                           Abbott
    ## 34                                                               Adaptiiv Medical Technologies Inc.
    ## 35                                                     Advanced Solutions, a DXC Technology Company
    ## 36                                                                                        æ¬§èŽ±é›…
    ## 37                                                                                Aird & Berlis LLP
    ## 38                                                                                           Aktana
    ## 39                                                                              Altitude-sports.com
    ## 40                                                                                         Ambature
    ## 41                                                                            Ametros Learning Inc.
    ## 42                                                              Analytica Software and Technologies
    ## 43                                                                                        AngelList
    ## 44                                                                                    Anges QuÃ©bec
    ## 45                                                                      Annabelle Agnew Photography
    ## 46                                                                                            Apple
    ## 47                                                                                           Aquila
    ## 48                                                                                    ArcelorMittal
    ## 49                                                                           Architrave Consultants
    ## 50                                                                                          Aritzia
    ## 51                                                                                   Arshinkooh Co.
    ## 52                                                                      ASA Co. (Vista Samaneh ASA)
    ## 53                                                                   Aslan Project Management Ltd. 
    ## 54                                                                                 Aspire New Media
    ## 55                                                                                 Assembly Digital
    ## 56                                                                                            Astra
    ## 57                                                                                      Atura Power
    ## 58                                                           Baltimore Regional Housing Partnership
    ## 59                                                                                        BC Greens
    ## 60                                                                                             BGIS
    ## 61                                                                                        BlackRock
    ## 62                                                                               BMO Private Wealth
    ## 63                                                                                     Bounced Inc.
    ## 64                                                                               Breville Australia
    ## 65                                                       Bristol & Weston NHS Purchasing Consortium
    ## 66                                                           British Columbia Rapid Transit Company
    ## 67                                                                                              BRP
    ## 68                                                                                    Burgoo Bistro
    ## 69                                                                                        ByteDance
    ## 70                                                                                              CAE
    ## 71                                                                                  Canary Biofuels
    ## 72                                                                                 Cash 4 You Corp.
    ## 73                                                                    Cassels Brock & Blackwell LLP
    ## 74                                                                               Casual Sports Talk
    ## 75                                                                               Cellular Analytics
    ## 76   Centre universitaire de santÃ© McGill et IR-CUSM | McGill University Health Centre and RI-MUHC
    ## 77                                                                                             CIBC
    ## 78                                                                                 City of Edmonton
    ## 79                                                                                         Clik2Pay
    ## 80                                                                                               CN
    ## 81                                                                                     CODE/+/TRUST
    ## 82                                                                                           Cofomo
    ## 83                                                                                            Comet
    ## 84                                                                                           Contra
    ## 85                                                                                 Corum Group Ltd.
    ## 86                                                                                           Criteo
    ## 87                                          CUNY Graduate School of Public Health and Health Policy
    ## 88                                                                                    D2V Analytics
    ## 89                                                                                      Danske Bank
    ## 90                                                                                         Dasos ML
    ## 91                                                                                   Denal Commerce
    ## 92                                                                                    Deutsche Bank
    ## 93                                                                                      DHL Express
    ## 94                                                                                         Dialogue
    ## 95                                                                               District of Tofino
    ## 96                                                                                        Dolon Ltd
    ## 97                                                                                          Dotdash
    ## 98                                                                                              DRW
    ## 99                                                                           Dublin City University
    ## 100                                                                  è¿\231å®¶å…¬å\217¸æ\230¯å\201šç”µå­\220çƒŸçš„
    ## 101                                                                                        EllisDon
    ## 102                                                                                      Enterprise
    ## 103                                                                                      eXp Realty
    ## 104                                                                                              EY
    ## 105                                                              Fairfield Inn & Suites by Marriott
    ## 106                                                            Family Services of Greater Vancouver
    ## 107                                                                                     FCB Chicago
    ## 108                                                                                   FEBE Ventures
    ## 109                                                                       Fenix Consulting - Canada
    ## 110                                                                                  FGL Sports Ltd
    ## 111                                                                            Fidelity Investments
    ## 112 Fondation du Centre universitaire de santÃ© McGill / McGill University Health Centre Foundation
    ## 113                                                                                   Frank And Oak
    ## 114                                                                                    Galerie Youn
    ## 115                                                                         GC Biotherapeutics Inc.
    ## 116                                               Global Affairs Canada | Affaires mondiales Canada
    ## 117                                                         Goji Labs - Software Development Studio
    ## 118                                                                                          Google
    ## 119                                                                                         Gorgias
    ## 120                                                                                        Graduway
    ## 121                                                                     Grant Thornton LLP (Canada)
    ## 122                                                   Greater Victoria Labour Relations Association
    ## 123                                                            Gregg Drilling & Testing Canada Ltd.
    ## 124                                                                            Groupe V3 Stent inc.
    ## 125                                                                                  HÃ©roux-Devtek
    ## 126                                                                                 Hair By Cassidy
    ## 127                                                                         Harvard Business School
    ## 128                                                                                   HEC MontrÃ©al
    ## 129                                                                             Hone Communications
    ## 130                                                                          Hutcheson & Co LLP CPA
    ## 131                                                                                  Hypertec Group
    ## 132                                         Indraprastha Institute of Information Technology, Delhi
    ## 133                                             INS QuÃ©bec - Institut national du sport du QuÃ©bec
    ## 134                                                  Institute for Research on Public Policy (IRPP)
    ## 135                                                                             Insulet Corporation
    ## 136                                                                                     Intelligems
    ## 137                                                       International Development Group LLC (IDG)
    ## 138                                                                                       iPSS inc.
    ## 139                                               IR-CUSM | Institut de recherche du CUSM | #ircusm
    ## 140                                                                             Jimmyâ\200\231s Feed Co. 
    ## 141                                                                           JJM Construction Ltd.
    ## 142                                                                                           KAANI
    ## 143                                                                                  Kabam Montreal
    ## 144                                                                                            Kami
    ## 145                                                                                 KeAi Publishing
    ## 146                                                                                      Korn Ferry
    ## 147                                                                           KPI Digital Solutions
    ## 148                                                                                            KPMG
    ## 149                                                                                     KPMG Canada
    ## 150                                                                                        L'OrÃ©al
    ## 151                                                                      Labatt Breweries of Canada
    ## 152                                                                                 LaSalle College
    ## 153                                                                                           Lemay
    ## 154                                           Leslie Dan Faculty of Pharmacy, University of Toronto
    ## 155                                                                                     Leslie Lab 
    ## 156                                            Liberal Party of Canada â\200¢ Parti LibÃ©ral du Canada
    ## 157                                                                  LMDG Building Code Consultants
    ## 158                                                                               Luxaviation Group
    ## 159                                                              Macdonald Campus Students' Society
    ## 160                                                                           Mackenzie Investments
    ## 161                                                                        McGill Steel Bridge Team
    ## 162                                                                                  MEDVALGO GROUP
    ## 163                                                                                       Microsoft
    ## 164                                                                                         Moneris
    ## 165                                                                            Montebello Packaging
    ## 166                                                                                  Morgan Stanley
    ## 167                                                                            Mosaic-HEC MontrÃ©al
    ## 168                                                                           Newcomer Legal Clinic
    ## 169                                                                                     NHS Tayside
    ## 170                                                                           Norton Rose Fulbright
    ## 171                                                                              Notch Therapeutics
    ## 172                                                                                         Novacap
    ## 173                                                                       Odette School of Business
    ## 174                                                                      Ontario Ministry of Health
    ## 175                                                                   Opal Health Informatics Group
    ## 176                                                           Opal Health Informatics Group l #OHIG
    ## 177                                                                    Osler, Hoskin & Harcourt LLP
    ## 178                                                      Pacific Community Resources Society (PCRS)
    ## 179                                                                                       PagerDuty
    ## 180                                                                            Parliament of Canada
    ## 181                                                                                     Parrish Lab
    ## 182                                                                               Phoenix Australia
    ## 183                                                                                           PIMCO
    ## 184                                                                              Polygon Homes Ltd.
    ## 185                                                                          Pratt & Whitney Canada
    ## 186                                                                         Pretty Prisms Resin Art
    ## 187                              Professional Regulatory Consulting Services, Special Dietary Foods
    ## 188                                                                                     PwC Tunisia
    ## 189                                                                                    RAKABOT Inc.
    ## 190                                                                              Raymond James Ltd.
    ## 191                                                                           RCS Technologies Inc.
    ## 192                                                                                  Recrute Action
    ## 193                                                                    Reliance Jio Infocom Limited
    ## 194                                                                                         Rev.com
    ## 195                                                                                      RSM Canada
    ## 196                                                                                      Salesforce
    ## 197                                                                     School District 22 (Vernon)
    ## 198                                                                                   Self-employed
    ## 199                                                                                    SHAD Network
    ## 200                                                                            SIA Innovations Inc.
    ## 201                                                                                    Sia Partners
    ## 202                                                                                          Slalom
    ## 203                                                  Smith School of Business at Queen's University
    ## 204                                                                                       Snowflake
    ## 205                                                                                     Soulightful
    ## 206                                                                              Spectra Plasmonics
    ## 207                                                                                 Squamish Nation
    ## 208                                                                             Squarepoint Capital
    ## 209                                                                         Standard Chartered Bank
    ## 210                                                                            Stanley Park Brewing
    ## 211                                                         Strategic Development Division | MoA RK
    ## 212                                                                               StrategyCorp Inc.
    ## 213                                                                                  Student Energy
    ## 214                                                                                     Student.com
    ## 215                                                                          Ta San Yuen Restaurant
    ## 216                                                                       Tata Consultancy Services
    ## 217                                                                                    TD Insurance
    ## 218                                                                           Teamrecruiter.com Inc
    ## 219                                                                                     Tebca PerÃº
    ## 220                                                                                      TEKsystems
    ## 221                                                                                           TELUS
    ## 222                                                                                        TENG Inc
    ## 223                                                                       The Cambie Malone's Group
    ## 224                                                                The EstÃ©e Lauder Companies Inc.
    ## 225                                                              The University of British Columbia
    ## 226                                                                     The University of Edinburgh
    ## 227                                                                     The University of Hong Kong
    ## 228                                                                        Thermo Fisher Scientific
    ## 229                                                                                         Tipalti
    ## 230                                                                                   TOCA Football
    ## 231                                                                          Town of Essex, Ontario
    ## 232                                                                   Trafalgar General Trading Co.
    ## 233                                                      Trans Canada Trail | Sentier Transcanadien
    ## 234                                                                                         Transat
    ## 235                                                                  Trek Bicycle Store of Victoria
    ## 236                                                                                 Trend Marketing
    ## 237                                                                                          Twitch
    ## 238                                                                       UBC Third Quadrant Design
    ## 239                                                                               Ubisoft MontrÃ©al
    ## 240                                                                                           Unity
    ## 241                                                               University Consulting Group (UCG)
    ## 242                                                                           University of Calgary
    ## 243                                                                          University of Manitoba
    ## 244                                                                           University of Toronto
    ## 245                                                                          University of Victoria
    ## 246                                                                                VAE Energy Spray
    ## 247                                                                                Vector Institute
    ## 248                                                                                          Verint
    ## 249                                                                                         Verizon
    ## 250                                                                                          Vestas
    ## 251                                                                                         Viatris
    ## 252                                                                                VINN Automotive 
    ## 253                                                                         WhiteStar Tubulars Ltd.
    ## 254                                                                      Wilfrid Laurier University
    ## 255                                                                                 Win Without War
    ## 256                                                                                     WindTech AI
    ## 257                                                                                           Wipro
    ## 258                                                                                 Worldwide media
    ## 259                                                                                  Wyth Financial
    ## 260                                                                                           Zynga
    ##      n
    ## 1   18
    ## 2   10
    ## 3    8
    ## 4    6
    ## 5    6
    ## 6    5
    ## 7    4
    ## 8    4
    ## 9    4
    ## 10   3
    ## 11   3
    ## 12   3
    ## 13   3
    ## 14   2
    ## 15   2
    ## 16   2
    ## 17   2
    ## 18   2
    ## 19   2
    ## 20   2
    ## 21   2
    ## 22   2
    ## 23   2
    ## 24   2
    ## 25   2
    ## 26   2
    ## 27   2
    ## 28   2
    ## 29   2
    ## 30   1
    ## 31   1
    ## 32   1
    ## 33   1
    ## 34   1
    ## 35   1
    ## 36   1
    ## 37   1
    ## 38   1
    ## 39   1
    ## 40   1
    ## 41   1
    ## 42   1
    ## 43   1
    ## 44   1
    ## 45   1
    ## 46   1
    ## 47   1
    ## 48   1
    ## 49   1
    ## 50   1
    ## 51   1
    ## 52   1
    ## 53   1
    ## 54   1
    ## 55   1
    ## 56   1
    ## 57   1
    ## 58   1
    ## 59   1
    ## 60   1
    ## 61   1
    ## 62   1
    ## 63   1
    ## 64   1
    ## 65   1
    ## 66   1
    ## 67   1
    ## 68   1
    ## 69   1
    ## 70   1
    ## 71   1
    ## 72   1
    ## 73   1
    ## 74   1
    ## 75   1
    ## 76   1
    ## 77   1
    ## 78   1
    ## 79   1
    ## 80   1
    ## 81   1
    ## 82   1
    ## 83   1
    ## 84   1
    ## 85   1
    ## 86   1
    ## 87   1
    ## 88   1
    ## 89   1
    ## 90   1
    ## 91   1
    ## 92   1
    ## 93   1
    ## 94   1
    ## 95   1
    ## 96   1
    ## 97   1
    ## 98   1
    ## 99   1
    ## 100  1
    ## 101  1
    ## 102  1
    ## 103  1
    ## 104  1
    ## 105  1
    ## 106  1
    ## 107  1
    ## 108  1
    ## 109  1
    ## 110  1
    ## 111  1
    ## 112  1
    ## 113  1
    ## 114  1
    ## 115  1
    ## 116  1
    ## 117  1
    ## 118  1
    ## 119  1
    ## 120  1
    ## 121  1
    ## 122  1
    ## 123  1
    ## 124  1
    ## 125  1
    ## 126  1
    ## 127  1
    ## 128  1
    ## 129  1
    ## 130  1
    ## 131  1
    ## 132  1
    ## 133  1
    ## 134  1
    ## 135  1
    ## 136  1
    ## 137  1
    ## 138  1
    ## 139  1
    ## 140  1
    ## 141  1
    ## 142  1
    ## 143  1
    ## 144  1
    ## 145  1
    ## 146  1
    ## 147  1
    ## 148  1
    ## 149  1
    ## 150  1
    ## 151  1
    ## 152  1
    ## 153  1
    ## 154  1
    ## 155  1
    ## 156  1
    ## 157  1
    ## 158  1
    ## 159  1
    ## 160  1
    ## 161  1
    ## 162  1
    ## 163  1
    ## 164  1
    ## 165  1
    ## 166  1
    ## 167  1
    ## 168  1
    ## 169  1
    ## 170  1
    ## 171  1
    ## 172  1
    ## 173  1
    ## 174  1
    ## 175  1
    ## 176  1
    ## 177  1
    ## 178  1
    ## 179  1
    ## 180  1
    ## 181  1
    ## 182  1
    ## 183  1
    ## 184  1
    ## 185  1
    ## 186  1
    ## 187  1
    ## 188  1
    ## 189  1
    ## 190  1
    ## 191  1
    ## 192  1
    ## 193  1
    ## 194  1
    ## 195  1
    ## 196  1
    ## 197  1
    ## 198  1
    ## 199  1
    ## 200  1
    ## 201  1
    ## 202  1
    ## 203  1
    ## 204  1
    ## 205  1
    ## 206  1
    ## 207  1
    ## 208  1
    ## 209  1
    ## 210  1
    ## 211  1
    ## 212  1
    ## 213  1
    ## 214  1
    ## 215  1
    ## 216  1
    ## 217  1
    ## 218  1
    ## 219  1
    ## 220  1
    ## 221  1
    ## 222  1
    ## 223  1
    ## 224  1
    ## 225  1
    ## 226  1
    ## 227  1
    ## 228  1
    ## 229  1
    ## 230  1
    ## 231  1
    ## 232  1
    ## 233  1
    ## 234  1
    ## 235  1
    ## 236  1
    ## 237  1
    ## 238  1
    ## 239  1
    ## 240  1
    ## 241  1
    ## 242  1
    ## 243  1
    ## 244  1
    ## 245  1
    ## 246  1
    ## 247  1
    ## 248  1
    ## 249  1
    ## 250  1
    ## 251  1
    ## 252  1
    ## 253  1
    ## 254  1
    ## 255  1
    ## 256  1
    ## 257  1
    ## 258  1
    ## 259  1
    ## 260  1

## Preprocessing for graphing

### First create new dataframe to store all possible pairs of nodes and their companies

``` r
connections$Nodes = paste(connections$First.Name, substr(connections$Last.Name,0,3)) # Node names are first + first three letters of last name (I encountered some duplicates still with only using First name & first letter of last name, using three letters should solve this issue)

graphDF <- merge(connections$Nodes, connections$Nodes, by=NULL) # all possible connections

# R magic incoming:
# We have all possible pairs, but we want to remove duplicated pairs
# Ie, Kelly-->Bob is the same as Bob-->Kelly for our purposes
# To achieve this we do an in-row sorting to align all names in one column, then remove the duplicate rows
graphDF = graphDF[!duplicated(t(apply(graphDF, 1, sort))), ]

graphDF = graphDF %>% rename(person1=x, person2=y) #fix col names

# Get companies for person 1
graphDF = merge(x=graphDF, y=subset(connections, select=c(Company,Nodes)), by.x="person1", by.y="Nodes", all.x=TRUE)
graphDF = graphDF %>% rename(company1=Company) # company for person 1
# drop blanks from merge
graphDF = graphDF[!(is.na(graphDF$person1) | graphDF$person1==" "), ]
graphDF = graphDF[!(is.na(graphDF$company1) | graphDF$company1==""), ]


# Get companies for person 2
graphDF = merge(x=graphDF, y=subset(connections, select=c(Company,Nodes)), by.x="person2", by.y="Nodes", all.x=TRUE)
graphDF = graphDF %>% rename(company2=Company) # company for person 2
# blanks
graphDF = graphDF[!(is.na(graphDF$person2) | graphDF$person2==" "), ]
graphDF = graphDF[!(is.na(graphDF$company2) | graphDF$company2==""), ]
```

### Now we find overlapping companies between columns to establish graph edges

``` r
graphDF = graphDF %>% mutate (
  edge = case_when(
    company1==company2 ~ 1,
    TRUE ~ 0
  )
)
```

### Now select the edges according to edge==1 for our edgelist and remove self-links

``` r
graphDF = graphDF[!(graphDF$edge==0), ]
graphDF = graphDF[!(graphDF$person2==graphDF$person1),] # remove self links
# Drop unneeded columns to generate final edge list
edgeList = subset(graphDF, select=-c(edge, company2))
vertexList = subset(connections, select=c(Nodes, Company))
```

## Visualize graph with igraph

``` r
linkedIn_graph = graph_from_data_frame(d=edgeList,vertices=NULL ,directed=FALSE)
linkedIn_graph
```

    ## IGRAPH c4ca104 UN-- 104 302 -- 
    ## + attr: name (v/c), company1 (e/c)
    ## + edges from c4ca104 (vertex names):
    ##  [1] Aidan Hep        --Brian Car    Aidan Hep        --Kate Cou    
    ##  [3] Aidan Hep        --Jordan War   Aidan Hep        --Neil Rob    
    ##  [5] Aidan Hep        --Blake Gag    Aidan Hep        --Bruce Tat   
    ##  [7] Aishwarya Cho    --Shoeb Hos    Aishwarya Cho    --Arpit Nag   
    ##  [9] Aishwarya Cho    --Siraj Hat    Aishwarya Cho    --Diwei Zhu   
    ## [11] Aishwarya Cho    --Oleg Kar     Alya Gab         --Kathleen Cas
    ## [13] Animesh Ani      --Carlos Fab   Animesh Ani      --John Kil    
    ## [15] Animesh Ani      --Joy Ben      Animesh Ani      --Rosanna Per 
    ## + ... omitted several edges

## Visualize

``` r
ggraph(linkedIn_graph, layout="kk") +
  geom_edge_link(aes(color=factor(company1))) +
  geom_node_text(aes(label=name)) +
  theme(legend.position = "bottom")
```

![](Exercise1_files/figure-gfm/unnamed-chunk-10-1.png)<!-- -->
