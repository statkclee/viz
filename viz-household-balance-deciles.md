# 데이터 과학



## 1. 분위별 가계 소득, 지출 현황 {#nso-balance-data}

소득분배 불평등에 대한 현황을 배율 혹은 백분율 기준으로 정보가 제공되고 있어 전문적으로 
훈련을 받지 않는 일반인 혹은 급하게 현황을 파악해야하는 입장에서는 쉽지 않다. [^household-inequality] [^chosun-inequality]

[^household-inequality]: [서울 소득 상위 20%, 하위 20% 보다 7배 더 벌어](http://news.hankyung.com/article/2017052557597)

[^chosun-inequality]: [2015 가계조사 - 하위 20% 소득 가장 많이 늘어…기초연금 강화 영향](http://biz.chosun.com/site/data/html_dir/2015/12/21/2015122101506.html)

[통계청 KOSIS - 소득10분위별 가구당 가계수지 (전국,2인이상)](http://kosis.kr/statHtml/statHtml.do?orgId=101&tblId=DT_1L9H008&conn_path=I3)를 통해 
소득 10분위별로 가구당 소득 및 지출에 대한 상세한 정보를 얻을 수 있다. 
엑셀이나 `.csv` 파일로 다운로드 받게 되면 빅데이터는 아니지만, 스몰데이터로 실제 엥겔지수 등 통계작업을 이어서 하거나, 시각화 작업이 그다지 녹록하지는 않다.

## 2. 주요 월소득 10 분위별 가계수지 항목별 분석 사전 준비 {#nso-setup}

통계청 소득 10 분위별 가구당 가계수지 데이터는 가구원수와 연령에 대한 정보와 함께 소득, 가계지출에 대한 금전적인 내용이 담겨있다.
데이터를 받아 분석이 가능한 형태로 만들고 반복되는 분석 시각화를 위해 시각화 함수를 별도로 작성한다.


~~~{.r}
# 0. 환경설정 -----------------------------------

# library(tidyverse)
# library(readxl)
# library(xts)
# library(stringr)

# 1. 데이터 가져오기 -----------------------------------
hh_dat <- read_excel("data/소득10분위별__가구당_가계수지__전국_2인이상__20170725133926.xlsx", sheet="데이터", skip=1)

names(hh_dat) <- c("월소득10분위별", "가계수지항목", "가구_03", "가구_04", "가구_05", "가구_06", "가구_07", "가구_08", 
                   "가구_09", "가구_10", "가구_11", "가구_12", "가구_13", "가구_14", "가구_15", "가구_16")

# 2. 데이터 정제 -----------------------------------
hh_df <- hh_dat %>% mutate(월소득10분위별 = zoo::na.locf(월소득10분위별, fromLast=FALSE)) %>% 
    mutate(월소득10분위별=factor(월소득10분위별, levels=c("１분위", "２분위", "３분위", "４분위", "５분위", "전체  평균",
                                              "６분위", "７분위", "８분위", "９분위", "１０분위")))
~~~


## 3. 주요 월소득 10 분위별 주요 지표 비교 {.tabset .tabset-fade .tabset-pills} {#major-index}


~~~{.r}
## 2.0. 시각화 함수 --------------------------------
decile_colors <- c("#ffee00", "#00e5ff", "#0026ff", "#ff0000")

draw_hh_graph <- function(var_name, var_rename) {
    
    graph_hh_df <- hh_df %>% filter(가계수지항목 == var_name) %>% 
        select(-가계수지항목) %>% 
        gather(연도, var_rename, -월소득10분위별) %>% 
        mutate(연도 = str_replace_all(연도, "가구_", "")) %>% 
        mutate(연도 = lubridate::ymd(paste0("20", 연도, "-01-01")))
    
    graph_hh_df %>% 
        dplyr::filter(월소득10분위별 %in% c("１분위", "５분위", "전체  평균", "１０분위")) %>% 
        ggplot(aes(x=연도, y=var_rename, color=월소득10분위별)) +
        theme_bw(base_family="NanumGothic") +
        labs(x="", y=var_name, title=paste0("주요 월소득10분위별 ", var_rename)) +
        geom_line(alpha=0.3, size=1.5) +
        geom_point(alpha=0.7, size=2.5) +
        scale_color_manual(values= decile_colors)
}
~~~

### 3.1. 가구원수(명) {#major-number-of-household}


~~~{.r}
draw_hh_graph("가구원수 (명)", "가구원수")
~~~

<img src="fig/household-number-1.png" style="display: block; margin: auto;" />

### 3.2. 가구주 평균연령(세) {#major-household-age}


~~~{.r}
draw_hh_graph("가구주연령 (세)", "가구주 연령")
~~~

<img src="fig/household-age-1.png" style="display: block; margin: auto;" />

### 3.3. 평균 소비성향(%) {#major-propenstiy-to-consume}


~~~{.r}
draw_hh_graph("평균소비성향 (%)", "평균소비성향")
~~~

<img src="fig/household-propensity-1.png" style="display: block; margin: auto;" />

### 3.4. 가계 흑자율(%) {#major-surplus}


~~~{.r}
draw_hh_graph("흑자율 (%)", "가계 흑자율")
~~~

<img src="fig/household-surplus-1.png" style="display: block; margin: auto;" />

## 4. 주요 월소득 10 분위별 가계소득, 소비지출 {.tabset .tabset-fade .tabset-pills} {#deciles-tab}


~~~{.r}
draw_hh_money_graph <- function(var_name, var_rename) {
    
    graph_hh_df <- hh_df %>% filter(가계수지항목 == var_name) %>% 
        select(-가계수지항목) %>% 
        gather(연도, var_rename, -월소득10분위별) %>% 
        mutate(연도 = str_replace_all(연도, "가구_", "")) %>% 
        mutate(연도 = lubridate::ymd(paste0("20", 연도, "-01-01")))
    
    graph_hh_df %>% 
        dplyr::filter(월소득10분위별 %in% c("１분위", "５분위", "전체  평균", "１０분위")) %>% 
        ggplot(aes(x=연도, y=var_rename, color=월소득10분위별)) +
        theme_bw(base_family="NanumGothic") +
        labs(x="", y=var_name, title=paste0("주요 월소득10분위별 ", var_rename)) +
        geom_line(alpha=0.3, size=1.5) +
        geom_point(alpha=0.7, size=2.5) +
        scale_color_manual(values= decile_colors) +
        scale_y_continuous(labels = scales::comma)
}
~~~


### 4.1. 가구소득 {#deciles-income}


~~~{.r}
draw_hh_money_graph("소득 (원)", "가구소득")
~~~

<img src="fig/household-income-1.png" style="display: block; margin: auto;" />

### 4.2. 가구 소비지출 {#deciles-expenditure}


~~~{.r}
draw_hh_money_graph("소비지출 (원)", "가구소비지출")
~~~

<img src="fig/household-expenditure-1.png" style="display: block; margin: auto;" />

### 4.3. 교육비 지출 {#deciles-education}


~~~{.r}
draw_hh_money_graph("10.교육 (원)", "교육비 지출")
~~~

<img src="fig/household-edu-1.png" style="display: block; margin: auto;" />

### 4.4. 보건의료지출 {#deciles-healthcare}


~~~{.r}
draw_hh_money_graph("06.보건 (원)", "보건의료지출")
~~~

<img src="fig/household-healthcare-1.png" style="display: block; margin: auto;" />

## 5. 엥겔지수 {#engel-coefficient}

엥겔지수(Engel Coefficient)는 소득이 높아도 식비는 다른 지출에 비해 크게 오르지 않는다는 것을 전제로 한 개념으로, 
보통 $\frac{\text{식비}}{\text{총소득}}$ 이나 $\frac{\text{식비}}{\text{총지출비}}$ 등으로 표현하는데, 가장 많이 정의되는 $\frac{\text{식비}}{\text{총소득}}$을 바탕으로 분위별 엥겔지수를 환산하여 애니메이션으로 제작한다. [^engel-namu]

대한민국 1분위(하위20%)는 2010년, 2011년 나무위키 자료에 의하면 22% 대를 유지하고 있다.

[^engel-namu]: [나무위키 - 엥겔 계수](https://namu.wiki/w/%EC%97%A5%EA%B2%94%20%EA%B3%84%EC%88%98)


~~~{.r}
# 3. 엥겔지수 -----------------------------------

exp_hh_df <- hh_df %>% filter(가계수지항목 %in% c("가계지출 (원)")) %>% 
    select(-가계수지항목) %>% 
    gather(연도, 지출총액, -월소득10분위별)

food_hh_df <- hh_df %>% filter(가계수지항목 %in% c("01.식료품 · 비주류음료 (원)")) %>% 
    select(-가계수지항목) %>% 
    gather(연도, 식료품비, -월소득10분위별)

engel_hh_df <- inner_join(exp_hh_df, food_hh_df) %>% 
    mutate(엥겔지수 = 식료품비/지출총액) %>% 
    filter(월소득10분위별 %in% c("전체  평균", "１분위", "５분위", "１０분위")) %>% 
    mutate(연도 = str_replace_all(연도, "가구_", "")) %>% 
    mutate(연도 = lubridate::ymd(paste0("20", 연도, "-01-01")))

engel_hh_df <- engel_hh_df %>% 
    mutate(year = lubridate::year(연도))

engel_hh_df %>% select(-year) %>% 
    DT::datatable() %>% 
      DT::formatRound(c("지출총액", "식료품비"), digits=0) %>% 
      DT::formatRound(c("엥겔지수"), digits=2)
~~~

<!--html_preserve--><div id="htmlwidget-88c31c226866e257ecd5" style="width:100%;height:auto;" class="datatables html-widget"></div>
<script type="application/json" data-for="htmlwidget-88c31c226866e257ecd5">{"x":{"filter":"none","data":[["1","2","3","4","5","6","7","8","9","10","11","12","13","14","15","16","17","18","19","20","21","22","23","24","25","26","27","28","29","30","31","32","33","34","35","36","37","38","39","40","41","42","43","44","45","46","47","48","49","50","51","52","53","54","55","56"],["전체  평균","１분위","５분위","１０분위","전체  평균","１분위","５분위","１０분위","전체  평균","１분위","５분위","１０분위","전체  평균","１분위","５분위","１０분위","전체  평균","１분위","５분위","１０분위","전체  평균","１분위","５분위","１０분위","전체  평균","１분위","５분위","１０분위","전체  평균","１분위","５분위","１０분위","전체  평균","１분위","５분위","１０분위","전체  평균","１분위","５분위","１０분위","전체  평균","１분위","５분위","１０분위","전체  평균","１분위","５분위","１０분위","전체  평균","１분위","５분위","１０분위","전체  평균","１분위","５분위","１０분위"],["2003-01-01","2003-01-01","2003-01-01","2003-01-01","2004-01-01","2004-01-01","2004-01-01","2004-01-01","2005-01-01","2005-01-01","2005-01-01","2005-01-01","2006-01-01","2006-01-01","2006-01-01","2006-01-01","2007-01-01","2007-01-01","2007-01-01","2007-01-01","2008-01-01","2008-01-01","2008-01-01","2008-01-01","2009-01-01","2009-01-01","2009-01-01","2009-01-01","2010-01-01","2010-01-01","2010-01-01","2010-01-01","2011-01-01","2011-01-01","2011-01-01","2011-01-01","2012-01-01","2012-01-01","2012-01-01","2012-01-01","2013-01-01","2013-01-01","2013-01-01","2013-01-01","2014-01-01","2014-01-01","2014-01-01","2014-01-01","2015-01-01","2015-01-01","2015-01-01","2015-01-01","2016-01-01","2016-01-01","2016-01-01","2016-01-01"],[2146888,910709,1908225,3991034,2276918,969367,2028616,4363371,2366421,965635,2085719,4585279,2475298,966494,2146824,4810833,2583685,986234,2217489,5167489,2718046,1075797,2410587,5356168,2775560,1098626,2428024,5389219,2960892,1154033,2648793,5684606,3114946,1184955,2862891,5773502,3216894,1226037,2851265,6071780,3261590,1221863,2823133,6177993,3355512,1204857,3026440,6420989,3373302,1205074,2992756,6468765,3361396,1264499,3032204,6580774],[255079,177551,236834,351990,270666,190903,256455,379898,273402,188779,265313,378430,275689,184424,260152,377295,277711,190737,267422,389223,298296,211343,295467,404559,297652,202724,296662,405528,316936,221474,305194,440035,339371,235407,336425,458004,349190,246747,337467,467704,348240,239697,329661,460581,351183,239021,347473,464798,354023,234915,339673,458668,349418,234569,347184,465723],[0.118813370795309,0.194959092311595,0.124112198509086,0.088195189517303,0.11887384613763,0.196935732287152,0.126418701222903,0.0870652529890307,0.115533964581957,0.19549726345876,0.127204575496507,0.0825315100782308,0.111376084818878,0.190817532235068,0.121179938364766,0.0784261270345489,0.107486400238419,0.193399335249038,0.120596765079782,0.0753214956045383,0.10974648699838,0.196452490572106,0.122570560614489,0.075531424705125,0.107240340688005,0.184525033997011,0.122182482545477,0.0752480090343332,0.107040716108524,0.191913056212431,0.115220026630998,0.0774081792124204,0.108949240211548,0.198663240376217,0.11751233281323,0.0793286293137164,0.108548805151802,0.201255753292927,0.118356939814433,0.0770291413720523,0.106770010945582,0.196173384413801,0.116771331708425,0.0745518811691758,0.104658543912226,0.198381218684043,0.114812452914976,0.07238729111668,0.104948504462393,0.194938236158111,0.113498394122341,0.0709050336501635,0.103950263521465,0.185503507713332,0.114498892554723,0.0707702467825213]],"container":"<table class=\"display\">\n  <thead>\n    <tr>\n      <th> <\/th>\n      <th>월소득10분위별<\/th>\n      <th>연도<\/th>\n      <th>지출총액<\/th>\n      <th>식료품비<\/th>\n      <th>엥겔지수<\/th>\n    <\/tr>\n  <\/thead>\n<\/table>","options":{"crosstalkOptions":{"key":null,"group":null},"columnDefs":[{"className":"dt-right","targets":[3,4,5]},{"orderable":false,"targets":0}],"order":[],"autoWidth":false,"orderClasses":false,"rowCallback":"function(row, data) {\nDTWidget.formatRound(this, row, data, 5, 2, 3, ',', '.');\nDTWidget.formatRound(this, row, data, 3, 0, 3, ',', '.');\nDTWidget.formatRound(this, row, data, 4, 0, 3, ',', '.');\n}"},"selection":{"mode":"multiple","selected":null,"target":"row"}},"evals":["options.rowCallback"],"jsHooks":[]}</script><!--/html_preserve-->

~~~{.r}
# 4. 시각화 -----------------------------------

decile_colors <- c("#ffee00", "#00e5ff", "#0026ff", "#ff0000")

engel_hh_gg <- ggplot(engel_hh_df, aes(x=연도, y=엥겔지수, color=월소득10분위별, cumulative = TRUE)) +
    geom_line(alpha=0.7, size=1) +
    geom_point(aes(x=연도, y=엥겔지수, frame=year), alpha=0.7, size=3) +
    theme_tufte(base_family="NanumGothic") + 
    labs(x="", y="엥겔지수", title="월소득 주요 분위수별 엥겔지수") +
    scale_color_manual(name="분위별", values=decile_colors)

# gganimate::gganimate(engel_hh_gg, "fig/engel_coefficient.gif")
~~~

<img src="fig/engel_coefficient.gif" alt="엥겔지수 애니메이션"/> 


## 6. 분위별 가계소득과 지출 흐름(Sankey) {.tabset .tabset-fade .tabset-pills} {#sankey}

주요 분위별 가계소득이 어떻게 들어오고, 소득을 어느 분야에 사용하는지와 함께 흑자와 적자에 대한 
내용을 생키도해(Sankey Diagram)을 통해 시각화한다. 이를 위해 `networkD3` 팩키지 
`sankeyNetwork()` 함수를 사용하고 분위별 가계소득, 소비지출 데이터를 적절한 형태로 가공한다.



~~~{.r}
library(networkD3)

# 1. 데이터 가져오기 -----------------------------------
income_dat <- read_excel("data/소득10분위별__가구당_가계수지__전국_2인이상__20170725133926.xlsx", sheet="01_소득", col_names=TRUE)
expense_dat <- read_excel("data/소득10분위별__가구당_가계수지__전국_2인이상__20170725133926.xlsx", sheet="02_지출", col_names=TRUE)

hh_dat <- bind_rows(income_dat, expense_dat)

names(hh_dat) <- c("분위별", "from", "to",  "2003", "2004", "2005", "2006", "2007", "2008", "2009", "2010", "2011", "2012", "2013", "2014", "2015", "2016")


draw_sankey <- function(hh_year, hh_decile) {
    
    # 데이터 정제작업
    hh_flow <- hh_dat %>% filter(분위별==hh_decile) %>% 
        select(from, to, 소득지출 = hh_year) %>% 
        mutate(소득지출 = ifelse(from=="소득" & to=="가계지출", 소득지출/2, 소득지출))
    
    # 생키그림 -----------------------------------
    
    hh_node <- unique(c(hh_flow$from, hh_flow$to))
    hh_node_df <- data.frame(id=c(0:(length(hh_node)-1)), name=hh_node)
    
    lut <- c("소득" = 0, "경상소득" = 1,"근로소득" = 2,"사업소득" =3,"재산소득" =4,"이전소득" =5,"비경상소득"=6,"가계지출" =7,
             "소비지출"=8, "01.식료품·비주류음료"=9, "02.주류·담배"=10, "03.의류·신발"=11, "04.주거·수도·광열"=12, 
             "05.가정용품·가사서비스"=13, "06.보건"=14, "07.교통"=15, "08.통신"=16, "09.오락·문화"=17, "10.교육"=18, 
             "11.음식·숙박"=19, "12.기타상품·서비스"=20, "비소비지출"=21) 
    
    hh_flow$from <-  lut[hh_flow$from]
    hh_flow$to <-  lut[hh_flow$to]
    
    sankeyNetwork(Links = hh_flow, Nodes = hh_node_df, Source = 'from',
                  Target = 'to', Value = '소득지출', NodeID = 'name',
                  width = 700, fontSize = 12, nodeWidth = 30, fontFamily="NanumGothic")
}
~~~

### 가계 소득과 소비지출 흐름 - 2003 {#sankey-2003}


~~~{.r}
# 2. 생키도해 시각화 -------
draw_sankey("2003", "１분위")
~~~

<!--html_preserve--><div id="htmlwidget-8fba9ef22d7edb96a1d2" style="width:700px;height:672px;" class="sankeyNetwork html-widget"></div>
<script type="application/json" data-for="htmlwidget-8fba9ef22d7edb96a1d2">{"x":{"links":{"source":[0,1,2,3,4,5,6,0,7,8,8,8,8,8,8,8,8,8,8,8,8,7],"target":[7,0,1,1,1,1,0,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21],"value":[319478.5,593159,239917,131457,11834,209952,45798,455354.5,771383,177551,16340,36644,124295,22821,77935,66222,56380,33162,40817,71845,47371,139325]},"nodes":{"name":["소득","경상소득","근로소득","사업소득","재산소득","이전소득","비경상소득","가계지출","소비지출","01.식료품·비주류음료","02.주류·담배","03.의류·신발","04.주거·수도·광열","05.가정용품·가사서비스","06.보건","07.교통","08.통신","09.오락·문화","10.교육","11.음식·숙박","12.기타상품·서비스","비소비지출"],"group":["소득","경상소득","근로소득","사업소득","재산소득","이전소득","비경상소득","가계지출","소비지출","01.식료품·비주류음료","02.주류·담배","03.의류·신발","04.주거·수도·광열","05.가정용품·가사서비스","06.보건","07.교통","08.통신","09.오락·문화","10.교육","11.음식·숙박","12.기타상품·서비스","비소비지출"]},"options":{"NodeID":"name","NodeGroup":"name","LinkGroup":null,"colourScale":"d3.scaleOrdinal(d3.schemeCategory20);","fontSize":12,"fontFamily":"NanumGothic","nodeWidth":30,"nodePadding":10,"units":"","margin":{"top":null,"right":null,"bottom":null,"left":null},"iterations":32,"sinksRight":true}},"evals":[],"jsHooks":[]}</script><!--/html_preserve-->

~~~{.r}
draw_sankey("2003", "전체평균")
~~~

<!--html_preserve--><div id="htmlwidget-c60c2b8d12be93c90d6c" style="width:700px;height:672px;" class="sankeyNetwork html-widget"></div>
<script type="application/json" data-for="htmlwidget-c60c2b8d12be93c90d6c">{"x":{"links":{"source":[0,1,2,3,4,5,6,0,7,8,8,8,8,8,8,8,8,8,8,8,8,7],"target":[7,0,1,1,1,1,0,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21],"value":[1315284,2534237,1661701,681341,17351,173845,96331,1073444,1700015,255079,23012,112646,168153,58937,98102,198399,125530,99522,187298,233853,139484,446873]},"nodes":{"name":["소득","경상소득","근로소득","사업소득","재산소득","이전소득","비경상소득","가계지출","소비지출","01.식료품·비주류음료","02.주류·담배","03.의류·신발","04.주거·수도·광열","05.가정용품·가사서비스","06.보건","07.교통","08.통신","09.오락·문화","10.교육","11.음식·숙박","12.기타상품·서비스","비소비지출"],"group":["소득","경상소득","근로소득","사업소득","재산소득","이전소득","비경상소득","가계지출","소비지출","01.식료품·비주류음료","02.주류·담배","03.의류·신발","04.주거·수도·광열","05.가정용품·가사서비스","06.보건","07.교통","08.통신","09.오락·문화","10.교육","11.음식·숙박","12.기타상품·서비스","비소비지출"]},"options":{"NodeID":"name","NodeGroup":"name","LinkGroup":null,"colourScale":"d3.scaleOrdinal(d3.schemeCategory20);","fontSize":12,"fontFamily":"NanumGothic","nodeWidth":30,"nodePadding":10,"units":"","margin":{"top":null,"right":null,"bottom":null,"left":null},"iterations":32,"sinksRight":true}},"evals":[],"jsHooks":[]}</script><!--/html_preserve-->

~~~{.r}
draw_sankey("2003", "１０분위")
~~~

<!--html_preserve--><div id="htmlwidget-8747d9b1d6018cbbe473" style="width:700px;height:672px;" class="sankeyNetwork html-widget"></div>
<script type="application/json" data-for="htmlwidget-8747d9b1d6018cbbe473">{"x":{"links":{"source":[0,1,2,3,4,5,6,0,7,8,8,8,8,8,8,8,8,8,8,8,8,7],"target":[7,0,1,1,1,1,0,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21],"value":[2904822,5543207,4007508,1254554,50257,230888,266437,1995517,2939906,351990,24489,235317,239204,124085,157933,399675,175335,206359,360150,397399,267971,1051128]},"nodes":{"name":["소득","경상소득","근로소득","사업소득","재산소득","이전소득","비경상소득","가계지출","소비지출","01.식료품·비주류음료","02.주류·담배","03.의류·신발","04.주거·수도·광열","05.가정용품·가사서비스","06.보건","07.교통","08.통신","09.오락·문화","10.교육","11.음식·숙박","12.기타상품·서비스","비소비지출"],"group":["소득","경상소득","근로소득","사업소득","재산소득","이전소득","비경상소득","가계지출","소비지출","01.식료품·비주류음료","02.주류·담배","03.의류·신발","04.주거·수도·광열","05.가정용품·가사서비스","06.보건","07.교통","08.통신","09.오락·문화","10.교육","11.음식·숙박","12.기타상품·서비스","비소비지출"]},"options":{"NodeID":"name","NodeGroup":"name","LinkGroup":null,"colourScale":"d3.scaleOrdinal(d3.schemeCategory20);","fontSize":12,"fontFamily":"NanumGothic","nodeWidth":30,"nodePadding":10,"units":"","margin":{"top":null,"right":null,"bottom":null,"left":null},"iterations":32,"sinksRight":true}},"evals":[],"jsHooks":[]}</script><!--/html_preserve-->

### 가계 소득과 소비지출 흐름 - 2016 {#sankey-2016}


~~~{.r}
draw_sankey("2016", "１분위")
~~~

<!--html_preserve--><div id="htmlwidget-389a115d217b3fb78112" style="width:700px;height:672px;" class="sankeyNetwork html-widget"></div>
<script type="application/json" data-for="htmlwidget-389a115d217b3fb78112">{"x":{"links":{"source":[0,1,2,3,4,5,6,0,7,8,8,8,8,8,8,8,8,8,8,8,8,7],"target":[7,0,1,1,1,1,0,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21],"value":[491568.5,920734,297080,110799,9350,503505,62402,632249.5,1069771,234569,22003,42306,192483,38193,133773,86706,63974,41793,57975,91359,64637,194727]},"nodes":{"name":["소득","경상소득","근로소득","사업소득","재산소득","이전소득","비경상소득","가계지출","소비지출","01.식료품·비주류음료","02.주류·담배","03.의류·신발","04.주거·수도·광열","05.가정용품·가사서비스","06.보건","07.교통","08.통신","09.오락·문화","10.교육","11.음식·숙박","12.기타상품·서비스","비소비지출"],"group":["소득","경상소득","근로소득","사업소득","재산소득","이전소득","비경상소득","가계지출","소비지출","01.식료품·비주류음료","02.주류·담배","03.의류·신발","04.주거·수도·광열","05.가정용품·가사서비스","06.보건","07.교통","08.통신","09.오락·문화","10.교육","11.음식·숙박","12.기타상품·서비스","비소비지출"]},"options":{"NodeID":"name","NodeGroup":"name","LinkGroup":null,"colourScale":"d3.scaleOrdinal(d3.schemeCategory20);","fontSize":12,"fontFamily":"NanumGothic","nodeWidth":30,"nodePadding":10,"units":"","margin":{"top":null,"right":null,"bottom":null,"left":null},"iterations":32,"sinksRight":true}},"evals":[],"jsHooks":[]}</script><!--/html_preserve-->

~~~{.r}
draw_sankey("2016", "전체평균")
~~~

<!--html_preserve--><div id="htmlwidget-4ac3e9a1dd261ef527e0" style="width:700px;height:672px;" class="sankeyNetwork html-widget"></div>
<script type="application/json" data-for="htmlwidget-4ac3e9a1dd261ef527e0">{"x":{"links":{"source":[0,1,2,3,4,5,6,0,7,8,8,8,8,8,8,8,8,8,8,8,8,7],"target":[7,0,1,1,1,1,0,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21],"value":[2199595,4269843,2948143,858727,16084,446888,129347,1680698,2549731,349418,34826,157964,273082,109242,177211,307652,144001,149652,282124,344359,220201,811665]},"nodes":{"name":["소득","경상소득","근로소득","사업소득","재산소득","이전소득","비경상소득","가계지출","소비지출","01.식료품·비주류음료","02.주류·담배","03.의류·신발","04.주거·수도·광열","05.가정용품·가사서비스","06.보건","07.교통","08.통신","09.오락·문화","10.교육","11.음식·숙박","12.기타상품·서비스","비소비지출"],"group":["소득","경상소득","근로소득","사업소득","재산소득","이전소득","비경상소득","가계지출","소비지출","01.식료품·비주류음료","02.주류·담배","03.의류·신발","04.주거·수도·광열","05.가정용품·가사서비스","06.보건","07.교통","08.통신","09.오락·문화","10.교육","11.음식·숙박","12.기타상품·서비스","비소비지출"]},"options":{"NodeID":"name","NodeGroup":"name","LinkGroup":null,"colourScale":"d3.scaleOrdinal(d3.schemeCategory20);","fontSize":12,"fontFamily":"NanumGothic","nodeWidth":30,"nodePadding":10,"units":"","margin":{"top":null,"right":null,"bottom":null,"left":null},"iterations":32,"sinksRight":true}},"evals":[],"jsHooks":[]}</script><!--/html_preserve-->

~~~{.r}
draw_sankey("2016", "１０분위")
~~~

<!--html_preserve--><div id="htmlwidget-0fc807820c1feb23399f" style="width:700px;height:672px;" class="sankeyNetwork html-widget"></div>
<script type="application/json" data-for="htmlwidget-0fc807820c1feb23399f">{"x":{"links":{"source":[0,1,2,3,4,5,6,0,7,8,8,8,8,8,8,8,8,8,8,8,8,7],"target":[7,0,1,1,1,1,0,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21],"value":[4975361.5,9548896,7439169,1615695,30350,463681,401827,3290387,4492014,465723,39293,300177,366066,226236,262339,672109,190504,322932,605326,604933,436376,2088761]},"nodes":{"name":["소득","경상소득","근로소득","사업소득","재산소득","이전소득","비경상소득","가계지출","소비지출","01.식료품·비주류음료","02.주류·담배","03.의류·신발","04.주거·수도·광열","05.가정용품·가사서비스","06.보건","07.교통","08.통신","09.오락·문화","10.교육","11.음식·숙박","12.기타상품·서비스","비소비지출"],"group":["소득","경상소득","근로소득","사업소득","재산소득","이전소득","비경상소득","가계지출","소비지출","01.식료품·비주류음료","02.주류·담배","03.의류·신발","04.주거·수도·광열","05.가정용품·가사서비스","06.보건","07.교통","08.통신","09.오락·문화","10.교육","11.음식·숙박","12.기타상품·서비스","비소비지출"]},"options":{"NodeID":"name","NodeGroup":"name","LinkGroup":null,"colourScale":"d3.scaleOrdinal(d3.schemeCategory20);","fontSize":12,"fontFamily":"NanumGothic","nodeWidth":30,"nodePadding":10,"units":"","margin":{"top":null,"right":null,"bottom":null,"left":null},"iterations":32,"sinksRight":true}},"evals":[],"jsHooks":[]}</script><!--/html_preserve-->


## 7. 가계 소득과 지출 와플 {#waffle}

[Make waffle (square pie) charts in R](https://github.com/hrbrmstr/waffle) 팩키지를 바탕으로 와플 파이(waffle pie) 그래프를 생성할 수 있다.

### 7.1. 가계소득 {#waffle-income}

가계소득을 대표 분위별로 뽑아서 와플 그래프로 시각화하는 것이 시각적으로 비교하기 좋은 경우가 있다.
가계소득의 원천을 각 분위별로 최하, 평균, 최상을 뽑아 와플 그래프로 시각화한다.



~~~{.r}
# 0. 환경설정 ----------------------------
# install.packages("waffle")
library(waffle)

# 3. 소득 데이터----------------------------
## 3.1. 1 분위 소득 -------------------------------
hh_16_df <- hh_df %>% select(월소득10분위별,가계수지항목, 가구_16) %>% 
  filter(str_detect(월소득10분위별, "１분위")) %>% 
  filter(str_detect(가계수지항목, "소득")) %>% 
  filter(!str_detect(가계수지항목, "처분가능소득")) %>% 
  mutate(가계수지항목 = str_replace_all(가계수지항목, "\\s|\\(원\\)", "")) %>% 
  filter(!가계수지항목 %in% c("소득", "경상소득")) %>% 
  mutate(가구_16 = 가구_16/10^4)

hh_items_v <- hh_16_df %>% 
  select(가계수지항목) %>% unlist
hh_value_v <- hh_16_df %>% 
  select(가구_16) %>% unlist  

names(hh_value_v) <- hh_items_v

hh_income_1 <- waffle(hh_value_v * 5, rows=10, legend_pos = "right", xlab="정사각형 1개 = 0.2만원",
                      title="1분위 가구당 가계수지 - 소득 (2016)")

## 3.2. 평균 소득 -------------------------------
hh_16_df <- hh_df %>% select(월소득10분위별,가계수지항목, 가구_16) %>% 
  filter(str_detect(월소득10분위별, "전체  평균")) %>% 
  filter(str_detect(가계수지항목, "소득")) %>% 
  filter(!str_detect(가계수지항목, "처분가능소득")) %>% 
  mutate(가계수지항목 = str_replace_all(가계수지항목, "\\s|\\(원\\)", "")) %>% 
  filter(!가계수지항목 %in% c("소득", "경상소득")) %>% 
  mutate(가구_16 = 가구_16/10^4)

hh_items_v <- hh_16_df %>% 
  select(가계수지항목) %>% unlist
hh_value_v <- hh_16_df %>% 
  select(가구_16) %>% unlist  

names(hh_value_v) <- hh_items_v

hh_income_mean <- waffle(hh_value_v, rows=10, legend_pos = "right", xlab="정사각형 1개 = 1만원",
  title="평균 가구당 가계수지 - 소득 (2016)")

## 3.3. 10 분위 소득 -------------------------------

hh_16_df <- hh_df %>% select(월소득10분위별,가계수지항목, 가구_16) %>% 
  filter(str_detect(월소득10분위별, "１０분위")) %>% 
  filter(str_detect(가계수지항목, "소득")) %>% 
  filter(!str_detect(가계수지항목, "처분가능소득")) %>% 
  mutate(가계수지항목 = str_replace_all(가계수지항목, "\\s|\\(원\\)", "")) %>% 
  filter(!가계수지항목 %in% c("소득", "경상소득")) %>% 
  mutate(가구_16 = 가구_16/10^4)

hh_items_v <- hh_16_df %>% 
  select(가계수지항목) %>% unlist
hh_value_v <- hh_16_df %>% 
  select(가구_16) %>% unlist  

names(hh_value_v) <- hh_items_v

hh_income_10 <- waffle(hh_value_v/2, rows=10, legend_pos = "right", xlab="정사각형 1개 = 2만원",
         title="10분위 가구당 가계수지 - 소득 (2016)")

## 3.4. 결합
iron(hh_income_1, hh_income_mean, hh_income_10)
~~~

<img src="fig/waffle-income-1.png" style="display: block; margin: auto;" />

### 7.2. 가계지출 {#waffle-expenditure}

가계지출을 대표 분위별로 뽑아서 가장 영향력 높은 13개 항목 중 지출비중이 높은 8개를 추린다.
가계지출의 원천을 각 분위별로 최하, 평균, 최상을 뽑아 와플 그래프로 시각화한다.


~~~{.r}
# 4. 지출 데이터----------------------------
## 4.1. 1 분위 소득 -------------------------------
hh_exp_16_df <- hh_df %>% select(월소득10분위별,가계수지항목, 가구_16) %>% 
  filter(str_detect(월소득10분위별, "１분위")) %>% 
  filter(str_detect(가계수지항목, "^[0-9]|비소비지출")) %>% 
  filter(!str_detect(가계수지항목, "11.음식|12.기타상품|03.의류|05.가정용품|07.교통")) %>% 
  mutate(가계수지항목 = str_replace_all(가계수지항목, "\\s|\\(원\\)", "")) %>% 
  mutate(가구_16 = 가구_16/10^4)

hh_exp_items_v <- hh_exp_16_df %>% 
  select(가계수지항목) %>% unlist
hh_exp_value_v <- hh_exp_16_df %>% 
  select(가구_16) %>% unlist  

names(hh_exp_value_v) <- hh_exp_items_v

hh_exp_1 <- waffle(hh_exp_value_v * 2, rows=10, legend_pos = "right", xlab="정사각형 1개 = 0.5만원",
                      title="1분위 가계 - 지출 (2016)")

## 4.2. 평균 지출 -------------------------------
hh_exp_16_df <- hh_df %>% select(월소득10분위별,가계수지항목, 가구_16) %>% 
  filter(str_detect(월소득10분위별, "전체  평균")) %>% 
  filter(str_detect(가계수지항목, "^[0-9]|비소비지출")) %>% 
  filter(!str_detect(가계수지항목, "11.음식|12.기타상품|03.의류|05.가정용품|07.교통")) %>% 
  mutate(가계수지항목 = str_replace_all(가계수지항목, "\\s|\\(원\\)", "")) %>% 
  mutate(가구_16 = 가구_16/10^4)

hh_exp_items_v <- hh_exp_16_df %>% 
  select(가계수지항목) %>% unlist
hh_exp_value_v <- hh_exp_16_df %>% 
  select(가구_16) %>% unlist  

names(hh_exp_value_v) <- hh_exp_items_v

hh_exp_mean <- waffle(hh_exp_value_v, rows=10, legend_pos = "right", xlab="정사각형 1개 = 1만원",
       title="평균 가계 - 지출 (2016)")

## 4.3. 1 분위 지출 -------------------------------
hh_exp_16_df <- hh_df %>% select(월소득10분위별,가계수지항목, 가구_16) %>% 
  filter(str_detect(월소득10분위별, "１０분위")) %>% 
  filter(str_detect(가계수지항목, "^[0-9]|비소비지출")) %>% 
  filter(!str_detect(가계수지항목, "11.음식|12.기타상품|03.의류|05.가정용품|07.교통")) %>% 
  mutate(가계수지항목 = str_replace_all(가계수지항목, "\\s|\\(원\\)", "")) %>% 
  mutate(가구_16 = 가구_16/10^4)

hh_exp_items_v <- hh_exp_16_df %>% 
  select(가계수지항목) %>% unlist
hh_exp_value_v <- hh_exp_16_df %>% 
  select(가구_16) %>% unlist  

names(hh_exp_value_v) <- hh_exp_items_v

hh_exp_10 <- waffle(hh_exp_value_v/2, rows=10, legend_pos = "right", xlab="정사각형 1개 = 2만원",
       title="10분위 가계 - 지출 (2016)")

## 4.4. 결합
iron(hh_exp_1, hh_exp_mean, hh_exp_10)
~~~

<img src="fig/waffle-expenditure-1.png" style="display: block; margin: auto;" />
