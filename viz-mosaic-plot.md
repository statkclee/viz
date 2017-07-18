# 데이터 과학



## 1. 통계청 출생, 사망 통계 

[2016년 출생.사망통계 잠정 결과](http://kostat.go.kr/portal/korea/kor_nw/2/1/index.board?bmode=read&bSeq=&aSeq=359243&pageNo=1&rowNum=10&navCount=10&currPg=&sTarget=title&sTxt)가 
2017년 2월 22일 발표되었다. 
다양한 그래프를 통해 출생 및 사망에 관한 통계 정보가 공개 되었다. 

## 2. 범주형 데이터 시각화

그래프 문법(Grammar of Graphics)을 사용한 모자이크 그래프(https://cran.r-project.org/web/packages/ggmosaic/index.html)를 통해 출생에 관한 비밀을 
시각적으로 풀어보자.

### 2.1. 환경설정

`ggmosaic`이 가장 중심적인 팩키지다. 엑셀파일(`readxl`)을 읽어오고, 데이터와 사투를 벌이는데 사용되는 `tidyverse`, 
그리고 한글 글꼴과 테마 적용을 위한 팩키지를 준비한다.


~~~{.r}
library(tidyverse)
library(ggplot2)
library(ggthemes)
library(readxl)
library(ggmosaic) # devtools::install_github("haleyjeppson/ggmosaic")
library(extrafont)
loadfonts()
~~~

### 2.2. 출생과 사망 추이

가장 먼저 출생과 사망에 대한 추이를 모자이크 그래프를 통해 시각화한다.


~~~{.r}
# 1. 출생사망 ------------------------------
birth_death_df <- read_excel("data/통계청_인구통계_2016.xlsx", sheet="출생사망")

DT::datatable(birth_death_df) %>% 
    DT::formatRound(c(2,3,4), digits=0)
~~~

<!--html_preserve--><div id="htmlwidget-3ffe5af3d2dafc0c9255" style="width:100%;height:auto;" class="datatables html-widget"></div>
<script type="application/json" data-for="htmlwidget-3ffe5af3d2dafc0c9255">{"x":{"filter":"none","data":[["1","2","3","4","5","6","7","8","9","10","11","12","13","14","15","16","17","18","19","20","21","22","23","24","25","26","27","28","29","30","31","32","33","34","35","36"],[1981,1982,1983,1984,1985,1986,1987,1988,1989,1990,1991,1992,1993,1994,1995,1996,1997,1998,1999,2000,2001,2002,2003,2004,2005,2006,2007,2008,2009,2010,2011,2012,2013,2014,2015,2016],[867409,848312,769155,674793,655489,636019,623831,633092,639431,649738,709275,730678,715826,721185,715020,691226,668344,634790,614233,634501,554895,492111,490543,472761,435031,448153,493189,465892,444849,470171,471265,484550,436455,435435,438420,406300],[237481,245767,254563,236445,240418,239256,243504,235779,236818,241616,242270,236162,234257,242439,242838,241149,241943,243193,245364,246163,241521,245317,244506,244217,243883,242266,244874,246113,246942,255405,257396,267221,266257,267692,275895,281000],[629928,602545,514592,438348,415071,396763,380327,397313,402613,408122,467005,494516,481569,478746,472182,450077,426401,391597,368869,388338,313374,246794,246037,228544,191148,205887,248315,219779,197907,214766,213869,217329,170198,167743,162525,125300],[22.4,21.6,19.3,16.7,16.1,15.4,15,15.1,15.1,15.2,16.4,16.7,16,16,15.7,15,14.4,13.6,13,13.3,11.6,10.2,10.2,9.8,8.9,9.2,10,9.4,9,9.4,9.4,9.6,8.6,8.6,8.6,7.9],[6.1,6.2,6.4,5.9,5.9,5.8,5.9,5.6,5.6,5.6,5.6,5.4,5.2,5.4,5.3,5.2,5.2,5.2,5.2,5.2,5,5.1,5.1,5,5,5,5,5,5,5.1,5.1,5.3,5.3,5.3,5.4,5.5],[16.3,15.3,12.9,10.8,10.2,9.6,9.1,9.5,9.5,9.5,10.8,11.3,10.8,10.6,10.3,9.8,9.2,8.4,7.8,8.2,6.5,5.1,5.1,4.7,3.9,4.2,5.1,4.4,4,4.3,4.3,4.3,3.4,3.3,3.2,2.5]],"container":"<table class=\"display\">\n  <thead>\n    <tr>\n      <th> <\/th>\n      <th>연도<\/th>\n      <th>출생아수<\/th>\n      <th>사망자수<\/th>\n      <th>자연증가건수<\/th>\n      <th>조출생률<\/th>\n      <th>조사망률<\/th>\n      <th>자연증가율<\/th>\n    <\/tr>\n  <\/thead>\n<\/table>","options":{"crosstalkOptions":{"key":null,"group":null},"columnDefs":[{"className":"dt-right","targets":[1,2,3,4,5,6,7]},{"orderable":false,"targets":0}],"order":[],"autoWidth":false,"orderClasses":false,"rowCallback":"function(row, data) {\nDTWidget.formatRound(this, row, data, 2, 0, 3, ',', '.');\nDTWidget.formatRound(this, row, data, 3, 0, 3, ',', '.');\nDTWidget.formatRound(this, row, data, 4, 0, 3, ',', '.');\n}"},"selection":{"mode":"multiple","selected":null,"target":"row"}},"evals":["options.rowCallback"],"jsHooks":[]}</script><!--/html_preserve-->

~~~{.r}
birth_death_df %>% 
    select(연도, 출생아수, 사망자수) %>% 
    gather(구분, 출생사망수, -연도) %>% 
    ggplot(data = .) +
    geom_mosaic(aes(weight = 출생사망수, x = product(구분, 연도), fill=factor(구분)), na.rm=TRUE) +
    labs(x="", title='연도별 출생/사망자수', fill="구분") + 
    theme_bw(base_family="NanumGothic") +
    theme(axis.text.x = element_text(angle = 90, hjust = 1),
          legend.position='right', legend.direction='vertical')  +
    scale_fill_brewer(palette="Set1")
~~~

<img src="fig/stat-birth-and-death-trend-2.png" style="display: block; margin: auto;" />

### 2.3. 산모나이를 고려한 출산 추이

산모 연령별로 출생자수 변화를 연도별로 시각화한다.


~~~{.r}
# 2. 산모나이 ------------------------------

mother_age_df <- read_excel("data/통계청_인구통계_2016.xlsx", sheet="산모나이")

DT::datatable(mother_age_df)  %>% 
    DT::formatRound(c(2:9), digits=0)
~~~

<!--html_preserve--><div id="htmlwidget-27863d6a889c8a7fca8f" style="width:100%;height:auto;" class="datatables html-widget"></div>
<script type="application/json" data-for="htmlwidget-27863d6a889c8a7fca8f">{"x":{"filter":"none","data":[["1","2","3","4","5","6","7","8","9","10","11","12","13","14","15","16","17","18","19","20","21","22","23","24","25","26","27","28","29","30","31","32","33","34","35"],[1981,1982,1983,1984,1985,1986,1987,1988,1989,1990,1991,1992,1993,1994,1995,1996,1997,1998,1999,2000,2001,2002,2003,2004,2005,2006,2007,2008,2009,2010,2011,2012,2013,2014,2015],[867409,848312,769155,674793,655489,636019,623831,633092,639431,649738,709275,730678,715826,721185,715020,691226,668344,634790,614233,634501,554895,492111,490543,472761,435031,448153,493189,465892,444849,470171,471265,484550,436455,435435,438420],[34606,30901,26711,24382,21361,18882,15318,12645,9750,8903,8816,9274,8718,7634,7055,6568,5890,5602,5104,4606,3875,4369,3852,3456,3128,3264,3467,2774,2815,2934,2998,2946,2833,2549,2227],[335331,335411,311641,273184,245440,213627,197538,186059,181976,172891,179965,178933,164358,147309,137092,124067,111014,95114,82809,74328,60906,52034,46654,39774,32833,30711,31883,28173,24911,24538,24648,24619,22051,21171,20514],[360510,360672,336079,303387,315397,327244,332339,345691,348421,351520,381141,382772,374424,386495,387247,374819,365385,346112,330590,328207,273266,226356,219105,198902,174743,172825,186912,168893,155906,147197,137008,127192,102801,96192,94622],[102251,91708,72585,57624,59165,63158,66352,75983,85486,99794,119694,135413,140507,148024,148825,148185,147875,149720,156003,183044,173741,169306,178729,185169,177319,187807,205704,198748,192112,214616,221434,239223,220346,221145,216252],[25459,21811,16292,12027,10668,10001,9514,10007,11233,13880,16829,21059,24292,27795,30243,32628,33693,33534,33304,36904,35387,33944,35618,38953,40516,46993,57649,59596,60694,70835,74245,78982,77183,82226,92081],[6883,5842,4402,3022,2409,2107,1838,1729,1717,1829,1841,2215,2569,2901,3403,3872,3907,4105,4465,5038,5106,5127,5263,5255,5147,5319,6212,6537,7303,8840,10126,11098,10714,11523,12138],[1878,1572,1068,823,667,580,507,479,384,395,362,410,301,346,341,325,256,270,313,336,339,383,424,386,415,385,387,366,358,451,509,440,312,334,349]],"container":"<table class=\"display\">\n  <thead>\n    <tr>\n      <th> <\/th>\n      <th>연도<\/th>\n      <th>합계<\/th>\n      <th>19세이하<\/th>\n      <th>20-24세<\/th>\n      <th>25-29세<\/th>\n      <th>30-34세<\/th>\n      <th>35-39세<\/th>\n      <th>40-44세<\/th>\n      <th>45세이상<\/th>\n    <\/tr>\n  <\/thead>\n<\/table>","options":{"crosstalkOptions":{"key":null,"group":null},"columnDefs":[{"className":"dt-right","targets":[1,2,3,4,5,6,7,8,9]},{"orderable":false,"targets":0}],"order":[],"autoWidth":false,"orderClasses":false,"rowCallback":"function(row, data) {\nDTWidget.formatRound(this, row, data, 2, 0, 3, ',', '.');\nDTWidget.formatRound(this, row, data, 3, 0, 3, ',', '.');\nDTWidget.formatRound(this, row, data, 4, 0, 3, ',', '.');\nDTWidget.formatRound(this, row, data, 5, 0, 3, ',', '.');\nDTWidget.formatRound(this, row, data, 6, 0, 3, ',', '.');\nDTWidget.formatRound(this, row, data, 7, 0, 3, ',', '.');\nDTWidget.formatRound(this, row, data, 8, 0, 3, ',', '.');\nDTWidget.formatRound(this, row, data, 9, 0, 3, ',', '.');\n}"},"selection":{"mode":"multiple","selected":null,"target":"row"}},"evals":["options.rowCallback"],"jsHooks":[]}</script><!--/html_preserve-->

~~~{.r}
mother_age_df %>% 
    select(-합계) %>% 
    gather(연령대, 신생아수, -연도) %>% 
    mutate(연령대 = factor(연령대)) %>% 
    ggplot(data = .) +
    geom_mosaic(aes(weight = 신생아수, x = product(연령대, 연도), fill=factor(연령대)), na.rm=TRUE) +
    labs(x="", title='연령대별 출생아수', fill="연령대") + 
    theme_bw(base_family="NanumGothic") +
    theme(axis.text.x = element_text(angle = 90, hjust = 1),
          legend.position='right', legend.direction='vertical') +
    scale_fill_brewer(palette="Set1")
~~~

<img src="fig/stat-mother-age-trend-2.png" style="display: block; margin: auto;" />

### 2.4. 출산 순위를 반영한 연도별 추이

신생아의 출산순의를 반영한 연도별 추이를 살펴본다.


~~~{.r}
# 3. 출산순위 ------------------------------

birth_order_df <- read_excel("data/통계청_인구통계_2016.xlsx", sheet="출산순위")

DT::datatable(birth_order_df)  %>% 
    DT::formatRound(c(2,3,4, 5), digits=0)
~~~

<!--html_preserve--><div id="htmlwidget-ae1db6b233ba4f291940" style="width:100%;height:auto;" class="datatables html-widget"></div>
<script type="application/json" data-for="htmlwidget-ae1db6b233ba4f291940">{"x":{"filter":"none","data":[["1","2","3","4","5","6","7","8","9","10","11","12","13","14","15","16","17","18","19","20","21","22","23","24","25","26","27","28","29","30","31","32","33","34","35","36"],[1981,1982,1983,1984,1985,1986,1987,1988,1989,1990,1991,1992,1993,1994,1995,1996,1997,1998,1999,2000,2001,2002,2003,2004,2005,2006,2007,2008,2009,2010,2011,2012,2013,2014,2015,2016],[867409,848312,769155,674793,655489,636019,623831,633092,639431,649738,709275,730678,715826,721185,715020,691226,668344,634790,614233,634501,554895,492111,490543,472761,435031,448153,493189,465892,444849,470171,471265,484550,436455,435435,438420,406300],[355314,355094,344208,333587,337643,337701,338834,343141,343693,348260,377191,380185,372610,359474,345574,334483,322692,310161,306769,298388,263254,237839,240659,239413,223162,231232,262154,242024,230184,235333,239577,248888,224807,225392,228613,212900],[290618,300907,294027,254940,246541,235935,232769,241391,248780,253002,283385,295244,286826,301906,307930,293986,281016,262416,246027,268314,234079,201706,199221,184452,166888,171180,181850,176079,170090,181871,178981,184020,165661,165332,166130,152700],[221467,192308,130914,86261,71294,62369,52217,48553,46944,48452,48677,55226,56355,59743,61394,62591,64611,62211,60061,66028,55599,48621,46047,45076,41450,42114,45913,44333,41862,49932,51644,50596,45234,43712,42456,39600]],"container":"<table class=\"display\">\n  <thead>\n    <tr>\n      <th> <\/th>\n      <th>연도<\/th>\n      <th>합계<\/th>\n      <th>첫째아<\/th>\n      <th>둘째아<\/th>\n      <th>셋째아이상<\/th>\n    <\/tr>\n  <\/thead>\n<\/table>","options":{"crosstalkOptions":{"key":null,"group":null},"columnDefs":[{"className":"dt-right","targets":[1,2,3,4,5]},{"orderable":false,"targets":0}],"order":[],"autoWidth":false,"orderClasses":false,"rowCallback":"function(row, data) {\nDTWidget.formatRound(this, row, data, 2, 0, 3, ',', '.');\nDTWidget.formatRound(this, row, data, 3, 0, 3, ',', '.');\nDTWidget.formatRound(this, row, data, 4, 0, 3, ',', '.');\nDTWidget.formatRound(this, row, data, 5, 0, 3, ',', '.');\n}"},"selection":{"mode":"multiple","selected":null,"target":"row"}},"evals":["options.rowCallback"],"jsHooks":[]}</script><!--/html_preserve-->

~~~{.r}
birth_order_df %>% 
    select(-합계) %>% 
    gather(출산순위, 신생아수, -연도) %>% 
    mutate(출산순위 = factor(출산순위, levels=c("첫째아", "둘째아", "셋째아이상"))) %>% 
    ggplot(data = .) +
    geom_mosaic(aes(weight = 신생아수, x = product(출산순위, 연도), fill=factor(출산순위)), na.rm=TRUE) +
    labs(x="", title='출산순위별 출생아수', fill="출산순위") + 
    theme_bw(base_family="NanumGothic") +
    theme(axis.text.x = element_text(angle = 90, hjust = 1),
          legend.position='right', legend.direction='vertical')  +
    scale_fill_brewer(palette="Set1")
~~~

<img src="fig/stat-birth-orders-trend-2.png" style="display: block; margin: auto;" />
