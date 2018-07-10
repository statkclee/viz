# xwMOOC 데이터 과학
xwMOOC  
`r Sys.Date()`  




## 통계청 남북군사력 {#kosis-military-stat}

남북군사력을 비교한 최신 데이터가 KOSIS 웹사이트를 통해서 확인 가능하다.
[남북군사력 비교, 자료갱신일 : 2017-03-17 / 수록기간 : 2008 ~ 2016](http://kosis.kr/statHtml/statHtml.do?orgId=101&tblId=DT_1ZGAB12)

이를 바탕으로 남북 군사력을 군인수, 함정수, 전투기수를 기준으로 시각화해보자.


### 1.1. 데이터 가져와서 정제

데이터를 불러와서 불피요한 정보를 제거하고 시각화가 가능한 형태로 변환한다.


~~~{.r}
# 0. 환경설정 ----------------------------
# library(waffle)
# library(tidyverse)
# library(stringr)
# library(extrafont)

# font_import(pattern="fontawesome")
# loadfonts()

# 1. 남북 군사력 데이터 ----------------------------
mil_dat <- read_csv("data/남북군사력_비교_20170903164117.csv",  locale = locale(encoding = 'euc-kr'))

# 2. 데이터 정제----------------------------

mil_df <- mil_dat %>% rename(구분 = 군사별, 남한 =`2016`, 북한=`2016_1`) %>% 
  mutate(구분 = ifelse(구분 == "계", "군인수", 구분)) %>% 
  filter(구분 %in% c("군인수", "전차", "전투함정", "전투임무기")) %>% 
  mutate(남한 = as.numeric(str_extract(남한, "[0-9]+")),
         북한 = as.numeric(str_extract(북한, "[0-9]+")))

mil_df %>% 
  DT::datatable()
~~~

<!--html_preserve--><div id="htmlwidget-8b2a151f299bc053075f" style="width:100%;height:auto;" class="datatables html-widget"></div>
<script type="application/json" data-for="htmlwidget-8b2a151f299bc053075f">{"x":{"filter":"none","data":[["1","2","3","4"],["군인수","전차","전투함정","전투임무기"],[62,2400,110,410],[128,4300,430,810]],"container":"<table class=\"display\">\n  <thead>\n    <tr>\n      <th> <\/th>\n      <th>구분<\/th>\n      <th>남한<\/th>\n      <th>북한<\/th>\n    <\/tr>\n  <\/thead>\n<\/table>","options":{"columnDefs":[{"className":"dt-right","targets":[2,3]},{"orderable":false,"targets":0}],"order":[],"autoWidth":false,"orderClasses":false}},"evals":[],"jsHooks":[]}</script><!--/html_preserve-->

### 1.2. 데이터 시각화

`waffle` 팩키지 함수를 활용하여 시각화를 한다. 특히, 
[Font Awesome - The iconic font and CSS toolkit](http://fontawesome.io/) 글꼴에 포함된 상형문자(glyph)를 정사각형 대신 세겨넣어 
시각적으로 즉각 이해가능하게 한다.


~~~{.r}
# 3. 시각화 ----------------------------
mil_df %>% filter(구분 =="군인수") %>% 
  select(-구분) %>% as.vector %>% 
  waffle(., rows=10, colors=c("#0000ff", "#ff0000"),
       use_glyph="male", glyph_size=10, legend_pos = "top")
~~~

<img src="fig/data-viz-1.png" style="display: block; margin: auto;" />

~~~{.r}
mil_df %>% filter(구분 =="전투함정") %>% 
  select(-구분) %>% as.vector %>% 
  waffle(., rows=20, colors=c("#0000ff", "#ff0000"),
         use_glyph="ship", glyph_size=6, legend_pos = "top")
~~~

<img src="fig/data-viz-2.png" style="display: block; margin: auto;" />

~~~{.r}
mil_df %>% filter(구분 =="전투임무기") %>% 
  select(-구분) %>% as.vector %>% 
  waffle(., rows=30, colors=c("#0000ff", "#ff0000"),
         use_glyph="fighter-jet", glyph_size=5, legend_pos = "top")
~~~

<img src="fig/data-viz-3.png" style="display: block; margin: auto;" />

