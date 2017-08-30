# xwMOOC 데이터 과학
xwMOOC  
`r Sys.Date()`  




## 북한 미사일 발사 연대기 {#north-korea-missile} 

북한이 미사일을 경술국치일에 맞춰 일본상공으로 쏘아올렸다. 
이와 관련하여 1990년대부터 북한이 쏘아올린 미사일 관련된 데이터분석을 시작해 보자.

[Chronology of North Korea’s missile, rocket launches](http://rpubs.com/hrbrmstr/nkorlaunch) Rpubs에 올라온 
분석을 기본으로 하여 최근 데이터를 반영하고 한글을 그래픽에 반영하였다. [^daum-recent-launch]

[^daum-recent-launch]: [그래픽 - 북한 미사일 발사 일지](http://v.media.daum.net/v/20170829111049783?f=o)

## 2. 북한 미사일 발사 일지 시각화 {#nk-crawl-data}

### 2.1. 데이터 가져오기 {#import-data}

아마도 조만간 연합뉴스에서 기사를 갱신할 것으로 보이는데 현재 시점으로 2017년 8월 발사한 두발의 발사체에 대한 
내용은 반영되어 있지 않아 [Chronology of North Korea missile, rocket launches](http://english.yonhapnews.co.kr/northkorea/2017/07/29/0401000000AEN20170729000700315.html)
기사를 기반으로 하고 [그래픽 - 북한 미사일 발사 일지](http://v.media.daum.net/v/20170829111049783?f=o) 내용을 수작업을 입력하여 데이터를 정제한다.


~~~{.r}
# 0. 환경설정 ----------------------------------

# library(stringr)
# library(rvest)
# library(ggthemes)
# library(lubridate)
# library(ggrepel)
# library(hrbrthemes)
# library(tidyverse)
# library(extrafont)
# loadfonts()

# 1. 데이터 가져오기 ----------------------------------

yp_news_url <- "http://english.yonhapnews.co.kr/northkorea/2017/07/29/0401000000AEN20170729000700315.html"

yp_news_list <- read_html(yp_news_url) %>% 
    html_nodes(xpath = '//*[@id="content"]/div[2]/div[2]') %>% 
    html_text() %>% 
    str_split(., pattern = "-- ") %>% 
    .[[1]]

yp_news_list <- yp_news_list[-c(1,2)]

# 2. 데이터 정제 ----------------------------------

yp_news_df <- tibble(yp_news_list) %>% 
    rename(news = yp_news_list) %>% 
    mutate(news_list = str_split(news, ":")) %>% 
    mutate(odate = map(news_list, 1)) %>%
    unnest(odate) %>% 
    select(odate, news)

yp_news_df <- yp_news_df %>% mutate(odate = str_replace(odate, "Aug.", "August"),
                      odate = str_replace(odate, "Dec.", "December"),
                      odate = str_replace(odate, "Nov.", "November"),
                      odate = str_replace(odate, "Feb.", "February"),
                      odate = str_replace(odate, "Oct.", "October")) 

write_csv(yp_news_df, "data/yp_news_df.csv")

yp_news_df <- read_csv("data/yp_news_df.csv",
                       col_types = cols(
                           odate = col_date("%B %d, %Y"),
                           news = col_character()
                       ))

yp_news_df <- yp_news_df %>% 
    add_row(odate="2017-08-29", news="평양시 순안 일대 탄도미사일 1발 발사") %>% 
    add_row(odate="2017-08-26", news="강원도 깃대령 일대에서 단거리 발사체 3발 발사")

wrap_20 <- scales::wrap_format(20)

yp_news_df %>% mutate(event = wrap_20(news)) %>% 
    select(일자 = odate, 도발=event) %>% 
    arrange(desc(일자)) %>% 
    DT::datatable()
~~~

<!--html_preserve--><div id="htmlwidget-62c8aa09da5877c4a2d1" style="width:100%;height:auto;" class="datatables html-widget"></div>
<script type="application/json" data-for="htmlwidget-62c8aa09da5877c4a2d1">{"x":{"filter":"none","data":[["1","2","3","4","5","6","7","8","9","10","11","12","13","14","15","16","17","18","19","20","21","22","23","24","25","26","27","28","29","30","31","32","33","34","35","36","37","38"],["2017-08-29","2017-08-26","2017-07-28","2017-07-04","2017-06-08","2017-05-29","2017-05-27","2017-05-21","2017-05-14","2017-04-05","2017-03-22","2017-03-06","2017-02-12","2016-10-20","2016-10-15","2016-08-24","2016-08-03","2016-07-19","2016-07-09","2016-06-22","2016-05-31","2016-04-28","2016-04-23","2016-04-15","2016-03-18","2016-02-07","2016-02-06","2016-02-02","2015-12-21","2015-11-28","2015-05-08","2012-12-12","2012-12-10","2012-12-01","2012-04-13","2009-04-05","2006-07-04","1998-08-31"],["평양시 순안 일대\n탄도미사일 1발 발사","강원도 깃대령\n일대에서 단거리\n발사체 3발 발사","July 28, 2017:\nNorth Korea\nlaunches a\nballistic missile\nfrom the northern\nprovince of Jagang\ninto the East\nSea.(END)\r","July 4, 2017: North\nKorea launches a\nballistic missile\nfrom a northwestern\nprovince into\nwaters off its east\ncoast. Pyongyang\nclaims that it\nsuccessfully\ntest-fired an\nintercontinental\nballistic missile\nand that it reached\nan altitude of\n2,802 kilometers\nand flew 933 km.","June 8, 2017: North\nKorea test-fires\nmultiple\nsurface-to-ship\ncruise missiles.","May 29, 2017: North\nKorea fires what is\npresumed to be a\nScud-type ballistic\nmissile. It flies\nabout 450\nkilometers.","May 27, 2017: North\nKorea is presumed\nto have launched a\nsurface-to-air\nguided missile,\nbelieved to be a\nKN-06, from the\neastern region.","May 21, 2017: North\nKorea fires the\nground-to-ground\nPukguksong-2\nmissile, also known\nas a KN-15. It\nflies more than 500\nkilometers.","May 14, 2017: North\nKorea fires a new\nmid-to-long-range\nballistic missile,\nthe Hwasong-12,\nfrom a northwest\nsite. It flies\nabout 700\nkilometers before\nlanding in the East\nSea.","April 5, 2017:\nNorth Korea fires\nwhat appears to be\na type of KN-15\nintermediate-range\nballistic missile.","March 22, 2017:\nNorth Korea\nlaunches a missile\nfrom its east coast\nthat is presumed to\nhave failed. The\ntype of the missile\nis not confirmed.","March 6, 2017:\nNorth Korea fires\nfour ballistic\nmissiles from its\nthe Dongchang-ri\nlaunch site toward\nthe East Sea.","Feb. 12, 2017:\nNorth Korea\nlaunches a new\nintermediate-range\nballistic missile,\nPukguksong-2, into\nthe East Sea.\nExperts say the\ncountry appears to\napply technology\nused in the SLBM to\nhave developed a\nnew missile.","Oct. 20, 2016:\nNorth Korea\nlaunches what\nappears to be an\nintermediate-range\nMusudan, but the\ntest ends in\nfailure.","Oct. 15, 2016:\nNorth Korea\nfires-off an\nintermediate-range\nMusudan ballistic\nmissile, but it\nexplodes after\nlaunch.","Aug. 24, 2016:\nNorth Korea\ntest-fires an SLBM\nin waters off its\neast coast towards\nJapan. The missile\nflies about 500\nkilometers, making\nit the longest\nflight by such a\nmissile.","Aug. 3, 2016: North\nKorea fires-off two\nmid-range Rodong\nballistic missiles\nfrom near the\nsouthwestern area.\nOne missile flies\nabout 1,000\nkilometers before\nfalling into\nJapan's exclusive\neconomic zone\n(EEZ).","July 19, 2016:\nNorth Korea\ntest-fires two\nmid-range Rodong\nmissiles and a\nshorter-range Scud\nmissile.","July 9, 2016: North\nKorea launches an\nSLBM off its east\ncoast, but Seoul\nsays the missile\nappears to have\nexploded at an\naltitude of some 10\nkilometers.","June 22, 2016:\nNorth Korea fires\noff two\nintermediate-range\nMusudan missiles.\nOne missile flies\nabout 400 km, which\nexperts widely view\nas a success.","May 31, 2016: North\nKorea test-fires an\nintermediate-range\nMusudan, but the\nlaunch ends in\nfailure.","April 28, 2016:\nNorth Korea\nlaunches two\nintermediate-range\nMusudan ballistic\nmissiles, but the\nlaunches end in\nfailure.","April 23, 2016:\nNorth Korea\ntest-fires an SLBM\nin the East Sea,\nwhich flies only\nabout 30 kilometers","April 15, 2016:\nNorth Korea\nconducts its first\ntest-launch of an\nintermediate-range\nMusudan ballistic\nmissile, also known\nas the BM-25, but\nthe launch ends in\nfailure.","March 18, 2016:\nNorth Korea\nlaunches what\nappears to be two\nmid-range Rodong\nballistic missiles\nfrom its western\nprovince.","Feb. 7, 2016: North\nKorea fires a\nlong-range rocket\nfrom the\nDongchang-ri launch\nsite at around 9:30\na.m. The North\nclaims it has\nsuccessfully placed\na satellite, named\nKwangmyongsong-4,\ninto orbit.","Feb. 6, 2016: North\nKorea informs the\nInternational\nMaritime\nOrganization of its\nplan to move up the\nlaunch date to Feb.\n7-14.","Feb. 2, 2016: North\nKorea notifies U.N.\nagencies of its\nplan to launch a\nsatellite between\nFeb. 8 and 25.","Dec. 21, 2015:\nSouth Korea's\nmilitary says North\nKorea conducted\nanother SLBM test\nin December, but\nthe test ended in\nfailure. The\nWashington Free\nBeacon reported\nthat North Korea\nsucceeded in the\nunderwater test of\na KN-11 missile\nnear the eastern\nport of Sinpo on\nDec. 21, citing\nunidentified U.S.\ndefense officials.","Nov. 28, 2015:\nNorth Korea fires\noff an SLBM in the\nEast Sea, but Seoul\nviews the test as a\nfailure.","May 8, 2015: North\nKorea for the first\ntime tests a\nsubmarine-launched\nballistic missile\n(SLBM), dubbed\nKN-11.  Seoul said\nthat it was more of\na test for the\nejection rather\nthan firing.","Dec. 12, 2012:\nNorth Korea\nlaunches a\nlong-range rocket\nfrom the\nDongchang-ri launch\nsite in North\nPyongan Province.","Dec. 10, 2012:\nNorth Korea extends\nthe rocket launch\nwindow until Dec.\n29, citing\ntechnical problems\nin the first-stage\ncontrol engine\nmodule.","Dec. 1, 2012: North\nKorea says it will\nlaunch a working\nsatellite, the\nKwangmyongsong-3,\non the carrier\nrocket Unha-3,\nbetween Dec. 10 and\n22.","April 13, 2012:\nNorth Korea fires\noff a long-range\nrocket, the Unha-3,\nfrom the\nDongchang-ri launch\nsite in North\nPyongan Province.\nBut the rocket\ncrashes in pieces\ninto the sea\nshortly after\ntakeoff.","April 5, 2009:\nNorth Korea\nlaunches the Unha-2\nrocket at the\nMusudan-ri launch\nsite with the\nattendance of\nleader Kim Jong-il\nand his son, Kim\nJong-un.","July 4, 2006: North\nKorea test-fires an\nadvanced version of\nthe Taepodong-2\nmissile at the\nMusudan-ri launch\nsite.","Aug. 31, 1998:\nNorth Korea fires\noff its first\nballistic missile,\nthe Unha-1, also\nknown as the\nTaepodong-1, from\nthe launch site of\nMusudan-ri in North\nHamgyong Province."]],"container":"<table class=\"display\">\n  <thead>\n    <tr>\n      <th> <\/th>\n      <th>일자<\/th>\n      <th>도발<\/th>\n    <\/tr>\n  <\/thead>\n<\/table>","options":{"crosstalkOptions":{"key":null,"group":null},"order":[],"autoWidth":false,"orderClasses":false,"columnDefs":[{"orderable":false,"targets":0}]},"selection":{"mode":"multiple","selected":null,"target":"row"}},"evals":[],"jsHooks":[]}</script><!--/html_preserve-->

## 3. 북한 미사일 발사 시각화 {#visualize}

### 3.1. 북한이 미사일 도발을 시작한 전체 기간 {#nk-full-timeline-view}

`ggplot`으로 전체적인 방향을 잡고나서, `fig.height=2`, `fig.width=10` 설정을 통해 출력 시각화 산출물을 최종정리한다.


~~~{.r}
# 3. 데이터 시각화 ----------------------------------
## 3.1. 전체 기간 
#+ fig.height=2, fig.width=10
ggplot(yp_news_df, aes(odate, 1)) +
    geom_hline(yintercept = 1, size=1.3, color="darkblue") +
    geom_point(shape=21, fill="red", color="white", size=5, alpha=0.3) +
    scale_x_date(date_labels = "%Y", date_breaks = "2 years") +
    scale_y_continuous(expand=c(0,0)) +
    labs(x=NULL, y=NULL,
         title="북핵 미사일 발사 연대기") +
    theme_ipsum_rc(grid="X", base_family = "NanumGothic") +
    theme(axis.text.y = element_blank())
~~~

<img src="fig/ggplot-whole-time-span-1.png" style="display: block; margin: auto;" />

### 3.2. 북한이 미사일 도발을 최근 3년(2015년부터) {#nk-recent-timeline-view}

데이터를 최근 3년으로 한정시킨다. 그리고 위와 마찬가지로 `ggplot`으로 전체적인 방향을 잡고나서, 
`fig.height=2`, `fig.width=10` 설정을 통해 출력 시각화 산출물을 최종정리한다.


~~~{.r}
## 3.2. 최근 3년 
#+ fig.height=2, fig.width=10
yp_news_df %>% filter(odate >= "2015-01-01") %>% 
    ggplot(., aes(odate, 1)) +
    geom_hline(yintercept = 1, size=1.3, color="darkblue") +
    geom_point(shape=21, fill="red", color="white", size=5, alpha=0.3) +
    scale_x_date(date_labels = "%Y-%m", date_breaks = "3 months") +
    scale_y_continuous(expand=c(0,0)) +
    labs(x=NULL, y=NULL,
         title="북핵 미사일 발사 연대기(최근 3년)") +
    theme_ipsum_rc(grid="X", base_family = "NanumGothic") +
    theme(axis.text.y = element_blank())
~~~

<img src="fig/ggplot-recent-time-span-1.png" style="display: block; margin: auto;" />


### 3.3. 북한이 미사일 도발 달력 형태 시각화 {#nk-tile-view}

`ggplot`의 유연성을 살펴볼 수 있는 시각화 산출물이다.
달력형태 시각화 산출물을 위해 데이터를 사전 준비한다.
그리고 나서 이를 `ggplot`에 넣어 시작화하는데 
`fig.height=7`, `fig.width=7` 설정을 통해 출력 시각화 산출물을 깔끔히 마무리 한다.


~~~{.r}
## 3.3. 시점별로 살펴보기
#+ fig.height=7, fig.width=7

yp_news_df %>%
    mutate(
        month = lubridate::month(odate, TRUE, TRUE),
        year = lubridate::year(odate)
    ) %>%
    count(year, month) %>%
    ungroup() %>%
    complete(year=1998:2017, month) %>%
    mutate(year = as.character(year), lab_col = ifelse(n<3, "white", "black")) %>%
    ggplot(aes(month, year)) +
    geom_tile(aes(fill=n), color="#b2b2b2", size=0.1) +
    geom_text(aes(label=n, color=lab_col)) +
    scale_x_discrete(expand=c(0,0), position="top") +
    scale_color_identity() +
    viridis::scale_fill_viridis(na.value="white") +
    labs(x=NULL, y=NULL,
         title="북한 미사일 발사횟수 (월별)") +
    theme_ipsum_rc(grid="", base_family = "NanumGothic") +
    theme(legend.position="none")
~~~

<img src="fig/ggplot-calendar-view-1.png" style="display: block; margin: auto;" />





