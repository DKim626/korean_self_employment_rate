#**********************************************************************  
#	*주의 사항
#		현재 스크립트 파일은 파일명만 출력되어 있습니다.
#		따라서, 저장된 추출 결과 파일의 경로를 'read.table' 또는 'read.fwf'에 추가하여야 합니다.
#	예) 다운로드 받은 폴더명 : C:\Download
#	  ※ 파일 경로 추가 : "[다운로드 받은 폴더명]\기업활동조사_기업활동조사(제공)_2019_20191201_92007.txt"
# 		read.table("C:\Download\기업활동조사_기업활동조사(제공)_2019_20191201_92007.txt", ~~~
#		또는
#		read.fwf("C:\Download\기업활동조사_기업활동조사(제공)_2019_20191201_92007.txt", ~~~
#
#		R 스크립트는 R 에서 파일 경로만 수정하시면 바로 실행(Ctrl+Alt+R)가능하며,
#		데이터셋 생성 후에 R 의 여러 가지 분석 기능을 사용할 수 있습니다.
#
#**********************************************************************

install.packages("dplyr")
library(dplyr)

mdis <- read.table("2013_연간자료(1990~2016)_20251107_20696.csv", header=FALSE, sep=",", colClasses = c("numeric"
, "numeric", "character", "character", "numeric", "numeric", "character", "character", "character", "character", "character", "character", "character", "character", "numeric", "character", "character", "character", "character", "character", "character"
, "character", "character", "numeric", "character", "character", "character", "character", "character", "character", "character", "character", "numeric", "character", "character", "character", "character", "character", "character", "character", "character"
, "numeric", "character", "character", "character", "character", "character", "character", "character", "character", "numeric", "character", "character", "character", "character", "character", "character", "character", "character", "numeric", "character"
, "character", "character", "character", "character", "character", "character", "character", "numeric", "character", "character", "character", "character", "character", "character", "character", "character", "numeric", "character", "character", "character"
, "character", "character", "character", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "character", "numeric", "character", "numeric", "numeric", "numeric", "numeric", "character", "numeric", "numeric", "numeric"
, "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric"
, "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric"
, "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric"
, "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric"
, "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric"
, "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric"
, "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric"
, "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "character"), skip=1, na.string=c("*","**","***","****","*****","******","*******","********","*********","**********","."))



colnames(mdis) = c("조사년도"
, "조사월", "가구별 일련번호", "가구구분", "가구원수", "취업인원수", "노인가구", "모자가구", "맞벌이가구", "일반가구", "세대구분", "배우자유무", "가구주_가구주와의 관계", "가구주-성별", "가구주-연령", "가구주_학력", "가구주_수학여부", "가구주_취업여부", "가구주_산업코드", "가구주_직업코드", "가구주_종사상지위"
, "가구원2_가구주와의 관계", "가구원2_성별", "가구원2_연령", "가구원2_학력", "가구원2_수학여부", "가구원2_취업여부", "가구원2_산업", "가구원2_직업", "가구원2_종사상지위", "가구원3_가구주와의 관계", "가구원3_성별", "가구원3_연령", "가구원3_학력", "가구원3_수학여부", "가구원3_취업여부", "가구원3_산업", "가구원3_직업", "가구원3_종사상지위", "가구원4_가구주와의 관계", "가구원4_성별"
, "가구원4_연령", "가구원4_학력", "가구원4_수학여부", "가구원4_취업여부", "가구원4_산업", "가구원4_직업", "가구원4_종사상지위", "가구원5_가구주와의 관계", "가구원5_성별", "가구원5_연령", "가구원5_학력", "가구원5_수학여부", "가구원5_취업여부", "가구원5_산업", "가구원5_직업", "가구원5_종사상지위", "가구원6_가구주와의 관계", "가구원6_성별", "가구원6_연령", "가구원6_학력"
, "가구원6_수학여부", "가구원6_취업여부", "가구원6_산업", "가구원6_직업", "가구원6_종사상지위", "가구원7_가구주와의 관계", "가구원7_성별", "가구원7_연령", "가구원7_학력", "가구원7_수학여부", "가구원7_취업여부", "가구원7_산업", "가구원7_직업", "가구원7_종사상지위", "가구원8_가구주와의 관계", "가구원8_성별", "가구원8_연령", "가구원8_학력", "가구원8_수학여부", "가구원8_취업여부"
, "가구원8_산업", "가구원8_직업", "가구원8_종사상지위", "취업배우자", "학업배우자", "기타배우자", "취업자녀", "학업자녀", "기타자녀", "거처구분", "자동차 소유대수(휘발유+경우+LPG)", "입주형태", "월세평가액", "전세보증금", "월세(사글세)", "사용면적", "(현재 살고 있는 주택 이외) 주택소유구분", "가중치(weight)", "소득", "경상소득"
, "근로소득", "가구주소득", "배우자소득", "기타가구원소득", "사업소득", "가구주사업소득", "배우자사업소득", "기타가구원사업소득", "주택등임대소득", "재산소득", "이자소득", "배당소득", "개인연금수입", "퇴직연급수입", "기타재산소득", "이전소득", "공적연금", "기초노령연금", "사회수혜금", "사회적현물이전"
, "세금환급금", "가구간이전", "할인혜택", "기타이전소득", "비경상소득", "경조소득", "퇴직금및연금일시금", "기타비경상소득", "기타수입", "자산변동으로인한수입", "부채증가로인한수입", "자산이전으로인한수입", "가계지출", "소비지출", "01.식료품및비주류음료", "곡물", "곡물가공품", "빵및떡류", "육류", "육류가공품"
, "신선수산동물", "염건수산동물", "기타수산동물가공", "유제품및알", "유지류", "과일및과일가공품", "채소및채소가공품", "해조및해조가공품", "당류및과자류", "조미식품", "기타식품", "커피및차", "쥬스및기타음료", "02.주류및담배", "주류", "담배", "03.의류및신발", "직물및외의", "내의", "기타의복"
, "의복관련서비스", "신발", "신발서비스", "04.주거및수도광열", "실제주거비", "주택유지및수선", "상하수도및폐기물처리", "기타주거관련서비스", "연료비", "05.가정용품및가사서비스", "가구및조명", "실내장식", "가구·조명및장식서비스", "가정용섬유", "가전및가정용기기", "가전관련서비스", "가사용품", "가정용공구및기타", "가사소모품", "가사서비스"
, "06.보건", "의약품", "의료용소모품", "보건의료용품및기구", "외래의료서비스", "치과서비스", "기타의료서비스", "입원서비스", "07.교통", "자동차구입", "기타운송기구구입", "운송기구유지및수리", "운송기구연료비", "기타개인교통서비스", "철도운송", "육상운송", "기타운송", "기타교통관련서비스", "08.통신", "우편서비스"
, "통신장비", "통신서비스", "09.오락·문화", "영상음향기기", "사진광학장비", "정보처리장치", "기록매체", "영상음향및정보기기수리", "오락문화내구재", "악기기구", "오락문화내구재유지및수리", "장난감및취미용품", "캠핑및운동관련용품", "화훼관련용품", "애완동물관련물품", "화훼및애완동물서비스", "운동및오락서비스", "문화서비스", "복권", "서적"
, "기타인쇄물", "문구", "단체여행비", "10.교육", "정규교육", "초등교육", "중등교육", "고등교육", "학원및보습교육", "학원및보습교육(성인미포함)", "성인학원교육", "기타교육", "11.음식·숙박", "식사비", "숙박비", "12.기타상품및서비스", "이미용서비스", "이미용기기", "위생및이미용용품", "시계및장신구"
, "기타개인용품", "사회복지", "보험", "기타금융", "기타서비스", "비소비지출", "경상조세", "비경상조세", "연금", "사회보장", "이자비용", "가구간이전지출", "비영리단체로이전", "기타지출", "자산변동으로 인한 지출", "부채감소를 위한 지출", "자산이전", "도시구분여부")

