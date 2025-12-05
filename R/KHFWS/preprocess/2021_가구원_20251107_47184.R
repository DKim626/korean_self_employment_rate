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

mdis <- read.table("2021_가구원_20251107_47184.csv", header=FALSE, sep=",", colClasses = c("character"
, "character", "character", "character", "numeric", "character", "character", "character", "character", "character", "character", "character", "character"), skip=1, na.string=c("*","**","***","****","*****","******","*******","********","*********","**********","."))

        mdis$V4<- recode_factor(mdis$V4, '1'="남")
        mdis$V4<- recode_factor(mdis$V4, '2'="여")
        mdis$V6<- recode_factor(mdis$V6, '1'="가구주")
        mdis$V6<- recode_factor(mdis$V6, '2'="배우자")
        mdis$V6<- recode_factor(mdis$V6, '3'="미혼 자녀")
        mdis$V6<- recode_factor(mdis$V6, '4'="기혼 자녀 및 그 배우자")
        mdis$V6<- recode_factor(mdis$V6, '5'="손자녀 및 그 배우자")
        mdis$V6<- recode_factor(mdis$V6, '6'="부모(배우자의 부모 포함)")
        mdis$V6<- recode_factor(mdis$V6, '7'="조부모")
        mdis$V6<- recode_factor(mdis$V6, '8'="미혼 형제자매")
        mdis$V6<- recode_factor(mdis$V6, '9'="기타")
        mdis$V7<- recode_factor(mdis$V7, '1'="1인 가구")
        mdis$V7<- recode_factor(mdis$V7, '2'="같이 살고 있음")
        mdis$V7<- recode_factor(mdis$V7, '3'="따로 살고 있음(직장)")
        mdis$V7<- recode_factor(mdis$V7, '4'="따로 살고 있음(학업 등)")
        mdis$V8<- recode_factor(mdis$V8, '1'="안 받음(미취학 포함)")
        mdis$V8<- recode_factor(mdis$V8, '2'="초등학교")
        mdis$V8<- recode_factor(mdis$V8, '3'="중학교")
        mdis$V8<- recode_factor(mdis$V8, '4'="고등학교")
        mdis$V8<- recode_factor(mdis$V8, '5'="대학(3년제 이하)")
        mdis$V8<- recode_factor(mdis$V8, '6'="대학교(4년제 이상)")
        mdis$V8<- recode_factor(mdis$V8, '7'="대학원석사")
        mdis$V8<- recode_factor(mdis$V8, '8'="대학원박사 이상")
        mdis$V9<- recode_factor(mdis$V9, '1'="졸업")
        mdis$V9<- recode_factor(mdis$V9, '2'="재학")
        mdis$V9<- recode_factor(mdis$V9, '3'="휴학")
        mdis$V9<- recode_factor(mdis$V9, '4'="중퇴")
        mdis$V9<- recode_factor(mdis$V9, '5'="수료")
        mdis$V10<- recode_factor(mdis$V10, '1'="미혼")
        mdis$V10<- recode_factor(mdis$V10, '2'="배우자있음")
        mdis$V10<- recode_factor(mdis$V10, '3'="사별")
        mdis$V10<- recode_factor(mdis$V10, '4'="이혼")
        mdis$V11<- recode_factor(mdis$V11, '1'="상용근로자")
        mdis$V11<- recode_factor(mdis$V11, '2'="임시.일용근로자")
        mdis$V11<- recode_factor(mdis$V11, '3'="고용원이 있는 자영업자")
        mdis$V11<- recode_factor(mdis$V11, '4'="고용원이 없는 자영업자")
        mdis$V11<- recode_factor(mdis$V11, '5'="무급가족종사자")
        mdis$V11<- recode_factor(mdis$V11, '6'="기타 종사자(실적급의 보험설계사, 대리 운전기사, 학습지 방문 교사 등)")
        mdis$V11<- recode_factor(mdis$V11, '7'="기타(무직자, 가사, 학생 등)")
        mdis$V12<- recode_factor(mdis$V12, '1'="예")
        mdis$V12<- recode_factor(mdis$V12, '2'="아니오")
        mdis$V13<- recode_factor(mdis$V13, '1'="다문화 가구")
        mdis$V13<- recode_factor(mdis$V13, '2'="그 외 가구")

colnames(mdis) = c("조사연도"
, "MD제공용_가구고유번호", "MD제공용_가구원고유번호", "가구원정보_성별코드", "가구원정보_만연령", "가구원정보_가구주관계코드", "가구원정보_동거코드", "가구원정보_교육정도_최종학력코드", "가구원정보_교육정도_수학구분코드", "가구원정보_혼인상태코드", "가구원정보_종사상지위코드", "가구원정보_등록장애인여부(2013년~)(보완)", "가구원정보_귀화및인지국적취득또는외국인여부")

