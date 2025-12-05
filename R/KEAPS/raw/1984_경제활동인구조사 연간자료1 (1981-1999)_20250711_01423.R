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

mdis <- read.table("1984_경제활동인구조사 연간자료1 (1981-1999)_20250711_01423.csv", header=FALSE, sep=",", colClasses = c("character"
, "character", "character", "character", "character", "character", "character", "character", "character", "character", "character", "character", "character", "character", "character", "character", "character", "character", "character", "character"), skip=1, na.string=c("*","**","***","****","*****","******","*******","********","*********","**********","."))

        mdis$V3<- recode_factor(mdis$V3, '0'="동거인")
        mdis$V3<- recode_factor(mdis$V3, '1'="가구주")
        mdis$V3<- recode_factor(mdis$V3, '2'="배우자")
        mdis$V3<- recode_factor(mdis$V3, '3'="미혼자녀")
        mdis$V3<- recode_factor(mdis$V3, '4'="미혼형제자매")
        mdis$V3<- recode_factor(mdis$V3, '5'="부모(장인, 장모포함)")
        mdis$V3<- recode_factor(mdis$V3, '6'="조부모")
        mdis$V3<- recode_factor(mdis$V3, '7'="기혼자녀, 그가족")
        mdis$V3<- recode_factor(mdis$V3, '8'="기혼형제 자매, 그가족")
        mdis$V3<- recode_factor(mdis$V3, '9'="기타친척")
        mdis$V4<- recode_factor(mdis$V4, '1'="남자")
        mdis$V4<- recode_factor(mdis$V4, '2'="여자")
        mdis$V6<- recode_factor(mdis$V6, '1'="미혼")
        mdis$V6<- recode_factor(mdis$V6, '2'="유배우")
        mdis$V6<- recode_factor(mdis$V6, '3'="사별")
        mdis$V6<- recode_factor(mdis$V6, '4'="이혼")
        mdis$V7<- recode_factor(mdis$V7, '1'="국민학교이하")
        mdis$V7<- recode_factor(mdis$V7, '2'="중학교")
        mdis$V7<- recode_factor(mdis$V7, '3'="고등학교")
        mdis$V7<- recode_factor(mdis$V7, '4'="초대/전문")
        mdis$V7<- recode_factor(mdis$V7, '5'="대학이상")
        mdis$V8<- recode_factor(mdis$V8, '1'="졸업")
        mdis$V8<- recode_factor(mdis$V8, '2'="재학")
        mdis$V8<- recode_factor(mdis$V8, '3'="중퇴")
        mdis$V8<- recode_factor(mdis$V8, '4'="안다녔음")
        mdis$V9<- recode_factor(mdis$V9, '1'="일하였음&lt;(가-1)로감&gt;")
        mdis$V9<- recode_factor(mdis$V9, '2'="일시휴직")
        mdis$V9<- recode_factor(mdis$V9, '3'="구직활동")
        mdis$V9<- recode_factor(mdis$V9, '4'="가사")
        mdis$V9<- recode_factor(mdis$V9, '5'="통학")
        mdis$V9<- recode_factor(mdis$V9, '6'="연소연로")
        mdis$V9<- recode_factor(mdis$V9, '7'="불구")
        mdis$V9<- recode_factor(mdis$V9, '8'="기타()")
        mdis$V10<- recode_factor(mdis$V10, '1'="있다")
        mdis$V10<- recode_factor(mdis$V10, '2'="없다")
        mdis$V11<- recode_factor(mdis$V11, '1'="구하여 보았음")
        mdis$V11<- recode_factor(mdis$V11, '2'="불가피한 사유로 구하여 보지 못했음")
        mdis$V11<- recode_factor(mdis$V11, '3'="구하여보지 않았음")
        mdis$V14<- recode_factor(mdis$V14, '11'="고용주")
        mdis$V14<- recode_factor(mdis$V14, '12'="자영자")
        mdis$V14<- recode_factor(mdis$V14, '13'="가족종사자")
        mdis$V14<- recode_factor(mdis$V14, '22'="상용/임시")
        mdis$V14<- recode_factor(mdis$V14, '23'="일용")
        mdis$V14<- recode_factor(mdis$V14, '30'="없음")
        mdis$V18<- recode_factor(mdis$V18, '11'="주로 일한자")
        mdis$V18<- recode_factor(mdis$V18, '12'="틈틈이 일한자")
        mdis$V18<- recode_factor(mdis$V18, '13'="일시휴직자")
        mdis$V18<- recode_factor(mdis$V18, '21'="구직실업자")
        mdis$V18<- recode_factor(mdis$V18, '22'="비구직실업자")
        mdis$V18<- recode_factor(mdis$V18, '23'="신규실업자")
        mdis$V18<- recode_factor(mdis$V18, '24'="전직실업자")
        mdis$V18<- recode_factor(mdis$V18, '31'="비경제활동인구")
        mdis$V18<- recode_factor(mdis$V18, '32'="비경제활동인구")
        mdis$V19<- recode_factor(mdis$V19, '1'="농가")
        mdis$V19<- recode_factor(mdis$V19, '2'="비농가")
        mdis$V20<- recode_factor(mdis$V20, '1'="시부")
        mdis$V20<- recode_factor(mdis$V20, '2'="군부")

colnames(mdis) = c("가구 일련번호"
, "가구원 일련번호", "가구주관계", "성별", "연령", "배우관계", "교육정도", "수료", "활동상태", "취업여부", "1주간 구직여부", "산업", "직업", "종사상의 지위", "취업시간", "조사대상년월", "승수", "경제활동상태구분", "농가^비농가", "시부.군부")

