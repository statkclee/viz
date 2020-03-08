# 다트 파이썬 라이브러리
import dart_fss as dart

# 인증키
api_key='XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX'
dart.set_api_key(api_key=api_key)

# 빅4 이름으로 찾기 ( 리스트 반환 )
corp_list = dart.get_corp_list()

samsung = corp_list.find_by_corp_name('삼성전자', exactly=True)[0]

pwc = corp_list.find_by_corp_name('삼일회계법인', exactly=True)[0]
kpmg = corp_list.find_by_corp_name('삼정회계법인', exactly=True)[0]
ey = corp_list.find_by_corp_name('한영회계법인', exactly=True)[0]
deloitte = corp_list.find_by_corp_name('안진회계법인', exactly=True)[0]

# 빅4 재무제표 

samsung_fs = samsung.extract_fs(bgn_de='20180101')

samsung_fs['is']

# 저장
filename = 'samsung'
path = 'c:/docs/viz/data/dart'
samsung_fs.save(path=path)
