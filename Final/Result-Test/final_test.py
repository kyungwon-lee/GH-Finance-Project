from rpy2.robjects import r
import rpy2.robjects as robjects
import sys
from datetime import datetime

import pandas as pd
df = pd.read_csv("result_survival.csv", encoding="UTF-8")

for i in range(len(df)):
    df.loc[i, 'establish_date_str']=str(df.loc[i,'establish_date']).split('.')[0]

df = df[['ked_code', 'business_number', 'establish_date_str', 'share_holders_count', 'accounts_receivable', 'suspension_date']]
df['bankruptcy']=df['suspension_date'].notnull()
df=df[['ked_code', 'business_number', 'establish_date_str', 'share_holders_count', 'accounts_receivable', 'bankruptcy']]
df=df.dropna(axis=0)
result = pd.DataFrame(columns=['ked_code', 'predict', 'bankruptcy'])

zipped_df = zip(range(len(df)),df['ked_code'],df['establish_date_str'], df['share_holders_count'], df['accounts_receivable'], df['bankruptcy'])

# r에서의 모듈 임포트
r("library(KMsurv)")
r("library(survival)")
r("library(ggplot2)")
r("library(survminer)")
r("library(car)")
r("library(maxstat)")
r("library(party)")
for company in (list(zipped_df)):
    share_holders_count = company[3]
    accounts_receivable = company[4]
    established_date_string = company[2]
    # 쿼리가 완료된 csv 파일 로드
    r("df = read.csv('result_survival.csv',header=T)")

    # 전처리
    r("df[,\"failure\"] = ifelse(is.na(df$suspension_date),0,1)")
    r("df$suspension_date = ifelse(is.na(df$suspension_date),\"20200205\",df$suspension_date)")
    r("df$establish_date=as.character(df$establish_date)")
    r("df[,\"time\"] = as.integer(as.Date(df$suspension_date,format=\"%Y%m%d\")-as.Date(df$establish_date,format = \"%Y%m%d\"))")
    r("data=df[,c(\"ked_code\",\"business_number\",\"debt\",\"accounts_receivable\",\"avg_credit_payment_ratio\",\"share_holders_count\",\"non_current_liabilities\",\"operating_profit\",\"failure\",\"time\")]")
    r("data<-na.omit(data)")
    r("data2 <- df[,c(\"ked_code\",\"business_number\",\"accounts_receivable\",\"share_holders_count\",\"failure\",\"time\")]")
    r("data2 <- na.omit(data2)")
    r("test<-coxph(Surv(time,failure)~share_holders_count+accounts_receivable,data=data2)")
    r("predict<-survfit(test,c(share_holders_count=" + str(share_holders_count) + ",accounts_receivable=" + str(accounts_receivable) + "))")


    predict = robjects.r['predict']
    today = datetime.today()
    established_date = datetime.strptime(established_date_string, '%Y%m%d')
    operation_days = (today-established_date).days

    times = predict[1]
    survs = predict[5]

    min_diff = sys.maxsize
    min_index = -1
    surv_probability="측정 불가"
    for i in range(len(times)):
        diff = abs(times[i]-operation_days)
        if(diff<min_diff):
            min_diff = diff
            min_index = i
    if(min_index==-1):
        continue
    else:
        surv_probability = survs[min_index]
    row = [company[1], surv_probability, company[5]]
    result.loc[len(result)]=row

def hit_ratio(result):
    predict_critic = result['predict'].mean()
    predict = result['predict']
    bankruptcy = result['bankruptcy']
    
    true_true = 0
    true_false = 0
    false_true = 0
    false_false = 0
    
    for i in range(len(predict)):
        # 이게 파산한다고 예측한 경우. 생존 확률이 평균보다 작다.
        if(predict[i]<predict_critic):
            # 근데 파산한 경우
            if(bankruptcy[i]==True):
                true_true = true_true + 1
            # 근데 파산하지 않은 경우
            else:
                false_true = false_true + 1
        # 이게 파산하지 않는다고 예측한 경우
        else:
            if(bankruptcy[i]==True):
                true_false = true_false + 1
            else:
                false_false = false_false + 1
    
    hit_ratio_result = (true_true+false_false)/len(result)
    
    print("--------------정확도 측정 결과--------------")
    print("      예측          True        False")
    print("실제")
    print("True               " + str(true_true) + "          " + str(true_false))
    print("False              " + str(false_true) + "          " + str(false_false))
    print("hit ratio는 " + str(true_true+false_false) + " / " + str(len(result)) + " = " + str(hit_ratio_result) + "% 입니다.")

hit_ratio(result)
