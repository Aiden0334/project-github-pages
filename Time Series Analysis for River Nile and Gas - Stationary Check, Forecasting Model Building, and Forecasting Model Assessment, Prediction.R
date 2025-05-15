## ARIMA Modeling
# 시계열 데이터의 정상성 평가

Nile # Data set (1871 - 1970 나일강 유량 체크.)
plot(Nile, col="darkviolet", lwd=2,
     xlab="Year", ylab = "Flow",
     main = "Flow of the River Nile")

# ADF Test >  정상성 평가
library(tseries)
adf.test(Nile) # P-value 값 너무 큼. > 비정상적인 시계열. > 차분화


library(forecast)
ndiffs(Nile) # 시계열이 정상적이려면 1회의 차분이 필요함. 


# 시계열 데이터에 1회의 차분을 적용해보겠음! 
dNile <- diff(Nile)
# 차분한 데이터의 시계열 그래프 그려보겠음. 
plot(dNile, col="salmon", lwd=2,
     xlab="Year", ylab = "Differenced Flow",
     main = "Flow of the River Nile: Differenced") # 본래의 데이터와 비교하면 트렌드가 사라졌기 때문에 

                                                   # 정상성 확인 가능했음! 
# ADF 검정을 통해서 정상성을 통계적으로 유의한지 확인해보겠음. 
adf.test(dNile) # 차분한 시계열 데이터의 p-value 값이 프린트된 p-value 값보다 작기 때문에 
                # 통계적으로 정상성을 가지고 있다고 판단할 수 있음. 


# 차분한 시계열 데이터가 정상성을 충족하기 때문에 ARIMA 예측 모델을 생성할 수 있음. 
# ARIMA(p,d,q) > 최대의 성능을 가지는 p,d,q를 가지면 됨. 
# d = 1 (시계열이 정상적이려면 1회의 차분이 필요.)
# p, q는 acf, pacf 도표를 통해서 얻을 수 있음! 

# 1.일단 차분 시계열에 대한 acf, pacf 도표를 생성한다. 
# 예측 모델 생성
acf(dNile, lwd=2,
    main="Autocorrelation for the River Nile")
pacf(dNile, lwd=2,
     main="Partial Autocorrelation for the River Nile")

### PACF를 통해서 p를 결정하는데 이것은 lag이 0이 되기 직전의 lag level = p = 1.
### ACF를 통해서 q를 결정함. q=2. 
### 예측모델은 ARIMA 모델을 사용하여 생성할 수 있음. 시작하겠음. 
Nile.arima <- arima(Nile, order=c(0, 1, 1)) # MA(1)을 설정했고 Original Time Series Data를 사용했다는 점! 
Nile.arima
accuracy(Nile.arima) # 잔차가 0이 되어야하고 얘네가 정규분포를 따르며 이는 잔차 간 상관관계가 없어야하며 
                    # 값이 작을수록 예측하기 쉬움. 

### 예측 모델 평가와 예측
### 잔차가 정규분포를 따르는지 확인하는 과정. 
qqnorm(Nile.arima$residuals, pch=21, col="black", 
       bg="gold", main="QQ plot of Residuals")
qqline(Nile.arima$residuals, col="royalblue", lwd=2)

# Box test를 이용하면 자기상관이 0이라는 귀무가설을 검정할 수 있음. 
Box.test(Nile.arima$residuals, type="Ljung-Box")

# 유의 수준 0.05에서 통계적으로 유의하지 않기 때문에 잔차들의 상관관계가 0이라는 귀무 가설을 기각 불가. 
# 잔차들의 자기상관은 0과 다르다고 할 수 없음. 
# 따라서 현재 생성된 예측 모델은 시계열 데이터를 잘 적합시켰다고 볼 수 있음! 
# 이제 예측 모델을 이용해서 미래의 값을 예측할 수 있음! > Forecast 함수 이용.
Nile.arima.pred <- forecast(Nile.arima, h=5) # 향후 5년간의 나일강 유량 예측.
Nile.arima.pred # 80% 및 95% 신뢰구간으로 출력이 됨. 

# 예측 결과를 그래프로 나타내기. 
plot(Nile.arima.pred, col="red", lwd=2, 
     flty=1, flwd=3,
     fcol="royalblue", shadecols=c("mistyrose", "salmon"),
     xlab = "Year", ylab = "Flow",
     main = "Forecast for flow of the River Nile")

# Gas 데이터 셋을 통해서 arima 예측 모델을 자동적으로 생성해보겠음. 
gas
gas.arima <- auto.arima(gas) # 최적의 모델을 예측 모델로 선정함.
gas.arima

### ARIMA(2,1,1)(0,1,1)[12] 
### 뒷부분 4개의 숫자는 계절 부분을 나타냄.
### 가운데의 3개의 숫자 - 비계절에 대한 p,d,q를 나타내고. 
### 12의 값은 연간 관측값의 계수임.
### ar1, ar2, ma1 - 비계절 부분에서 3개의 계수가 측정이 되었고 
### sma1 - 계절의 부분에서 1개의 계수가 측정이 됨. 

arima(gas, order=c(2,1,1), 
      seasonal=list(order=c(0,1,1), period=12))

forecast(gas.arima, h=5*12) 
# 결과 그래프로 표현.
plot(forecast(gas.arima, h=5*12), col="red", lwd=2, 
     flty=1, flwd=3,  # flty, flwd, fcol은 예측선에 대한 선의 유형, 선의 굴기, 선의 색상을 나타냄.
     fcol="orangered", shadecols=c("lavender", "skyblue"), # shadecols는 신뢰구간에 대한 색상 지정.
     xlab = "Year", ylab = "Monthly Production",
     main = "Forecast for Australian Monthly Gas Production")





















