#필요한 라이브러리 불러오기#
library(dplyr)
library(ggplot2)
library(psych)    #자료의 요약을 위한 패키지. `describe()` 함수의 사용을 위해 불러옴.
library(corrplot) #변수들간의 상관계수를 계산하여 다양한 형태의 그림으로 시각화 시킬 수 있는 패키지

# autoparts1.csv는 
# 자동차 부품의 생산데이터로서 공정변수들과 함께 생산된 oil gasket의 탕구 두께가 추출되어 기록된 자료이다.
car <- read.csv("autoparts1.csv", header = TRUE)
head(car)
str(car)
summary(car)

#생산품 품질에 주된 영향을 미치는 공정변수 및 그 연관성 등을 파악하여 본다.
# 반응변수의 분포
ggplot(car,aes(x=car$c_thickness))+
  geom_histogram()+
  geom_vline(xintercept=c(21,27),color="red")
car_c<-as.factor((car$c_thickness > 27)+(car$c_thickness < 21))
table(car_c)

######자료의 요약#######
###car0의 데이터 셋을 만들어 분석에 필요한 데이터만을 선택한다. 

#1. 생산품의 품질에 주된 영향을 미치는 공정변수를 알아보기 위해
#   mold가 '양산'인 데이터만 분석한다.
car0<-filter(car, mold == "양산")
car0$mold <- NULL #사용한 column은 삭제한다.

#2. 'prod'와 'prod_name' column은 factor가 1 level이기 때문에 분석에 필요없다고 판단하였다.
car0$prod <- NULL
car0$prod_name <- NULL

#3. 변수 시각화를 위해 돕기 위한 columns인 'prod_date', 'degree'의 자료형을 변환시켜준다. 
car0$prod_date <- as.Date(car0$prod_date, format = "%Y-%m-%d") #날짜형식 변환(factor -> Date)
table(car0$prod_no)

##c_thickness//prod_date와 prod_no의 관계
ggplot(car0, aes(x=prod_date, y=c_thickness, color = prod_no))+
  geom_point()

# 제품번호 45231-3B400에 해당하는 oil gasket에 대한 분석을 실시한다.
# 생산날짜와 데이터의 분포를 기준으로 선택하였음.
car0 <- car0[car0$prod_no == "45231-3B400",]

#4. 반응 변수인 탕구두께(c_thickness)중 자료의 범위를 크게 벗어나는 점들(10 미만, 50 초과)이 탐지됨.   
#   이는 명백한 불량으로 볼 수 있으며 불량이 나타나는 프로세스 분석시 의미있는 정보를 제공할 수도 있으나,   
#   통계적 모형화를 실시할 때 지나치게 큰 영향력을 발휘할 수 있다고 판단되므로 제거하고 분석을 실시함. 
summary(car0$c_thickness)
car0 <- car0[(car0$c_thickness > 10)*(car0$c_thickness < 50) ==1,]

## A 변수들의 분포 파악  
# 변수들의 분포의 특성을 파악하기 위해 히스토그램과 상자그림등을 활용한다. 
#1. fix_time
# 고정시간(fix_time)에 대한 히스토그램을 보면, 대부분이 80~90 사이에 몰려 있음을 알 수 있다.  
ggplot(car0, aes(x=fix_time)) + 
  geom_histogram(, colour = "black")
boxplot(car0$fix_time)

# a속도, b속도, 실압력, 하중시간, 고압시간 등도 값이 특정구간에 집중되는 경향이 있으나 
# 일부 자료들은 분포의 중심에서 떨어져 있다.
# 특히 대부분 공정변수의 값들이 크게 두 그룹으로 분리되어 있는 것처럼 보인다.
#2. a_speed
ggplot(car0, aes(x=a_speed)) + 
  geom_histogram(, colour = "black")
boxplot(car0$a_speed)

#3. b_speed
ggplot(car0, aes(x=b_speed)) +
  geom_histogram(, colour = "black")
boxplot(car0$b_speed)

#4. mpa
ggplot(car0, aes(x=mpa)) +
  geom_histogram(, colour = "black")
boxplot(car0$mpa)

#5.load_time
ggplot(car0, aes(x=load_time)) +
  geom_histogram(, colour = "black")
boxplot(car0$load_time)

#6. highpressure_time
ggplot(car0, aes(x=highpressure_time)) +
  geom_histogram(, colour = "black")
boxplot(car0$highpressure_time)

#7. c_thickness
# 평균 24.24이며, 히스토그램에서 비교적 두꺼운 제품이 많이 발생하는 것을 볼 수 있다.   
# 왜도(skewness)가 1.71로 양수의 값을 가지므로, 분포가 대칭이 아니라 오른쪽으로 약간 긴 꼬리를 가질 것이다.
# 즉, 불량 발생시 두께가 얇은 쪽 보다는 두꺼운 쪽으로 발생할 가능성이 높다고 여겨진다.
describe(car0$c_thickness)
ggplot(car0, aes(x=c_thickness)) + geom_histogram(, fill="blue", colour = "black")

## A 공정변수의 영향력 분석 - 각 공정변수의 수준에 따른 탕구두께의 분포  
# 공정변수들이 탕구두께에 미치는 영향을 파악하기 위해 간단한 시각화를 통해 확인하여 본다.
# 본 분석에서는 탕구두께에 영향을 줄 것으로 생각되는 공정변수로 
# 고정시간(fix_time), a 속도(a_speed), b 속도(b_speed), 실압력(mpa), 하중시간(load_time), 고압시간(highpressure_time)를 고려한다. 
car1 <- car0[,c(5,6,7,8,9,10,11,12,13,14)]
head(car1)
summary(car1)

ggplot(car0, aes(x=fix_time, y=c_thickness)) + 
  geom_jitter(aes(colour = fix_time), position = position_jitter(width = .3), alpha = 0.5) + 
  geom_boxplot(aes(fill=fix_time),outlier.colour = NA) + 
  geom_hline(yintercept=c(21,27),color="red") + ggtitle("탕구두께 vs 고정시간")

# 변수들은 모두 연속형변수이다. 
# 각 변수들을 임의로 3개의 구간(low,middle,high = 1,2,3)으로 나누어 각 구간별로 탕구두께에 대한 상자그림을 출력한다.
# as.factor()' : 각 변수들을 3개의 범주를 가지는 범주형 변수로 변환
# geom_jitter() : 상자 바깥에 있는 점들을 겹치지 않게 흩뿌려서, 점들의 밀도를 보다 시각적으로 쉽게 파악하기 위해 사용.

#1. fix_time VS c_thickness
## 고정시간(fix_time) 이 보통 수준보다 길면, 탕구두께의 변동도 커진다.
car1$fixtime_bin <- as.factor((car1$fix_time > 81.3) + (car1$fix_time > 81.1) + 1)
ggplot(car1, aes(x=fixtime_bin,c_thickness)) + 
  geom_jitter(aes(colour = fixtime_bin), position = position_jitter(width = .3), alpha = 0.5) + 
  geom_boxplot(aes(fill=fixtime_bin),outlier.colour = NA) + 
  geom_hline(yintercept=c(21,27),color="red") + ggtitle("탕구두께 vs 고정시간")

#2. a_speed VS c_thickness
##  a시간이 낮은 수준이면, 탕구두께의 변동도 커진다.
car1$as_bin <- as.factor((car1$a_speed > 0.659) + (car1$a_speed > 0.667) + 1)
ggplot(car1, aes(x=as_bin,c_thickness)) + 
  geom_jitter(aes(colour = as_bin), position = position_jitter(width = .3), alpha = 0.5) + 
  geom_boxplot(aes(fill=as_bin),outlier.colour = NA) + 
  geom_hline(yintercept=c(21,27),color="red") + ggtitle("탕구두께 vs a속도")

#3. b_speed VS c_thickness
##  b시간은 뚜렷한 경향성은 관측되지 않는다.
car1$bs_bin <- as.factor((car1$b_speed > 1.632) + (car1$b_speed > 1.732) + 1)
ggplot(car1, aes(x=bs_bin,c_thickness)) + 
  geom_jitter(aes(colour = bs_bin), position = position_jitter(width = .3), alpha = 0.5) + 
  geom_boxplot(aes(fill=bs_bin),outlier.colour = NA) + 
  geom_hline(yintercept=c(21,27),color="red") + ggtitle("탕구두께 vs b속도")

#4. mpa VS c_thickness
##  실압력이 낮은 수준이면, 탕구두께가 두꺼워지는 경향이 있고.
##  실압력이 높은 수준이면, 탕구두께는 얇아지는 경향을 보인다. 
car1$mpa_bin <- as.factor((car1$mpa > 76.4) + (car1$mpa > 75.4) + 1)
ggplot(car1, aes(x=mpa_bin,c_thickness)) + 
  geom_jitter(aes(colour = mpa_bin), position = position_jitter(width = .3), alpha = 0.5) + 
  geom_boxplot(aes(fill=mpa_bin),outlier.colour = NA) + 
  geom_hline(yintercept=c(21,27),color="red") + ggtitle("탕구두께 vs 실압력")

#5. loadtime VS c_thickness
##  하중시간이 길수록 두께가 전반적으로 얇아지는 경향이 보이며, 
##  특히 하중시간이 가장 높은 수준에 속할 때는 두께의 변동이 매우 크게 증가하는 것으로 보인다.
car1$loadtime_bin <- as.factor((car1$load_time > 20.1) + (car1$load_time > 20.3) + 1)
ggplot(car1, aes(x=loadtime_bin,c_thickness)) + 
  geom_jitter(aes(colour = loadtime_bin), position = position_jitter(width = .3), alpha = 0.5) + 
  geom_boxplot(aes(fill=loadtime_bin),outlier.colour = NA) + 
  geom_hline(yintercept=c(21,27),color="red") + ggtitle("탕구두께 vs 하중시간")

#6. highpressure_time VS c_thickness
##  고압시간의 경우 뚜렷한 경향성은 관측되지 않는다.
car1$hp_bin <- as.factor((car1$highpressure_time > 72) + (car1$highpressure_time > 69) + 1)
ggplot(car1, aes(x=hp_bin,c_thickness)) + 
  geom_jitter(aes(colour = hp_bin), position = position_jitter(width = .3), alpha = 0.5) + 
  geom_boxplot(aes(fill=hp_bin),outlier.colour = NA) + 
  geom_hline(yintercept=c(21,27),color="red")+ ggtitle("탕구두께 vs 고압시간")

## A 상관분석
# 회귀분석 전, 탕구두께 및 공정변수들간의 상관계수를 계산하여 선형적 연관성의 정도를 파악한다.
car1 <- car0[,c(5,6,7,11,12,13,14)]
corrplot.mixed(cor(car1),  upper = "ellipse" ,lower="number",tl.pos="lt",bg="black")
# 실압력과 a속도간의 상관관계가 가장 강하게 나타남.
# 탕구두께와 가장 강한 선형적 연관성을 가지는 변수는 a속도와 실압력으로 파악되며 둘 모두 음의 상관이다.
# a속도나 실압력이 커질수록 탕구두께는 얇아지는 경향성이 있는 것으로 파악된다. 

###### 데이터 분석_회귀분석#######
#1. 선형회귀모형(Linear Model)
## -1) 선형회귀모형의 적합 및 변수선택  
# 변수들간에 어느정도 연관성이 존재하는가를 선형회귀모형을 이용하여 
# 변수들간의 연관성 및 각 변수의 유의성을 살펴본다.  
# 다중선형회귀모형에서 필수적인 변수선택의 절차를 수행한다.  
lm1 <- lm(c_thickness ~ .,data=car1)
lm11<- lm(c_thickness ~ 1, data = car1)
summary(lm1)
summary(lm11)
# 선형회귀모형 적합결과 모든 공정변수들이 유의수준 0.05에서 유의하다.   
# 모두 탕구두께에 유의미한 영향력을 가지고 있다 하겠다.  
# 모든 변수들의 계수가 음수이므로, 공정변수들이 커질 때 탕구두께는 얇아지는 경향이 있음을 알 수 있다.
# F-통계량 및 유의확률을 확인할 때, 모형 전체도 매우 유의함을 알 수 있다.
library(MASS)
library(leaps)
# regsubsets()` 함수는 best subset selection을 수행하며, 
# 모형의 크기(변수가 포함되는 개수) 순으로 가장 좋은 모형을 출력함
vs1 <- regsubsets(c_thickness~.,data=car1)
summary(vs1)
# 변수 1개만 모형에 포함하는 경우에는 실압력(mpa)이 선택되었으므로, 가장 영향력이 큰 공정변수로 볼 수 있다. 
# 두 개의 변수만 포함하는 경우에는 실압력 및 b속도(b_speed)가 포함됨.  
# 탕구두께와 개별적인 상관계수를 계산했을 때(실압력과 a속도가 가장 큰 상관관계를 가짐) 와는 약간 다른 결과이다.

par(mfrow=c(1,3))
plot(vs1,scale="Cp")
plot(vs1,scale="adjr2")
plot(vs1,scale="bic")

vs2 <- stepAIC(lm11,direction="both", scope=list(upper=lm1,lower=lm11))
vs21 <- stepAIC(lm11,direction="forward", scope=list(upper=lm1,lower=lm11))
vs22 <- stepAIC(lm1,direction="backward")
vs2$anova
# 최적의 모형은 변수 6개를 모두 고려하는 것임.   
# Cp, 수정결정계수, bic를 기준으로 했을 때 모두 같은 결과를 준다. 

library(car)
par(mfrow=c(2,2))
plot(lm1)
vif(lm1)
# vif()는 분산팽창계수를 출력해 주는 함수임
# 분산팽창계수가 전반적으로 4를 넘지 않으므로 다중공선성이 심각하게 존재하지 않는 것으로 볼 수 있다. 
# (보통 분산팽창계수가 10 을 넘으면 심각한 것으로 보고 4~5 를 넘으면 의심해 볼 수 있는 것으로 알려져 있음.) 


## B.2 축소추정법
# 선형회귀모형에서 설명변수의 개수가 다수 존재하거나 변수들간의 상관성이 큰 경우 다중공선성 문제가 발생하며,
# 이는 추정량의 분산을 크게 하여 모형을 매우 불안정하게 만든다.  
# 축소추정법(shrinkage method)에 의해 모형의 성능을 개선할 있다.  
# 이미 모든 변수들이 유의미한 것으로 판명되었고, 다중공선성도 크게 의심할만 하지 않아 필수적인 절차이지는 않지만,
# 공부를 위해 연습해보려한다. 
library(glmnet)
set.seed(1)
ind.train <- sample(1:nrow(car1),nrow(car1)*0.7)
car.train <- car1[ind.train,]
car.test <- car1[-ind.train,]
X <- as.matrix(car.train[,1:6])
Y <- as.matrix(car.train[,7])
nX <- as.matrix(car.test[,1:6])
nY <- as.matrix(car.test[,7])

### B.2.1 능형(Ridge)회귀
# 축소추정의 조절모수를 선택하기 위해 교차검증를 실행하였다.
cv.ridge <- cv.glmnet(X,Y,alpha=0,lambda=10^seq(10,-2,length=100))
(cv.ridge)
?cv.glmnet
plot(cv.ridge)
ridge.pred <- predict(glmnet(X,Y,alpha=0,lambda=cv.ridge$lambda.min),newx=nX) # Ridge 모형적합을 위해 alpha=0 으로 설정
mean((nY - ridge.pred)^2)
coef(glmnet(X,Y,alpha=0,lambda=cv.ridge$lambda.min))

# 그림에서 볼 수 있듯이 조절모수의 값은 작은 쪽에서 형성된다.   
# 실제 선택된 값은 0.03053856다.
# 예측오차는 2.496445이다. 

### B.2.2 LASSO 
cv.lasso <- cv.glmnet(X, Y, alpha=1, lambda=10^seq(10, -2, length=100))
plot(cv.lasso)
(cv.lasso)
lasso.pred <- predict(glmnet(X,Y,alpha=1,lambda=cv.lasso$lambda.min),newx=nX)
mean((nY - lasso.pred)^2)
coef(glmnet(X,Y,alpha=1,lambda=cv.lasso$lambda.min))
# 그림에서 볼 수 있듯이 조절모수의 값은 작은 쪽에서 형성된다.   
# 실제 선택된 값은 0.01로 현재 설정한 값들 중 최소에 해당하는 값이다.  
# 추정된 값중 0은 없다. 즉, 모형에서 빠지는 변수는 없다.  
# 예측오차는 2.500823이다.  

lm.pred <- predict(lm(c_thickness~.,data=car.train),newdata=car.test[,1:6])
mean((car.test[,7] -lm.pred)^2)
#선형회귀모형(축소추정법 X)에 의한 예측오차도 계산.  
# 축소추정법을 적용하지 않은 선형회귀모형의 경우 예측오차가 2.494666이다.   
# 즉, Ridge(2.496445)나 LASSO(2.500823)에 비해 작은 값을 주었다.

# 앞에서 모두 변수가 유의하였던 점, 축소추정에 의해 변수가 모형에서 빠지지 않았던 점, 축소추정에 의해서 예측오차가 좋아지지 않았던 점, 분산팽창계수가 그리 크지 않았던 점 등을 종합하여 볼 때, 본 자료에서는 축소추정법이 필요하지 않은 것으로 보인다.  

# 다만, 앞에서 보았듯이 선형모형 가정 자체에도 의심할 만한 여지가 있으며, 공정변수들이 두 그룹으로 완전히 분리되어 나타나는 경우가 많았던 점 등을 생각할 때 선형회귀 모형도 그 성능이 매우 뛰어나다고 보기는 어렵다. 


######분류모형 (Classification) - 불량 vs 양품 분석##### 
# 탕구두께가 [21,27] 구간을 벗어나는 것은 불량이 의심된다.  
# 탕구두께가 [21,27] 구간에 속하는지 여부를 반응변수(불량품or양품)로 반응변수를 연속형이 아닌 이산형(binary)으로 변환하여 분류문제로 분석한다.
car1$failure <- as.factor((car1$c_thickness > 27)+(car1$c_thickness < 21))
car2 <- car1[,-7]
head(car2)
table(car2$failure)
car2.train <- car2[ind.train,]
car2.test <- car2[-ind.train,]

## C.1 로지스틱(Logistic) 회귀모형  
# 로지스틱 회귀모형은 선형모형의 확장된 형태이며, 불량발생확률의 logit 변환을 공정변수들의 선형결합으로 모형화한 것이다.
# 선형회귀모형과 비슷한 함수들을 이용하여 적합 및 변수선택이 가능하다.
lm2 <- glm(failure~.,data=car2.train, family=binomial)
lm22<- glm(failure~1, data=car2.train, family = binomial)
summary(lm2)

vs3 <- stepAIC(lm2,direction="both")
vs31 <- stepAIC(lm22, direction = "forward", scope=list(upper=lm2,lower=lm22))
vs33 <- stepAIC(lm2, direction = "backward")
vs31$anova
# 유의수준 0.05에서 a속도와 하중시간이 유의하지 않은 것으로 나왔다. 
# AIC에 의한 변수선택결과 a속도를 제외한 모형이 최적으로 나왔다.

library(caret)
library(ROCR)
set.seed(1)
fit.log <- predict.glm(vs3, newdata=car2.test, type="response")
(fit.log)
pred.log <- prediction(fit.log, car2.test$failure)
perf.log <- performance(pred.log, "tpr", "fpr")
plot(perf.log, colorize=T, colorkey.pos="top", print.cutoffs.at=seq(0, 1, by=0.2), text.cex=1,
     text.adj=c(-0.5, 0.5), lwd=2) #roc
auc <- performance(pred.log,"auc")
auc
a<-as.factor(1*(fit.log>0.5))
confusionMatrix(a, car2.test$failure)

# 로지스틱 모형에 의한 오분류율은 0.065
# 양품은 양품으로 대부분 잘 분류하였으나(sensitivity=0.9934), 불량품에 대한 분류성능이 떨어지는 것으로 보인다(specificity=0.4649).
# AUC값은 0.8383322이다.

Label0.log <- fit.log[car2.test$failure==0]
Label1.log <- fit.log[car2.test$failure==1]
plot(ecdf(Label0.log), col="red",main="로지스틱")
plot(ecdf(Label1.log), col="blue",add=T)


## C.2 의사결정나무 (Decision tree)  
# 로지스틱 모형과 달리 비선형모형이다.
# 공정변수들의 값이 이원화되어 있는 경우가 많았으므로 
# 로지스틱 모형보다는 의사결정나무 모형에 의한 분류성능이 더 좋을 것으로 기대해 볼 수 있다.
library(tree)
tree.car <- tree(failure~., data=car2.train)
summary(tree.car)
plot(tree.car)
text(tree.car)
# 의사결정나무 적합결과 실제 나무적합을 위해 쓰인 변수는 실압력, b속도, 고압시간, 하중시간, 고정시간이다.   
# a속도는 사용되지 않았다.
# 보통 큰 나무모형은 과적합(overfitting) 문제가 있는 것으로 알려져 있다.   
# 적절한 가지치기(pruning)를 통해 나무의 크기를 줄여 예측 정확도(0.93544)를 향상시켜 볼 것이다.

set.seed(3)
cv.car <- cv.tree(tree.car, FUN=prune.misclass)
cv.car
plot(cv.car$size,cv.car$dev, type="b")
#크기가 7인 경우 deviance가 최소가 된다. 크기가 7인 모형을 선택한다.
prune.car <- prune.misclass(tree.car, best=7)
plot(prune.car)
text(prune.car)
prune.car

# 분류를 위해 쓰인 변수는 실압력, 하중시간, b속도 세 개이다.
set.seed(3)
fit.tree <- predict(prune.car, newdata=car2.test, type="vector")
pred.tree <- prediction(fit.tree[,2], car2.test$failure)
perf.tree <- performance(pred.tree, "tpr", "fpr")
plot(perf.tree, colorize=T, colorkey.pos="top", print.cutoffs.at=seq(0,1,by=0.2), text.cex=1, text.adj=c(-0.5, 0.5), lwd=2)
perf.tree1 <- performance(pred.tree, "auc")
perf.tree1
confusionMatrix(1*(fit.tree[,2]>0.5), car2.test$failure)
# 오분류율은 0.0631이다.   
# 로지스틱 모형에 비해서, 불량의심품을 분류해 내는 성능이 다소 개선되었음을 확인할 수 있다 
# (Specificity : 0.4649 -> Specificity : 0.6228)   
# AUC는 0.8456868로 로지스틱 모형에 비해 작다.  
# 양품과 불량의심품을 구분해 내는 데에는 상대적으로 성공적이었으나 전체적인 분류성능이 매우 좋다고 할 수는 없다. 

Label0.tree <- fit.tree[,2][car2.test$failure==0]
Label1.tree <- fit.tree[,2][car2.test$failure==1]
plot(ecdf(Label0.tree),col="red",main="의사결정나무")
plot(ecdf(Label1.tree),col="blue",add=T)

### C.3.2 랜덤포레스트 (Random forest)  
library(randomForest)
set.seed(3)
rf.car <- randomForest(failure~.,data=car2.train, mtry=6)
rf.car
fit.rf <- predict(rf.car,newdata=car2.test,type="prob")
pred.rf <- prediction(fit.rf[,2], car2.test$failure)
perf.rf <- performance(pred.rf,"tpr","fpr")
plot(perf.rf, colorize=T, colorkey.pos="top", print.cutoffs.at=seq(0,1,by=0.2), text.cex=1,text.adj=c(-0.5, 0.5), lwd=2)
perf.rf1 <- performance(pred.rf,"auc")
perf.rf1
confusionMatrix(as.factor(1*(fit.rf[,2]>0.5)),car2.test$failure)
# 오분류율은 0.0641.
# AUC는 0.9410432 이다.
Label0.rf <- fit.rf[,2][car2.test$failure==0]
Label1.rf <- fit.rf[,2][car2.test$failure==1]
plot(ecdf(Label0.rf),col="red",main="랜덤포레스트")
plot(ecdf(Label1.rf),col="blue",add=T)


## C.4 신경망모형 (Neural network model)
#install.packages("RSNNS")
library(RSNNS)
set.seed(3)
Y <- decodeClassLabels(car2.train$failure)
X <- normalizeData(car2.train[,-7])
test.Y <- decodeClassLabels(car2.test$failure)
test.X <- normalizeData(car2.test[,-7])
nn.car <- mlp(X,Y,size=5,maxit=50)
fit.nn <- predict(nn.car,test.X)
pred.nn <- prediction(fit.nn[,2], car2.test$failure)
perf.nn <- performance(pred.nn,"tpr","fpr")
plot(perf.nn, colorize=T, colorkey.pos="top", print.cutoffs.at=seq(0,1,by=0.2), text.cex=1,text.adj=c(-0.5, 0.5), lwd=2)
perf.nn1 <- performance(pred.nn,"auc")
perf.nn1

library(caret)
caret::confusionMatrix(as.factor(1*(fit.nn[,2]>0.5)) ,car2.test$failure)
# 오분류율이 0.0573으로 로지스틱모형에 비해서는 좋으나 다른 모형들에 비해서는 다소 떨어진다.  
# AUC가 0.8867023으로 로지스틱 및 의사결정나무에 비해서 큰 값을 가진다. 

Label0.nn <- fit.nn[,2][car2.test$failure==0]
Label1.nn <- fit.nn[,2][car2.test$failure==1]
plot(ecdf(Label0.nn),col="red",main="신경망모형")
plot(ecdf(Label1.nn),col="blue",add=T)


## C.5 모형성능비교
par(mfrow=c(2,2))
boxplot(Label0.log,Label1.log,main="로지스틱")
boxplot(Label0.tree,Label1.tree,main="의사결정나무")
boxplot(Label0.rf,Label1.rf,main="랜덤포레스트")
boxplot(Label0.nn,Label1.nn,main="신경망모형")
# 위 그림은 양품(좌), 불량의심품(우) 그룹에서 각 모형에 의한 예측확률을 상자그림으로 표현한 것이다.   
# 좌측 상자는 0에 가깝고 우측상자는 1에 가까울 수록, 
# 또한 두 상자의 폭이 좁고 서로 멀리 떨어져 있을 수록 분류가 잘 된 것으로 볼 수 있다.  

# 약간씩의 차이가 있기는 하지만 양품은 불량확률을 0에 매우 가깝게 대부분 예측하고 있음을 알 수 있다.   
# 다만, 양품을 양품으로 가장 잘 예측하는 것은 의사결정나무모형인 것으로 보인다.  

# 불량의심품의 경우에는 모든 분류모형이 저하된 성능을 보였다.   
# 상대적으로는 로지스틱이 가장 좋지 않은 성능을 보이고 있으며, 랜덤포레스트(Random forest)가 가장 좋은 성능을 보여주고 있다.