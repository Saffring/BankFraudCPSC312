module Fraud where
import Account
import Data.List

--suspiciousdailytotal


--suspicioustransactiontotal


getpurchasesaverage :: [Transaction] -> Double
getpurchasesaverage [] = 0
getpurchasesaverage transactions = (sumpurchases transactions)/(genericLength transactions)

getdailytotalaverage :: [Transaction] -> Double
getdailytotalaverage [] = 0
getdailytotalaverage transactions = (sum dailylist)/(genericLength dailylist)
    where dailylist = createdailysumlist transactions
