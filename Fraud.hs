module Fraud where
import Account
import Data.List
import Debug.Trace

lower_theshhold = 0.05

--suspicioustransaction :: [Transaction] Double String -> (Bool, Double)
suspicioustransaction transactions threshold origin_country = 
    if (genericLength transactions) < 50 then (False, threshold)
    else 
        if (combineactivity rankdaily suspicioustransaction suspiciousname suspiciouscountry) < threshold then 
            (False, threshold)
        else (True, (threshold-0.05))
    where rankdaily = suspiciousdailytotal transactions
          ranktransaction = suspicioustransaction transactions
          rankname = suspiciousname transactions
          rankcounrty = suspiciouscountry transactions

combineactivity daily transaction name country = 0

suspiciouscountry x = 8
suspiciousname x = 9

suspiciousdailytotal :: [Transaction] -> Double
suspiciousdailytotal ((Transaction sum purchase date name country):prevtransactions)
    | (sum < prevaverage || not purchase) = 1
    | sum < (prevaverage + (0.5*prevstdev)) = 0.33
    | sum < (prevaverage + (1*prevstdev)) = 0.66
    | sum < (prevaverage + (1.5*prevstdev)) = traceShow(sum) $ 0.85
    | sum < (prevaverage + (2*prevstdev)) = 0.95
    | sum < (prevaverage + (2.5*prevstdev)) = 0.98
    | otherwise = 0.99
    where prevstdev = stdev $ listdailysums (getpurchasetransactions prevtransactions)
          prevaverage = getdailytotalaverage prevtransactions

suspicioustransactiontotal :: [Transaction] -> Double
suspicioustransactiontotal ((Transaction sum purchase date name country):prevtransactions)
    | (sum < prevaverage || not purchase) = 1
    | sum < (prevaverage + (0.5*prevstdev)) = 0.33
    | sum < (prevaverage + (1*prevstdev)) = 0.66
    | sum < (prevaverage + (1.5*prevstdev)) = traceShow(sum) $ 0.85
    | sum < (prevaverage + (2*prevstdev)) = 0.95
    | sum < (prevaverage + (2.5*prevstdev)) = 0.98
    | otherwise = 0.99
    where prevstdev = stdev $ listpurchases (getpurchasetransactions prevtransactions)
          prevaverage = getpurchasesaverage prevtransactions

stdev :: Floating c => [c] -> c
stdev lst = sqrt . average . map ((^2) . (-) avglst) $ lst
           where average = (/) <$> sum <*> realToFrac . length
                 avglst     = average lst

getpurchasesaverage :: [Transaction] -> Double
getpurchasesaverage [] = 0
getpurchasesaverage transactions = (sumpurchases transactions)/(genericLength (getpurchasetransactions transactions))

getdailytotalaverage :: [Transaction] -> Double
getdailytotalaverage [] = 0
getdailytotalaverage transactions = (sum dailylist)/(genericLength dailylist)
    where dailylist = listdailysums (getpurchasetransactions transactions)
