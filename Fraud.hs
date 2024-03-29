module Fraud where
import Account
import Data.List
import Data.Char
import Debug.Trace
import Countrylist
import Freq
import qualified Data.ByteString.Char8 as BC

lower_theshhold = 0.05

makeIO :: (Bool, Double) -> IO (Bool, Double)
makeIO x = do return x

suspicioustransaction :: [Transaction] -> Double -> String -> (Bool, Double)
suspicioustransaction transactions threshold origin_country = 
    if (genericLength transactions) < 50 then (False, threshold)
    else 
        if (combineactivity rankdaily ranktransaction rankcountry) < threshold then 
            (False, threshold)
        else (True, (threshold-0.05))
    where rankdaily = suspiciousdailytotal transactions
          ranktransaction = suspicioustransactiontotal transactions
          rankcountry = suspiciouscountry transactions origin_country


combineactivity :: Double -> Double -> Double -> Double
combineactivity daily transaction country =
  if country == 0 then (daily+transaction)/2
  else (daily+transaction+country)/3

buffer = 0.10
suspiciouscountry :: [Transaction] -> String -> Double
suspiciouscountry ((Transaction sum purchase date name country):prevtransactions) origincountry
  | country == origincountry = 0
  | notElem (map toLower country) countrylist = 1
  | otherwise = (1 - evaluatecountry country prevtransactions-buffer)


evaluatecountry country transactions = measure (tabulate (train training)) (BC.pack country)
   where training = ((listcountriesasbytestrings transactions) <> (BC.pack countrystring))


suspiciousdailytotal :: [Transaction] -> Double
suspiciousdailytotal ((Transaction sum purchase date name country):prevtransactions)
    | (sum < prevaverage || not purchase) = 0+buffer
    | sum < (prevaverage + (0.5*prevstdev)) = 0.33
    | sum < (prevaverage + (1*prevstdev)) = 0.66
    | sum < (prevaverage + (1.5*prevstdev)) = 0.85
    | sum < (prevaverage + (2*prevstdev)) = 0.95
    | sum < (prevaverage + (2.5*prevstdev)) = 0.98
    | otherwise = 0.99
    where prevstdev = stdev $ listdailysums (getpurchasetransactions prevtransactions)
          prevaverage = getdailytotalaverage prevtransactions

suspicioustransactiontotal :: [Transaction] -> Double
suspicioustransactiontotal ((Transaction sum purchase date name country):prevtransactions)
    | (sum < prevaverage || not purchase) = 0
    | sum < (prevaverage + (0.5*prevstdev)) = 0.33
    | sum < (prevaverage + (1*prevstdev)) = 0.66
    | sum < (prevaverage + (1.5*prevstdev)) = 0.85
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
