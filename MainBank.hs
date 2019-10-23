module MainBank where
import Control.Monad (when)
import Debug.Trace
import Data.Time as Date
import Data.Char
import Account
import Fraud

makelow lst = [toLower x | x <- lst]

suspiciousthreshhold = 5

-- try: startAccount A
startAccount :: Account -> IO Account
startAccount account = do
    comprimised <- evaluateAccount account
    if comprimised then return account
    else do
    putStrLn("\n")
    summary <- (summarizeaccount account) 
    putStrLn("\n")
    putStrLn("Press t to make a transaction")
    putStrLn("Press d to get deactivate account")
    action <- getLine
    if (action == "t") 
        then do
        newaccount <- maketransaction account
        evaluatedaccount <- evaluatelatesttransaction newaccount
        startAccount evaluatedaccount
    else if (action == "d") 
        then do return account    
    else do startAccount account

evaluateAccount :: Account -> IO Bool
evaluateAccount (Account balance transactions (sus, threshold) origin_country) = 
    do 
        if (sus > suspiciousthreshhold) then 
            do 
                putStrLn("Account is determined to have been comprimised and will be deactivated.")
                return True
        else return False

evaluatelatesttransaction :: Account -> IO Account
evaluatelatesttransaction (Account balance ((Transaction sum purchase day name country):t) (sus, threshold) origin_country) =
    do 
        let transactions = ((Transaction sum purchase day name country):t)
        if purchase /= True then return (Account balance transactions (sus, threshold) origin_country)
        else do
            latesteval <- makeIO $ (suspicioustransaction transactions threshold origin_country)
            if (fst latesteval == True) then 
                do 
                    putStrLn("The last transaction was deemed to be suspicious. The theshhold for fraudulent activity has been lowered for account protection.")
                    return (Account balance transactions ((sus+1), (snd latesteval)) origin_country)
            else return (Account balance transactions (sus, threshold) origin_country)

summarizeaccount :: Account -> IO ()
summarizeaccount (Account balance transactions (sus, threshold) origin_country) = 
    do
        putStr("Current Balence: ")
        putStrLn(show balance)
        putStr("Amount spent today: ")
        today <- getDate
        putStrLn(show (getpurchasesondate transactions today))
        putStr("Average spent per day: ")
        putStrLn(take 5 (show (getdailytotalaverage transactions)))
        putStr("Average transaction cost: ")
        putStrLn(take 5(show (getpurchasesaverage transactions)))
        return ()

maketransaction :: Account -> IO Account
maketransaction (Account balance transactions (sus, threshold) origin_country) = 
    do
        transactiontype <- depositorpurchase
        transactioncountry <- gettransactioncountry
        transactionsum <- gettransactionsum
        affordable <- (affordabletransction transactiontype transactionsum balance)
        transactiondate <- getDate
        if (not affordable) then do 
            let denied = (Transaction 0 False transactiondate "Purchase Denied" transactioncountry)
            return (Account balance (denied:transactions) (sus, threshold) origin_country)
        else do
        transactionname <- gettransactionname     
        let transaction = (Transaction transactionsum transactiontype transactiondate transactionname transactioncountry) 
        if transactiontype==True then return (Account (balance-transactionsum) (transaction:transactions) (sus, threshold) origin_country)
        else return (Account (balance+transactionsum) (transaction:transactions) (sus, threshold) origin_country)

affordabletransction :: Ord a => Bool -> a -> a -> IO Bool
affordabletransction purchase sum balance = 
    if (balance < sum && purchase)
  then do
    putStrLn "Insufficient funds, transaction denied"
    return False
  else return True 

depositorpurchase :: IO Bool
depositorpurchase = 
    do 
       putStrLn("Press p to make a purchase")
       putStrLn("Press d to make a deposit")
       transactiontype <- getLine
       if (transactiontype == "p") then 
        return True
       else if (transactiontype == "d") then return False 
       else do depositorpurchase

gettransactionsum :: IO Double
gettransactionsum = 
    do
        putStrLn("Please input the transaction sum")
        amount <- getLineFixed
        let converted = (converttoDouble amount)
        if (converted /= -1) then return converted 
        else do gettransactionsum

digits = "0123456789"

converttoDouble :: [Char] -> Double
converttoDouble string = 
    if filtered /= [] then read filtered :: Double
    else -1
    where filtered = filter (\x-> x `elem` digits) string

gettransactioncountry ::  IO String
gettransactioncountry = 
    do
        putStrLn("Please input the transaction country")
        country <- getLineFixed
        return country

gettransactionname::  IO String
gettransactionname = 
    do
        putStrLn("Please give a name for the transaction")
        name <- getLineFixed
        return name

getDate :: IO (Integer, Int, Int)
getDate = do
    date <- getCurrentTime
    return (toGregorian $ utctDay date)

getLineFixed =
   do
     line <- getLine
     return (fixdel line)


fixdel st
   | '\DEL' `elem` st = fixdel (remdel st)
   | otherwise = st
remdel ('\DEL':r) = r
remdel (a:'\DEL':r) = r
remdel (a:r) = a: remdel r
