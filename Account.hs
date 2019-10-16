module Account where
import Debug.Trace

transactionlist = [transaction0, transaction1, transaction2, transaction3, transaction4, transaction5]

-- Account
-- This keeps track of the current balance in account. This would be a Double
-- The previous transactions - [Transactions]
-- the number of suspicious actions performed and current fraud threshold (Int, Double)
-- Country which would be a string
data Account = Account Double [Transaction] (Int, Double) String deriving (Show)

-- Transaction
-- This data type must contain an amount of money Double
-- Type of transaction; purchasing or adding money Bool True if taking money away
-- date of transaction (Integer, Int, Int)
-- A String denoting the name of the transaction.
-- A String denoting the country where the transaction took place.
data Transaction = Transaction Double Bool (Integer, Int, Int) String String deriving (Show)

transaction0 = Transaction 100 False (2019, 10, 15) "Paycheck" "Canada"
transaction1 = Transaction 2.65 True (2019, 10, 14) "Coffee" "Canada"
transaction2 = Transaction 20.95 True (2019, 10, 13) "Earls" "Canada"
transaction3 = Transaction 37.72 True (2019, 10, 13) "Safeway" "Canada"
transaction4 = Transaction 14.38 True (2019, 10, 13) "Starbucks" "Canada"
transaction5 = Transaction 84.33 True (2019, 10, 12) "Shell" "Canada"

a = Account 100 transactionlist (1, 0.8) "Canada"

sumpurchases :: [Transaction] -> Double
sumpurchases [] = 0
sumpurchases ((Transaction sum purchase date name country):t) 
   | purchase = sum + (sumpurchases t)
   | otherwise = (sumpurchases t)

getpurchasetransactions :: [Transaction] -> [Transaction]
getpurchasetransactions transactions = filter (\(Transaction balence purchase date name country)->purchase) transactions

listpurchases :: [Transaction] -> [Double]
listpurchases [] = []
listpurchases ((Transaction sum purchase date name country):t) 
   | purchase = sum:(listpurchases t)
   | otherwise = (listpurchases t)

listdailysums  :: [Transaction] -> [Double]
listdailysums  [] = []
listdailysums  ((Transaction balence purchase date name country):transactions) = 
    reverse (sumDays date [balence] transactions)

-- assume transactions are sorted in order of date
sumDays :: (Integer, Int, Int) -> [Double] -> [Transaction] -> [Double]
sumDays currDay daylist [] = daylist
sumDays currDay (day:t) ((Transaction balence purchase date name country):transactions) 
   | currDay == date = sumDays currDay ((day+balence):t) transactions
   | otherwise = sumDays date (balence:day:t) transactions

getpurchasesondate :: [Transaction] -> (Integer, Int, Int) -> Double
getpurchasesondate [] wanteddate = 0
getpurchasesondate ((Transaction balence purchase date name country):transactions) wanteddate  
    | (date == wanteddate && purchase) = balence + getpurchasesondate transactions wanteddate
    | otherwise = getpurchasesondate transactions wanteddate
