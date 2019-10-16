module Account where
import Debug.Trace

transaction1 = Transaction 2.65 True (2019, 10, 14) "Coffee" "Canada"
transaction2 = Transaction 20.95 True (2019, 10, 13) "Earls" "Canada"
transaction3 = Transaction 37.72 True (2019, 10, 13) "Safeway" "Canada"
transaction4 = Transaction 14.38 True (2019, 10, 13) "Starbucks" "Canada"
transaction5 = Transaction 84.33 True (2019, 10, 12) "Shell" "Canada"

transactionlist = [transaction1, transaction2, transaction3, transaction4, transaction5]

data Account = Account Double [Transaction] (Int, Double) String deriving (Show)

-- A data type for a possible bank transaction
-- Transaction
-- This data type must contain an amount of money Double
-- Type of transaction; purchasing or adding money Bool True if taking money away
-- date of transaction (Integer, Int, Int)
-- A String denoting the name of the transaction.
-- A String denoting the country where the transaction took place.
data Transaction = Transaction Double Bool (Integer, Int, Int) String String deriving (Show)

sumpurchases :: [Transaction] -> Double
sumpurchases [] = 0
sumpurchases ((Transaction sum purchase date name country):t) 
   | purchase = sum + (sumpurchases t)
   | otherwise = (sumpurchases t)

getpurchasetransactions transactions = filter (\(Transaction balence purchase date name country)->purchase) transactions

createdailysumlist :: [Transaction] -> [Double]
createdailysumlist [] = []
createdailysumlist ((Transaction balence purchase date name country):transactions) = 
	sumDays date [balence] transactions

-- assume transactions are sorted in order of date
sumDays currDay daylist [] = daylist
sumDays currDay (day:t) ((Transaction balence purchase date name country):transactions) 
   | currDay == date = sumDays currDay ((day+balence):t) transactions
   | otherwise = sumDays date (balence:day:t) transactions
