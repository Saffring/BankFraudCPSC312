module Account where
import Debug.Trace
import Data.Char
import qualified Data.ByteString.Char8

transactionlist = [t0, t1, t2, t3, t4, t5, t6, t7, t8, t9, t10,t11, t12, t13, t14, t15, t16, t17, t18, t19, 
   t20,t21, t22, t23, t24, t25, t26, t27, t28, t29,
   t40, t41, t42, t43, t44, t45, t46, t47, t48, t49,
   t50, t51, t52, t53, t54, t55, t56, t57, t58, t59,
   t60, t61, t62, t63, t64, t65, t66, t67, t68, t69,
   t70, t71, t72, t73, t74, t75, t76, t77, t78, t79,
   t80, t81, t82, t83, t84, t85, t86, t87, t88, t89,
   t90, t91, t92, t93, t94, t95, t96, t97, t98, t99, t100]

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

t0 = Transaction 100 True (2019, 10, 15) "Paycheck" "USA"
t1 = Transaction 2.65 True (2019, 10, 14) "Coffee" "Canada"
t2 = Transaction 20.95 True (2019, 10, 13) "Earls" "Canada"
t3 = Transaction 37.72 True (2019, 10, 13) "Safeway" "Canada"
t4 = Transaction 14.38 True (2019, 10, 13) "Starbucks" "Canada"
t5 = Transaction 84.33 True (2019, 10, 12) "Shell" "Canada"
t6 = Transaction 100 False (2019, 10, 11) "Paycheck" "Canada"
t7 = Transaction 297.44 True (2019, 10, 11) "Taxes Canada" "Canada"
t8 = Transaction 45.67 True (2019, 10, 10) "Whole Foods" "Canada"
t9 = Transaction 2.89 True (2019, 10, 09) "Starbucks" "Canada"
t10 = Transaction 5.99 True (2019, 10, 09) "JJ Bean" "Canada"
t11 = Transaction 16.72 True (2019, 10, 08) "Meet in Gastown" "Canada"
t12 = Transaction 146 False (2019, 10, 08) "Paycheck" "Canada"
t13 = Transaction 2.65 True (2019, 10, 08) "Coffee" "Canada"
t14 = Transaction 85.50 True (2019, 10, 07) "VISA" "Canada"
t15 = Transaction 9.44 True (2019, 10, 07) "Nori Bento" "Canada"
t16 = Transaction 6.90 True (2019, 10, 07) "Gallery" "Canada"
t17 = Transaction 2.62 True (2019, 10, 06) "Starbucks" "Canada"
t18 = Transaction 115 True (2019, 10, 06) "Utilities" "Canada"
t19 = Transaction 100 False (2019, 10, 05) "Paycheck" "Canada"
t20 = Transaction 15.96 True (2019, 10, 05) "Pho Goodness" "Canada"
t21 = Transaction 2.65 True (2019, 10, 04) "Starbucks" "Canada"
t22 = Transaction 15.70 True (2019, 10, 03) "Cactus Club" "Canada"
t23 = Transaction 78.90 True (2019, 10, 03) "Shell" "Canada"
t24 = Transaction 50 False (2019, 10, 03) "Paycheck" "Canada"
t25 = Transaction 300 True (2019, 10, 02) "VISA" "Canada"
t26 = Transaction 18.94 True (2019, 10, 02) "Steamworks" "Canada"
t27 = Transaction 32.90 True (2019, 10, 02) "Safeway" "Canada"
t28 = Transaction 16.33 True (2019, 10, 01) "IHOP" "Canada"
t29 = Transaction 11 True (2019, 09, 30) "Petsmart" "Canada"
t30 = Transaction 119 False (2019, 09, 30) "Paycheck" "Canada"
t31 = Transaction 156 True (2019, 09, 30) "Taxes Canada" "Canada"
t32 = Transaction 22.34 True (2019, 09, 29) "Jinya Ramen Bar" "Canada"
t33 = Transaction 50.40 True (2019, 09, 28) "Sephora" "Canada"
t34 = Transaction 5.90 True (2019, 09, 28) "49 Parrallel Coffee" "Canada"
t35 = Transaction 28.77 True (2019, 09, 27) "Shell" "Canada"
t36 = Transaction 100 False (2019, 09, 26) "Paycheck" "Canada"
t37 = Transaction 870 True (2019, 09, 25) "Rent" "Canada"
t38 = Transaction 34.43 True (2019, 09, 25) "Earls" "Canada"
t39 = Transaction 56.47 True (2019, 09, 25) "Safeway" "Canada"
t40 = Transaction 41.82 True (2019, 09, 25) "Starbucks" "Canada"
t41 = Transaction 56 True (2019, 09, 24) "Shell" "Canada"
t42 = Transaction 100 False (2019, 09, 24) "Paycheck" "Canada"
t43 = Transaction 25 True (2019, 09, 24) "Taxes Canada" "Canada"
t44 = Transaction 3.43 True (2019, 09, 23) "Whole Foods" "Canada"
t45 = Transaction 3.12 True (2019, 09, 23) "Starbucks" "Canada"
t46 = Transaction 7.76 True (2019, 09, 22) "Starbucks" "Canada"
t47 = Transaction 23.24 True (2019, 09, 21) "Shell" "Canada"
t48 = Transaction 100 False (2019, 09, 19) "Paycheck" "Canada"
t49 = Transaction 2.65 True (2019, 09, 19) "Coffee" "Canada"
t50 = Transaction 15.43 True (2019, 09, 19) "Earls" "Canada"
t51 = Transaction 43.23 True (2019, 09, 18) "Safeway" "Canada"
t52 = Transaction 4.38 True (2019, 09, 17) "Starbucks" "Canada"
t53 = Transaction 18.23 True (2019, 09, 17) "Shell" "Canada"
t54 = Transaction 100 False (2019, 09, 16) "Paycheck" "Canada"
t55 = Transaction 133.99 True (2019, 09, 16) "Best Buy" "Canada"
t56 = Transaction 45.67 True (2019, 09, 15) "Whole Foods" "Canada"
t57 = Transaction 2.89 True (2019, 09, 14) "Starbucks" "Canada"
t58 = Transaction 6.87 True (2019, 09, 13) "Starbucks" "Canada"
t59 = Transaction 55.34 True (2019, 09, 13) "Shell" "Canada"
t60 = Transaction 100 False (2019, 09, 13) "Paycheck" "Canada"
t61 = Transaction 2.65 True (2019, 09, 13) "Coffee" "Canada"
t62 = Transaction 20.95 True (2019, 09, 12) "Earls" "Canada"
t63 = Transaction 37.72 True (2019, 09, 12) "Safeway" "Canada"
t64 = Transaction 27.54 True (2019, 09, 12) "Pizza Hut" "Canada"
t65 = Transaction 13.45 True (2019, 09, 11) "Belmont Hotel" "Canada"
t66 = Transaction 100 False (2019, 09, 11) "Paycheck" "Canada"
t67 = Transaction 11.90 True (2019, 09, 11) "Coloney Bar" "Canada"
t68 = Transaction 24.32 True (2019, 09, 10) "London Drugs" "Canada"
t69 = Transaction 2.89 True (2019, 09, 09) "7/11" "Canada"
t70 = Transaction 2.65 True (2019, 09, 09) "Starbucks" "Canada"
t71 = Transaction 76.23 True (2019, 09, 08) "Shell" "Canada"
t72 = Transaction 100 False (2019, 09, 06) "Paycheck" "Canada"
t73 = Transaction 300 True (2019, 09, 06) "Withdrawel" "Mexico"
t74 = Transaction 20 True (2019, 09, 05) "Withdrawel" "Mexico"
t75 = Transaction 400 True (2019, 09, 05) "Withdrawel" "Mexico"
t76 = Transaction 1.98 True (2019, 09, 05) "Starbucks" "Mexico"
t77 = Transaction 198 True (2019, 09, 04) "Shell" "Mexico"
t78 = Transaction 5.45 True (2019,  09, 04) "Airport" "Mexico"
t79 = Transaction 297.44 True (2019, 09, 03) "Taxes Canada" "USA "
t80 = Transaction 45.67 True (2019, 09, 02) "Whole Foods" "USA"
t81 = Transaction 2.89 True (2019, 09, 02) "Starbucks" "USA"
t82 = Transaction 6.87 True (2019, 09, 02) "Starbucks" "USA"
t83 = Transaction 90.34 True (2019, 09, 01) "Shell" "USA"
t84 = Transaction 5.90 True (2019, 09, 01) "Hot Pie Pizzer" "USA"
t85 = Transaction 15.98 True (2019, 08, 31) "Uber" "USA"
t86 = Transaction 190.23 True (2019, 08, 31) "Trader Joes" "USA"
t87 = Transaction 20 True (2019, 08, 31) "Air Canada" "Canada"
t88 = Transaction 20.79 True (2019, 08, 30) "BC Liquor Store" "Canada"
t89 = Transaction 870 True (2019, 08, 29) "Rent" "Canada"
t90 = Transaction 300 False (2019, 08, 29) "Paycheck" "Canada"
t91 = Transaction 297.44 True (2019, 08, 28) "Taxes Canada" "Canada"
t92 = Transaction 500 True (2019, 08, 27) "Air Canada" "Canada"
t93 = Transaction 2.89 True (2019, 08, 26) "Starbucks" "Canada"
t94 = Transaction 6.87 True (2019, 08, 25) "Starbucks" "Canada"
t95 = Transaction 11.50 True (2019, 08, 24) "Port Moody Breweries" "Canada"
t96 = Transaction 12 True (2019, 08, 24) "Port Moody Breweries" "Canada"
t97 = Transaction 15.66 True (2019, 08, 23) "Front and Company" "Canada"
t98 = Transaction 35.65 True (2019, 08, 22) "The Emerald" "Canada"
t99 = Transaction 19.87 False (2019, 08, 21) "Black Checker Cab" "Canada"
t100 = Transaction 7.08 True (2019, 08, 21) "BC Liquor Store" "Canada"

a = Account 1000 transactionlist (1, 0.8) "Canada"

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

listcountriesasbytestrings :: [Transaction] -> Data.ByteString.Char8.ByteString
listcountriesasbytestrings transactions =
  Data.ByteString.Char8.pack (listcountries transactions)
       where listcountries [] = [] 
             listcountries ((Transaction balence purchase date name country):transactions) = ([toLower x |x<-country]++" ")++(listcountries transactions)
