-- Order of SQL Operations
-- SELECT (4)
-- FROM (1)
-- WHERE (2)
-- GROUP BY (3)
-- HAVING (5)
-- ORDER BY (6)


-- Return entire contents of Customers table
SELECT *
FROM Customers;


-- Return selected columns from Customers table
SELECT 
	CustomerID, 
	CompanyName, 
	Country 
FROM Customers;


-- Filter Customers table
SELECT 
    CustomerID, 
    CompanyName, 
    Country 
FROM 
    Customers
WHERE
-- 	Country = 'Germany' OR Country = 'France'
--  	lower(Country) in ('germany','france','spain')
    Country IN ('Germany', 'France', 'Spain')
ORDER BY
    Country DESC
-- ORDER BY 3 DESC
-- Can order using index number, but better to specify variable explicitly
;

--everything after is a comment
/*everything after is a comment*/


-- Return first 10 rows. Akin to HEAD() in R
SELECT * 
FROM [Order Details] 
LIMIT 10;


-- Return revenue 
SELECT 
	sum(Quantity * UnitPrice) 
FROM [Order Details];


-- Aggregate by OrderID
SELECT 
    OrderID,
    sum(Quantity * UnitPrice) AS [Total Sales],
    avg(Quantity * UnitPrice) AS Average,
    round(Max(UnitPrice)) AS Maximum,
    round(Min(UnitPrice)) AS Minimum,
    count(orderID),
    Count(distinct OrderID)
    --Need [] only when there is space in between the words
FROM 
    [Order Details]
GROUP BY
     OrderID
;


-- Correct the spelling error
SELECT distinct OrderID 
FROM [Order Detials];


-- Number of rows in Order Details
-- Not case-sensitive <- hate that
SELECT count(*) 
FROM [Order DEtails];


-- Old Join syntax
-- Don't use this syntax > Correct it when you see it
SELECT
    Orders.OrderID,
    CustomerID, 
    LineNo
FROM
    Orders, 
    [Order Details] details -- change the name to details
WHERE
    Orders.OrderID = details.OrderID
;


-- ANSI standard Join syntax
-- same as above
SELECT
    Orders.OrderID,
    CustomerID, 
    Quantity
FROM
-- USING employed when Join keys are given same variable name in both tables
-- 	Orders JOIN [Order Details] USING (OrderID)
    Orders 
-- ON employed when Join keys are given different variable names in both tables
-- Or when using Hive Query Language
JOIN 
	[Order Details] 
ON (Orders.OrderID = [Order Details].OrderID)
;


-- Can specify implicit GROUP BY
-- But don't do that
-- Explicitly GROUP BY because other (ahem - better) SQL editors will not work this way
-- So you should learn how to do it correctly the first time
SELECT
    Orders.OrderID,
    cust.CustomerID,
    cust.CompanyName,
    sum(details.Quantity * details.UnitPrice) AS [Total Sale]
FROM
    Orders, 
    Customers cust,
    [Order Details] details
GROUP BY
    Orders.OrderID
;
    

-- Join 3 tables
-- Executes first join resulting in a temporary table
-- And then executes the second join to that temporary table
-- So any rows not in common between Orders and Customers will not be returned for a potential join to Order Details
SELECT
    Orders.OrderID, 
    cust.CompanyName, 
    sum(details.UnitPrice * details.Quantity)
FROM
    Orders 
JOIN 
	Customers cust 
USING (CustomerID)
JOIN 
	[Order Details] details 
USING (OrderID)
GROUP BY
    details.[OrderID],
    cust.CompanyName
;


-- Subquery
SELECT
    Orders.OrderID,
    cust.CompanyName, 
    Summed.[Total Sales]
FROM
    Orders 
JOIN Customers cust 
USING (CustomerID)
JOIN
	(SELECT
     	OrderID,
        sum(UnitPrice * Quantity) AS [Total Sales]
     FROM
     	[Order Details]
     GROUP BY
     	OrderID
    ) Summed
USING(OrderID)
;


-- Self-join
SELECT 
	a.UnitPrice,
	b.Total,
    a.UnitPrice / b.Total
FROM
    [Order Details] a,
    (SELECT 
    	sum(UnitPrice) AS Total 
     FROM 
     	[Order Details]) b
;