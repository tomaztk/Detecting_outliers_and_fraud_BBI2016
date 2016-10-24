USE FRAUD_detection;
GO

SELECT  * FROM Visa_Transactions
--order by day_purchase
order by invoice_euro desc


SELECT * From visa_transactions
where point_of_sale like '%ALIEXP%'



UPDATE visa_transactions
SET Country = 'China'
,City = 'Hangzhou'
,LAT = '30.274085'
,LNG = '120.155070'
where point_of_sale like '%ALIEXP%'


UPDATE visa_transactions
SET Country = 'Germany'
,City = 'Munich'
,LAT = '48.1351253'
,LNG = '11.581980'
where point_of_sale like '%Amazon DE Marketplac%'



CREATE TABLE White_table
(
ID INT IDENTITY (1,1)
,Point_of_sale NVARCHAR(500))


INSERT INTO White_table
SELECT 'WWW.ALIEXPRESS.COM'
UNION ALL SELECT 'Amazon DE Marketplac'


DROP VIEW all_transactions

CREATE VIEW All_Transactions
AS
SELECT
 Purchase_day As Invoice_day
,CAST([Value] AS DECIMAL(10,2)) AS Invoice_Value
,LTRIM(RTRIM([Description])) AS Invoice_POS
,Country
,CASE 
			WHEN Country = 'Slovenija' THEN 1
			WHEN Country = 'USA' THEN 2
			WHEN Country = 'Italy' THEN 3
			WHEN Country = 'Swiss' THEN 4
			WHEN Country = 'Gibraltar' THEN 5
			WHEN Country = 'Germany' THEN 6
			WHEN Country = 'Hungary' THEN 7
			WHEN Country = 'China' THEN 8
			WHEN Country = 'Brasil' THEN 9
			WHEN Country = 'Austria' THEN 10
			WHEN Country = 'Denmark' THEN 11
	END AS Country_ID
,City
,LAT
,LNT AS LNG
,'Maestro' AS Invoice_Type
,1 AS Invoice_Type_ID
,CASE WHEN	
      (ISNULL(PARSENAME(CAST([value] AS DECIMAL(10,2)),2),0) % 10 = 0
	  AND ISNULL(PARSENAME(CAST([value] AS DECIMAL(10,2)),2),0) > 0
	  AND ISNULL(PARSENAME(CAST([value] AS DECIMAL(10,2)),1),0) = 0
	  ) THEN 1 ELSE 0 END AS Limit_number
,0 AS Fraud
 FROM [dbo].[Maestro_Transactions]
 WHERE
	[Value] > 0 -- Exclude income

UNION ALL 

SELECT 
 Day_Purchase As Invoice_day
,CAST([Invoice_Euro]  AS DECIMAL(10,2)) AS Invoice_Value
,[Point_of_sale] AS Invoice_POS
,Country
,CASE 
			WHEN Country = 'Slovenija' THEN 1
			WHEN Country = 'USA' THEN 2
			WHEN Country = 'Italy' THEN 3
			WHEN Country = 'Swiss' THEN 4
			WHEN Country = 'Gibraltar' THEN 5
			WHEN Country = 'Germany' THEN 6
			WHEN Country = 'Hungary' THEN 7
			WHEN Country = 'China' THEN 8
			WHEN Country = 'Brasil' THEN 9
			WHEN Country = 'Austria' THEN 10
			WHEN Country = 'Denmark' THEN 11
	END AS Country_ID
,City
,LAT
,LNG
,'Visa' AS Invoice_Type
,2 AS Invoice_Type_ID
,CASE WHEN	
      (ISNULL(PARSENAME(CAST(invoice_euro AS DECIMAL(10,2)),2),0) % 10 = 0
	  AND ISNULL(PARSENAME(CAST(invoice_euro AS DECIMAL(10,2)),2),0) > 0
	  AND ISNULL(PARSENAME(CAST(invoice_euro AS DECIMAL(10,2)),1),0) = 0
	  ) THEN 1 ELSE 0 END AS Limit_number
,CASE WHEN Point_of_sale = 'WHO INTERNET' THEN 1 ELSE 0 END AS Fraud
 FROM [dbo].[Visa_Transactions]

