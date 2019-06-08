Given a Weather table, write a SQL query to find all dates' Ids with higher temperature compared to its previous (yesterday's) dates.
+---------+------------------+------------------+
| Id(INT) | RecordDate(DATE) | Temperature(INT) |
+---------+------------------+------------------+
|       1 |       2015-01-01 |               10 |
|       2 |       2015-01-02 |               25 |
|       3 |       2015-01-03 |               20 |
|       4 |       2015-01-04 |               30 |
+---------+------------------+------------------+
For example, return the following Ids for the above Weather table:
+----+
| Id |
+----+
|  2 |
|  4 |
+----+

###
show databases;
use test_one;
drop table Weather;
CREATE TABLE Weather (
    Id INT,
    Date DATE,
    Temperature INT
);

INSERT INTO Weather VALUES
(1, '2015-01-01', 10),
(2, '2015-01-02', 25),
(3, '2015-01-03', 20),
(4, '2015-01-04', 32);

select * from Weather;

#!!!vip
Select W1.Id
From Weather W1 JOIN Weather W2 
ON to_days(W1.Date) = to_days(W2.Date) + 1
Where W1.Temperature > W2.Temperature;

# to_days() : Return the number of days between the date and year 0

#2
select W1.Id, W1.Temperature from Weather W1
left join Weather W2
on to_days(W1.Date)=to_days(W2.Date)+1
where W1.Temperature>W2.Temperature;