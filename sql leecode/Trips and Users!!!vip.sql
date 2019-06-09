Create table Trips(
    Id INT,
    Client_id INT,
    Driver_id INT,
    City_id INT,
    Status ENUM('completed', 'cancelled_by_driver', 'cancelled_by_client'),
    Request_at DATE
);
Insert into Trips Values
(1,1,10,1,'completed', str_to_date('10/1/13', '%m/%d/%Y')),
(2,2,11,1,'cancelled_by_driver', str_to_date('10/1/13', '%m/%d/%Y')),
(3,3,12,6,'completed', str_to_date('10/1/13', '%m/%d/%Y')),
(4,4,13,6,'cancelled_by_driver', str_to_date('10/1/13', '%m/%d/%Y')),
(5,1,10,1,'completed', str_to_date('10/2/13', '%m/%d/%Y')),
(6,2,11,6,'completed', str_to_date('10/2/13', '%m/%d/%Y')),
(7,3,12,6,'completed', str_to_date('10/2/13', '%m/%d/%Y')),
(8,2,12,12,'completed', str_to_date('10/3/13', '%m/%d/%Y')),
(9,3,10,12,'completed', str_to_date('10/3/13', '%m/%d/%Y')),
(10,4,13,12,'cancelled_by_driver', str_to_date('10/3/13', '%m/%d/%Y'));

Create table Users(
    Users_id INT,
    Banned ENUM('No', 'Yes'),
    Role ENUM('client', 'driver', 'partner')
);
Insert into Users Values
(1, 'No', 'client'),
(2, 'Yes', 'client'),
(3, 'No', 'client'),
(4, 'No', 'client'),
(10, 'No', 'driver'),
(11, 'No', 'driver'),
(12, 'No', 'driver'),
(13, 'No', 'driver');

select * from Trips;
select * from Users;

###!!!
Select T.Request_at AS Day, Round(Sum(If(Status='cancelled_by_driver',1,0))/count(*), 2) AS 'Cancellation Rate'
From Trips T JOIN Users U
ON T.Client_Id = U.Users_Id
Where Banned='No' And Role='client' And Request_at Between '2013-10-01' and '2013-10-03'
Group by Request_at
Order by Request_at;

#round() : The ROUND() is a mathematical function that allows you to round a number to a specified number of decimal places.