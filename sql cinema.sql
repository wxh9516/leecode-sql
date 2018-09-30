#X city opened a new cinema, many people would like to go to this cinema. 
#The cinema also gives out a poster indicating the movies’ ratings and descriptions.

#Please write a SQL query to output movies with an odd numbered ID 
#and a description that is not 'boring'. Order the result by rating.



#For example, table cinema:
#+---------+-----------+--------------+-----------+
#|   id    | movie     |  description |  rating   |
#+---------+-----------+--------------+-----------+
#|   1     | War       |   great 3D   |   8.9     |
#|   2     | Science   |   fiction    |   8.5     |
#|   3     | irish     |   boring     |   6.2     |
#|   4     | Ice song  |   Fantacy    |   8.6     |
#|   5     | House card|   Interesting|   9.1     |
#+---------+-----------+--------------+-----------+


#For the example above, the output should be:
#+---------+-----------+--------------+-----------+
#|   id    | movie     |  description |  rating   |
#+---------+-----------+--------------+-----------+
#|   5     | House card|   Interesting|   9.1     |
#|   1     | War       |   great 3D   |   8.9     |
#+---------+-----------+--------------+-----------+

#1.
show databases;
use test_one;
CREATE TABLE movie (
  id INT NOT NULL,
  movie VARCHAR(30) NOT NULL,
  movie_description varchar(30) not null,
  rating float,   #float!!!
  primary key (id),
  UNIQUE (movie)
);

INSERT INTO movie 
    (id,movie,movie_description,rating) 
VALUES 
    (1,"War","greate 3D",8.9),
    (2,"Science","Fiction",8.5),
    (3,"irish","boring",6.2),
    (4,"ice song","fantacy",8.6),
    (5,"house card","interesting",9.1);
    
select *from movie;

#drop table movie;

#2.
#an odd numbered ID
#a description that is not 'boring'. 
#Order the result by rating.

select * from movie
where id%2<>0 and movie_description<>"boring"
order by rating desc;

#3.
#n%2=0 is even，n%2<>0 is odd;   (??? %;
#<> means not equal to;

#4.
#order:
#SELECT * FROM table_name ORDER BY column_name ASC|DESC

#table_name: name of the table.
#column_name: name of the column according to which the data is needed to be arranged.
#ASC: to sort the data in ascending order.
#DESC: to sort the data in descending order.
#| : use either ASC or DESC to sort in ascending or descending order

#Sort according to multiple columns:
#SELECT * FROM table_name ORDER BY column1 ASC|DESC , column2 ASC|DESC





