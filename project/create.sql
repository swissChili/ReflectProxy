create database if not exists reflectjs;

use reflectjs;

create table if not exists requests (
  id int not null auto_increment primary key comment 'ID',
  requested_at datetime comment 'When the request was made',
  client_ip varchar(31) comment 'The clients IP address',
  host varchar(255) comment 'The host requested',
  path varchar(255) comment 'The path requested',
  query varchar(255) comment 'Query Parameters'
);
