

INSERT INTO sessions(source,paragraph,accuracy,mean_iksi,num_characters)
VALUES("Alice",1,0.3,100,100);

INSERT INTO userPW(username,password)
VALUES('walter','123');

ALTER table typing

CREATE table sessions(

username varchar(16) references userPW.username,
startTime date,
endTime date,
source text,
pnum integer
);
CREATE table characters(
session_id integer references sessions.id,
characters text,
wordCountVector integer,
word text,
typed text,
editDist double,
mean_IKSI integer
);

ALTER table userpw
MODIFY COLUMN username varchar(30);

ALTER table userpw
ADD CONSTRAINT user_id
PRIMARY KEY(username);
ALTER table sessions
ADD COLUMN id integer;

ALTER table sessions
ADD CONSTRAINT id
PRIMARY KEY(username,endTime);
ALTER table characters
ADD COLUMN id integer;
ALTER table characters
ADD CONSTRAINT id
PRIMARY KEY(session_id,wordCountVector);

SELECT constraint_name, table_name, constraint_type
FROM information_schema.table_constraints
WHERE constraint_type = 'PRIMARY KEY'
AND table_name='userpw';
ALTER table userpw
DROP COLUMN id;
SHOW INDEX FROM userpw;

ALTER table sessions MODIFY COLUMN endTime integer;

INSERT INTO userpw
VALUES("Alan","secret");
INSERT INTO sessions
VALUES("Alan",100,100,"alice",1);
SHOW INDEX FROM sessions
ALTER TABLE sessions DROP COLUMN id;

SELECT * FROM sessions WHERE username="NA" 
SHOW INDEX FROM sessions

ALTER table characters
DROP COLUMN session_id;

CREATE table characters(
characters text,
wordCountVector integer,
word text,
typed text,
editDist integer,
mean_IKSI integer
);

INSERT INTO characters
VALUES("A",1,"Alice","ABC",1,300);

CREATE table userpw(
username text,
password text
);
INSERT INTO userpw(username,password)
VALUES('dbz','rocks');

CREATE table sessions(
id serial,
username varchar(30) REFERENCES userpw.username,
endtime integer,
source text,
pnum textuserpw
);

SELECT * FROM userpw WHERE username='john'

ALTER table test.characters ADD COLUMN session_id integer REFERENCES sessions.id;

SELECT * FROM test.sessions;
INSERT INTO test.sessions(username,endtime,source,pnum) VALUES
('walter',100,'alice',1)
ALTER table test.sessions MODIFY COLUMN endtime BIGINT;

SELECT MAX(id) FROM test.sessions;

INSERT INTO test.characters (characters,wordCountVector,word,typed,editDist,mean_IKSI,session_id)
VALUES ('i,42,in,in,0,100,78')

INSERT INTO userpw (username,password) VALUES ('walter','dbz')

