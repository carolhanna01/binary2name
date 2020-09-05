
create table history       (
  asof            date,
  open            integer,
  analyzed        integer,
  suspended       integer,
  feedback        integer,
  closed          integer,
  total           integer
);

revoke insert, delete, update, references, index, alter on history from public;

