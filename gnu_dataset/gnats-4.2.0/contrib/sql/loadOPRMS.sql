delete from o_gnats;

insert into o_gnats
  select * from buf_gnats;

delete from buf_gnats;

insert into buf_gnats
  select * from gnats;

