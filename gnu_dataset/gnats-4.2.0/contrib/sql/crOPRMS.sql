
drop table o_gnats;
create table o_gnats         (
  gnatsid               char(8),
  category             char(16),
  synopsis             char(128),
  confidential         char(3),
  severity             integer,
  priority             integer,
  respengr             char(16),
  gnatsstate            integer,
  gnatsclass            integer,
  customer             char(16),
  arrival              datetime year to minute,
  age                  interval day(5) to hour,
  custcontact          char(64),
  release              char(64)
);

create unique index o_gnats_prid
  on o_gnats(gnatsid);

create index o_gnats_cat
  on o_gnats(category);

create index o_gnats_resp
  on o_gnats(respengr);

create index o_gnats_cust
  o_gnats(customer);

revoke insert, delete, update, references, index, alter on o_gnats from public;


drop table buf_gnats;
create table buf_gnats         (
  gnatsid               char(8),
  category             char(16),
  synopsis             char(128),
  confidential         char(3),
  severity             integer,
  priority             integer,
  respengr             char(16),
  gnatsstate            integer,
  gnatsclass            integer,
  customer             char(16),
  arrival              datetime year to minute,
  age                  interval day(5) to hour,
  custcontact          char(64),
  release              char(64)
);

create unique index buf_gnats_prid
  on buf_gnats(gnatsid);

create index buf_gnats_cat
  on buf_gnats(category);

create index buf_gnats_resp
  on buf_gnats(respengr);

create index buf_gnats_cust
  buf_gnats(customer);

revoke insert, delete, update, references, index, alter on buf_gnats from public;


