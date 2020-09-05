grant dba to creator;
grant connect to public;

drop table GNATS;
create table gnats         (
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

create unique index gnats_prid
  on gnats(gnatsid);

create index gnats_cat
  on gnats(category);

create index gnats_resp
  on gnats(respengr);

create index gnats_cust
  gnats(customer);

revoke insert, delete, update, references, index, alter on gnats from public;
