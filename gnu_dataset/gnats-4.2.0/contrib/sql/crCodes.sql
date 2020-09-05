drop table severityCode;
create table severityCode  (
  sevNum          integer,
  sevStr          char (  16)
);

revoke insert, delete, update, references, index, alter on severityCode from public;


insert into severityCode
  values (0, "unspecified");
insert into severityCode
  values (1, "critical");
insert into severityCode
  values (2, "serious");
insert into severityCode
  values (3, "non-critical");


drop table priorityCode;
create table priorityCode  (
  priNum          integer,
  priStr          char (  16)
);

revoke insert, delete, update, references, index, alter on priorityCode from public;


insert into priorityCode
  values (1, "high");
insert into priorityCode
  values (2, "medium");
insert into priorityCode
  values (3, "low");


drop table stateCode;
create table stateCode     (
  stateNum        integer,
  stateStr        char (  16)
);

revoke insert, delete, update, references, index, alter on stateCode from public;


insert into stateCode
  values (1, "open");
insert into stateCode
  values (2, "analyzed");
insert into stateCode
  values (3, "suspended");
insert into stateCode
  values (4, "feedback");
insert into stateCode
  values (5, "closed");


drop table classCode;
create table classCode     (
  classNum        integer,
  classStr        char (  16)
);


revoke insert, delete, update, references, index, alter on classCode from public;

insert into classCode
  values (1, "sw-bug");
insert into classCode
  values (2, "doc-bug");
insert into classCode
  values (3, "support");
insert into classCode
  values (4, "change-request");
insert into classCode
  values (5, "mistake");
insert into classCode
  values (6, "duplicate");
