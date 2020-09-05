/*

Copyright (C) 2012-2015 GNU remotecontrol authors.

This program is free software: you can redistribute it and/or modify
it under the terms of the GNU Affero General Public License as
published by the Free Software Foundation, either version 3 of the
License, or (at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU Affero General Public License for more details.

You should have received a copy of the GNU Affero General Public License
along with this program.  If not, see <http://www.gnu.org/licenses/>.

*/

SET NAMES 'utf8';

create table v2_exception_log (
  id mediumint not null auto_increment,
  user_id mediumint not null,
  `timestamp` timestamp default current_timestamp,
--  mode enum( 'created', 'caught' ) not null,
  previous_id mediumint not null default 0,
  type varchar(64) charset ascii collate ascii_general_ci not null,
  message longtext not null,
  `code` int signed not null default 0,
  `file` varchar(256) charset ascii collate ascii_bin not null,
  line int unsigned not null default 0,
  message_stack longtext not null,
  trace longtext not null,
  trace_data longtext not null,
  primary key( id )
) ENGINE=InnoDB;

create table v2_url(
  `hash` char(40) charset ascii collate ascii_general_ci not null,
  url longtext charset ascii collate ascii_bin not null,
  redirect bool not null default 0,
  primary key ( `hash` )
) ENGINE=InnoDB;

create table v2_intl_language(
  langtag varchar( 64 ) charset ascii collate ascii_general_ci not null,
  fallback varchar( 64 ) charset ascii collate ascii_general_ci not null,
  english_name varchar( 64 ) not null,
  local_name varchar( 64 ) not null,
  active bool not null default 0,
  primary key ( langtag )
) ENGINE=InnoDB;

create table v2_intl_context(
  context varchar( 64 ) charset ascii collate ascii_general_ci not null,
  seq mediumint not null default 0,
  primary key ( context )
) ENGINE=InnoDB;

create table v2_intl_message(
  `type` ENUM( 'html', 'attr', 'text', 'safe' ),
  context varchar( 64 ) charset ascii collate ascii_general_ci not null,
  `hash` char( 32 ) charset ascii collate ascii_general_ci not null,
  `content` longtext not null,
  seq mediumint auto_increment not null,
  words mediumint unsigned not null,
  primary key ( `hash`, `type`, context ),
  unique key ( seq )
) ENGINE=InnoDB;

create table v2_intl_translation(
  langtag varchar( 64 ) charset ascii collate ascii_general_ci not null,
  `type` ENUM( 'html', 'attr', 'text', 'safe' ),
  context varchar( 64 ) charset ascii collate ascii_general_ci not null,
  `hash` char( 32 ) charset ascii collate ascii_general_ci not null,
  `text` longtext not null,
  translator varchar(20) charset ascii collate ascii_general_ci not null,
  primary key ( `hash`, langtag, `type`, context )
) ENGINE=InnoDB;

/*
create table v2_intl_translator(
  username varchar(20) charset ascii collate ascii_general_ci not null,
  primary key ( username )
) ENGINE=InnoDB;
*/

create table v2_intl_translator_language(
  -- username varchar(20) charset ascii collate ascii_general_ci not null,
  user_id SMALLINT(5) NOT NULL,
  langtag varchar( 64 ) charset ascii collate ascii_general_ci not null,
  total_words int not null default 0,
  paid_words int not null default 0,
  paid_dollars int not null default 0,
  primary key ( user_id, langtag )
) ENGINE=InnoDB;

create table v2_intl_translator_translation(
  -- username varchar(20) charset ascii collate ascii_general_ci not null,
  user_id SMALLINT(5) NOT NULL,
  langtag varchar( 64 ) charset ascii collate ascii_general_ci not null,
  `type` ENUM( 'html', 'attr', 'text', 'safe' ),
  context varchar( 64 ) charset ascii collate ascii_general_ci not null,
  `hash` char( 32 ) charset ascii collate ascii_general_ci not null,
  primary key ( user_id, langtag, `type`, context, `hash` )
) ENGINE=InnoDB;

create table v2_intl_langtag_invalid(
  langtag varchar( 64 ) charset ascii collate ascii_bin not null,
  `count` int not null default 0,
  primary key ( langtag )
) ENGINE=InnoDB;

create table v2_intl_langtag_missing(
  langtag varchar( 64 ) charset ascii collate ascii_bin not null,
  `count` int not null default 0,
  primary key ( langtag )
) ENGINE=InnoDB;

CREATE TABLE v2_user (
  id SMALLINT(5) NOT NULL AUTO_INCREMENT,
  username VARCHAR(20) NOT NULL,
  is_admin bool not null default 0,
  is_translator bool not null default 0,
  -- password_hash
  location_id smallint not null,
  max_thermostats smallint not null DEFAULT 0,
  langtag VARCHAR(64) NOT NULL,
  xsrf char(40) charset ascii collate ascii_general_ci not null,
  deleted bool not null default 0,
  PRIMARY KEY ( id )
)
ENGINE = INNODB
AUTO_INCREMENT = 1;

CREATE TABLE v2_thermostat (
  id MEDIUMINT NOT NULL AUTO_INCREMENT,
  location_id smallint not null,
  group_id tinyint NOT NULL,
  name VARCHAR(32) NOT NULL,
  description VARCHAR(128) DEFAULT NULL,
  host VARCHAR(128) NOT NULL,
  port smallint UNSIGNED NOT NULL,
  user VARCHAR(64) DEFAULT NULL,
  pass VARCHAR(64) DEFAULT NULL,
  user_id SMALLINT NOT NULL,
  deleted boolean NOT NULL DEFAULT 0,
  PRIMARY KEY ( id )
  -- , unique key ( user_id, host, port )
)
ENGINE = INNODB
AUTO_INCREMENT = 1;

CREATE TABLE v2_group (
  id tinyint NOT NULL AUTO_INCREMENT,
  name VARCHAR(32) NOT NULL,
  description VARCHAR(128) DEFAULT NULL,
  primary key ( id ),
  unique key ( name )
)
ENGINE = INNODB
AUTO_INCREMENT = 1;

CREATE TABLE v2_location (
  id smallint NOT NULL AUTO_INCREMENT,
  name VARCHAR(32) NOT NULL,
  timezone VARCHAR(128) DEFAULT NULL,
  primary key ( id ),
  unique key ( name )
)
ENGINE = INNODB
AUTO_INCREMENT = 1;


create table v2_history (
  id bigint not null auto_increment,
  user_id mediumint not null,
  thermostat_id mediumint not null,
  field varchar( 128 ) not null,
  previous_value varchar( 2048 ) not null,
  current_value varchar( 2048 ) not null,
  timestamp timestamp,
  primary key ( id )
)
ENGINE = INNODB
AUTO_INCREMENT = 1;
