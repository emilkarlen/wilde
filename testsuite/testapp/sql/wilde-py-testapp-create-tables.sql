--
-- create table - keys_test -
--
CREATE TABLE keys_test
(
  bCplxCplxBool	TINYINT UNSIGNED NOT NULL,
  dCplxCplxDate	DATE NOT NULL,
  nCplxSimp	SMALLINT UNSIGNED NOT NULL,
  nSimp	INT NOT NULL,

  PRIMARY KEY (bCplxCplxBool,dCplxCplxDate,nCplxSimp),
  CONSTRAINT UNIQUE (nSimp),
  CONSTRAINT UNIQUE (nSimp,bCplxCplxBool,dCplxCplxDate,nCplxSimp)
);
--
-- create table - simple_datatypes -
--
CREATE TABLE simple_datatypes
(
  idSdId	INT NOT NULL,
  bBoolean	TINYINT UNSIGNED,
  nInt	INT NOT NULL,
  fReal	REAL,
  dDate	DATE,
  tTime	TIME,
  dtDatetime	DATETIME,
  sString	VARCHAR(30),
  txtText	TEXT,
  txtHtmlText	TEXT,
  sUrl	VARCHAR(255),

  PRIMARY KEY (idSdId)
);
--
-- create table - simple_datatypes2 -
--
CREATE TABLE simple_datatypes2
(
  idSd2Id	INT NOT NULL,
  nHhMm	SMALLINT,
  nHhMmSs	INTEGER,
  nHhMmU	SMALLINT UNSIGNED,
  nHhMmSsU	INTEGER UNSIGNED,

  PRIMARY KEY (idSd2Id)
);
--
-- create table - complex_datatypes -
--
CREATE TABLE complex_datatypes
(
  idCdId	INT NOT NULL,
  nRatNum	INT,
  nRatDenom	INT,
  nDcYear	SMALLINT,
  nDcMonth	SMALLINT,
  nDcDay	SMALLINT,
  nTcHour	SMALLINT,
  nTcMinute	SMALLINT,
  nTcSecond	SMALLINT,
  nTcTenth	SMALLINT,

  PRIMARY KEY (idCdId)
);
--
-- create table - complex_datatypes2 -
--
CREATE TABLE complex_datatypes2
(
  idCd2Id	INT NOT NULL,
  bMinNeg	TINYINT UNSIGNED,
  nMinRatNum	INT,
  nMinRatDenom	INT,
  bMaxNeg	TINYINT UNSIGNED NOT NULL,
  nMaxRatNum	INT NOT NULL,
  nMaxRatDenom	INT NOT NULL,

  PRIMARY KEY (idCd2Id)
);
--
-- create table - enum_datatypes -
--
CREATE TABLE enum_datatypes
(
  idEnumDtId	INT NOT NULL DEFAULT NULL AUTO_INCREMENT,
  nSimpleEnum	SMALLINT UNSIGNED,

  PRIMARY KEY (idEnumDtId)
);
--
-- create table - html_escape_class_validval -
--
CREATE TABLE html_escape_class_validval
(
  sMayNeedEscapeId	VARCHAR(5) NOT NULL,
  sTextIdMayNeedEscape	VARCHAR(30),

  PRIMARY KEY (sMayNeedEscapeId)
);
--
-- create table - html_escape_class -
--
CREATE TABLE html_escape_class
(
  idHecId	INT NOT NULL,
  sHecvvHecvv	VARCHAR(5) REFERENCES html_escape_class_validval,
  nEscapeHeader	INT,
  nHee	SMALLINT UNSIGNED,
  sString	VARCHAR(30),
  txtText	TEXT,

  PRIMARY KEY (idHecId)
);
--
-- create table - ot_without_textid -
--
CREATE TABLE ot_without_textid
(
  idOtwotId	INT NOT NULL DEFAULT NULL AUTO_INCREMENT,
  sName	VARCHAR(30) NOT NULL,

  PRIMARY KEY (idOtwotId)
);
--
-- create table - ot_with_1_textid -
--
CREATE TABLE ot_with_1_textid
(
  idOtw1tId	INT NOT NULL DEFAULT NULL AUTO_INCREMENT,
  sTextId	VARCHAR(30) NOT NULL,

  PRIMARY KEY (idOtw1tId)
);
--
-- create table - ot_with_mult_textid -
--
CREATE TABLE ot_with_mult_textid
(
  idOtwmtId	INT NOT NULL DEFAULT NULL AUTO_INCREMENT,
  sTextId1	VARCHAR(30) NOT NULL,
  sTextId2	VARCHAR(30) NOT NULL,

  PRIMARY KEY (idOtwmtId)
);
--
-- create table - otwot_referer -
--
CREATE TABLE otwot_referer
(
  idOtwotrId	INT NOT NULL DEFAULT NULL AUTO_INCREMENT,
  idOtwotOtwot	INT NOT NULL REFERENCES ot_without_textid,
  sOtwotrName	VARCHAR(30) NOT NULL,

  PRIMARY KEY (idOtwotrId)
);
--
-- create table - otw1t_referer -
--
CREATE TABLE otw1t_referer
(
  idOtw1trId	INT NOT NULL DEFAULT NULL AUTO_INCREMENT,
  idOtw1tOtw1t	INT NOT NULL REFERENCES ot_with_1_textid,
  sOtw1trName	VARCHAR(30) NOT NULL,

  PRIMARY KEY (idOtw1trId)
);
--
-- create table - otwmt_referer -
--
CREATE TABLE otwmt_referer
(
  idOtwmtrId	INT NOT NULL DEFAULT NULL AUTO_INCREMENT,
  idOtwmtOtwmt	INT NOT NULL REFERENCES ot_with_mult_textid,
  sOtwmtrName	VARCHAR(30) NOT NULL,
  nEnum	SMALLINT UNSIGNED,

  PRIMARY KEY (idOtwmtrId)
);
--
-- create table - complex_type_test2 -
--
CREATE TABLE complex_type_test2
(
  idCtt2	INT NOT NULL DEFAULT NULL AUTO_INCREMENT,
  bACplxBool	TINYINT UNSIGNED,
  dACplxDate	DATE,
  nASimp	SMALLINT UNSIGNED,
  bBBool	TINYINT UNSIGNED NOT NULL,
  dBDate	DATE NOT NULL,

  PRIMARY KEY (idCtt2)
);
--
-- create table - complex_type_pk -
--
CREATE TABLE complex_type_pk
(
  bComplexPkBool	TINYINT UNSIGNED NOT NULL,
  dComplexPkDate	DATE NOT NULL,
  nPkAttr	INT UNSIGNED,
  bValue	TINYINT UNSIGNED,

  PRIMARY KEY (bComplexPkBool,dComplexPkDate,nPkAttr)
);
--
-- create table - complex_text_id_sup -
--
CREATE TABLE complex_text_id_sup
(
  idCtisup	INT NOT NULL DEFAULT NULL AUTO_INCREMENT,
  bTxtidBool	TINYINT UNSIGNED NOT NULL,
  dTxtidDate	DATE NOT NULL,

  PRIMARY KEY (idCtisup)
);
--
-- create table - complex_text_id_sub -
--
CREATE TABLE complex_text_id_sub
(
  idCtisub	INT NOT NULL DEFAULT NULL AUTO_INCREMENT,
  idCtisupCtisup	INT NOT NULL REFERENCES complex_text_id_sup,
  bValue	TINYINT UNSIGNED,

  PRIMARY KEY (idCtisub)
);
--
-- create table - sorting -
--
CREATE TABLE sorting
(
  idSortingId	INT NOT NULL,
  dDate	DATE,
  nInteger	INT,

  PRIMARY KEY (idSortingId)
);
--
-- create table - comp_super -
--
CREATE TABLE comp_super
(
  idCosId	INT NOT NULL DEFAULT NULL AUTO_INCREMENT,

  PRIMARY KEY (idCosId)
);
--
-- create table - shared_comp_super -
--
CREATE TABLE shared_comp_super
(
  idScosId	INT NOT NULL DEFAULT NULL AUTO_INCREMENT,

  PRIMARY KEY (idScosId)
);
--
-- create table - comp_sub -
--
CREATE TABLE comp_sub
(
  idSosubId	INT NOT NULL DEFAULT NULL AUTO_INCREMENT,
  idCosCos	INT NOT NULL REFERENCES comp_super,
  idScosScos	INT REFERENCES shared_comp_super,
  sName	VARCHAR(30) NOT NULL,

  PRIMARY KEY (idSosubId)
);
