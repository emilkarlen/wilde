CREATE TABLE reference
(
  idRefId INT,
  sds_idSdId INT
);

CREATE TABLE auto_inc
(
  idAutoincId INT NOT NULL DEFAULT NULL AUTO_INCREMENT,
  sString VARCHAR(50),

  PRIMARY KEY (idAutoincId)
);
