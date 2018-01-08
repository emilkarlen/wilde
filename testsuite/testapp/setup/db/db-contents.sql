-- MySQL dump 10.13  Distrib 5.5.37, for debian-linux-gnu (x86_64)
--
-- Host: localhost    Database: wilde_test
-- ------------------------------------------------------
-- Server version	5.5.37-0ubuntu0.14.04.1

/*!40101 SET @OLD_CHARACTER_SET_CLIENT=@@CHARACTER_SET_CLIENT */;
/*!40101 SET @OLD_CHARACTER_SET_RESULTS=@@CHARACTER_SET_RESULTS */;
/*!40101 SET @OLD_COLLATION_CONNECTION=@@COLLATION_CONNECTION */;
/*!40101 SET NAMES utf8 */;
/*!40103 SET @OLD_TIME_ZONE=@@TIME_ZONE */;
/*!40103 SET TIME_ZONE='+00:00' */;
/*!40014 SET @OLD_UNIQUE_CHECKS=@@UNIQUE_CHECKS, UNIQUE_CHECKS=0 */;
/*!40014 SET @OLD_FOREIGN_KEY_CHECKS=@@FOREIGN_KEY_CHECKS, FOREIGN_KEY_CHECKS=0 */;
/*!40101 SET @OLD_SQL_MODE=@@SQL_MODE, SQL_MODE='NO_AUTO_VALUE_ON_ZERO' */;
/*!40111 SET @OLD_SQL_NOTES=@@SQL_NOTES, SQL_NOTES=0 */;

--
-- Table structure for table `auto_inc`
--

DROP TABLE IF EXISTS `auto_inc`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `auto_inc` (
  `id` int(11) NOT NULL AUTO_INCREMENT,
  `string` varchar(50) COLLATE utf8_swedish_ci DEFAULT NULL,
  PRIMARY KEY (`id`)
) ENGINE=MyISAM AUTO_INCREMENT=11 DEFAULT CHARSET=utf8 COLLATE=utf8_swedish_ci;
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Dumping data for table `auto_inc`
--

LOCK TABLES `auto_inc` WRITE;
/*!40000 ALTER TABLE `auto_inc` DISABLE KEYS */;
INSERT INTO `auto_inc` VALUES (10,'hej');
/*!40000 ALTER TABLE `auto_inc` ENABLE KEYS */;
UNLOCK TABLES;

--
-- Table structure for table `auto_inc2`
--

DROP TABLE IF EXISTS `auto_inc2`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `auto_inc2` (
  `string` varchar(50) COLLATE utf8_swedish_ci DEFAULT NULL,
  `id` int(11) NOT NULL AUTO_INCREMENT,
  PRIMARY KEY (`id`)
) ENGINE=InnoDB AUTO_INCREMENT=3 DEFAULT CHARSET=utf8 COLLATE=utf8_swedish_ci;
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Dumping data for table `auto_inc2`
--

LOCK TABLES `auto_inc2` WRITE;
/*!40000 ALTER TABLE `auto_inc2` DISABLE KEYS */;
INSERT INTO `auto_inc2` VALUES ('1',1),('2',2);
/*!40000 ALTER TABLE `auto_inc2` ENABLE KEYS */;
UNLOCK TABLES;

--
-- Table structure for table `comp_sub`
--

DROP TABLE IF EXISTS `comp_sub`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `comp_sub` (
  `id` int(11) NOT NULL AUTO_INCREMENT,
  `comp_super_id` int(11) NOT NULL,
  `shared_comp_super_id` int(11) DEFAULT NULL,
  `name` varchar(30) COLLATE utf8_swedish_ci NOT NULL,
  PRIMARY KEY (`id`)
) ENGINE=MyISAM AUTO_INCREMENT=13 DEFAULT CHARSET=utf8 COLLATE=utf8_swedish_ci;
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Dumping data for table `comp_sub`
--

LOCK TABLES `comp_sub` WRITE;
/*!40000 ALTER TABLE `comp_sub` DISABLE KEYS */;
INSERT INTO `comp_sub` VALUES (1,1,1,'One'),(2,1,NULL,'Two2'),(8,1,1,'tvÃ¥4'),(9,1,NULL,'rtre'),(10,1,NULL,'ny'),(11,1,NULL,'ny'),(12,1,NULL,'ny');
/*!40000 ALTER TABLE `comp_sub` ENABLE KEYS */;
UNLOCK TABLES;

--
-- Table structure for table `comp_super`
--

DROP TABLE IF EXISTS `comp_super`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `comp_super` (
  `id` int(11) NOT NULL AUTO_INCREMENT,
  PRIMARY KEY (`id`)
) ENGINE=MyISAM AUTO_INCREMENT=4 DEFAULT CHARSET=utf8 COLLATE=utf8_swedish_ci;
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Dumping data for table `comp_super`
--

LOCK TABLES `comp_super` WRITE;
/*!40000 ALTER TABLE `comp_super` DISABLE KEYS */;
INSERT INTO `comp_super` VALUES (1),(3);
/*!40000 ALTER TABLE `comp_super` ENABLE KEYS */;
UNLOCK TABLES;

--
-- Table structure for table `complex_datatypes`
--

DROP TABLE IF EXISTS `complex_datatypes`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `complex_datatypes` (
  `id` int(11) NOT NULL,
  `rat_nnum` int(11) DEFAULT NULL,
  `rat_denom` int(11) DEFAULT NULL,
  `dc_year` smallint(6) DEFAULT NULL,
  `dc_month` smallint(6) DEFAULT NULL,
  `dc_day` smallint(6) DEFAULT NULL,
  `tc_hour` smallint(6) DEFAULT NULL,
  `tc_minute` smallint(6) DEFAULT NULL,
  `tc_second` smallint(6) DEFAULT NULL,
  `tc_tenth` smallint(6) DEFAULT NULL,
  PRIMARY KEY (`id`)
) ENGINE=MyISAM DEFAULT CHARSET=utf8 COLLATE=utf8_swedish_ci;
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Dumping data for table `complex_datatypes`
--

LOCK TABLES `complex_datatypes` WRITE;
/*!40000 ALTER TABLE `complex_datatypes` DISABLE KEYS */;
INSERT INTO `complex_datatypes` VALUES (1,1,2,2002,2,2,2,2,2,2);
/*!40000 ALTER TABLE `complex_datatypes` ENABLE KEYS */;
UNLOCK TABLES;

--
-- Table structure for table `complex_text_id_sub`
--

DROP TABLE IF EXISTS `complex_text_id_sub`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `complex_text_id_sub` (
  `id` int(11) NOT NULL AUTO_INCREMENT,
  `complex_text_id_sup_id` int(11) NOT NULL,
  `bool_value` tinyint(3) unsigned DEFAULT NULL,
  PRIMARY KEY (`id`)
) ENGINE=MyISAM DEFAULT CHARSET=utf8 COLLATE=utf8_swedish_ci;
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Dumping data for table `complex_text_id_sub`
--

LOCK TABLES `complex_text_id_sub` WRITE;
/*!40000 ALTER TABLE `complex_text_id_sub` DISABLE KEYS */;
/*!40000 ALTER TABLE `complex_text_id_sub` ENABLE KEYS */;
UNLOCK TABLES;

--
-- Table structure for table `complex_text_id_sup`
--

DROP TABLE IF EXISTS `complex_text_id_sup`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `complex_text_id_sup` (
  `id` int(11) NOT NULL AUTO_INCREMENT,
  `txtid_bool` tinyint(3) unsigned NOT NULL,
  `txti_date` date NOT NULL,
  PRIMARY KEY (`id`)
) ENGINE=MyISAM DEFAULT CHARSET=utf8 COLLATE=utf8_swedish_ci;
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Dumping data for table `complex_text_id_sup`
--

LOCK TABLES `complex_text_id_sup` WRITE;
/*!40000 ALTER TABLE `complex_text_id_sup` DISABLE KEYS */;
/*!40000 ALTER TABLE `complex_text_id_sup` ENABLE KEYS */;
UNLOCK TABLES;

--
-- Table structure for table `complex_type_pk`
--

DROP TABLE IF EXISTS `complex_type_pk`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `complex_type_pk` (
  `pk_bool` tinyint(3) unsigned NOT NULL,
  `pk_date` date NOT NULL,
  `pk_attr` int(10) unsigned NOT NULL DEFAULT '0',
  `bool_value` tinyint(3) unsigned DEFAULT NULL,
  PRIMARY KEY (`pk_bool`,`pk_date`,`pk_attr`)
) ENGINE=MyISAM DEFAULT CHARSET=utf8 COLLATE=utf8_swedish_ci;
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Dumping data for table `complex_type_pk`
--

LOCK TABLES `complex_type_pk` WRITE;
/*!40000 ALTER TABLE `complex_type_pk` DISABLE KEYS */;
/*!40000 ALTER TABLE `complex_type_pk` ENABLE KEYS */;
UNLOCK TABLES;

--
-- Table structure for table `complex_type_test2`
--

DROP TABLE IF EXISTS `complex_type_test2`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `complex_type_test2` (
  `id` int(11) NOT NULL AUTO_INCREMENT,
  `acplx_bool` tinyint(3) unsigned DEFAULT NULL,
  `acplx_date` date DEFAULT NULL,
  `a_simp` smallint(5) unsigned DEFAULT NULL,
  `b_bool` tinyint(3) unsigned NOT NULL,
  `b_date` date NOT NULL,
  PRIMARY KEY (`id`)
) ENGINE=MyISAM DEFAULT CHARSET=utf8 COLLATE=utf8_swedish_ci;
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Dumping data for table `complex_type_test2`
--

LOCK TABLES `complex_type_test2` WRITE;
/*!40000 ALTER TABLE `complex_type_test2` DISABLE KEYS */;
/*!40000 ALTER TABLE `complex_type_test2` ENABLE KEYS */;
UNLOCK TABLES;

--
-- Table structure for table `enum_datatypes`
--

DROP TABLE IF EXISTS `enum_datatypes`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `enum_datatypes` (
  `id` int(11) NOT NULL AUTO_INCREMENT,
  `simple_enum` smallint(5) unsigned DEFAULT NULL,
  PRIMARY KEY (`id`)
) ENGINE=MyISAM AUTO_INCREMENT=4 DEFAULT CHARSET=utf8 COLLATE=utf8_swedish_ci;
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Dumping data for table `enum_datatypes`
--

LOCK TABLES `enum_datatypes` WRITE;
/*!40000 ALTER TABLE `enum_datatypes` DISABLE KEYS */;
INSERT INTO `enum_datatypes` VALUES (1,1),(2,2);
/*!40000 ALTER TABLE `enum_datatypes` ENABLE KEYS */;
UNLOCK TABLES;

--
-- Table structure for table `html_escape_class`
--

DROP TABLE IF EXISTS `html_escape_class`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `html_escape_class` (
  `id` int(11) NOT NULL,
  `s` varchar(5) COLLATE utf8_swedish_ci DEFAULT NULL,
  `escape_header` int(11) DEFAULT NULL,
  `hee` smallint(5) unsigned DEFAULT NULL,
  `string` varchar(30) COLLATE utf8_swedish_ci DEFAULT NULL,
  `txt` text COLLATE utf8_swedish_ci,
  PRIMARY KEY (`id`)
) ENGINE=MyISAM DEFAULT CHARSET=utf8 COLLATE=utf8_swedish_ci;
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Dumping data for table `html_escape_class`
--

LOCK TABLES `html_escape_class` WRITE;
/*!40000 ALTER TABLE `html_escape_class` DISABLE KEYS */;
INSERT INTO `html_escape_class` VALUES (1,'\"2\"',2,2,'<><>','<><>');
/*!40000 ALTER TABLE `html_escape_class` ENABLE KEYS */;
UNLOCK TABLES;

--
-- Table structure for table `html_escape_class_validval`
--

DROP TABLE IF EXISTS `html_escape_class_validval`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `html_escape_class_validval` (
  `id` varchar(5) COLLATE utf8_swedish_ci NOT NULL,
  `text_id_may_need_escape` varchar(30) COLLATE utf8_swedish_ci DEFAULT NULL,
  PRIMARY KEY (`id`)
) ENGINE=MyISAM DEFAULT CHARSET=utf8 COLLATE=utf8_swedish_ci;
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Dumping data for table `html_escape_class_validval`
--

LOCK TABLES `html_escape_class_validval` WRITE;
/*!40000 ALTER TABLE `html_escape_class_validval` DISABLE KEYS */;
INSERT INTO `html_escape_class_validval` VALUES ('<1>','<1>'),('\"2\"','\"2\"');
/*!40000 ALTER TABLE `html_escape_class_validval` ENABLE KEYS */;
UNLOCK TABLES;

--
-- Table structure for table `keys_test`
--

DROP TABLE IF EXISTS `keys_test`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `keys_test` (
  `cplx_cplx_bool` tinyint(3) unsigned NOT NULL,
  `cplx_cplx_date` date NOT NULL,
  `cplx_simp` smallint(5) unsigned NOT NULL,
  `simp` int(11) NOT NULL,
  PRIMARY KEY (`cplx_cplx_bool`,`cplx_cplx_date`,`cplx_simp`),
  UNIQUE KEY `simp` (`simp`),
  UNIQUE KEY `simp_2` (`simp`,`cplx_cplx_bool`,`cplx_cplx_date`,`cplx_simp`)
) ENGINE=MyISAM DEFAULT CHARSET=utf8 COLLATE=utf8_swedish_ci;
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Dumping data for table `keys_test`
--

LOCK TABLES `keys_test` WRITE;
/*!40000 ALTER TABLE `keys_test` DISABLE KEYS */;
/*!40000 ALTER TABLE `keys_test` ENABLE KEYS */;
UNLOCK TABLES;

--
-- Table structure for table `ot_with_1_textid`
--

DROP TABLE IF EXISTS `ot_with_1_textid`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `ot_with_1_textid` (
  `id` int(11) NOT NULL AUTO_INCREMENT,
  `text_id` varchar(30) COLLATE utf8_swedish_ci NOT NULL,
  PRIMARY KEY (`id`)
) ENGINE=MyISAM AUTO_INCREMENT=5 DEFAULT CHARSET=utf8 COLLATE=utf8_swedish_ci;
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Dumping data for table `ot_with_1_textid`
--

LOCK TABLES `ot_with_1_textid` WRITE;
/*!40000 ALTER TABLE `ot_with_1_textid` DISABLE KEYS */;
INSERT INTO `ot_with_1_textid` VALUES (1,'Textid-2-1'),(2,'Textid-1-2'),(3,'igen'),(4,'2010-10-17 13.54.18');
/*!40000 ALTER TABLE `ot_with_1_textid` ENABLE KEYS */;
UNLOCK TABLES;

--
-- Table structure for table `ot_with_mult_textid`
--

DROP TABLE IF EXISTS `ot_with_mult_textid`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `ot_with_mult_textid` (
  `id` int(11) NOT NULL AUTO_INCREMENT,
  `text_id_1` varchar(30) COLLATE utf8_swedish_ci NOT NULL,
  `text_id_2` varchar(30) COLLATE utf8_swedish_ci NOT NULL,
  PRIMARY KEY (`id`)
) ENGINE=MyISAM AUTO_INCREMENT=4 DEFAULT CHARSET=utf8 COLLATE=utf8_swedish_ci;
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Dumping data for table `ot_with_mult_textid`
--

LOCK TABLES `ot_with_mult_textid` WRITE;
/*!40000 ALTER TABLE `ot_with_mult_textid` DISABLE KEYS */;
INSERT INTO `ot_with_mult_textid` VALUES (1,'1','20'),(2,'2','30'),(3,'2','10');
/*!40000 ALTER TABLE `ot_with_mult_textid` ENABLE KEYS */;
UNLOCK TABLES;

--
-- Table structure for table `ot_without_textid`
--

DROP TABLE IF EXISTS `ot_without_textid`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `ot_without_textid` (
  `id` int(11) NOT NULL AUTO_INCREMENT,
  `name` varchar(30) COLLATE utf8_swedish_ci NOT NULL,
  PRIMARY KEY (`id`)
) ENGINE=MyISAM AUTO_INCREMENT=11 DEFAULT CHARSET=utf8 COLLATE=utf8_swedish_ci;
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Dumping data for table `ot_without_textid`
--

LOCK TABLES `ot_without_textid` WRITE;
/*!40000 ALTER TABLE `ot_without_textid` DISABLE KEYS */;
INSERT INTO `ot_without_textid` VALUES (1,'Utan-textid-1'),(2,'Utan-textid-2'),(3,'sdf'),(4,'sdf'),(5,'sdf'),(6,'sdf'),(7,'sdf2'),(8,'sdf2'),(9,'sdfsdfsdf'),(10,'2010-10-17 12.55.46');
/*!40000 ALTER TABLE `ot_without_textid` ENABLE KEYS */;
UNLOCK TABLES;

--
-- Table structure for table `otw1t_referer`
--

DROP TABLE IF EXISTS `otw1t_referer`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `otw1t_referer` ( -- Otw1tr
  `id` int(11) NOT NULL AUTO_INCREMENT,
  `idOtw1tOtw1t` int(11) NOT NULL,
  `sOtw1trName` varchar(30) COLLATE utf8_swedish_ci NOT NULL,
  PRIMARY KEY (`id`)
) ENGINE=MyISAM AUTO_INCREMENT=4 DEFAULT CHARSET=utf8 COLLATE=utf8_swedish_ci;
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Dumping data for table `otw1t_referer`
--

LOCK TABLES `otw1t_referer` WRITE;
/*!40000 ALTER TABLE `otw1t_referer` DISABLE KEYS */;
INSERT INTO `otw1t_referer` VALUES (1,1,'Ref-post-1'),(2,2,'Ref-post-2');
/*!40000 ALTER TABLE `otw1t_referer` ENABLE KEYS */;
UNLOCK TABLES;

--
-- Table structure for table `otwmt_referer`
--

DROP TABLE IF EXISTS `otwmt_referer`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `otwmt_referer` (
  `idOtwmtrId` int(11) NOT NULL AUTO_INCREMENT,
  `idOtwmtOtwmt` int(11) NOT NULL,
  `sOtwmtrName` varchar(30) COLLATE utf8_swedish_ci NOT NULL,
  `nEnum` smallint(5) unsigned DEFAULT NULL,
  PRIMARY KEY (`idOtwmtrId`)
) ENGINE=MyISAM AUTO_INCREMENT=5 DEFAULT CHARSET=utf8 COLLATE=utf8_swedish_ci;
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Dumping data for table `otwmt_referer`
--

LOCK TABLES `otwmt_referer` WRITE;
/*!40000 ALTER TABLE `otwmt_referer` DISABLE KEYS */;
INSERT INTO `otwmt_referer` VALUES (1,1,'1/20',NULL),(2,3,'2/10',NULL),(3,2,'1/30',1),(4,2,'1/30#2',2);
/*!40000 ALTER TABLE `otwmt_referer` ENABLE KEYS */;
UNLOCK TABLES;

--
-- Table structure for table `otwot_referer`
--

DROP TABLE IF EXISTS `otwot_referer`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `otwot_referer` (
  `idOtwotrId` int(11) NOT NULL AUTO_INCREMENT,
  `idOtwotOtwot` int(11) NOT NULL,
  `sOtwotrName` varchar(30) COLLATE utf8_swedish_ci NOT NULL,
  PRIMARY KEY (`idOtwotrId`)
) ENGINE=MyISAM AUTO_INCREMENT=3 DEFAULT CHARSET=utf8 COLLATE=utf8_swedish_ci;
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Dumping data for table `otwot_referer`
--

LOCK TABLES `otwot_referer` WRITE;
/*!40000 ALTER TABLE `otwot_referer` DISABLE KEYS */;
INSERT INTO `otwot_referer` VALUES (1,1,'ref-1'),(2,2,'ref-2');
/*!40000 ALTER TABLE `otwot_referer` ENABLE KEYS */;
UNLOCK TABLES;

--
-- Table structure for table `ref`
--

DROP TABLE IF EXISTS `ref`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `ref` (
  `x` int(11) DEFAULT NULL,
  `y` int(11) DEFAULT NULL,
  KEY `fk` (`x`,`y`)
) ENGINE=MyISAM DEFAULT CHARSET=utf8 COLLATE=utf8_swedish_ci;
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Dumping data for table `ref`
--

LOCK TABLES `ref` WRITE;
/*!40000 ALTER TABLE `ref` DISABLE KEYS */;
/*!40000 ALTER TABLE `ref` ENABLE KEYS */;
UNLOCK TABLES;

--
-- Table structure for table `reference`
--

DROP TABLE IF EXISTS `reference`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `reference` (
  `idRefId` int(11) DEFAULT NULL,
  `sds_idSdId` int(11) DEFAULT NULL,
  `idAutoIncId` int(11) DEFAULT NULL,
  `idAutoIncId_opt` int(11) DEFAULT NULL
) ENGINE=MyISAM DEFAULT CHARSET=utf8 COLLATE=utf8_swedish_ci;
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Dumping data for table `reference`
--

LOCK TABLES `reference` WRITE;
/*!40000 ALTER TABLE `reference` DISABLE KEYS */;
INSERT INTO `reference` VALUES (1,1,10,10),(11,3,10,NULL),(10,3,10,NULL);
/*!40000 ALTER TABLE `reference` ENABLE KEYS */;
UNLOCK TABLES;

--
-- Table structure for table `shared_comp_super`
--

DROP TABLE IF EXISTS `shared_comp_super`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `shared_comp_super` (
  `idScosId` int(11) NOT NULL AUTO_INCREMENT,
  PRIMARY KEY (`idScosId`)
) ENGINE=MyISAM AUTO_INCREMENT=11 DEFAULT CHARSET=utf8 COLLATE=utf8_swedish_ci;
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Dumping data for table `shared_comp_super`
--

LOCK TABLES `shared_comp_super` WRITE;
/*!40000 ALTER TABLE `shared_comp_super` DISABLE KEYS */;
INSERT INTO `shared_comp_super` VALUES (1);
/*!40000 ALTER TABLE `shared_comp_super` ENABLE KEYS */;
UNLOCK TABLES;

--
-- Table structure for table `simple_datatypes`
--

DROP TABLE IF EXISTS `simple_datatypes`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `simple_datatypes` (
  `idSdId` int(11) NOT NULL,
  `bBoolean` tinyint(3) unsigned DEFAULT NULL,
  `nInt` int(11) NOT NULL,
  `fReal` double DEFAULT NULL,
  `dDate` date DEFAULT NULL,
  `tTime` time DEFAULT NULL,
  `dtDatetime` datetime DEFAULT NULL,
  `sString` varchar(30) COLLATE utf8_swedish_ci DEFAULT NULL,
  `txtText` text COLLATE utf8_swedish_ci,
  `txtHtmlText` text COLLATE utf8_swedish_ci,
  `sUrl` varchar(255) COLLATE utf8_swedish_ci DEFAULT NULL,
  PRIMARY KEY (`idSdId`)
) ENGINE=MyISAM DEFAULT CHARSET=utf8 COLLATE=utf8_swedish_ci;
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Dumping data for table `simple_datatypes`
--

LOCK TABLES `simple_datatypes` WRITE;
/*!40000 ALTER TABLE `simple_datatypes` DISABLE KEYS */;
INSERT INTO `simple_datatypes` VALUES (1,1,2,2.2,'2002-02-22','22:22:22','2006-07-23 22:00:00','str222','                                                                                          text244\r\n                           \r\n                           \r\n                           ','                                                                                          <i>HTML</i>\r\n                           \r\n                           \r\n                           ','Url2'),(0,0,3,10,'2001-01-11','11:11:11',NULL,'str1 Ã¤ndrad','                              text1\r\n                           ','                           ','Url1'),(3,0,1,3.4,'2006-07-23','22:00:00','2006-07-23 22:00:00','sdf','sdf','<ul>\r\n  <li>hej dÃ¤r</li>\r\n</ul>\r\n','sdfsdf'),(10,0,10,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL),(20,0,20,2,NULL,NULL,NULL,'rÃ¶dbeta','                              rÃ¤ttika 2\r\n                           ','                           ',''),(100,NULL,0,100,NULL,NULL,NULL,'hej','','',''),(2,NULL,0,NULL,NULL,NULL,NULL,'nya frÃ¤cha tjÃ¤nster',NULL,NULL,NULL),(1000,0,0,3.4,'2006-07-23',NULL,NULL,'sdf','                              sdf\r\n                           ','                              <ul>\r\n  <li>hej dÃ¤r</li>\r\n</ul>\r\n\r\n                           ','sdfsdf'),(5,1,0,6,'2013-06-10',NULL,NULL,'stÃ¤dat2','lite text = longString2','html2','url2'),(4,1,0,2.2,'2002-02-22',NULL,NULL,'str444','                                                                                          text244\r\n                           \r\n                           \r\n                           ','                                                                                          <i>HTML</i>\r\n                           \r\n                           \r\n                           ','Url2');
/*!40000 ALTER TABLE `simple_datatypes` ENABLE KEYS */;
UNLOCK TABLES;

--
-- Table structure for table `simple_datatypes2`
--

DROP TABLE IF EXISTS `simple_datatypes2`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `simple_datatypes2` (
  `idSd2Id` int(11) NOT NULL,
  `nHhMm` smallint(6) DEFAULT NULL,
  `nHhMmSs` int(11) DEFAULT NULL,
  `nHhMmU` smallint(5) unsigned DEFAULT NULL,
  `nHhMmSsU` int(10) unsigned DEFAULT NULL,
  PRIMARY KEY (`idSd2Id`)
) ENGINE=MyISAM DEFAULT CHARSET=utf8 COLLATE=utf8_swedish_ci;
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Dumping data for table `simple_datatypes2`
--

LOCK TABLES `simple_datatypes2` WRITE;
/*!40000 ALTER TABLE `simple_datatypes2` DISABLE KEYS */;
INSERT INTO `simple_datatypes2` VALUES (0,90,252,3905,18065),(1,620,37230,620,37230);
/*!40000 ALTER TABLE `simple_datatypes2` ENABLE KEYS */;
UNLOCK TABLES;

--
-- Table structure for table `sorting`
--

DROP TABLE IF EXISTS `sorting`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `sorting` (
  `idSortingId` int(11) NOT NULL,
  `dDate` date DEFAULT NULL,
  `nInteger` int(11) DEFAULT NULL,
  PRIMARY KEY (`idSortingId`)
) ENGINE=MyISAM DEFAULT CHARSET=utf8 COLLATE=utf8_swedish_ci;
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Dumping data for table `sorting`
--

LOCK TABLES `sorting` WRITE;
/*!40000 ALTER TABLE `sorting` DISABLE KEYS */;
INSERT INTO `sorting` VALUES (0,'2008-08-01',0),(1,'2008-08-02',1),(3,NULL,3);
/*!40000 ALTER TABLE `sorting` ENABLE KEYS */;
UNLOCK TABLES;

--
-- Table structure for table `t`
--

DROP TABLE IF EXISTS `t`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `t` (
  `x` int(11) NOT NULL DEFAULT '0',
  `y` int(11) NOT NULL DEFAULT '0',
  PRIMARY KEY (`x`,`y`)
) ENGINE=MyISAM DEFAULT CHARSET=utf8 COLLATE=utf8_swedish_ci;
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Dumping data for table `t`
--

LOCK TABLES `t` WRITE;
/*!40000 ALTER TABLE `t` DISABLE KEYS */;
/*!40000 ALTER TABLE `t` ENABLE KEYS */;
UNLOCK TABLES;
/*!40103 SET TIME_ZONE=@OLD_TIME_ZONE */;

/*!40101 SET SQL_MODE=@OLD_SQL_MODE */;
/*!40014 SET FOREIGN_KEY_CHECKS=@OLD_FOREIGN_KEY_CHECKS */;
/*!40014 SET UNIQUE_CHECKS=@OLD_UNIQUE_CHECKS */;
/*!40101 SET CHARACTER_SET_CLIENT=@OLD_CHARACTER_SET_CLIENT */;
/*!40101 SET CHARACTER_SET_RESULTS=@OLD_CHARACTER_SET_RESULTS */;
/*!40101 SET COLLATION_CONNECTION=@OLD_COLLATION_CONNECTION */;
/*!40111 SET SQL_NOTES=@OLD_SQL_NOTES */;

-- Dump completed on 2015-01-17 21:37:22
