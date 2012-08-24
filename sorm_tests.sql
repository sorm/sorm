/*
 Navicat Premium Data Transfer

 Source Server         : localhost
 Source Server Type    : MySQL
 Source Server Version : 50525
 Source Host           : localhost
 Source Database       : vorm_tests

 Target Server Type    : MySQL
 Target Server Version : 50525
 File Encoding         : utf-8

 Date: 07/22/2012 01:47:49 AM
*/

SET NAMES utf8;
SET FOREIGN_KEY_CHECKS = 0;

-- ----------------------------
--  Table structure for `artist`
-- ----------------------------
DROP TABLE IF EXISTS `artist`;
CREATE TABLE `artist` (
  `id` int(11) unsigned NOT NULL AUTO_INCREMENT,
  PRIMARY KEY (`id`)
) ENGINE=InnoDB AUTO_INCREMENT=5 DEFAULT CHARSET=utf8;

-- ----------------------------
--  Records of `artist`
-- ----------------------------
BEGIN;
INSERT INTO `artist` VALUES ('1'), ('2'), ('3'), ('4');
COMMIT;

-- ----------------------------
--  Table structure for `artist$names`
-- ----------------------------
DROP TABLE IF EXISTS `artist$names`;
CREATE TABLE `artist$names` (
  `p_id` int(10) unsigned NOT NULL,
  `i` int(11) NOT NULL,
  `v_id` int(10) unsigned NOT NULL,
  PRIMARY KEY (`p_id`,`i`),
  KEY `v_id` (`v_id`),
  CONSTRAINT `artist@0024names_ibfk_1` FOREIGN KEY (`p_id`) REFERENCES `artist` (`id`) ON DELETE CASCADE,
  CONSTRAINT `artist@0024names_ibfk_2` FOREIGN KEY (`v_id`) REFERENCES `name` (`id`) ON DELETE CASCADE
) ENGINE=InnoDB DEFAULT CHARSET=utf8;

-- ----------------------------
--  Records of `artist$names`
-- ----------------------------
BEGIN;
INSERT INTO `artist$names` VALUES ('1', '0', '1'), ('2', '0', '2'), ('2', '1', '3'), ('1', '1', '4'), ('1', '2', '5'), ('3', '1', '6'), ('3', '0', '7'), ('4', '1', '8'), ('4', '2', '9'), ('4', '0', '10');
COMMIT;

-- ----------------------------
--  Table structure for `artist$styles`
-- ----------------------------
DROP TABLE IF EXISTS `artist$styles`;
CREATE TABLE `artist$styles` (
  `p_id` int(10) unsigned NOT NULL,
  `h` int(11) NOT NULL,
  `v_id` int(10) unsigned NOT NULL,
  PRIMARY KEY (`p_id`,`h`),
  KEY `v_id` (`v_id`),
  CONSTRAINT `artist@0024styles_ibfk_2` FOREIGN KEY (`v_id`) REFERENCES `style` (`id`) ON DELETE CASCADE,
  CONSTRAINT `artist@0024styles_ibfk_1` FOREIGN KEY (`p_id`) REFERENCES `artist` (`id`) ON DELETE CASCADE
) ENGINE=InnoDB DEFAULT CHARSET=utf8;

-- ----------------------------
--  Records of `artist$styles`
-- ----------------------------
BEGIN;
INSERT INTO `artist$styles` VALUES ('3', '1', '1'), ('1', '2', '2'), ('1', '3', '3'), ('2', '3', '3'), ('3', '3', '3'), ('4', '3', '3'), ('1', '4', '4'), ('3', '4', '4');
COMMIT;

-- ----------------------------
--  Table structure for `language`
-- ----------------------------
DROP TABLE IF EXISTS `language`;
CREATE TABLE `language` (
  `id` int(10) unsigned NOT NULL AUTO_INCREMENT,
  `code` varchar(2) NOT NULL COMMENT 'ISO 639-1 code',
  PRIMARY KEY (`id`),
  UNIQUE KEY `code` (`code`)
) ENGINE=InnoDB AUTO_INCREMENT=3 DEFAULT CHARSET=utf8;

-- ----------------------------
--  Records of `language`
-- ----------------------------
BEGIN;
INSERT INTO `language` VALUES ('2', 'en'), ('1', 'ru');
COMMIT;

-- ----------------------------
--  Table structure for `name`
-- ----------------------------
DROP TABLE IF EXISTS `name`;
CREATE TABLE `name` (
  `id` int(10) unsigned NOT NULL AUTO_INCREMENT,
  `language_id` int(10) unsigned NOT NULL,
  `value` text NOT NULL,
  PRIMARY KEY (`id`),
  KEY `language_id` (`language_id`),
  CONSTRAINT `name_ibfk_1` FOREIGN KEY (`language_id`) REFERENCES `language` (`id`) ON DELETE CASCADE
) ENGINE=InnoDB AUTO_INCREMENT=19 DEFAULT CHARSET=utf8;

-- ----------------------------
--  Records of `name`
-- ----------------------------
BEGIN;
INSERT INTO `name` VALUES ('1', '2', 'Metallica'), ('2', '1', 'Кино'), ('3', '2', 'Kino'), ('4', '1', 'Металика'), ('5', '1', 'Металлика'), ('6', '1', 'Нирвана'), ('7', '2', 'Nirvana'), ('8', '2', 'Rolling Stones'), ('9', '2', 'Rolling Stones, the'), ('10', '2', 'The Rolling Stones'), ('11', '2', 'Grunge'), ('12', '1', 'Гранж'), ('13', '1', 'Метал'), ('14', '1', 'Рок'), ('15', '2', 'Metal'), ('16', '2', 'Rock'), ('17', '2', 'Hard Rock'), ('18', '1', 'Грандж');
COMMIT;

-- ----------------------------
--  Table structure for `style`
-- ----------------------------
DROP TABLE IF EXISTS `style`;
CREATE TABLE `style` (
  `id` int(10) unsigned NOT NULL AUTO_INCREMENT,
  PRIMARY KEY (`id`)
) ENGINE=InnoDB AUTO_INCREMENT=5 DEFAULT CHARSET=utf8;

-- ----------------------------
--  Records of `style`
-- ----------------------------
BEGIN;
INSERT INTO `style` VALUES ('1'), ('2'), ('3'), ('4');
COMMIT;

-- ----------------------------
--  Table structure for `style$names`
-- ----------------------------
DROP TABLE IF EXISTS `style$names`;
CREATE TABLE `style$names` (
  `p_id` int(10) unsigned NOT NULL,
  `i` int(11) NOT NULL,
  `v_id` int(10) unsigned NOT NULL,
  PRIMARY KEY (`p_id`,`i`),
  KEY `v_id` (`v_id`) USING BTREE,
  CONSTRAINT `style@0024names_ibfk_1` FOREIGN KEY (`p_id`) REFERENCES `style` (`id`) ON DELETE CASCADE,
  CONSTRAINT `style@0024names_ibfk_2` FOREIGN KEY (`v_id`) REFERENCES `name` (`id`) ON DELETE CASCADE
) ENGINE=InnoDB DEFAULT CHARSET=utf8 ROW_FORMAT=COMPACT;

-- ----------------------------
--  Records of `style$names`
-- ----------------------------
BEGIN;
INSERT INTO `style$names` VALUES ('1', '0', '11'), ('1', '1', '12'), ('2', '0', '13'), ('3', '0', '14'), ('2', '1', '15'), ('3', '1', '16'), ('4', '0', '17'), ('1', '3', '18');
COMMIT;

SET FOREIGN_KEY_CHECKS = 1;
