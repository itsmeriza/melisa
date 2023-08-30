/*
SQLyog Ultimate v12.4.3 (64 bit)
MySQL - 8.0.31 : Database - terasehat
*********************************************************************
*/

/*!40101 SET NAMES utf8 */;

/*!40101 SET SQL_MODE=''*/;

/*!40014 SET @OLD_UNIQUE_CHECKS=@@UNIQUE_CHECKS, UNIQUE_CHECKS=0 */;
/*!40014 SET @OLD_FOREIGN_KEY_CHECKS=@@FOREIGN_KEY_CHECKS, FOREIGN_KEY_CHECKS=0 */;
/*!40101 SET @OLD_SQL_MODE=@@SQL_MODE, SQL_MODE='NO_AUTO_VALUE_ON_ZERO' */;
/*!40111 SET @OLD_SQL_NOTES=@@SQL_NOTES, SQL_NOTES=0 */;
/*Table structure for table `base` */

DROP TABLE IF EXISTS `base`;

CREATE TABLE `base` (
  `id` int unsigned DEFAULT NULL,
  `notes` text,
  `deleted` tinyint(1) DEFAULT '0',
  `deleted_at` datetime DEFAULT NULL,
  `deleted_by` int unsigned DEFAULT NULL,
  `created_at` datetime DEFAULT CURRENT_TIMESTAMP,
  `created_by` int unsigned DEFAULT NULL,
  `modified_at` datetime DEFAULT NULL,
  `modified_by` int unsigned DEFAULT NULL,
  `entity_id` int unsigned DEFAULT NULL
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_0900_ai_ci;

/*Data for the table `base` */

/*Table structure for table `sys_access` */

DROP TABLE IF EXISTS `sys_access`;

CREATE TABLE `sys_access` (
  `id` smallint unsigned NOT NULL,
  `notes` text,
  `deleted` tinyint(1) DEFAULT '0',
  `deleted_at` datetime DEFAULT NULL,
  `deleted_by` int unsigned DEFAULT NULL,
  `created_at` datetime DEFAULT CURRENT_TIMESTAMP,
  `created_by` int unsigned DEFAULT NULL,
  `modified_at` datetime DEFAULT NULL,
  `modified_by` int unsigned DEFAULT NULL,
  `entity_id` int unsigned DEFAULT NULL,
  `module_id` smallint unsigned DEFAULT NULL,
  `access_name` varchar(100) DEFAULT NULL,
  `command` varchar(100) CHARACTER SET utf8mb4 COLLATE utf8mb4_0900_ai_ci DEFAULT NULL,
  PRIMARY KEY (`id`),
  KEY `fk_sys_access_module` (`module_id`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_0900_ai_ci;

/*Data for the table `sys_access` */

insert  into `sys_access`(`id`,`notes`,`deleted`,`deleted_at`,`deleted_by`,`created_at`,`created_by`,`modified_at`,`modified_by`,`entity_id`,`module_id`,`access_name`,`command`) values 
(1,NULL,0,NULL,NULL,'2023-03-01 03:30:25',NULL,NULL,NULL,NULL,1,'module',NULL),
(2,NULL,0,NULL,NULL,'2023-03-08 08:02:57',NULL,NULL,NULL,NULL,2,'module',NULL);

/*Table structure for table `sys_caption` */

DROP TABLE IF EXISTS `sys_caption`;

CREATE TABLE `sys_caption` (
  `id` int unsigned NOT NULL AUTO_INCREMENT,
  `notes` text,
  `deleted` tinyint(1) DEFAULT '0',
  `deleted_at` datetime DEFAULT NULL,
  `deleted_by` int unsigned DEFAULT NULL,
  `created_at` datetime DEFAULT CURRENT_TIMESTAMP,
  `created_by` int unsigned DEFAULT NULL,
  `modified_at` datetime DEFAULT NULL,
  `modified_by` int unsigned DEFAULT NULL,
  `menu_id` int unsigned DEFAULT NULL,
  `entity_id` int unsigned DEFAULT NULL,
  `route` varchar(200) CHARACTER SET utf8mb4 COLLATE utf8mb4_0900_ai_ci DEFAULT NULL,
  `css_selector` varchar(200) CHARACTER SET utf8mb4 COLLATE utf8mb4_0900_ai_ci DEFAULT NULL,
  `language_id` tinyint unsigned DEFAULT NULL,
  `caption` text,
  PRIMARY KEY (`id`),
  UNIQUE KEY `UNIQUE_PAGE` (`route`,`css_selector`,`language_id`),
  UNIQUE KEY `UNIQUE_MENU` (`menu_id`,`language_id`)
) ENGINE=InnoDB AUTO_INCREMENT=29 DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_0900_ai_ci;

/*Data for the table `sys_caption` */

insert  into `sys_caption`(`id`,`notes`,`deleted`,`deleted_at`,`deleted_by`,`created_at`,`created_by`,`modified_at`,`modified_by`,`menu_id`,`entity_id`,`route`,`css_selector`,`language_id`,`caption`) values 
(1,NULL,0,NULL,NULL,'2023-03-15 03:25:32',NULL,NULL,NULL,1,NULL,NULL,NULL,1,'Dasbor'),
(2,NULL,0,NULL,NULL,'2023-03-15 03:25:45',NULL,NULL,NULL,1,NULL,NULL,NULL,2,'Dashboard'),
(3,NULL,0,NULL,NULL,'2023-03-15 06:16:21',NULL,NULL,NULL,2,NULL,NULL,NULL,1,'Alur Pelayanan'),
(4,NULL,0,NULL,NULL,'2023-03-15 06:16:32',NULL,NULL,NULL,2,NULL,NULL,NULL,2,'Service Flow'),
(5,NULL,0,NULL,NULL,'2023-03-15 06:29:51',NULL,NULL,NULL,3,NULL,NULL,NULL,1,'Pendaftaran'),
(6,NULL,0,NULL,NULL,'2023-03-15 06:30:02',NULL,NULL,NULL,3,NULL,NULL,NULL,2,'Admission'),
(7,NULL,0,NULL,NULL,'2023-03-15 07:57:57',NULL,NULL,NULL,4,NULL,NULL,NULL,1,'Pemeriksaan Awal'),
(8,NULL,0,NULL,NULL,'2023-03-15 07:58:11',NULL,NULL,NULL,4,NULL,NULL,NULL,2,'Assessment'),
(9,NULL,0,NULL,NULL,'2023-03-15 08:09:27',NULL,NULL,NULL,5,NULL,NULL,NULL,1,'Pemeriksaan Dokter'),
(10,NULL,0,NULL,NULL,'2023-03-15 08:09:34',NULL,NULL,NULL,5,NULL,NULL,NULL,2,'Diagnosis'),
(11,NULL,0,NULL,NULL,'2023-03-15 08:19:44',NULL,NULL,NULL,6,NULL,NULL,NULL,1,'Penunjang'),
(12,NULL,0,NULL,NULL,'2023-03-15 08:20:20',NULL,NULL,NULL,6,NULL,NULL,NULL,2,'Supporting Unit'),
(13,NULL,0,NULL,NULL,'2023-03-15 08:24:17',NULL,NULL,NULL,7,NULL,NULL,NULL,1,'Laboratorium'),
(14,NULL,0,NULL,NULL,'2023-03-15 08:24:31',NULL,NULL,NULL,7,NULL,NULL,NULL,2,'Laboratory'),
(15,NULL,0,NULL,NULL,'2023-03-15 08:33:22',NULL,NULL,NULL,8,NULL,NULL,NULL,1,'Radiologi'),
(16,NULL,0,NULL,NULL,'2023-03-15 08:33:36',NULL,NULL,NULL,8,NULL,NULL,NULL,2,'Radiology'),
(17,NULL,0,NULL,NULL,'2023-03-15 10:32:16',NULL,NULL,NULL,9,NULL,NULL,NULL,1,'Fisioterapi'),
(18,NULL,0,NULL,NULL,'2023-03-15 10:32:29',NULL,NULL,NULL,9,NULL,NULL,NULL,2,'Physiotherapy'),
(19,NULL,0,NULL,NULL,'2023-03-16 02:26:12',NULL,NULL,NULL,10,NULL,NULL,NULL,1,'Kasir'),
(20,NULL,0,NULL,NULL,'2023-03-16 02:26:48',NULL,NULL,NULL,10,NULL,NULL,NULL,2,'Cashier'),
(21,NULL,0,NULL,NULL,'2023-03-16 03:18:04',NULL,NULL,NULL,11,NULL,NULL,NULL,1,'Apotek'),
(22,NULL,0,NULL,NULL,'2023-03-16 03:18:53',NULL,NULL,NULL,11,NULL,NULL,NULL,2,'Pharmacy'),
(23,NULL,0,NULL,NULL,'2023-03-28 07:15:56',NULL,NULL,NULL,12,NULL,NULL,NULL,1,'Entitas Medis'),
(24,NULL,0,NULL,NULL,'2023-03-28 07:16:09',NULL,NULL,NULL,12,NULL,NULL,NULL,2,'Medical Entity'),
(25,NULL,0,NULL,NULL,'2023-03-28 07:26:30',NULL,NULL,NULL,13,NULL,NULL,NULL,1,'Pasien'),
(26,NULL,0,NULL,NULL,'2023-03-28 07:26:49',NULL,NULL,NULL,13,NULL,NULL,NULL,2,'Patient'),
(27,NULL,0,NULL,NULL,'2023-03-28 08:29:57',NULL,NULL,NULL,14,NULL,NULL,NULL,1,'Paramedis'),
(28,NULL,0,NULL,NULL,'2023-03-28 08:30:07',NULL,NULL,NULL,14,NULL,NULL,NULL,2,'Paramedic');

/*Table structure for table `sys_city` */

DROP TABLE IF EXISTS `sys_city`;

CREATE TABLE `sys_city` (
  `id` int unsigned NOT NULL,
  `notes` text,
  `deleted` tinyint(1) DEFAULT '0',
  `deleted_at` datetime DEFAULT NULL,
  `deleted_by` int unsigned DEFAULT NULL,
  `created_at` datetime DEFAULT CURRENT_TIMESTAMP,
  `created_by` int unsigned DEFAULT NULL,
  `modified_at` datetime DEFAULT NULL,
  `modified_by` int unsigned DEFAULT NULL,
  `entity_id` int unsigned DEFAULT NULL,
  `name` varchar(100) DEFAULT NULL,
  `name_2nd` varchar(100) DEFAULT NULL,
  `nickname` varchar(50) DEFAULT NULL,
  `country_id` tinyint unsigned DEFAULT NULL,
  `state_id` int unsigned DEFAULT NULL,
  PRIMARY KEY (`id`),
  KEY `fk_sys_city_country` (`country_id`),
  KEY `fk_sys_city_state` (`state_id`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_0900_ai_ci;

/*Data for the table `sys_city` */

insert  into `sys_city`(`id`,`notes`,`deleted`,`deleted_at`,`deleted_by`,`created_at`,`created_by`,`modified_at`,`modified_by`,`entity_id`,`name`,`name_2nd`,`nickname`,`country_id`,`state_id`) values 
(1,NULL,0,NULL,NULL,'2023-03-23 21:48:59',NULL,NULL,NULL,NULL,'Tangerang Selatan',NULL,NULL,1,1);

/*Table structure for table `sys_country` */

DROP TABLE IF EXISTS `sys_country`;

CREATE TABLE `sys_country` (
  `id` tinyint unsigned NOT NULL,
  `notes` text,
  `deleted` tinyint(1) DEFAULT '0',
  `deleted_at` datetime DEFAULT NULL,
  `deleted_by` int unsigned DEFAULT NULL,
  `created_at` datetime DEFAULT CURRENT_TIMESTAMP,
  `created_by` int unsigned DEFAULT NULL,
  `modified_at` datetime DEFAULT NULL,
  `modified_by` int unsigned DEFAULT NULL,
  `entity_id` int unsigned DEFAULT NULL,
  `code_alpha_2` varchar(2) CHARACTER SET utf8mb4 COLLATE utf8mb4_0900_ai_ci DEFAULT NULL,
  `code_alpha_3` varchar(3) CHARACTER SET utf8mb4 COLLATE utf8mb4_0900_ai_ci DEFAULT NULL,
  `code_numeric` varchar(3) CHARACTER SET utf8mb4 COLLATE utf8mb4_0900_ai_ci DEFAULT NULL,
  `name` varchar(100) CHARACTER SET utf8mb4 COLLATE utf8mb4_0900_ai_ci DEFAULT NULL,
  `name_2nd` varchar(100) CHARACTER SET utf8mb4 COLLATE utf8mb4_0900_ai_ci DEFAULT NULL,
  `nickname` varchar(25) CHARACTER SET utf8mb4 COLLATE utf8mb4_0900_ai_ci DEFAULT NULL,
  PRIMARY KEY (`id`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_0900_ai_ci;

/*Data for the table `sys_country` */

insert  into `sys_country`(`id`,`notes`,`deleted`,`deleted_at`,`deleted_by`,`created_at`,`created_by`,`modified_at`,`modified_by`,`entity_id`,`code_alpha_2`,`code_alpha_3`,`code_numeric`,`name`,`name_2nd`,`nickname`) values 
(1,NULL,0,NULL,NULL,'2023-03-23 21:46:56',NULL,NULL,NULL,NULL,NULL,NULL,NULL,'Indonesia',NULL,NULL);

/*Table structure for table `sys_entity` */

DROP TABLE IF EXISTS `sys_entity`;

CREATE TABLE `sys_entity` (
  `id` int unsigned NOT NULL,
  `notes` text,
  `deleted` tinyint(1) DEFAULT '0',
  `deleted_at` datetime DEFAULT NULL,
  `deleted_by` int unsigned DEFAULT NULL,
  `created_at` datetime DEFAULT CURRENT_TIMESTAMP,
  `created_by` int unsigned DEFAULT NULL,
  `modified_at` datetime DEFAULT NULL,
  `modified_by` int unsigned DEFAULT NULL,
  `entity_id` int unsigned DEFAULT NULL,
  `name` varchar(100) DEFAULT NULL,
  `name_2nd` varchar(100) DEFAULT NULL,
  `nickname` varchar(50) DEFAULT NULL,
  `code` varchar(5) DEFAULT NULL,
  `zone_id` smallint unsigned DEFAULT NULL,
  `type_id` tinyint unsigned DEFAULT NULL,
  `owner_id` int unsigned NOT NULL,
  `street` varchar(100) DEFAULT NULL,
  `street_2nd` varchar(100) DEFAULT NULL,
  `city_id` smallint unsigned DEFAULT NULL,
  `state_id` tinyint unsigned DEFAULT NULL,
  `country_id` tinyint unsigned DEFAULT NULL,
  `postal_code` varchar(10) CHARACTER SET utf8mb4 COLLATE utf8mb4_0900_ai_ci DEFAULT NULL,
  `phone_number` varchar(20) DEFAULT NULL,
  `phone_number_2nd` varchar(20) DEFAULT NULL,
  `fax_number` varchar(20) DEFAULT NULL,
  `fax_number_2nd` varchar(20) DEFAULT NULL,
  `email` varchar(254) DEFAULT NULL,
  `website` text,
  PRIMARY KEY (`id`),
  KEY `fk_sys_entity_city` (`country_id`,`state_id`,`city_id`),
  KEY `fk_sys_entity_user` (`owner_id`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_0900_ai_ci;

/*Data for the table `sys_entity` */

insert  into `sys_entity`(`id`,`notes`,`deleted`,`deleted_at`,`deleted_by`,`created_at`,`created_by`,`modified_at`,`modified_by`,`entity_id`,`name`,`name_2nd`,`nickname`,`code`,`zone_id`,`type_id`,`owner_id`,`street`,`street_2nd`,`city_id`,`state_id`,`country_id`,`postal_code`,`phone_number`,`phone_number_2nd`,`fax_number`,`fax_number_2nd`,`email`,`website`) values 
(1,NULL,0,NULL,NULL,'2022-12-11 22:58:20',NULL,NULL,NULL,NULL,'Your Company',NULL,NULL,NULL,NULL,NULL,1,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL);

/*Table structure for table `sys_flexible_key` */

DROP TABLE IF EXISTS `sys_flexible_key`;

CREATE TABLE `sys_flexible_key` (
  `id` int unsigned DEFAULT NULL,
  `notes` text,
  `deleted` tinyint(1) DEFAULT '0',
  `deleted_at` datetime DEFAULT NULL,
  `deleted_by` int unsigned DEFAULT NULL,
  `created_at` datetime DEFAULT CURRENT_TIMESTAMP,
  `created_by` int unsigned DEFAULT NULL,
  `modified_at` datetime DEFAULT NULL,
  `modified_by` int unsigned DEFAULT NULL,
  `entity_id` int unsigned NOT NULL,
  `code` varchar(50) CHARACTER SET utf8mb4 COLLATE utf8mb4_0900_ai_ci NOT NULL,
  `format` text,
  `apart` text,
  `length` tinyint unsigned DEFAULT NULL,
  `last_apart` varchar(50) DEFAULT NULL,
  `is_reset` tinyint(1) DEFAULT NULL,
  `is_leading_zero` tinyint(1) DEFAULT NULL,
  `description` text,
  PRIMARY KEY (`entity_id`,`code`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_0900_ai_ci;

/*Data for the table `sys_flexible_key` */

/*Table structure for table `sys_flexible_key_counter` */

DROP TABLE IF EXISTS `sys_flexible_key_counter`;

CREATE TABLE `sys_flexible_key_counter` (
  `id` int unsigned DEFAULT NULL,
  `notes` text,
  `deleted` tinyint(1) DEFAULT '0',
  `deleted_at` datetime DEFAULT NULL,
  `deleted_by` int unsigned DEFAULT NULL,
  `created_at` datetime DEFAULT CURRENT_TIMESTAMP,
  `created_by` int unsigned DEFAULT NULL,
  `modified_at` datetime DEFAULT NULL,
  `modified_by` int unsigned DEFAULT NULL,
  `entity_id` int unsigned DEFAULT NULL,
  `code` varchar(50) DEFAULT NULL,
  `last_apart` varchar(50) DEFAULT NULL,
  `counter` int unsigned DEFAULT NULL,
  UNIQUE KEY `index_unique` (`entity_id`,`code`,`last_apart`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_0900_ai_ci;

/*Data for the table `sys_flexible_key_counter` */

/*Table structure for table `sys_group` */

DROP TABLE IF EXISTS `sys_group`;

CREATE TABLE `sys_group` (
  `id` smallint unsigned NOT NULL,
  `notes` text,
  `deleted` tinyint(1) DEFAULT '0',
  `deleted_at` datetime DEFAULT NULL,
  `deleted_by` int unsigned DEFAULT NULL,
  `created_at` datetime DEFAULT CURRENT_TIMESTAMP,
  `created_by` int unsigned DEFAULT NULL,
  `modified_at` datetime DEFAULT NULL,
  `modified_by` int unsigned DEFAULT NULL,
  `entity_id` int unsigned NOT NULL,
  `name` varchar(100) DEFAULT NULL,
  `name2nd` varchar(100) DEFAULT NULL,
  `nickname` varchar(100) DEFAULT NULL,
  PRIMARY KEY (`entity_id`,`id`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_0900_ai_ci;

/*Data for the table `sys_group` */

insert  into `sys_group`(`id`,`notes`,`deleted`,`deleted_at`,`deleted_by`,`created_at`,`created_by`,`modified_at`,`modified_by`,`entity_id`,`name`,`name2nd`,`nickname`) values 
(1,NULL,0,NULL,NULL,'2022-12-30 04:43:34',NULL,NULL,NULL,1,'Administrators',NULL,NULL),
(2,NULL,0,NULL,NULL,'2022-12-30 04:45:18',NULL,NULL,NULL,1,'Operators',NULL,NULL);

/*Table structure for table `sys_group_access` */

DROP TABLE IF EXISTS `sys_group_access`;

CREATE TABLE `sys_group_access` (
  `id` int unsigned DEFAULT NULL,
  `notes` text,
  `deleted` tinyint(1) DEFAULT '0',
  `deleted_at` datetime DEFAULT NULL,
  `deleted_by` int unsigned DEFAULT NULL,
  `created_at` datetime DEFAULT CURRENT_TIMESTAMP,
  `created_by` int unsigned DEFAULT NULL,
  `modified_at` datetime DEFAULT NULL,
  `modified_by` int unsigned DEFAULT NULL,
  `entity_id` int unsigned NOT NULL,
  `group_id` smallint unsigned NOT NULL,
  `access_id` smallint unsigned NOT NULL,
  PRIMARY KEY (`entity_id`,`group_id`,`access_id`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_0900_ai_ci;

/*Data for the table `sys_group_access` */

insert  into `sys_group_access`(`id`,`notes`,`deleted`,`deleted_at`,`deleted_by`,`created_at`,`created_by`,`modified_at`,`modified_by`,`entity_id`,`group_id`,`access_id`) values 
(NULL,NULL,0,NULL,NULL,'2023-03-01 03:36:19',NULL,NULL,NULL,1,1,1),
(NULL,NULL,0,NULL,NULL,'2023-03-08 08:03:27',NULL,NULL,NULL,1,1,2);

/*Table structure for table `sys_language` */

DROP TABLE IF EXISTS `sys_language`;

CREATE TABLE `sys_language` (
  `id` tinyint unsigned NOT NULL,
  `notes` text,
  `deleted` tinyint(1) DEFAULT '0',
  `deleted_at` datetime DEFAULT NULL,
  `deleted_by` int unsigned DEFAULT NULL,
  `created_at` datetime DEFAULT CURRENT_TIMESTAMP,
  `created_by` int unsigned DEFAULT NULL,
  `modified_at` datetime DEFAULT NULL,
  `modified_by` int unsigned DEFAULT NULL,
  `entity_id` int unsigned DEFAULT NULL,
  `name` varchar(50) DEFAULT NULL,
  PRIMARY KEY (`id`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_0900_ai_ci;

/*Data for the table `sys_language` */

insert  into `sys_language`(`id`,`notes`,`deleted`,`deleted_at`,`deleted_by`,`created_at`,`created_by`,`modified_at`,`modified_by`,`entity_id`,`name`) values 
(1,NULL,0,NULL,NULL,'2023-03-10 08:17:13',NULL,NULL,NULL,NULL,'Bahasa Indonesia'),
(2,NULL,0,NULL,NULL,'2023-03-10 08:17:17',NULL,NULL,NULL,NULL,'English');

/*Table structure for table `sys_list` */

DROP TABLE IF EXISTS `sys_list`;

CREATE TABLE `sys_list` (
  `id` tinyint unsigned NOT NULL,
  `notes` text,
  `deleted` tinyint(1) DEFAULT '0',
  `deleted_at` datetime DEFAULT NULL,
  `deleted_by` int unsigned DEFAULT NULL,
  `created_at` datetime DEFAULT CURRENT_TIMESTAMP,
  `created_by` int unsigned DEFAULT NULL,
  `modified_at` datetime DEFAULT NULL,
  `modified_by` int unsigned DEFAULT NULL,
  `entity_id` int unsigned NOT NULL,
  `name` varchar(50) CHARACTER SET utf8mb4 COLLATE utf8mb4_0900_ai_ci NOT NULL,
  `item_name` varchar(50) CHARACTER SET utf8mb4 COLLATE utf8mb4_0900_ai_ci DEFAULT NULL,
  PRIMARY KEY (`entity_id`,`name`,`id`),
  UNIQUE KEY `uk_id` (`entity_id`,`id`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_0900_ai_ci;

/*Data for the table `sys_list` */

insert  into `sys_list`(`id`,`notes`,`deleted`,`deleted_at`,`deleted_by`,`created_at`,`created_by`,`modified_at`,`modified_by`,`entity_id`,`name`,`item_name`) values 
(1,NULL,0,NULL,NULL,'2022-12-11 23:13:52',NULL,NULL,NULL,1,'Entity Types','Headquarter'),
(2,NULL,0,NULL,NULL,'2022-12-11 23:14:17',NULL,NULL,NULL,1,'Entity Types','Branch'),
(3,NULL,0,NULL,NULL,'2022-12-11 23:15:17',NULL,NULL,NULL,1,'Entity Types','Warehouse'),
(4,NULL,0,NULL,NULL,'2022-12-11 23:15:52',NULL,NULL,NULL,1,'Entity Types','Representative Office');

/*Table structure for table `sys_menu` */

DROP TABLE IF EXISTS `sys_menu`;

CREATE TABLE `sys_menu` (
  `id` int unsigned NOT NULL,
  `notes` text CHARACTER SET utf8mb4 COLLATE utf8mb4_0900_ai_ci,
  `deleted` tinyint(1) DEFAULT '0',
  `deleted_at` datetime DEFAULT NULL,
  `deleted_by` int unsigned DEFAULT NULL,
  `created_at` datetime DEFAULT CURRENT_TIMESTAMP,
  `created_by` int unsigned DEFAULT NULL,
  `modified_at` datetime DEFAULT NULL,
  `modified_by` int unsigned DEFAULT NULL,
  `entity_id` int unsigned DEFAULT NULL,
  `parent_id` int unsigned DEFAULT NULL,
  `has_child` tinyint(1) DEFAULT '0',
  `level` tinyint unsigned DEFAULT NULL,
  `order` smallint unsigned DEFAULT NULL,
  `icon` varchar(100) DEFAULT NULL,
  `route` varchar(100) DEFAULT NULL,
  `access_id` smallint unsigned DEFAULT NULL,
  PRIMARY KEY (`id`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_0900_ai_ci;

/*Data for the table `sys_menu` */

insert  into `sys_menu`(`id`,`notes`,`deleted`,`deleted_at`,`deleted_by`,`created_at`,`created_by`,`modified_at`,`modified_by`,`entity_id`,`parent_id`,`has_child`,`level`,`order`,`icon`,`route`,`access_id`) values 
(1,NULL,0,NULL,NULL,'2023-01-14 11:17:00',NULL,NULL,NULL,NULL,NULL,0,1,NULL,'fa-solid fa-chart-simple','/main/dashboard',1),
(2,NULL,0,NULL,NULL,'2023-03-08 08:05:07',NULL,NULL,NULL,NULL,NULL,1,1,NULL,'fa-solid fa-route',NULL,NULL),
(3,NULL,0,NULL,NULL,'2023-03-15 04:03:16',NULL,NULL,NULL,NULL,2,0,2,NULL,'fa-solid fa-address-card','/main/medical-record/admission',2),
(4,NULL,0,NULL,NULL,'2023-03-15 07:45:19',NULL,NULL,NULL,NULL,2,0,2,NULL,'fa-solid fa-arrows-to-eye','/main/assessment',2),
(5,NULL,0,NULL,NULL,'2023-03-15 08:00:18',NULL,NULL,NULL,NULL,2,0,2,NULL,'fa-solid fa-stethoscope','/main/diagnosis',2),
(6,NULL,0,NULL,NULL,'2023-03-15 08:19:25',NULL,NULL,NULL,NULL,NULL,1,1,NULL,'fa-solid fa-toolbox',NULL,NULL),
(7,NULL,0,NULL,NULL,'2023-03-15 08:22:49',NULL,NULL,NULL,NULL,6,0,2,NULL,'fa-solid fa-microscope','/main/laboratory',2),
(8,NULL,0,NULL,NULL,'2023-03-15 08:32:18',NULL,NULL,NULL,NULL,6,0,2,NULL,'fa-solid fa-radiation','/main/radiology',2),
(9,NULL,0,NULL,NULL,'2023-03-15 10:31:04',NULL,NULL,NULL,NULL,6,0,2,NULL,'fa-solid fa-circle','/main/physiotherapy',2),
(10,NULL,0,NULL,NULL,'2023-03-16 02:25:30',NULL,NULL,NULL,NULL,2,0,2,NULL,'fa-solid fa-cash-register','/main/billing',2),
(11,NULL,0,NULL,NULL,'2023-03-16 03:03:55',NULL,NULL,NULL,NULL,2,0,2,NULL,'fa-solid fa-pills','/main/pharmacy/orders',2),
(12,NULL,0,NULL,NULL,'2023-03-28 07:10:03',NULL,NULL,NULL,NULL,NULL,1,1,NULL,'fa-solid fa-circle',NULL,NULL),
(13,NULL,0,NULL,NULL,'2023-03-28 07:19:46',NULL,NULL,NULL,NULL,12,0,2,NULL,'fa-solid fa-hospital-user','/main/medical-record/patient',2),
(14,NULL,0,NULL,NULL,'2023-03-28 08:27:21',NULL,NULL,NULL,NULL,12,0,2,NULL,'fa-solid fa-user-nurse','/main/hrd/paramedic',2);

/*Table structure for table `sys_module` */

DROP TABLE IF EXISTS `sys_module`;

CREATE TABLE `sys_module` (
  `id` smallint unsigned NOT NULL,
  `notes` text,
  `deleted` tinyint(1) DEFAULT '0',
  `deleted_at` datetime DEFAULT NULL,
  `deleted_by` int unsigned DEFAULT NULL,
  `created_at` datetime DEFAULT CURRENT_TIMESTAMP,
  `created_by` int unsigned DEFAULT NULL,
  `modified_at` datetime DEFAULT NULL,
  `modified_by` int unsigned DEFAULT NULL,
  `entity_id` int unsigned DEFAULT NULL,
  `name` varchar(100) DEFAULT NULL,
  PRIMARY KEY (`id`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_0900_ai_ci;

/*Data for the table `sys_module` */

insert  into `sys_module`(`id`,`notes`,`deleted`,`deleted_at`,`deleted_by`,`created_at`,`created_by`,`modified_at`,`modified_by`,`entity_id`,`name`) values 
(1,NULL,0,NULL,NULL,'2023-03-01 03:26:48',NULL,NULL,NULL,NULL,'overall.dashboard'),
(2,NULL,0,NULL,NULL,'2023-03-08 07:58:43',NULL,NULL,NULL,NULL,'registration');

/*Table structure for table `sys_salutation` */

DROP TABLE IF EXISTS `sys_salutation`;

CREATE TABLE `sys_salutation` (
  `id` tinyint unsigned NOT NULL,
  `notes` text,
  `deleted` tinyint(1) DEFAULT '0',
  `deleted_at` datetime DEFAULT NULL,
  `deleted_by` int unsigned DEFAULT NULL,
  `created_at` datetime DEFAULT CURRENT_TIMESTAMP,
  `created_by` int unsigned DEFAULT NULL,
  `modified_at` datetime DEFAULT NULL,
  `modified_by` int unsigned DEFAULT NULL,
  `entity_id` int unsigned DEFAULT NULL,
  `name` varchar(10) CHARACTER SET utf8mb4 COLLATE utf8mb4_0900_ai_ci DEFAULT NULL,
  PRIMARY KEY (`id`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_0900_ai_ci;

/*Data for the table `sys_salutation` */

insert  into `sys_salutation`(`id`,`notes`,`deleted`,`deleted_at`,`deleted_by`,`created_at`,`created_by`,`modified_at`,`modified_by`,`entity_id`,`name`) values 
(1,'Mr',0,NULL,NULL,'2022-12-11 04:10:30',NULL,NULL,NULL,NULL,NULL),
(2,'Mrs',0,NULL,NULL,'2022-12-11 04:10:45',NULL,NULL,NULL,NULL,NULL),
(3,'Ms',0,NULL,NULL,'2022-12-11 04:11:00',NULL,NULL,NULL,NULL,NULL),
(4,'Tn',0,NULL,NULL,'2022-12-11 04:11:38',NULL,NULL,NULL,NULL,NULL),
(5,'Ny',0,NULL,NULL,'2022-12-11 04:11:57',NULL,NULL,NULL,NULL,NULL),
(6,'Bp',0,NULL,NULL,'2022-12-11 04:12:06',NULL,NULL,NULL,NULL,NULL),
(7,'Ibu',0,NULL,NULL,'2022-12-11 04:12:21',NULL,NULL,NULL,NULL,NULL),
(8,'Nn',0,NULL,NULL,'2022-12-11 04:12:54',NULL,NULL,NULL,NULL,NULL),
(9,'An',0,NULL,NULL,'2022-12-11 04:13:10',NULL,NULL,NULL,NULL,NULL);

/*Table structure for table `sys_state` */

DROP TABLE IF EXISTS `sys_state`;

CREATE TABLE `sys_state` (
  `id` int unsigned NOT NULL,
  `notes` text,
  `deleted` tinyint(1) DEFAULT '0',
  `deleted_at` datetime DEFAULT NULL,
  `deleted_by` int unsigned DEFAULT NULL,
  `created_at` datetime DEFAULT CURRENT_TIMESTAMP,
  `created_by` int unsigned DEFAULT NULL,
  `modified_at` datetime DEFAULT NULL,
  `modified_by` int unsigned DEFAULT NULL,
  `entity_id` int unsigned DEFAULT NULL,
  `code` varchar(2) CHARACTER SET utf8mb4 COLLATE utf8mb4_0900_ai_ci DEFAULT NULL,
  `name` varchar(100) CHARACTER SET utf8mb4 COLLATE utf8mb4_0900_ai_ci DEFAULT NULL,
  `name_2nd` varchar(100) CHARACTER SET utf8mb4 COLLATE utf8mb4_0900_ai_ci DEFAULT NULL,
  `nickname` varchar(50) CHARACTER SET utf8mb4 COLLATE utf8mb4_0900_ai_ci DEFAULT NULL,
  `country_id` tinyint unsigned DEFAULT NULL,
  PRIMARY KEY (`id`),
  KEY `fk_sys_state_country` (`country_id`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_0900_ai_ci;

/*Data for the table `sys_state` */

insert  into `sys_state`(`id`,`notes`,`deleted`,`deleted_at`,`deleted_by`,`created_at`,`created_by`,`modified_at`,`modified_by`,`entity_id`,`code`,`name`,`name_2nd`,`nickname`,`country_id`) values 
(1,NULL,0,NULL,NULL,'2023-03-23 21:48:14',NULL,NULL,NULL,NULL,NULL,'Banten',NULL,NULL,1);

/*Table structure for table `sys_user` */

DROP TABLE IF EXISTS `sys_user`;

CREATE TABLE `sys_user` (
  `id` int unsigned NOT NULL AUTO_INCREMENT,
  `notes` text,
  `deleted` tinyint(1) DEFAULT '0',
  `deleted_at` datetime DEFAULT NULL,
  `deleted_by` int unsigned DEFAULT NULL,
  `created_at` datetime DEFAULT CURRENT_TIMESTAMP,
  `created_by` int unsigned DEFAULT NULL,
  `modified_at` datetime DEFAULT NULL,
  `modified_by` int unsigned DEFAULT NULL,
  `entity_id` int unsigned DEFAULT NULL,
  `password` blob,
  `activation_code` varchar(6) CHARACTER SET utf8mb4 COLLATE utf8mb4_0900_ai_ci DEFAULT NULL,
  `active` tinyint(1) DEFAULT '0',
  `activated_at` datetime DEFAULT NULL,
  PRIMARY KEY (`id`),
  KEY `fk_sys_user_entity` (`entity_id`)
) ENGINE=InnoDB AUTO_INCREMENT=4 DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_0900_ai_ci;

/*Data for the table `sys_user` */

insert  into `sys_user`(`id`,`notes`,`deleted`,`deleted_at`,`deleted_by`,`created_at`,`created_by`,`modified_at`,`modified_by`,`entity_id`,`password`,`activation_code`,`active`,`activated_at`) values 
(1,NULL,0,NULL,NULL,'2022-12-11 19:38:22',NULL,NULL,NULL,1,'Êu¸ªªè(\0GØ¦ga¿•Í©Ÿ6o”`²9œ¢å',NULL,1,NULL),
(2,NULL,0,NULL,NULL,'2022-12-31 17:34:09',NULL,NULL,NULL,1,'123',NULL,1,NULL);

/*Table structure for table `sys_user_group` */

DROP TABLE IF EXISTS `sys_user_group`;

CREATE TABLE `sys_user_group` (
  `id` int unsigned DEFAULT NULL,
  `notes` text,
  `deleted` tinyint(1) DEFAULT '0',
  `deleted_at` datetime DEFAULT NULL,
  `deleted_by` int unsigned DEFAULT NULL,
  `created_at` datetime DEFAULT CURRENT_TIMESTAMP,
  `created_by` int unsigned DEFAULT NULL,
  `modified_at` datetime DEFAULT NULL,
  `modified_by` int unsigned DEFAULT NULL,
  `user_id` int unsigned DEFAULT NULL,
  `entity_id` int unsigned DEFAULT NULL,
  `group_id` smallint unsigned DEFAULT NULL,
  KEY `fk_sys_user_group_user` (`user_id`),
  KEY `fk_sys_user_group_group` (`entity_id`,`group_id`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_0900_ai_ci;

/*Data for the table `sys_user_group` */

insert  into `sys_user_group`(`id`,`notes`,`deleted`,`deleted_at`,`deleted_by`,`created_at`,`created_by`,`modified_at`,`modified_by`,`user_id`,`entity_id`,`group_id`) values 
(NULL,NULL,0,NULL,NULL,'2022-12-31 14:39:04',NULL,NULL,NULL,1,1,1),
(NULL,NULL,0,NULL,NULL,'2022-12-31 17:36:34',NULL,NULL,NULL,2,1,2),
(NULL,NULL,0,NULL,NULL,'2022-12-30 12:02:59',NULL,NULL,NULL,1,1,2);

/*Table structure for table `sys_user_profile` */

DROP TABLE IF EXISTS `sys_user_profile`;

CREATE TABLE `sys_user_profile` (
  `id` int unsigned NOT NULL AUTO_INCREMENT,
  `notes` text CHARACTER SET utf8mb4 COLLATE utf8mb4_0900_ai_ci,
  `deleted` tinyint(1) DEFAULT '0',
  `deleted_at` datetime DEFAULT NULL,
  `deleted_by` int unsigned DEFAULT NULL,
  `created_at` datetime DEFAULT CURRENT_TIMESTAMP,
  `created_by` int unsigned DEFAULT NULL,
  `modified_at` datetime DEFAULT NULL,
  `modified_by` int unsigned DEFAULT NULL,
  `entity_id` int unsigned DEFAULT NULL,
  `user_id` int unsigned DEFAULT NULL,
  `first_name` varchar(100) DEFAULT NULL,
  `last_name` varchar(100) DEFAULT NULL,
  `salutation_id` tinyint unsigned DEFAULT NULL,
  `about_you` text,
  `birthplace` int unsigned DEFAULT NULL,
  `birthdate` date DEFAULT NULL,
  `street` varchar(100) CHARACTER SET utf8mb4 COLLATE utf8mb4_0900_ai_ci DEFAULT NULL,
  `street_2nd` varchar(100) CHARACTER SET utf8mb4 COLLATE utf8mb4_0900_ai_ci DEFAULT NULL,
  `city_id` smallint unsigned DEFAULT NULL,
  `state_id` tinyint unsigned DEFAULT NULL,
  `country_id` tinyint unsigned DEFAULT NULL,
  `postal_code` varchar(10) CHARACTER SET utf8mb4 COLLATE utf8mb4_0900_ai_ci DEFAULT NULL,
  `mobile_phone` varchar(20) DEFAULT NULL,
  `mobile_phone_2nd` varchar(20) DEFAULT NULL,
  `email` varchar(254) DEFAULT NULL,
  `email_2nd` varchar(254) DEFAULT NULL,
  `whatsapp` varchar(20) DEFAULT NULL,
  `twitter` varchar(20) DEFAULT NULL,
  `facebook` text,
  `instagram` text,
  `linkedin` text,
  `picture_handle` varchar(32) DEFAULT NULL,
  PRIMARY KEY (`id`),
  UNIQUE KEY `key_unique` (`user_id`),
  KEY `fk_sys_user_profile_city` (`country_id`,`state_id`,`city_id`),
  KEY `fk_sys_user_profile_salutation` (`salutation_id`)
) ENGINE=InnoDB AUTO_INCREMENT=2 DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_0900_ai_ci;

/*Data for the table `sys_user_profile` */

insert  into `sys_user_profile`(`id`,`notes`,`deleted`,`deleted_at`,`deleted_by`,`created_at`,`created_by`,`modified_at`,`modified_by`,`entity_id`,`user_id`,`first_name`,`last_name`,`salutation_id`,`about_you`,`birthplace`,`birthdate`,`street`,`street_2nd`,`city_id`,`state_id`,`country_id`,`postal_code`,`mobile_phone`,`mobile_phone_2nd`,`email`,`email_2nd`,`whatsapp`,`twitter`,`facebook`,`instagram`,`linkedin`,`picture_handle`) values 
(1,NULL,0,NULL,NULL,'2022-12-27 02:45:51',NULL,NULL,NULL,NULL,1,'Riza','Anshari',1,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,'rizast@gmail.com',NULL,NULL,NULL,NULL,NULL,NULL,NULL);

/*!40101 SET SQL_MODE=@OLD_SQL_MODE */;
/*!40014 SET FOREIGN_KEY_CHECKS=@OLD_FOREIGN_KEY_CHECKS */;
/*!40014 SET UNIQUE_CHECKS=@OLD_UNIQUE_CHECKS */;
/*!40111 SET SQL_NOTES=@OLD_SQL_NOTES */;
