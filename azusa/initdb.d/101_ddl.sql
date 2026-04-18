
CREATE TABLE `m_kind_category` (
  `id` bigint(20) unsigned NOT NULL AUTO_INCREMENT,
  `name` varchar(20) DEFAULT NULL,
  `description` varchar(20) DEFAULT NULL,
  PRIMARY KEY (`id`),
  UNIQUE KEY `m_kind_category_name_unique` (`name`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_bin;

CREATE TABLE `m_purpose_category` (
  `id` bigint(20) unsigned NOT NULL AUTO_INCREMENT,
  `name` varchar(20) DEFAULT NULL,
  `description` varchar(20) DEFAULT NULL,
  PRIMARY KEY (`id`),
  UNIQUE KEY `m_purpose_category_name_unique` (`name`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_bin;

CREATE TABLE `m_place_category` (
  `id` bigint(20) unsigned NOT NULL AUTO_INCREMENT,
  `name` varchar(20) DEFAULT NULL,
  `description` varchar(20) DEFAULT NULL,
  PRIMARY KEY (`id`),
  UNIQUE KEY `m_place_category_name_unique` (`name`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_bin;

CREATE TABLE `m_kind_element` (
  `id` bigint(20) unsigned NOT NULL AUTO_INCREMENT,
  `name` varchar(20) DEFAULT NULL,
  `description` varchar(20) DEFAULT NULL,
  `priority` int(11) DEFAULT 0,
  `category_id` bigint(20) unsigned NOT NULL,
  PRIMARY KEY (`id`),
  UNIQUE KEY `m_kind_element_name_unique` (`name`),
  KEY `m_kind_element_category_id_foreign` (`category_id`),
  CONSTRAINT `m_kind_element_category_id_foreign` FOREIGN KEY (`category_id`) REFERENCES `m_kind_category` (`id`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_bin;

CREATE TABLE `m_purpose_element` (
  `id` bigint(20) unsigned NOT NULL AUTO_INCREMENT,
  `name` varchar(20) DEFAULT NULL,
  `description` varchar(20) DEFAULT NULL,
  `priority` int(11) DEFAULT 0,
  `category_id` bigint(20) unsigned NOT NULL,
  PRIMARY KEY (`id`),
  UNIQUE KEY `m_purpose_element_name_unique` (`name`),
  KEY `m_purpose_element_category_id_foreign` (`category_id`),
  CONSTRAINT `m_purpose_element_category_id_foreign` FOREIGN KEY (`category_id`) REFERENCES `m_purpose_category` (`id`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_bin;

CREATE TABLE `m_place_element` (
  `id` bigint(20) unsigned NOT NULL AUTO_INCREMENT,
  `name` varchar(20) DEFAULT NULL,
  `description` varchar(20) DEFAULT NULL,
  `priority` int(11) DEFAULT 0,
  `category_id` bigint(20) unsigned NOT NULL,
  PRIMARY KEY (`id`),
  UNIQUE KEY `m_place_element_name_unique` (`name`),
  KEY `m_place_element_category_id_foreign` (`category_id`),
  CONSTRAINT `m_place_element_category_id_foreign` FOREIGN KEY (`category_id`) REFERENCES `m_place_category` (`id`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_bin;

CREATE TABLE `m_balance` (
  `id` bigint(20) unsigned NOT NULL AUTO_INCREMENT,
  `amount` int(11) NOT NULL,
  `item` varchar(50) DEFAULT NULL,
  `kind_element_id` bigint(20) unsigned NOT NULL,
  `purpose_element_id` bigint(20) unsigned NOT NULL,
  `place_element_id` bigint(20) unsigned NOT NULL,
  `date` date NOT NULL,
  `group_id` bigint(20) unsigned DEFAULT NULL,
  PRIMARY KEY (`id`),
  KEY `m_balance_kind_element_id_foreign` (`kind_element_id`),
  KEY `m_balance_purpose_element_id_foreign` (`purpose_element_id`),
  KEY `m_balance_place_element_id_foreign` (`place_element_id`),
  CONSTRAINT `m_balance_kind_element_id_foreign` FOREIGN KEY (`kind_element_id`) REFERENCES `m_kind_element` (`id`),
  CONSTRAINT `m_balance_place_element_id_foreign` FOREIGN KEY (`place_element_id`) REFERENCES `m_place_element` (`id`),
  CONSTRAINT `m_balance_purpose_element_id_foreign` FOREIGN KEY (`purpose_element_id`) REFERENCES `m_purpose_element` (`id`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_bin;

CREATE TABLE `m_fixed_balance` (
  `id` bigint(20) unsigned NOT NULL AUTO_INCREMENT,
  `amount` int(11) NOT NULL,
  `item` varchar(50) NOT NULL,
  `kind_element_id` bigint(20) unsigned NOT NULL,
  `purpose_element_id` bigint(20) unsigned NOT NULL,
  `place_element_id` bigint(20) unsigned NOT NULL,
  PRIMARY KEY (`id`),
  KEY `idx_kind_element_id` (`kind_element_id`),
  KEY `idx_purpose_element_id` (`purpose_element_id`),
  KEY `idx_place_element_id` (`place_element_id`),
  CONSTRAINT `fk_m_fixed_balance_kind_element` FOREIGN KEY (`kind_element_id`) REFERENCES `m_kind_element` (`id`) ON UPDATE CASCADE,
  CONSTRAINT `fk_m_fixed_balance_place_element` FOREIGN KEY (`place_element_id`) REFERENCES `m_place_element` (`id`) ON UPDATE CASCADE,
  CONSTRAINT `fk_m_fixed_balance_urpose_element` FOREIGN KEY (`purpose_element_id`) REFERENCES `m_purpose_element` (`id`) ON UPDATE CASCADE
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_bin;

CREATE TABLE `r_check_place_sum` (
  `id` bigint(20) unsigned NOT NULL AUTO_INCREMENT,
  `sum` int(11) NOT NULL,
  `place_element_id` bigint(20) unsigned NOT NULL,
  `date` date NOT NULL,
  PRIMARY KEY (`id`),
  KEY `m_balance_place_element_id_foreign` (`place_element_id`),
  CONSTRAINT `r_check_place_sum_place_element_id_foreign` FOREIGN KEY (`place_element_id`) REFERENCES `m_place_element` (`id`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_bin;

CREATE TABLE template (
    id INT NOT NULL AUTO_INCREMENT,
    name VARCHAR(20) NOT NULL,
    PRIMARY KEY (id)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4;

CREATE TABLE template_detail (
    template_id INT NOT NULL,
    seq INT NOT NULL,
    type TINYINT NOT NULL,
    amount INT NOT NULL,
    item VARCHAR(50) NOT NULL,
    kind_element_id BIGINT UNSIGNED NOT NULL,
    purpose_element_id BIGINT UNSIGNED NULL,
    place_element_id BIGINT UNSIGNED NULL,
    move_before_purpose_id BIGINT UNSIGNED NULL,
    move_after_purpose_id BIGINT UNSIGNED NULL,
    move_before_place_id BIGINT UNSIGNED NULL,
    move_after_place_id BIGINT UNSIGNED NULL,
    PRIMARY KEY (template_id, seq),
    CONSTRAINT fk_detail_template
        FOREIGN KEY (template_id) REFERENCES template (id)
        ON DELETE CASCADE
        ON UPDATE RESTRICT,
    CONSTRAINT fk_detail_kind_element
        FOREIGN KEY (kind_element_id) REFERENCES m_kind_element (id)
        ON UPDATE RESTRICT,
    CONSTRAINT fk_detail_purpose_element
        FOREIGN KEY (purpose_element_id) REFERENCES m_purpose_element (id)
        ON UPDATE RESTRICT,
    CONSTRAINT fk_detail_place_element
        FOREIGN KEY (place_element_id) REFERENCES m_place_element (id)
        ON UPDATE RESTRICT,
    CONSTRAINT fk_detail_move_before_purpose
        FOREIGN KEY (move_before_purpose_id) REFERENCES m_purpose_element (id)
        ON UPDATE RESTRICT,
    CONSTRAINT fk_detail_move_after_purpose
        FOREIGN KEY (move_after_purpose_id) REFERENCES m_purpose_element (id)
        ON UPDATE RESTRICT,
    CONSTRAINT fk_detail_move_before_place
        FOREIGN KEY (move_before_place_id) REFERENCES m_place_element (id)
        ON UPDATE RESTRICT,
    CONSTRAINT fk_detail_move_after_place
        FOREIGN KEY (move_after_place_id) REFERENCES m_place_element (id)
        ON UPDATE RESTRICT,
    CONSTRAINT chk_type
        CHECK (type IN (1, 2, 3))
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4;
