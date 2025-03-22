insert into m_kind_category (id, name, description) values (1, "test", "test");
insert into m_purpose_category (id, name, description) values (1, "test", "test");
insert into m_place_category (id, name, description) values (1, "test", "test");
insert into m_kind_element (id, name, description, priority, category_id) values (1, "test", "test", 1, 1);
insert into m_purpose_element (id, name, description, priority, category_id) values (1, "test", "test", 1, 1);
insert into m_place_element (id, name, description, priority, category_id) values (1, "test", "test", 1, 1);
insert into m_balance (id, amount, item, kind_element_id, purpose_element_id, place_element_id, date) values (1, 234, "hoge", 1, 1, 1, "2025-03-22");
