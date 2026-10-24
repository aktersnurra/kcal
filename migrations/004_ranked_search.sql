CREATE VIRTUAL TABLE products_fts USING fts5(name, basis, source, content='products', content_rowid='rowid', tokenize='unicode61 remove_diacritics 0');
INSERT INTO products_fts(products_fts) VALUES ('rebuild');
CREATE TRIGGER products_fts_insert AFTER INSERT ON products BEGIN
  INSERT INTO products_fts(rowid, name, basis, source) VALUES (new.rowid, new.name, new.basis, new.source);
END;
CREATE TRIGGER products_fts_update AFTER UPDATE ON products BEGIN
  INSERT INTO products_fts(products_fts, rowid, name, basis, source) VALUES ('delete', old.rowid, old.name, old.basis, old.source);
  INSERT INTO products_fts(rowid, name, basis, source) VALUES (new.rowid, new.name, new.basis, new.source);
END;
CREATE TRIGGER products_fts_delete AFTER DELETE ON products BEGIN
  INSERT INTO products_fts(products_fts, rowid, name, basis, source) VALUES ('delete', old.rowid, old.name, old.basis, old.source);
END;
CREATE VIRTUAL TABLE meals_fts USING fts5(description, estimate_source, notes, content='meals', content_rowid='rowid', tokenize='unicode61 remove_diacritics 0');
INSERT INTO meals_fts(meals_fts) VALUES ('rebuild');
CREATE TRIGGER meals_fts_insert AFTER INSERT ON meals BEGIN
  INSERT INTO meals_fts(rowid, description, estimate_source, notes) VALUES (new.rowid, new.description, new.estimate_source, new.notes);
END;
CREATE TRIGGER meals_fts_update AFTER UPDATE ON meals BEGIN
  INSERT INTO meals_fts(meals_fts, rowid, description, estimate_source, notes) VALUES ('delete', old.rowid, old.description, old.estimate_source, old.notes);
  INSERT INTO meals_fts(rowid, description, estimate_source, notes) VALUES (new.rowid, new.description, new.estimate_source, new.notes);
END;
CREATE TRIGGER meals_fts_delete AFTER DELETE ON meals BEGIN
  INSERT INTO meals_fts(meals_fts, rowid, description, estimate_source, notes) VALUES ('delete', old.rowid, old.description, old.estimate_source, old.notes);
END;
