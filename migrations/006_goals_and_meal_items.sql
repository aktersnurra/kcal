CREATE TABLE goals (
  user_id TEXT PRIMARY KEY REFERENCES users(id),
  timezone TEXT NOT NULL,
  weekday_calorie_target_kcal INTEGER NOT NULL,
  weekend_calorie_target_kcal INTEGER NOT NULL,
  protein_target_g REAL,
  eating_window_start_minute INTEGER,
  eating_window_end_minute INTEGER,
  target_weight_kg REAL,
  updated_at TEXT NOT NULL,
  CHECK ((eating_window_start_minute IS NULL) = (eating_window_end_minute IS NULL))
);
ALTER TABLE products ADD COLUMN version INTEGER NOT NULL DEFAULT 1;
ALTER TABLE products ADD COLUMN basis_quantity REAL;
ALTER TABLE products ADD COLUMN basis_unit TEXT;
CREATE TABLE meal_items (
  id TEXT PRIMARY KEY,
  meal_id TEXT NOT NULL REFERENCES meals(id),
  position INTEGER NOT NULL,
  description TEXT NOT NULL,
  calories_kcal INTEGER NOT NULL,
  protein_g REAL NOT NULL,
  carbs_g REAL,
  fat_g REAL,
  confidence REAL,
  estimate_source TEXT,
  notes TEXT,
  product_id TEXT REFERENCES products(id),
  product_version INTEGER,
  product_quantity REAL,
  created_at TEXT NOT NULL,
  updated_at TEXT NOT NULL,
  deleted_at TEXT,
  CHECK ((product_id IS NULL) = (product_version IS NULL)
     AND (product_id IS NULL) = (product_quantity IS NULL))
);
-- Every existing meal was one estimate: it becomes a meal with that one item,
-- which keeps the meal's id.
INSERT INTO meal_items
SELECT id, id, 0, description, calories_kcal, protein_g, carbs_g, fat_g, confidence,
       estimate_source, notes, NULL, NULL, NULL, created_at, updated_at, NULL
FROM meals;
CREATE INDEX meal_items_meal_idx ON meal_items(meal_id, position);
CREATE INDEX meal_items_product_idx ON meal_items(product_id) WHERE product_id IS NOT NULL;
DROP TRIGGER meals_fts_insert;
DROP TRIGGER meals_fts_update;
DROP TRIGGER meals_fts_delete;
DROP TABLE meals_fts;
ALTER TABLE meals DROP COLUMN description;
ALTER TABLE meals DROP COLUMN calories_kcal;
ALTER TABLE meals DROP COLUMN protein_g;
ALTER TABLE meals DROP COLUMN carbs_g;
ALTER TABLE meals DROP COLUMN fat_g;
ALTER TABLE meals DROP COLUMN confidence;
ALTER TABLE meals DROP COLUMN estimate_source;
ALTER TABLE meals DROP COLUMN notes;
ALTER TABLE meals ADD COLUMN label TEXT;
ALTER TABLE meals ADD COLUMN idempotency_key TEXT;
CREATE UNIQUE INDEX meals_user_idempotency_key_idx ON meals(user_id, idempotency_key) WHERE idempotency_key IS NOT NULL;
CREATE VIRTUAL TABLE meal_items_fts USING fts5(description, estimate_source, notes, content='meal_items', content_rowid='rowid', tokenize='unicode61 remove_diacritics 0');
INSERT INTO meal_items_fts(meal_items_fts) VALUES ('rebuild');
CREATE TRIGGER meal_items_fts_insert AFTER INSERT ON meal_items BEGIN
  INSERT INTO meal_items_fts(rowid, description, estimate_source, notes) VALUES (new.rowid, new.description, new.estimate_source, new.notes);
END;
CREATE TRIGGER meal_items_fts_update AFTER UPDATE ON meal_items BEGIN
  INSERT INTO meal_items_fts(meal_items_fts, rowid, description, estimate_source, notes) VALUES ('delete', old.rowid, old.description, old.estimate_source, old.notes);
  INSERT INTO meal_items_fts(rowid, description, estimate_source, notes) VALUES (new.rowid, new.description, new.estimate_source, new.notes);
END;
CREATE TRIGGER meal_items_fts_delete AFTER DELETE ON meal_items BEGIN
  INSERT INTO meal_items_fts(meal_items_fts, rowid, description, estimate_source, notes) VALUES ('delete', old.rowid, old.description, old.estimate_source, old.notes);
END;
CREATE TABLE meal_item_revisions (
  item_id TEXT NOT NULL REFERENCES meal_items(id),
  revision INTEGER NOT NULL,
  revised_at TEXT NOT NULL,
  reason TEXT,
  description TEXT NOT NULL,
  calories_kcal INTEGER NOT NULL,
  protein_g REAL NOT NULL,
  carbs_g REAL,
  fat_g REAL,
  confidence REAL,
  estimate_source TEXT,
  notes TEXT,
  product_id TEXT,
  product_version INTEGER,
  product_quantity REAL,
  PRIMARY KEY (item_id, revision)
);
