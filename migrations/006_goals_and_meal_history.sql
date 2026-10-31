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
ALTER TABLE meals ADD COLUMN product_id TEXT REFERENCES products(id);
ALTER TABLE meals ADD COLUMN product_version INTEGER;
ALTER TABLE meals ADD COLUMN product_quantity REAL;
ALTER TABLE meals ADD COLUMN idempotency_key TEXT;
CREATE UNIQUE INDEX meals_user_idempotency_key_idx ON meals(user_id, idempotency_key) WHERE idempotency_key IS NOT NULL;
CREATE INDEX meals_product_idx ON meals(product_id) WHERE product_id IS NOT NULL;
CREATE TABLE meal_revisions (
  meal_id TEXT NOT NULL REFERENCES meals(id),
  revision INTEGER NOT NULL,
  revised_at TEXT NOT NULL,
  reason TEXT,
  eaten_at TEXT NOT NULL,
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
  PRIMARY KEY (meal_id, revision)
);
