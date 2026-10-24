CREATE TABLE products (
  id TEXT PRIMARY KEY,
  user_id TEXT NOT NULL REFERENCES users(id),
  name TEXT NOT NULL,
  basis TEXT NOT NULL,
  calories_kcal INTEGER NOT NULL,
  protein_g REAL NOT NULL,
  carbs_g REAL,
  fat_g REAL,
  source TEXT NOT NULL,
  created_at TEXT NOT NULL,
  updated_at TEXT NOT NULL
);
CREATE INDEX products_user_name_id_idx ON products(user_id, name, id);
