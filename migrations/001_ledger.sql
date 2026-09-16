CREATE TABLE schema_migrations (version INTEGER PRIMARY KEY, applied_at TEXT NOT NULL);
CREATE TABLE users (
  id TEXT PRIMARY KEY, oidc_issuer TEXT NOT NULL, oidc_subject TEXT NOT NULL,
  created_at TEXT NOT NULL, UNIQUE (oidc_issuer, oidc_subject)
);
CREATE TABLE meals (
  id TEXT PRIMARY KEY, user_id TEXT NOT NULL REFERENCES users(id), eaten_at TEXT NOT NULL,
  description TEXT NOT NULL, calories_kcal INTEGER NOT NULL, protein_g REAL NOT NULL,
  carbs_g REAL, fat_g REAL, confidence REAL, estimate_source TEXT, notes TEXT,
  created_at TEXT NOT NULL, updated_at TEXT NOT NULL, deleted_at TEXT
);
CREATE INDEX meals_user_eaten_at_idx ON meals(user_id, eaten_at);
CREATE TABLE weigh_ins (
  id TEXT PRIMARY KEY, user_id TEXT NOT NULL REFERENCES users(id), measured_at TEXT NOT NULL,
  weight_kg REAL NOT NULL, source TEXT NOT NULL CHECK (source IN ('manual', 'withings')),
  external_id TEXT, created_at TEXT NOT NULL, updated_at TEXT NOT NULL, deleted_at TEXT,
  UNIQUE(source, external_id)
);
CREATE INDEX weigh_ins_user_measured_at_idx ON weigh_ins(user_id, measured_at);
