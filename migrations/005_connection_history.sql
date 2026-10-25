CREATE TABLE weigh_ins_history (
  id TEXT PRIMARY KEY, user_id TEXT NOT NULL REFERENCES users(id), measured_at TEXT NOT NULL,
  weight_kg REAL NOT NULL, source TEXT NOT NULL CHECK (source IN ('manual', 'withings')),
  external_id TEXT, created_at TEXT NOT NULL, updated_at TEXT NOT NULL, deleted_at TEXT,
  withings_connection_id TEXT REFERENCES withings_connections(id) ON DELETE SET NULL,
  UNIQUE(source, external_id)
);
INSERT INTO weigh_ins_history
SELECT id, user_id, measured_at, weight_kg, source, external_id, created_at, updated_at, deleted_at,
       CASE WHEN EXISTS (SELECT 1 FROM withings_connections c WHERE c.id = weigh_ins.withings_connection_id)
            THEN withings_connection_id ELSE NULL END
FROM weigh_ins;
DROP TABLE weigh_ins;
ALTER TABLE weigh_ins_history RENAME TO weigh_ins;
CREATE INDEX weigh_ins_user_measured_at_idx ON weigh_ins(user_id, measured_at);
CREATE INDEX weigh_ins_withings_connection_idx ON weigh_ins(withings_connection_id);
