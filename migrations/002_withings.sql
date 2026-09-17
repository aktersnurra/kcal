CREATE TABLE withings_connections (
  id TEXT PRIMARY KEY,
  user_id TEXT NOT NULL UNIQUE REFERENCES users(id),
  withings_user_id TEXT UNIQUE,
  access_token_encrypted TEXT,
  refresh_token_encrypted TEXT,
  token_expires_at TEXT,
  sync_cursor INTEGER,
  requires_reauthorization INTEGER NOT NULL DEFAULT 0 CHECK (requires_reauthorization IN (0, 1)),
  created_at TEXT NOT NULL,
  updated_at TEXT NOT NULL
);
CREATE INDEX withings_connections_user_idx ON withings_connections(user_id);

CREATE TABLE withings_oauth_states (
  state_hash TEXT PRIMARY KEY,
  user_id TEXT NOT NULL REFERENCES users(id),
  expires_at TEXT NOT NULL,
  consumed_at TEXT,
  created_at TEXT NOT NULL
);
CREATE INDEX withings_oauth_states_user_expiry_idx ON withings_oauth_states(user_id, expires_at);

ALTER TABLE weigh_ins ADD COLUMN withings_connection_id TEXT REFERENCES withings_connections(id);
CREATE INDEX weigh_ins_withings_connection_idx ON weigh_ins(withings_connection_id);
