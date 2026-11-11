-- withings_connections.user_id is UNIQUE, which already indexes it.
DROP INDEX withings_connections_user_idx;
