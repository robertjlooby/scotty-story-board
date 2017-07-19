CREATE TABLE oauth_logins (
  id SERIAL PRIMARY KEY NOT NULL,
  user_id SERIAL REFERENCES users(id),
  provider_name TEXT NOT NULL,
  provider_user_id TEXT NOT NULL
);

CREATE UNIQUE INDEX ON oauth_logins (provider_name, provider_user_id);
