CREATE TABLE projects (
  id SERIAL PRIMARY KEY NOT NULL,
  name TEXT NOT NULL,
  description TEXT
);

CREATE UNIQUE INDEX ON projects (name);
