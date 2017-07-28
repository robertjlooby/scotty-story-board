CREATE TABLE projects_users (
  project_id SERIAL REFERENCES projects(id) ON DELETE CASCADE,
  user_id SERIAL REFERENCES users(id) ON DELETE CASCADE
);

CREATE UNIQUE INDEX ON projects_users (project_id, user_id);
