-- Enable WAL mode to make backups easier with Litestream
PRAGMA journal_mode=WAL;

-- Paste data
CREATE TABLE IF NOT EXISTS pastes
  ( id TEXT PRIMARY KEY NOT NULL
  , user_id TEXT NOT NULL
  , filename TEXT NOT NULL
  , data TEXT NOT NULL
  , FOREIGN KEY(user_id) REFERENCES users(id)
  );

-- Correlates to tailcfg.UserProfile
CREATE TABLE IF NOT EXISTS users
  ( id TEXT PRIMARY KEY NOT NULL
  , login_name TEXT NOT NULL
  , display_name TEXT NOT NULL
  , profile_pic_url TEXT NOT NULL
  );
