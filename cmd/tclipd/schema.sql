-- Enable WAL mode to make backups easier with Litestream
PRAGMA journal_mode=WAL;

-- Paste data
CREATE TABLE IF NOT EXISTS pastes
  ( id TEXT PRIMARY KEY NOT NULL
  , created_at TEXT NOT NULL -- RFC 3339 timestamp
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

-- Tokens that allow an anonymous user to set a paste
CREATE TABLE IF NOT EXISTS paste_tokens
    ( id TEXT PRIMARY KEY NOT NULL
    , token TEXT NOT NULL UNIQUE
    , paste_id TEXT NOT NULL
    , description TEXT NOT NULL
    , FOREIGN KEY (paste_id) REFERENCES pastes(id)
    );
