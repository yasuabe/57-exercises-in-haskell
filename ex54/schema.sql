CREATE TABLE IF NOT EXISTS url (
    short_url TEXT PRIMARY KEY,
    long_url TEXT NOT NULL,
    visit_count INTEGER DEFAULT 0
);