# MEMO
## Preparation
### DB Setup
```
$ sqlite3 ex54/urlShortner.db
sqlite> .read ex54/schema.sql
sqlite> .schema url
CREATE TABLE url (
    short_url TEXT PRIMARY KEY,
    long_url TEXT NOT NULL,
    visit_count INTEGER DEFAULT 0
);
sqlite> .exit
$
```
